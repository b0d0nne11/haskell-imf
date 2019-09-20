{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Text.IMF.Network.Client
  ( ClientParams(..)
  , ConnParams(..)
  , TLSParams(..)
  , AuthParams(..)
  , ClientLog
  , Client(..)
  , Envelope(..)
  , newClient
  , deliver
  , termClient
  , runChat
  , connect
  , greeting
  , hello
  , startTLS
  , auth
  , mailFrom
  , rcptTo
  , dataInit
  , dataBlock
  , dataTerm
  , quit
  , disconnect
  )
where

import           Control.Applicative         (empty, (<|>))
import           Control.Exception           (catch, throwIO)
import           Control.Monad               (unless)
import           Control.Monad.Except        (ExceptT, catchError, liftEither,
                                              runExceptT, throwError)
import           Control.Monad.Reader        (MonadReader, asks)
import           Control.Monad.RWS           (RWST, execRWST)
import           Control.Monad.State         (MonadState, gets, modify)
import           Control.Monad.Trans         (liftIO)
import           Control.Monad.Writer        (MonadWriter, tell)
import           Data.Attoparsec.ByteString  (IResult (..), parse)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as B
import qualified Data.ByteString.Base64      as B64
import qualified Data.ByteString.Char8       as C
import qualified Data.ByteString.Lazy        as LB
import           Data.Char                   (toLower)
import           Data.Default.Class          (Default, def)
import           Data.List                   (sortOn)
import           Data.Maybe                  (fromJust)
import           Data.Time.Clock             (NominalDiffTime, UTCTime,
                                              addUTCTime, diffUTCTime,
                                              getCurrentTime)
import           Network.DNS.Lookup          (lookupA, lookupMX)
import           Network.DNS.Resolver        (Resolver, defaultResolvConf,
                                              makeResolvSeed, withResolver)
import           System.Timeout              (timeout)


import           Text.IMF.Format             (formatMessage)
import           Text.IMF.Mailbox            (Mailbox (..), mboxAngleAddr)
import           Text.IMF.Network.Connection (Connection)
import qualified Text.IMF.Network.Connection as Connection
import           Text.IMF.Network.Errors     (ClientException (..))
import           Text.IMF.Parsers.Client     (pReply)

-- | Connection parameters
data ConnParams = ConnParams
    { connSourceIP        :: Maybe String   -- ^ specify source IP
    , connRecipientDomain :: String         -- ^ recipient domain
    , connProxyHosts      :: Maybe [String] -- ^ proxy hosts, skips recipient domain lookup
    }
  deriving (Show, Eq)

-- | Opportunistic TLS parameters
data TLSParams = TLSParams
    { tlsRequired :: Bool -- ^ require TLS?
    , tlsValidate :: Bool -- ^ validate certificate?
    }
  deriving (Show, Eq)

-- | Authentication parameters
data AuthParams = AuthParams
    { authRequired :: Bool   -- ^ require authentication?
    , authUsername :: String -- ^ username
    , authPassword :: String -- ^ password
    }
  deriving (Show, Eq)

-- | Client parameters
data ClientParams = ClientParams
    { clientName :: ByteString       -- ^ client name
    , connParams :: ConnParams       -- ^ connection parameters
    , tlsParams  :: Maybe TLSParams  -- ^ opportunistic tls parameters
    , authParams :: Maybe AuthParams -- ^ authentication parameters
    }
  deriving (Show, Eq)

-- | Message envelope
data Envelope = Envelope
    { sender    :: Mailbox -- ^ sender
    , recipient :: Mailbox -- ^ recipient
    }
  deriving (Show, Eq)

data LogEntryType = Send | Recv | Both
  deriving (Show, Eq)

-- | SMTP chat log
type ClientLog = [(UTCTime, LogEntryType, ByteString)]

-- | SMTP chat client
data Client = Client
    { connection  :: Maybe Connection          -- ^ mx server connection
    , hostname    :: String                    -- ^ mx server hostname
    , extensions  :: [(String, [String])]      -- ^ a list of supported extensions and parameters
    , lastCommand :: Maybe LB.ByteString       -- ^ the last command sent
    , lastReply   :: Maybe (Int, [ByteString]) -- ^ the last reply received
    , lastErrors  :: [ClientException]         -- ^ any errors encountered
    }
  deriving (Show)

instance Default Client where
    def = Client
        { connection  = Nothing
        , hostname    = ""
        , extensions  = []
        , lastCommand = Nothing
        , lastReply   = Nothing
        , lastErrors  = []
        }

newClient :: ClientParams -- ^ client parameters
          -> IO (Client, ClientLog)
newClient clientParams =
    initClient clientParams $ Client
        { connection  = Nothing
        , hostname    = ""
        , extensions  = []
        , lastCommand = Nothing
        , lastReply   = Nothing
        , lastErrors  = []
        }

-- | Initialize a client
--
-- Gets a client ready for sending by performing the connection, greeting,
-- hello, auth, and starttls commands. This should only be run once per client
-- and must be completed before attempting to deliver any messages.
--
initClient :: ClientParams -- ^ client parameters
           -> Client       -- ^ client
           -> IO (Client, ClientLog)
initClient (ClientParams clientName connParams tlsParams authParams) =
    runChat $ do
        connect connParams
        greeting
        hello clientName
        startTLS clientName tlsParams
        auth authParams

-- | Deliver a message
--
-- Deliver a message by performing the mail from, recipient to, and data
-- commands. This may be run many times per client but the exact number allowed
-- depends on the receiving MX server.
--
-- The envelope sender and recipient here may differ from the message from and
-- to/cc/bcc headers for a variety of reasons. Also, the message body is sent
-- verbatim so any required modifications like message signing or masking BCC
-- addresses should be done before calling `deliver`.
--
deliver :: Envelope      -- ^ envelope
        -> LB.ByteString -- ^ body
        -> Client        -- ^ client
        -> IO (Client, ClientLog)
deliver (Envelope sender recipeint) body =
    runChat $ do
        mailFrom sender
        rcptTo recipeint
        dataInit
        dataBlock body
        dataTerm

-- | Terminate a client
--
-- Closes a client gracefully by sending the quit command. This should only be
-- run once per client and only after all message deliveries are complete.
--
termClient :: Client -- ^ client
           -> IO (Client, ClientLog)
termClient =
    runChat $ do
        quit
        disconnect

-- | SMTP chat monad
type Chat = ExceptT [ClientException] (RWST () ClientLog Client IO)

-- | Unwrap a SMTP chat monad into an IO action
runChat :: Chat () -- ^ chat monad
        -> Client  -- ^ client
        -> IO (Client, ClientLog)
runChat f = execRWST (runExceptT $ f `catchError` handleError) ()
  where
    handleError es = modify $ \s -> s { lastErrors = es }

-- | Lookup and connect to the MX server
connect :: ConnParams -> Chat ()
connect (ConnParams srcIP recipientDomain proxyHosts) = do
    hosts <- maybe (liftChat $ resolveMX recipientDomain) return proxyHosts
    foldl (<|>) empty $ with hosts $ \host ->
        foldl (<|>) empty $ with ports $ \port -> do
            dstIP <- liftChat $ resolveA host
            conn <- liftChat $ Connection.open srcIP dstIP port
            modify $ \s -> s { connection = Just conn
                             , hostname   = host
                             }
            return ()
  where
    ports = ["25", "587", "2525"]
    with = flip map
    resolveMX :: String -> IO [String]
    resolveMX "localhost" = return ["localhost"]
    resolveMX domain = do
        rseed <- makeResolvSeed defaultResolvConf
        withResolver rseed $ \resolver -> do
            records <- lookupMX resolver $ C.pack domain
            case records of
                Left _   -> throwIO DNSLookupFailed
                Right [] -> return [domain] -- return implicit record
                Right rs -> return $ map (C.unpack . fst) $ sortOn snd rs
    resolveA :: String -> IO String
    resolveA "localhost" = return "127.0.0.1"
    resolveA host = do
        rseed <- makeResolvSeed defaultResolvConf
        withResolver rseed $ \resolver -> do
            records <- lookupA resolver $ C.pack host
            case records of
                Left _      -> throwIO DNSLookupFailed
                Right []    -> throwIO DNSLookupNotFound
                Right (r:_) -> return $ show r

 -- | Verify the server greeting
greeting :: Chat ()
greeting = do
    _ <- listen 300
    return ()

-- | Send the HELO command
helo :: ByteString -> Chat ()
helo clientName = do
    talk $ LB.fromStrict $ "HELO " `B.append` clientName `B.append` "\r\n"
    _ <- listen 300
    return ()

-- | Send the EHLO command
ehlo :: ByteString -> Chat ()
ehlo clientName = do
    talk $ LB.fromStrict $ "EHLO " `B.append` clientName `B.append` "\r\n"
    r <- listen 300
    saveExtensions r
    return ()

-- | Try to send the EHLO command, fallback to HELO if necessary
hello :: ByteString -> Chat ()
hello clientName = (ehlo clientName) `catchError` const (helo clientName)

-- | Send the STARTTLS command and negotiate TLS context
startTLS :: ByteString -> Maybe TLSParams -> Chat ()
startTLS clientName tlsParams = do
    conn <- fromJust <$> gets connection
    tlsServerParams <- lookup "starttls" <$> gets extensions
    case (tlsParams, tlsServerParams) of
        (Nothing, _) -> return ()
        (Just (TLSParams _ isValidated), Just _) -> do
            -- issue command and wait for response
            talk "STARTTLS\r\n"
            _ <- listen 120
            -- negotiate TLS context
            hostname <- gets hostname
            conn' <- liftChat $ Connection.negotiateTLS conn hostname isValidated
            now <- liftChat getCurrentTime
            tell [(now, Both, "[negotiate tls]r\n")]
            modify $ \s -> s { connection = Just conn' }
            -- test TLS context by re-issuing hello
            hello clientName
        (Just (TLSParams isRequired _), Nothing)
            | isRequired -> throwError [TLSNotSupported]
            | otherwise  -> return ()

auth :: Maybe AuthParams -> Chat ()
auth authParams = do
    conn <- fromJust <$> gets connection
    authServerParams <- lookup "auth" <$> gets extensions
    case (authParams, authServerParams) of
        (Nothing, _) -> return ()
        (Just (AuthParams _ user pass), Just methods)
            | "login" `elem` methods -> do
                unless (Connection.isTLS conn) $ throwError [AuthNotEncrypted]
                talk "AUTH LOGIN\r\n"
                _ <- listen 120
                whisper $ LB.fromStrict $ B64.encode (C.pack user) `B.append` "\r\n"
                _ <- listen 120
                whisper $ LB.fromStrict $ B64.encode (C.pack pass) `B.append` "\r\n"
                _ <- listen 120
                return ()
            | "plain" `elem` methods -> do
                unless (Connection.isTLS conn) $ throwError [AuthNotEncrypted]
                whisper $ LB.fromStrict $ "AUTH PLAIN " `B.append` B64.encode (B.concat [C.pack user, C.singleton '\0', C.pack user, C.singleton '\0', C.pack pass]) `B.append` "\r\n"
                _ <- listen 120
                return ()
            | otherwise -> throwError [AuthMethodNotSupported]
        (Just (AuthParams isRequired _ _), Nothing)
            | isRequired -> throwError [AuthNotSupported]
            | otherwise  -> return ()

-- | Send the MAIL FROM command
mailFrom :: Mailbox -> Chat ()
mailFrom sender = do
    talk $ LB.fromStrict $ "MAIL FROM: " `B.append` C.pack (mboxAngleAddr sender) `B.append` "\r\n"
    _ <- listen 300
    return ()

-- | Send the RCPT TO command
rcptTo :: Mailbox -> Chat ()
rcptTo recipient = do
    talk $ LB.fromStrict $ "RCPT TO: " `B.append` C.pack (mboxAngleAddr recipient) `B.append` "\r\n"
    _ <- listen 300
    return ()

-- | Send the DATA command
dataInit :: Chat ()
dataInit = do
    talk "DATA\r\n"
    _ <- listen 120
    return ()

-- | Send the data block
dataBlock :: LB.ByteString -> Chat ()
dataBlock msg = do
    whisper $ msg `LB.append` "\r\n"
    return ()

-- | Terminate the data block
dataTerm :: Chat ()
dataTerm = do
    talk ".\r\n"
    _ <- listen 600
    return ()

-- | Send the QUIT command
quit :: Chat ()
quit = do
    talk "QUIT\r\n"
    _ <- listen 120
    return ()

-- | Close the connection to the MX server
disconnect :: Chat ()
disconnect = do
    conn <- fromJust <$> gets connection
    conn' <- liftChat $ Connection.close conn
    modify $ \s -> s { connection = Just conn' }
    return ()

-- | Send and log the command
{-# ANN talk ("HLint: ignore Reduce duplication" :: String) #-}
talk :: LB.ByteString -- ^ command
     -> Chat ()
talk msg = do
    conn <- fromJust <$> gets connection
    _ <- liftChat $ Connection.send conn msg
    now <- liftChat getCurrentTime
    tell $ map (now, Send, ) $ LB.toChunks msg
    modify $ \s -> s { lastCommand = Just msg
                     , lastReply   = Nothing
                     }
    return ()

-- | Send the command and log a redacted string
{-# ANN whisper ("HLint: ignore Reduce duplication" :: String) #-}
whisper :: LB.ByteString -- ^ command
        -> Chat ()
whisper msg = do
    conn <- fromJust <$> gets connection
    _ <- liftChat $ Connection.send conn msg
    now <- liftChat getCurrentTime
    tell [(now, Send, "[...]\r\n")]
    modify $ \s -> s { lastCommand = Nothing
                     , lastReply   = Nothing
                     }
    return ()

-- | Listen for a server reply and log it
listen :: NominalDiffTime -- ^ reply timeout
       -> Chat (Int, [ByteString])
listen secs = do
    deadline <- liftChat $ addUTCTime secs <$> getCurrentTime
    listen' (timeoutAt deadline) (parse pReply)
  where
    listen' timeoutF parseF = do
        conn <- fromJust <$> gets connection
        resp <- liftChat $ timeoutF $ Connection.recv conn
        case resp of
            Nothing    -> throwError [ResponseTimeout]
            Just chunk -> do
                now <- liftChat getCurrentTime
                tell [(now, Recv, chunk)]
                case parseF chunk of
                    Fail{}          -> throwError [BadResponseFormat]
                    Partial parseF' -> listen' timeoutF parseF'
                    Done _ reply    -> do
                        checkRC reply
                        modify $ \s -> s { lastReply = Just reply }
                        return reply
    checkRC :: (Int, [ByteString]) -> Chat ()
    checkRC (rcode, _)
        | rcode >= 200 && rcode <= 399 = return ()
        | rcode >= 400 && rcode <= 499 = throwError [TemporaryFailure]
        | otherwise                    = throwError [PermanentFailure]
    timeoutAt :: UTCTime -> IO a -> IO (Maybe a)
    timeoutAt deadline f = do
        secs <- diffUTCTime deadline <$> getCurrentTime
        timeoutIn secs f
    timeoutIn :: NominalDiffTime -> IO a -> IO (Maybe a)
    timeoutIn secs f
        | secs <= 0 = return Nothing
        | otherwise = timeout (ceiling $ 1000000 * secs) f

-- | Save supported extensions
saveExtensions :: (Int, [ByteString]) -- ^ reply
               -> Chat ()
saveExtensions (_, rtext) = do
    let es = map ((\(w:ws) -> (w, ws)) . words . map toLower . C.unpack) $ drop 1 rtext
    modify $ \s -> s { extensions = es }
    return ()

-- | Lift an IO action into the Chat monad, re-throwing any ClientException
liftChat :: IO a -> Chat a
liftChat f = liftIO (fmap Right f `catch` \e -> return $ Left [e]) >>= liftEither

