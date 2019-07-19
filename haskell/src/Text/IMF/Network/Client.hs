{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Text.IMF.Network.Client
  ( Request(..)
  , TLSParams(..)
  , AuthParams(..)
  , ClientLog
  , MX(..)
  , ClientState(..)
  , deliver
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

-- | Request parameters
data Request = Request
    { reqClientName  :: ByteString       -- ^ client name
    , reqSourceIP    :: Maybe String     -- ^ source IP address
    , reqTargetHosts :: Maybe [String]   -- ^ target hosts
    , reqTLS         :: Maybe TLSParams  -- ^ opportunistic TLS
    , reqAuth        :: Maybe AuthParams -- ^ authentication
    , reqSender      :: Mailbox          -- ^ sender
    , reqRecipient   :: Mailbox          -- ^ recipient
    , reqMessage     :: LB.ByteString    -- ^ message
    }
  deriving (Show, Eq)

data LogEntryType = Send | Recv | Both
  deriving (Show, Eq)

-- | SMTP chat log
type ClientLog = [(UTCTime, LogEntryType, ByteString)]

-- | Mail exchange server details
data MX = MX
    { mxHostname   :: String               -- ^ hostname
    , mxIP         :: String               -- ^ ip address
    , mxPort       :: String               -- ^ port
    , mxExtentions :: [(String, [String])] -- ^ a list of supported extensions and parameters
    }
  deriving (Show, Eq)

-- | SMTP chat state
data ClientState = ClientState
    { connection      :: Maybe Connection          -- ^ mx server connection
    , mxServer        :: Maybe MX                  -- ^ mx server details
    , respLastCommand :: Maybe LB.ByteString       -- ^ the last command sent
    , respLastReply   :: Maybe (Int, [ByteString]) -- ^ the last reply received
    , respLastErrors  :: [ClientException]         -- ^ any errors encountered
    }
  deriving (Show, Eq)

instance Default ClientState where
    def = ClientState
        { connection      = Nothing
        , mxServer        = Nothing
        , respLastCommand = Nothing
        , respLastReply   = Nothing
        , respLastErrors  = []
        }

-- | Deliver a message
--
-- Deliver a message according to the request parameters and return a tuple
-- of the final chat state and chat log. Any IO error will terminate the chat
-- early and be saved to the chat state.
--
-- The sender and recipient parameters shouldn't be confused with the from and
-- to/cc/bcc addresses in the message. The sender here is the envelope return
-- path which may differ from the message's from address. Similarly, the
-- envelope recipient might not even be included in the message's to/CC/BCC
-- addresses.
--
-- The message parameter is sent verbatim so any required modifications like
-- adding DKIM headers or masking BCC addresses should be done before calling
-- `deliver`.
--
deliver :: Request -- ^ request
        -> IO (ClientState, ClientLog)
deliver req = runChat chat req def
  where
    chat = do
        connect
        greeting
        hello
        startTLS
        auth
        mailFrom
        rcptTo
        dataInit
        dataBlock
        dataTerm
        quit
        disconnect

-- | SMTP chat monad
type Chat = ExceptT [ClientException] (RWST Request ClientLog ClientState IO)

-- | Unwrap a SMTP chat monad into an IO action
runChat :: Chat ()     -- ^ chat monad
        -> Request     -- ^ request
        -> ClientState -- ^ initial chat state
        -> IO (ClientState, ClientLog)
runChat f = execRWST (runExceptT $ f `catchError` handleError)
  where
    handleError es = modify $ \s -> s { respLastErrors = es }

-- | Lookup and connect to the MX server
connect :: Chat ()
connect = do
    srcIP <- asks reqSourceIP
    targetHosts <- asks reqTargetHosts
    recipientDomain <- mboxDomain <$> asks reqRecipient
    hosts <- maybe (liftChat $ resolveMX recipientDomain) return targetHosts
    foldl (<|>) empty $ with hosts $ \host ->
        foldl (<|>) empty $ with ports $ \port -> do
            dstIP <- liftChat $ resolveA host
            conn <- liftChat $ Connection.open srcIP dstIP port
            modify $ \s -> s { connection = Just conn
                             , mxServer = Just $ MX host dstIP port []
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
helo :: Chat ()
helo = do
    clientName <- asks reqClientName
    talk $ LB.fromStrict $ "HELO " `B.append` clientName `B.append` "\r\n"
    _ <- listen 300
    return ()

-- | Send the EHLO command
ehlo :: Chat ()
ehlo = do
    clientName <- asks reqClientName
    talk $ LB.fromStrict $ "EHLO " `B.append` clientName `B.append` "\r\n"
    r <- listen 300
    saveExtentions r
    return ()

-- | Try to send the EHLO command, fallback to HELO if necessary
hello :: Chat ()
hello = ehlo `catchError` const helo

-- | Send the STARTTLS command and negotiate TLS context
startTLS :: Chat ()
startTLS = do
    conn <- fromJust <$> gets connection
    tlsServerParams <- lookup "starttls" . mxExtentions . fromJust <$> gets mxServer
    tlsParams <- asks reqTLS
    case (tlsParams, tlsServerParams) of
        (Nothing, _) -> return ()
        (Just (TLSParams _ isValidated), Just _) -> do
            -- issue command and wait for response
            talk "STARTTLS\r\n"
            _ <- listen 120
            -- negotiate TLS context
            hostname <- mxHostname . fromJust <$> gets mxServer
            conn' <- liftChat $ Connection.negotiateTLS conn hostname isValidated
            now <- liftChat getCurrentTime
            tell [(now, Both, "[negotiate tls]r\n")]
            modify $ \s -> s { connection = Just conn' }
            -- test TLS context by re-issuing hello
            hello
        (Just (TLSParams isRequired _), Nothing)
            | isRequired -> throwError [TLSNotSupported]
            | otherwise  -> return ()

auth :: Chat ()
auth = do
    conn <- fromJust <$> gets connection
    authServerParams <- lookup "auth" . mxExtentions . fromJust <$> gets mxServer
    authParams <- asks reqAuth
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
mailFrom :: Chat ()
mailFrom = do
    sender <- asks reqSender
    talk $ LB.fromStrict $ "MAIL FROM: " `B.append` C.pack (mboxAngleAddr sender) `B.append` "\r\n"
    _ <- listen 300
    return ()

-- | Send the RCPT TO command
rcptTo :: Chat ()
rcptTo = do
    recipient <- asks reqRecipient
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
dataBlock :: Chat ()
dataBlock = do
    msg <- asks reqMessage
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
    liftChat $ Connection.close conn
    modify $ \s -> s { connection = Nothing }
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
    modify $ \s -> s { respLastCommand = Just msg
                     , respLastReply   = Nothing
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
    modify $ \s -> s { respLastCommand = Nothing
                     , respLastReply   = Nothing
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
                        modify $ \s -> s { respLastReply = Just reply }
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

-- | Save supported extentions
saveExtentions :: (Int, [ByteString]) -- ^ reply
               -> Chat ()
saveExtentions (_, rtext) = do
    let es = map ((\(w:ws) -> (w, ws)) . words . map toLower . C.unpack) $ drop 1 rtext
    mxServer <- fromJust <$> gets mxServer
    modify $ \s -> s { mxServer = Just $ mxServer { mxExtentions = es } }
    return ()

-- | Lift an IO action into the Chat monad, re-throwing any ClientException
liftChat :: IO a -> Chat a
liftChat f = liftIO (fmap Right f `catch` \e -> return $ Left [e]) >>= liftEither

