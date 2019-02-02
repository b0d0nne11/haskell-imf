{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Text.IMF.Network.Client
  ( Request(..)
  , TLSParams(..)
  , AuthParams(..)
  , ChatLog
  , MX(..)
  , ChatState(..)
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

import           Control.Applicative            ( (<|>)
                                                , empty
                                                )
import           Control.Exception              ( bracket )
import           Control.Monad                  ( unless )
import           Control.Monad.Reader           ( MonadReader
                                                , asks
                                                )
import           Control.Monad.Writer           ( MonadWriter
                                                , tell
                                                )
import           Control.Monad.State            ( MonadState
                                                , gets
                                                , modify
                                                )
import           Control.Monad.RWS              ( RWST
                                                , execRWST
                                                )
import           Control.Monad.Except           ( ExceptT
                                                , MonadError
                                                , runExceptT
                                                , throwError
                                                , catchError
                                                , liftEither
                                                )
import           Control.Monad.Trans            ( MonadIO
                                                , liftIO
                                                )
import           Data.Attoparsec.ByteString     ( IResult(..)
                                                , parse
                                                )
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.Lazy          as LB
import qualified Data.ByteString.Base64        as B64
import           Data.Char                      ( toLower )
import           Data.Default.Class             ( Default
                                                , def
                                                )
import           Data.List                      ( sortOn )
import           Data.Maybe                     ( isJust
                                                , fromJust
                                                )
import           Data.Time.Clock                ( UTCTime
                                                , NominalDiffTime
                                                , getCurrentTime
                                                , addUTCTime
                                                , diffUTCTime
                                                )
import           Network.DNS.Lookup             ( lookupMX
                                                , lookupA
                                                )
import           Network.DNS.Resolver           ( makeResolvSeed
                                                , withResolver
                                                , defaultResolvConf
                                                , Resolver
                                                )
import           Safe                           ( headDef
                                                , tailSafe
                                                )
import           System.IO.Error                ( tryIOError
                                                , catchIOError
                                                , annotateIOError
                                                )
import           System.Timeout                 ( timeout )


import           Text.IMF.Mailbox               ( Mailbox(..)
                                                , mboxAngleAddr
                                                )
import           Text.IMF.Message               ( Message(..) )
import           Text.IMF.Format                ( formatMessage )
import           Text.IMF.Parsers.Client        ( pReply )
import qualified Text.IMF.Network.Connection   as Connection
import           Text.IMF.Network.Connection    ( Connection )

mxLookupFailed :: IOError
mxLookupFailed = userError "MX lookup failed"

aLookupFailed :: IOError
aLookupFailed = userError "A lookup failed"

aLookupNotFound :: IOError
aLookupNotFound = userError "A record not found"

responseTimeout :: IOError
responseTimeout = userError "response timeout"

badResponseFormat :: IOError
badResponseFormat = userError "bad response format"

badResponseCode :: IOError
badResponseCode = userError "bad response code"

tlsNotSupported :: IOError
tlsNotSupported = userError "tls not supported"

authNotSupported :: IOError
authNotSupported = userError "authentication not supported"

authMethodNotSupported :: IOError
authMethodNotSupported = userError "authentication method not supported"

authNotEncrypted :: IOError
authNotEncrypted = userError "unencrypted authentication not supported"

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
    { reqClientName    :: ByteString       -- ^ client name
    , reqTLS           :: Maybe TLSParams  -- ^ opportunistic TLS
    , reqAuth          :: Maybe AuthParams -- ^ authentication
    , reqSender        :: Mailbox          -- ^ sender
    , reqRecipient     :: Mailbox          -- ^ recipient
    , reqMessage       :: LB.ByteString    -- ^ message
    }
  deriving (Show, Eq)

-- | SMTP chat log
type ChatLog = [ByteString]

-- | Mail exchange server details
data MX = MX
    { mxDomain     :: String               -- ^ domain
    , mxHostname   :: String               -- ^ hostname
    , mxIP         :: String               -- ^ ip address
    , mxPort       :: String               -- ^ port
    , mxExtentions :: [(String, [String])] -- ^ a list of supported extensions and parameters
    }
  deriving (Show, Eq)

-- | SMTP chat state
data ChatState = ChatState
    { connection  :: Maybe Connection          -- ^ mx server connection
    , mxServer    :: Maybe MX                  -- ^ mx server details
    , lastCommand :: Maybe LB.ByteString       -- ^ the last command sent
    , lastReply   :: Maybe (Int, [ByteString]) -- ^ the last reply received
    , timestamps  :: [(String, UTCTime)]       -- ^ timestamps
    , err         :: Maybe IOError             -- ^ any error encountered
    }
  deriving (Show, Eq)

instance Default ChatState where
    def = ChatState
        { connection  = Nothing
        , mxServer    = Nothing
        , lastCommand = Nothing
        , lastReply   = Nothing
        , timestamps  = []
        , err         = Nothing
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
        -> IO (ChatState, ChatLog)
deliver req = runChat chat req def
  where
    chat = do
        timestamp "opening chat"
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
        timestamp "closing chat"

-- | SMTP chat monad
type ChatM = ExceptT IOError (RWST Request ChatLog ChatState IO)

-- | Unwrap a SMTP chat monad into an IO action
runChat :: ChatM ()   -- ^ chat monad
        -> Request    -- ^ request
        -> ChatState  -- ^ initial chat state
        -> IO (ChatState, ChatLog)
runChat f = execRWST (runExceptT $ f `catchError` handleError)
  where
    handleError e = modify $ \s -> s { err = Just e }

-- | Lookup and connect to the MX server
connect :: ChatM ()
connect = do
    domain <- mboxDomain <$> asks reqRecipient
    resolvSeed <- liftError $ makeResolvSeed defaultResolvConf
    (conn, mxServer) <- liftError $ withResolver resolvSeed $ dial domain
    modify $ \s -> s { connection = Just conn, mxServer = Just mxServer }
    return ()
  where
    with = flip map
    ports = ["25", "587", "2525"]
    dial :: String -> Resolver -> IO (Connection, MX)
    dial domain resolv = do
        hosts <- resolveMX resolv domain
        foldl (<|>) empty $ with hosts $ \host ->
            foldl (<|>) empty $ with ports $ \port -> do
                ip <- resolveA resolv host
                conn <- Connection.open ip port
                return (conn, MX domain host ip port [])
    resolveMX :: Resolver -> String -> IO [String]
    resolveMX _ "localhost" = return ["localhost"]
    resolveMX resolv domain = do
        records <- lookupMX resolv $ C.pack domain
        case records of
            Left _   -> ioError mxLookupFailed
            Right [] -> return [domain] -- return implicit record
            Right rs -> return $ map (C.unpack . fst) $ sortOn snd rs
    resolveA :: Resolver -> String -> IO String
    resolveA _ "localhost" = return "127.0.0.1"
    resolveA resolv host = do
        records <- lookupA resolv $ C.pack host
        case records of
            Left _      -> ioError aLookupFailed
            Right []    -> ioError aLookupNotFound
            Right (r:_) -> return $ show r

 -- | Verify the server greeting
greeting :: ChatM ()
greeting = do
    _ <- listen 300 >>= checkRC 220
    return ()

-- | Send the HELO command
helo :: ChatM ()
helo = do
    clientName <- asks reqClientName
    talk $ LB.fromStrict $ B.append "HELO " clientName
    _ <- listen 300 >>= checkRC 250
    return ()

-- | Send the EHLO command
ehlo :: ChatM ()
ehlo = do
    clientName <- asks reqClientName
    talk $ LB.fromStrict $ B.append "EHLO " clientName
    r <- listen 300 >>= checkRC 250
    saveExtentions r
    return ()

-- | Try to send the EHLO command, fallback to HELO if necessary
hello :: ChatM ()
hello = ehlo `catchError` const helo

-- | Send the STARTTLS command and negotiate TLS context
startTLS :: ChatM ()
startTLS = do
    conn <- fromJust <$> gets connection
    tlsServerParams <- lookup "starttls" . mxExtentions . fromJust <$> gets mxServer
    tlsParams <- asks reqTLS
    case (tlsParams, tlsServerParams) of
        (Nothing, _) -> return ()
        (Just (TLSParams _ isValidated), Just _) -> do
            -- issue command and wait for response
            talk "STARTTLS"
            _ <- listen 120 >>= checkRC 220
            -- negotiate TLS context
            hostname <- mxHostname . fromJust <$> gets mxServer
            conn' <- liftError $ Connection.negotiateTLS conn hostname isValidated
            tell ["[...]\r\n"]
            modify $ \s -> s { connection = Just conn' }
            -- test TLS context by re-issuing hello
            hello
        (Just (TLSParams isRequired _), Nothing)
            | isRequired -> throwError tlsNotSupported
            | otherwise  -> return ()

auth :: ChatM ()
auth = do
    conn <- fromJust <$> gets connection
    authServerParams <- lookup "auth" . mxExtentions . fromJust <$> gets mxServer
    authParams <- asks reqAuth
    case (authParams, authServerParams) of
        (Nothing, _) -> return ()
        (Just (AuthParams _ user pass), Just methods)
            | "login" `elem` methods -> do
                unless (Connection.isTLS conn) $ throwError authNotEncrypted
                talk "AUTH LOGIN"
                _ <- listen 120 >>= checkRC 334
                whisper $ LB.fromStrict $ B64.encode $ C.pack user
                _ <- listen 120 >>= checkRC 334
                whisper $ LB.fromStrict $ B64.encode $ C.pack pass
                _ <- listen 120 >>= checkRC 235
                return ()
            | "plain" `elem` methods -> do
                unless (Connection.isTLS conn) $ throwError authNotEncrypted
                whisper $ LB.fromStrict $ B.append "AUTH PLAIN " $ B64.encode $ B.concat [C.pack user, C.singleton '\0', C.pack user, C.singleton '\0', C.pack pass]
                _ <- listen 120 >>= checkRC 235
                return ()
            | otherwise -> throwError authMethodNotSupported
        (Just (AuthParams isRequired _ _), Nothing)
            | isRequired -> throwError authNotSupported
            | otherwise  -> return ()

-- | Send the MAIL FROM command
mailFrom :: ChatM ()
mailFrom = do
    sender <- asks reqSender
    talk $ LB.fromStrict $ B.append "MAIL FROM: " $ C.pack $ mboxAngleAddr sender
    _ <- listen 300 >>= checkRC 250
    return ()

-- | Send the RCPT TO command
rcptTo :: ChatM ()
rcptTo = do
    recipient <- asks reqRecipient
    talk $ LB.fromStrict $ B.append "RCPT TO: " $ C.pack $ mboxAngleAddr recipient
    _ <- listen 300 >>= checkRC 250
    return ()

-- | Send the DATA command
dataInit :: ChatM ()
dataInit = do
    talk "DATA"
    _ <- listen 120 >>= checkRC 354
    return ()

-- | Send the data block
dataBlock :: ChatM ()
dataBlock = do
    msg <- asks reqMessage
    whisper msg
    return ()

-- | Terminate the data block
dataTerm :: ChatM ()
dataTerm = do
    talk "."
    _ <- listen 600 >>= checkRC 250
    return ()

-- | Send the QUIT command
quit :: ChatM ()
quit = do
    talk "QUIT"
    _ <- listen 120 >>= checkRC 221
    return ()

-- | Close the connection to the MX server
disconnect :: ChatM ()
disconnect = do
    conn <- fromJust <$> gets connection
    liftError $ Connection.close conn
    modify $ \s -> s { connection = Nothing }
    return ()

-- | Send and log the command
talk :: LB.ByteString -- ^ command
     -> ChatM ()
talk command = do
    conn <- fromJust <$> gets connection
    _ <- liftError $ Connection.send conn msg
    tell $ LB.toChunks msg
    modify $ \s -> s { lastCommand = Just msg
                     , lastReply   = Nothing
                     }
    return ()
  where
    msg = LB.append command "\r\n"

-- | Send the command and log a redacted string
whisper :: LB.ByteString -- ^ command
        -> ChatM ()
whisper command = do
    conn <- fromJust <$> gets connection
    _ <- liftError $ Connection.send conn msg
    tell ["[...]\r\n"]
    modify $ \s -> s { lastCommand = Nothing
                     , lastReply   = Nothing
                     }
    return ()
  where
    msg = LB.append command "\r\n"

-- | Listen for a server reply and log it
listen :: NominalDiffTime -- ^ reply timeout
       -> ChatM (Int, [ByteString])
listen secs = do
    deadline <- liftError $ addUTCTime secs <$> getCurrentTime
    listen' (timeoutAt deadline) (parse pReply)
  where
    listen' timeoutF parseF = do
        conn <- fromJust <$> gets connection
        resp <- liftError $ timeoutF $ Connection.recv conn
        maybe (return ()) (tell . pure) resp
        case resp of
            Nothing -> throwError responseTimeout
            Just a  -> case parseF a of
                Fail{}          -> throwError badResponseFormat
                Partial parseF' -> listen' timeoutF parseF'
                Done _ reply    -> do
                    modify $ \s -> s { lastReply = Just reply }
                    return reply

-- | Check the reply code
checkRC :: Int                 -- ^ expected reply code
        -> (Int, [ByteString]) -- ^ reply
        -> ChatM (Int, [ByteString])
checkRC expected reply@(rcode, _)
    | rcode == expected = return reply
    | otherwise         = throwError badResponseCode

-- | Save supported extentions
saveExtentions :: (Int, [ByteString]) -- ^ reply
               -> ChatM ()
saveExtentions (_, rtext) = do
    let es = map (split . words . map toLower . C.unpack) $ drop 1 rtext
    mxServer <- fromJust <$> gets mxServer
    modify $ \s -> s { mxServer = Just $ mxServer { mxExtentions = es } }
    return ()
  where
    split as = (headDef "" as, tailSafe as)

-- | Add a timestamp
timestamp :: String -- ^ timestamp tag
          -> ChatM ()
timestamp tag = do
    t <- (,) tag <$> liftError getCurrentTime
    ts <- gets timestamps
    modify $ \s -> s { timestamps = t:ts }
    return ()

-- | Lift an IO action into the Error monad
liftError :: IO a -> ChatM a
liftError f = liftIO (tryIOError f) >>= liftEither

-- | Timeout in a certain number of seconds
timeoutIn :: NominalDiffTime -> IO a -> IO (Maybe a)
timeoutIn secs f
    | secs <= 0 = return Nothing
    | otherwise = timeout (ceiling $ 1000000 * secs) f

-- | Timeout at a certain time
timeoutAt :: UTCTime -> IO a -> IO (Maybe a)
timeoutAt deadline f = do
    secs <- diffUTCTime deadline <$> getCurrentTime
    timeoutIn secs f
