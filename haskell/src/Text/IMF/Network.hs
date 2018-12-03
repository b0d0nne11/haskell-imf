{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Text.IMF.Network
  ( Conn(..)
  , SendState(..)
  , send
  , open
  , quit
  , runChatT
  , chat
  , greeting
  , helo
  , mailFrom
  , rcptTo
  , dataInit
  , dataBlock
  , dataTerm
  )
where

import           Prelude                 hiding ( takeWhile )

import           Control.Applicative            ( liftA2
                                                , liftA3
                                                , many
                                                , (<|>)
                                                )
import qualified Control.Exception           as Exception
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
                                                , liftEither
                                                )
import           Control.Monad.Trans            ( MonadIO
                                                , liftIO
                                                )
import           Data.Attoparsec.ByteString     ( Parser
                                                , IResult(..)
                                                , Result
                                                , parse
                                                , try
                                                )
import           Data.Attoparsec.ByteString.Char8 ( satisfy
                                                  , takeWhile
                                                  , string
                                                  )
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Char8         as C
import           Data.Char                      ( isDigit
                                                , isPrint
                                                )
import           Data.List                      ( sortOn )
import           Data.Maybe                     ( fromMaybe )
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
import           Network.Socket                 ( AddrInfo(..)
                                                , SocketType(..)
                                                , Socket
                                                , getAddrInfo
                                                , socket
                                                , connect
                                                , defaultHints
                                                , close
                                                )
import qualified Network.Socket.ByteString     as Socket
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


type Domain   = ByteString
type Hostname = ByteString
type Port     = ByteString
type IPv4     = ByteString
type Reply    = (Int, [ByteString])

responseTimeout :: IOError
responseTimeout = userError "response timeout"

badResponseFormat :: IOError
badResponseFormat = userError "bad response format"

badResponseCode :: IOError
badResponseCode = userError "bad response code"

-- | SMTP connection object
data Conn = Conn
    { connHostname :: Hostname -- ^ mx host
    , connPort     :: Port     -- ^ mx host
    , connSocket   :: Socket   -- ^ socket
    }
  deriving (Show, Eq)

-- | State information for the SMTP chat
data SendState = SendState
    { mxHostname  :: Hostname            -- ^ mx hostname
    , mxPort      :: Port                -- ^ mx port
    , lastCommand :: Maybe ByteString    -- ^ the last command sent, if any
    , lastReply   :: Maybe Reply         -- ^ the reply to the last command, if any
    , timestamps  :: [(String, UTCTime)] -- ^ timestamps
    }
  deriving (Show, Eq)

-- | Initial state information for a new connection
initState :: Conn -> SendState
initState conn = SendState
    { mxHostname  = connHostname conn
    , mxPort      = connPort conn
    , lastCommand = Nothing
    , lastReply   = Nothing
    , timestamps  = []
    }

-- | Default MX ports
defaultPorts :: [Port]
defaultPorts = ["25", "587", "2525"]

-- | Open a new connection
open :: Domain -> [Port] -> IO Conn
open domain ports = do
    resolvSeed <- makeResolvSeed defaultResolvConf
    withResolver resolvSeed $ \resolver -> do
        hosts <- resolveMX resolver domain
        let hostports = [ (h,p) | h <- hosts, p <- ports ]
        robocall resolver [] hostports

-- | Dial each hostname and port in the list, returning the first one to connect
robocall :: Resolver           -- ^ dns resolver
         -> [IOError]          -- ^ accumulator for io errors
         -> [(Hostname, Port)] -- ^ mx hostname/port pairs
         -> IO Conn
robocall _ es []     = fail $ "failed to connect: " ++ show es
robocall r es (a:as) = dial r a `catchIOError` \e -> handleError r e es a as
  where
    handleError _r _e _es _a _as = do
        let _e' = annotateIOError _e (show _a) Nothing Nothing
        robocall _r (_e':_es) _as

-- | Try to open a connection to a hostname and port
dial :: Resolver         -- ^ dns resolver
     -> (Hostname, Port) -- ^ mx hostname/port pair
     -> IO Conn
dial resolver (host, port) = do
    ip <- resolveA resolver host
    let hints = defaultHints { addrSocketType = Stream }
    addr:_ <- getAddrInfo (Just hints) (Just $ C.unpack ip) (Just $ C.unpack port)
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    connect sock $ addrAddress addr
    return $ Conn host port sock

-- | Resolve the MX records for a domain
resolveMX :: Resolver -> Domain -> IO [Hostname]
resolveMX _ "localdomain" = return ["localhost"]
resolveMX resolver domain = do
    records <- lookupMX resolver domain
    case records of
        Left e   -> fail $ show e
        Right [] -> return [domain] -- implicit record
        Right rs -> return $ map fst $ sortOn snd rs

-- | Resolve the A records for a host
resolveA :: Resolver -> Hostname -> IO IPv4
resolveA _ "localhost" = return "127.0.0.1"
resolveA resolver host = do
    records <- lookupA resolver host
    case records of
        Left e      -> fail $ show e
        Right []    -> fail "failed to lookup A record"
        Right (r:_) -> return $ C.pack $ show r

-- | Send the QUIT command and close the connection
quit :: Conn -> IO ()
quit conn = do
    let sock = connSocket conn
    _ <- Socket.send sock "QUIT\r\n"
    _ <- timeout 60000000 $ Socket.recv sock 4096
    close sock

-- | The SMTP chat transformer
type ChatT = ExceptT IOError (RWST Conn [ByteString] SendState IO)

-- | Unwrap a SMTP chat transformer into an IO action
runChatT :: ChatT a -- ^ chat transformer
         -> Conn    -- ^ connection
         -> IO (SendState, [ByteString])
runChatT f conn =
    execRWST (runExceptT f) conn (initState conn)

-- | The SMTP chat
chat ::
    ( MonadIO m
    , MonadReader Conn m
    , MonadWriter [ByteString] m
    , MonadState SendState m
    , MonadError IOError m
    )
    => ByteString -- ^ client name
    -> Mailbox    -- ^ sender
    -> Mailbox    -- ^ recipeint
    -> Message    -- ^ message
    -> m ()
chat client sender recipient msg = do
    timestamp "opening chat"
    greeting
    helo client
    mailFrom sender
    rcptTo recipient
    dataInit
    dataBlock msg
    dataTerm
    timestamp "closing chat"

-- | Verify the server greeting
greeting ::
    ( MonadIO m
    , MonadReader Conn m
    , MonadWriter [ByteString] m
    , MonadState SendState m
    , MonadError IOError m
    )
    => m ()
greeting = do
    _ <- listen 300 >>= checkRC 220
    return ()

-- | Send the HELO command
helo ::
    ( MonadIO m
    , MonadReader Conn m
    , MonadWriter [ByteString] m
    , MonadState SendState m
    , MonadError IOError m
    )
    => ByteString -- ^ client name
    -> m ()
helo client = do
    talk $ C.append "HELO " client
    _ <- listen 300 >>= checkRC 250
    return ()

-- | Send the MAIL FROM command
mailFrom ::
    ( MonadIO m
    , MonadReader Conn m
    , MonadWriter [ByteString] m
    , MonadState SendState m
    , MonadError IOError m
    )
    => Mailbox -- ^ sender
    -> m ()
mailFrom mbox = do
    talk $ C.append "MAIL FROM: " $ C.pack $ mboxAngleAddr mbox
    _ <- listen 300 >>= checkRC 250
    return ()

-- | Send the RCPT TO command
rcptTo ::
    ( MonadIO m
    , MonadReader Conn m
    , MonadWriter [ByteString] m
    , MonadState SendState m
    , MonadError IOError m
    )
    => Mailbox -- ^ recipient
    -> m ()
rcptTo mbox = do
    talk $ C.append "RCPT TO: " $ C.pack $ mboxAngleAddr mbox
    _ <- listen 300 >>= checkRC 250
    return ()

-- | Send the DATA command
dataInit ::
    ( MonadIO m
    , MonadReader Conn m
    , MonadWriter [ByteString] m
    , MonadState SendState m
    , MonadError IOError m
    )
    => m ()
dataInit = do
    talk "DATA"
    _ <- listen 120 >>= checkRC 354
    return ()

-- | Send the data block
dataBlock ::
    ( MonadIO m
    , MonadReader Conn m
    , MonadWriter [ByteString] m
    , MonadState SendState m
    , MonadError IOError m
    )
    => Message -- ^ message
    -> m ()
dataBlock msg = whisper $ C.pack $ formatMessage msg

-- | Terminate the data block
dataTerm ::
    ( MonadIO m
    , MonadReader Conn m
    , MonadWriter [ByteString] m
    , MonadState SendState m
    , MonadError IOError m
    )
    => m ()
dataTerm = do
    talk "."
    _ <- listen 600 >>= checkRC 250
    return ()

-- | Send and log the command
talk ::
    ( MonadIO m
    , MonadReader Conn m
    , MonadWriter [ByteString] m
    , MonadState SendState m
    , MonadError IOError m
    )
    => ByteString -- ^ command
    -> m ()
talk command = do
    sock <- asks connSocket
    _ <- liftError $ Socket.send sock $ C.append command "\r\n"
    tell [command]
    modify $ \s -> s { lastCommand = Just command
                     , lastReply   = Nothing
                     }
    return ()

-- Send the command and log a "__REDACTED__" string
whisper ::
    ( MonadIO m
    , MonadReader Conn m
    , MonadWriter [ByteString] m
    , MonadState SendState m
    , MonadError IOError m
    )
    => ByteString -- ^ command
    -> m ()
whisper command = do
    sock <- asks connSocket
    _ <- liftError $ Socket.send sock $ C.append command "\r\n"
    tell ["__REDACTED__"]
    modify $ \s -> s { lastCommand = Nothing
                     , lastReply   = Nothing
                     }
    return ()

-- | Listen for a server reply and log it
listen ::
    ( MonadIO m
    , MonadReader Conn m
    , MonadWriter [ByteString] m
    , MonadState SendState m
    , MonadError IOError m
    )
    => NominalDiffTime -- ^ reply timeout
    -> m Reply
listen secs = do
    deadline <- liftError $ addUTCTime secs <$> getCurrentTime
    listen' (timeoutAt deadline) (parse pReply)
  where
    listen' timeoutF parseF = do
        sock <- asks connSocket
        resp <- liftError $ timeoutF $ Socket.recv sock 1024
        tell [stripCRLF $ fromMaybe "" resp]
        case resp of
            Nothing -> throwError responseTimeout
            Just a  -> case parseF a of
                Fail{}          -> throwError badResponseFormat
                Partial parseF' -> listen' timeoutF parseF'
                Done _ reply    -> do
                    modify $ \s -> s { lastReply = Just reply }
                    return reply

-- | Check and log the reply code
checkRC :: (MonadIO m, MonadError IOError m) => Int -> Reply -> m Reply
checkRC expected reply@(rcode, _)
    | rcode == expected = return reply
    | otherwise         = throwError badResponseCode

-- | Add a timestamp to the chat state object
timestamp ::
    ( MonadIO m
    , MonadReader Conn m
    , MonadWriter [ByteString] m
    , MonadState SendState m
    , MonadError IOError m
    )
    => String -- ^ timestamp tag
    -> m ()
timestamp tag = do
    t <- (,) tag <$> liftError getCurrentTime
    ts <- gets timestamps
    modify $ \s -> s { timestamps = t:ts }
    return ()

-- | Lift an IO action into the Error monad
liftError :: (MonadIO m, MonadError IOError m) => IO a -> m a
liftError f = liftIO (Exception.try f) >>= liftEither

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

-- | Strip the CRLF line ending off of a bytestring
stripCRLF :: ByteString -> ByteString
stripCRLF msg = fromMaybe msg $ C.stripSuffix "\r\n" msg

-- | Parser for SMTP reply
pReply :: Parser Reply
pReply =
    liftA2 (,) pCode (pEmptyLine <|> pSingleLine <|> pMultiLine)
  where
    pFirstDigit = satisfy $ \c -> c >= '2' && c <= '5'
    pDigit = satisfy isDigit
    pCode = read <$> sequence [pFirstDigit, pDigit, pDigit]
    pText = takeWhile $ \c -> isPrint c || c == '\t'
    pEmptyLine = [] <$ string "\r\n"
    pSingleLine = pure <$> (string " " *> pText <* string "\r\n")
    pMultiLine = liftA3 (\a b c -> [a] ++ b ++ [c])
                        (string "-" *> pText <* string "\r\n")
                        (many $ try $ pCode *> string "-" *> pText <* string "\r\n")
                        (pCode *> string " " *> pText <* string "\r\n")

-- | Send a message
--
-- The client name parameter is used to identifiy the SMTP client in the HELO
-- or EHLO command. Many SMTP servers check this name against the return record
-- of the client's IP address and may reject messages if they don't match.
--
-- The sender and recipient parameters shouldn't be confused with the from and
-- to/cc/bcc addresses in the message. The sender here is the envelope return
-- path which may differ from the message's from address. Similarly, the
-- envelope recipient might not even be included in the message's to/CC/BCC
-- addresses.
--
-- The message parameter is sent verbatim so any required modifications like
-- adding DKIM headers or masking BCC addresses should be done before calling
-- `send`.
--
-- This function returns a tuple of the chat state object and chat log. It may
-- also raise an `IOError` if the error occurs while opening the connection.
-- Exceptions that occur during the SMTP chat will terminate the chat early but
-- are not re-thrown. Exceptions that occur while closing the connection are
-- ignored.
--
send :: Domain  -- ^ client name
     -> Mailbox -- ^ sender
     -> Mailbox -- ^ recipient
     -> Message -- ^ message
     -> IO (SendState, [ByteString])
send client sender recipient msg =
    Exception.bracket (open (C.pack $ mboxDomain recipient) defaultPorts)
                      (tryIOError . quit)
                      (runChatT $ chat client sender recipient msg)

