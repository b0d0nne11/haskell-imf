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

import           Control.Exception              ( bracket
                                                , try
                                                )
import           Control.Monad                  ( when )
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
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Char8         as C
import           Data.List                      ( sortOn )
import           Data.Maybe                     ( fromMaybe )
import           Data.Time.Clock                ( UTCTime
                                                , getCurrentTime
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
                                                , AddrInfoFlag(..)
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

responseTimeout   = userError "response timeout"
badResponseFormat = userError "bad response format"
badResponseCode   = userError "bad response code"

-- | SMTP connection object
data Conn = Conn
    { connHostname :: Hostname -- ^ mx host
    , connPort     :: Port     -- ^ mx host
    , connSocket   :: Socket   -- ^ socket
    }
  deriving (Show, Eq)

-- | State information for the SMTP chat
data SendState = SendState
    { mxHostname   :: Hostname            -- ^ mx hostname
    , mxPort       :: Port                -- ^ mx port
    , lastCommand  :: Maybe ByteString    -- ^ the last command sent, if any
    , lastResponse :: Maybe ByteString    -- ^ the response to the last command, if any
    , lastRC       :: Maybe Int           -- ^ the rc from the last response, if any
    , timestamps   :: [(String, UTCTime)] -- ^ timestamps
    }
  deriving (Show, Eq)

-- | Initial state information for a new connection
initState :: Conn -> SendState
initState conn = SendState
    { mxHostname   = connHostname conn
    , mxPort       = connPort conn
    , lastCommand  = Nothing
    , lastResponse = Nothing
    , lastRC       = Nothing
    , timestamps   = []
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
    handleError r e es a as = do
        let e' = annotateIOError e (show a) Nothing Nothing
        robocall r (e':es) as

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
    Socket.send sock "QUIT\r\n"
    _ <- timeoutMin 1 $ Socket.recv sock 4096
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
greeting = listen 5 >>= checkRC 220

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
    listen 5 >>= checkRC 250

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
    listen 5 >>= checkRC 250

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
    listen 5 >>= checkRC 250

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
    listen 2 >>= checkRC 354

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
    listen 10 >>= checkRC 250

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
    liftError $ Socket.send sock $ C.append command "\r\n"
    tell [command]
    modify $ \s -> s { lastCommand  = Just command
                     , lastResponse = Nothing
                     , lastRC       = Nothing
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
    liftError $ Socket.send sock $ C.append command "\r\n"
    tell ["__REDACTED__"]
    modify $ \s -> s { lastCommand  = Nothing
                     , lastResponse = Nothing
                     , lastRC       = Nothing
                     }
    return ()

-- | Listen for a server response and log it
listen ::
    ( MonadIO m
    , MonadReader Conn m
    , MonadWriter [ByteString] m
    , MonadState SendState m
    , MonadError IOError m
    )
    => Int -- ^ response timeout in minutes
    -> m ByteString
listen minutes = do
    sock <- asks connSocket
    resp <- liftError $ timeoutMin minutes $ Socket.recv sock 4096
    tell [stripCRLF $ fromMaybe "" resp]
    modify $ \s -> s { lastResponse = resp }
    maybe (throwError responseTimeout) return resp

-- | Check and log the response code
checkRC ::
    ( MonadIO m
    , MonadReader Conn m
    , MonadWriter [ByteString] m
    , MonadState SendState m
    , MonadError IOError m
    )
    => Int        -- ^ expected response code
    -> ByteString -- ^ response
    -> m ()
checkRC expected msg =
    -- TODO: replace this with an actual parser
    case fst <$> C.readInt (C.take 3 msg) of
        Nothing -> throwError badResponseFormat
        Just rc -> do
            modify $ \s -> s { lastRC = Just rc }
            when (rc /= expected) $ throwError badResponseCode

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
liftError f = liftEither =<< liftIO (try f)

-- | A version of timeout that takes a number of minutes rather than
-- microseconds because the mandatory SMTP wait times are absurdly long
timeoutMin :: Int -> IO a -> IO (Maybe a)
timeoutMin = timeout . (60*1000000*)

-- | Strip the CRLF line ending off of a bytestring
stripCRLF :: ByteString -> ByteString
stripCRLF msg = fromMaybe msg $ C.stripSuffix "\r\n" msg

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
    bracket (open (C.pack $ mboxDomain recipient) defaultPorts)
            (tryIOError . quit)
            (runChatT $ chat client sender recipient msg)

