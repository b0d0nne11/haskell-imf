{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module Data.IMF.Network.Client
  ( Client(..)
  , ClientParams(..)
  , newClient
  , getClient
  , deliver
  , closeClient
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

import           Control.Monad                  ( unless
                                                , when
                                                )
import           Control.Monad.Except           ( catchError
                                                , throwError
                                                )
import           Control.Monad.State            ( gets
                                                , modify
                                                )
import qualified Data.ByteString               as B
import qualified Data.ByteString.Base64        as B64
import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.Lazy          as LB
import           Data.Char                      ( toLower )
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( encodeUtf8 )
import           Data.Time.Clock                ( NominalDiffTime
                                                , UTCTime
                                                , getCurrentTime
                                                )

import           Data.IMF.Types
import           Data.IMF.Network.Chat
import qualified Data.IMF.Network.Connection   as Conn
import qualified Data.IMF.Network.DNS          as DNS
import           Data.IMF.Network.Errors
import           Data.IMF.Parsers.Network       ( pReply )

-- | SMTP chat client
data Client = Client
    { clientConnection   :: Conn.Connection                  -- ^ mx server connection
    , clientHostname     :: Conn.HostName                    -- ^ mx server hostname
    , clientExtensions   :: [(B.ByteString, [B.ByteString])] -- ^ list of supported extensions and parameters
    , clientLastCommand  :: Maybe LB.ByteString              -- ^ last command sent
    , clientLastReply    :: Maybe (Int, [B.ByteString])      -- ^ last reply received
    , clientError        :: ChatException                    -- ^ error or OK if everything is fine
    , clientCreatedAt    :: UTCTime                          -- ^ created at
    , clientMessagesSent :: Int                              -- ^ number of messages delivered
    }
  deriving (Show)

newClient :: IO Client
newClient = do
    now <- getCurrentTime
    return $ Client
        { clientConnection   = Conn.blank
        , clientHostname     = ""
        , clientExtensions   = []
        , clientLastCommand  = Nothing
        , clientLastReply    = Nothing
        , clientError        = OK
        , clientCreatedAt    = now
        , clientMessagesSent = 0
        }

-- | Connection parameters
data ClientParams = ClientParams
    { clientName            :: String                 -- ^ name, used for hello
    , clientSourceIP        :: String                 -- ^ source IP, use 0.0.0.0 for any
    , clientRecipientDomain :: T.Text                 -- ^ recipient domain
    , clientProxyHosts      :: Maybe [String]         -- ^ optional proxy hosts, skips recipient domain lookup
    , clientAuthCredentials :: Maybe (String, String) -- ^ optional username and password
    , clientTLSRequired     :: Bool                   -- ^ tls is required?
    , clientTLSValidated    :: Bool                   -- ^ tls validates certificates?
    }
  deriving (Show, Eq)

handleError :: ChatException -> Chat Client ()
handleError e = modify $ \c -> c { clientError = e }

-- | Get a client
--
-- Gets a client thats ready for sending by performing the connection, greeting,
-- hello, optional starttls, and optional auth portions of the SMTP chat.
--
getClient :: ClientParams -- ^ client parameters
          -> IO (Client, ChatLog)
getClient params = newClient >>= (runChat $ chat `catchError` handleError)
  where
    chat = do
        connect params
        greeting
        hello params
        startTLS params
        auth params

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
        -> LB.ByteString -- ^ message
        -> Client        -- ^ client
        -> IO (Client, ChatLog)
deliver (Envelope sender recipeint) msg = runChat $ chat `catchError` handleError
  where
    chat = do
        mailFrom sender
        rcptTo recipeint
        dataInit
        dataBlock msg
        dataTerm

-- | Gracefully close a client
--
-- Closes a client gracefully by sending the quit command. This should only be
-- run once per client and only after all message deliveries are complete.
--
closeClient :: Client -- ^ client
            -> IO (Client, ChatLog)
closeClient = runChat $ chat `catchError` handleError
  where
    chat = do
        quit
        disconnect

-- | Lookup and connect to the MX server
connect :: ClientParams -> Chat Client ()
connect ClientParams{..} = do
    hosts <- whenNothing clientProxyHosts $ do
        debug $ "looking up MX records for " <> encodeUtf8 clientRecipientDomain
        liftChat $ DNS.lookupMX clientRecipientDomain
    tryEach hosts $ \host -> do
        debug $ "looking up A record for " <> C.pack host
        dstIP <- liftChat $ DNS.lookupA host
        tryEach ports $ \port -> do
            debug $ "connecting to " <> C.pack dstIP <> ":" <> C.pack port
            conn <- liftChat $ Conn.connect (clientSourceIP, "0") (dstIP, port)
            modify $ \c -> c { clientConnection = conn
                             , clientHostname   = host
                             }
            when (port == "465") $ do
                debug "setting up tls"
                conn' <- liftChat $ Conn.secure conn host clientTLSValidated
                modify $ \c -> c { clientConnection = conn' }
            return ()
  where
    ports :: [Conn.ServiceName]
    ports = ["587", "465", "25", "2525"]
    whenNothing :: Applicative f => Maybe a -> f a -> f a
    whenNothing (Just x) _ = pure x
    whenNothing Nothing m  = m
    tryEach :: [a] -> (a -> Chat Client ()) -> Chat Client ()
    tryEach [] _     = return ()
    tryEach (a:[]) f = f a
    tryEach (a:as) f = f a `catchError` (const $ tryEach as f)

 -- | Verify the server greeting
greeting :: Chat Client ()
greeting = do
    _ <- listen 300
    return ()

-- | Send the HELO command
helo :: ClientParams -> Chat Client ()
helo ClientParams{..} = do
    say $ LB.fromStrict $ "HELO " `B.append` C.pack clientName `B.append` "\r\n"
    _ <- listen 300
    return ()

-- | Send the EHLO command
ehlo :: ClientParams -> Chat Client ()
ehlo ClientParams{..} = do
    say $ LB.fromStrict $ "EHLO " `B.append` C.pack clientName `B.append` "\r\n"
    (_, rtext) <- listen 300
    modify $ \c -> c { clientExtensions = replyToExtentions rtext }
    return ()
  where
    replyToExtentions :: [B.ByteString] -> [(B.ByteString, [B.ByteString])]
    replyToExtentions (_:lines) = map (extention . C.words . C.map toLower) lines
    replyToExtentions []        = []
    extention :: [B.ByteString] -> (B.ByteString, [B.ByteString])
    extention (a:as) = (a, as)
    extention []     = ("", [])

-- | Try to send the EHLO command, fallback to HELO if necessary
hello :: ClientParams -> Chat Client ()
hello params = (ehlo params) `catchError` (const $ helo params)

-- | Send the STARTTLS command and negotiate TLS context
startTLS :: ClientParams -> Chat Client ()
startTLS params@ClientParams{..} = do
    conn <- gets clientConnection
    serverTLS <- lookup "starttls" <$> gets clientExtensions
    unless (Conn.isSecure conn) $
        case (clientTLSRequired, serverTLS) of
            (_, Just _) -> do
                -- issue command and wait for response
                say "STARTTLS\r\n"
                _ <- listen 120
                -- negotiate TLS context
                host <- gets clientHostname
                debug "setting up tls"
                conn' <- liftChat $ Conn.secure conn host clientTLSValidated
                modify $ \c -> c { clientConnection = conn' }
                -- test TLS context by re-issuing hello
                hello params
            (False, Nothing) ->
                return ()
            (True, Nothing) ->
                throwError TLSNotSupported

auth :: ClientParams -> Chat Client ()
auth ClientParams{..} = do
    conn <- gets clientConnection
    serverAuthExtention <- lookup "auth" <$> gets clientExtensions
    case (clientAuthCredentials, serverAuthExtention) of
        (Just (user, pass), Just methods)
            | "login" `elem` methods -> do
                unless (Conn.isSecure conn) $ throwError AuthNotEncrypted
                say "AUTH LOGIN\r\n"
                _ <- listen 120
                whisper $ LB.fromStrict $ B64.encode (C.pack user) `B.append` "\r\n"
                _ <- listen 120
                whisper $ LB.fromStrict $ B64.encode (C.pack pass) `B.append` "\r\n"
                _ <- listen 120
                return ()
            | "plain" `elem` methods -> do
                unless (Conn.isSecure conn) $ throwError AuthNotEncrypted
                whisper $ LB.fromStrict $ "AUTH PLAIN " `B.append` B64.encode (B.concat [C.pack user, C.singleton '\0', C.pack user, C.singleton '\0', C.pack pass]) `B.append` "\r\n"
                _ <- listen 120
                return ()
            | otherwise -> throwError AuthMethodNotSupported
        _ ->
            return ()

-- | Send the MAIL FROM command
mailFrom :: Mailbox -> Chat Client ()
mailFrom sender = do
    say $ LB.fromStrict $ "MAIL FROM: <" `B.append` encodeUtf8 (formatAddrSpec sender) `B.append` ">\r\n"
    _ <- listen 300
    return ()

-- | Send the RCPT TO command
rcptTo :: Mailbox -> Chat Client ()
rcptTo recipient = do
    say $ LB.fromStrict $ "RCPT TO: <" `B.append` encodeUtf8 (formatAddrSpec recipient) `B.append` ">\r\n"
    _ <- listen 300
    return ()

-- | Send the DATA command
dataInit :: Chat Client ()
dataInit = do
    say "DATA\r\n"
    _ <- listen 120
    return ()

-- | Send the data block
dataBlock :: LB.ByteString -> Chat Client ()
dataBlock msg = do
    whisper $ msg `LB.append` "\r\n"
    return ()

-- | Terminate the data block
dataTerm :: Chat Client ()
dataTerm = do
    say ".\r\n"
    (rcode, _) <- listen 600
    when (rcode == 250) $ modify $ \c -> c { clientMessagesSent = clientMessagesSent c + 1 }
    return ()

-- | Send the QUIT command
quit :: Chat Client ()
quit = do
    say "QUIT\r\n"
    _ <- listen 120
    return ()

-- | Close the connection to the MX server
disconnect :: Chat Client ()
disconnect = do
    conn <- gets clientConnection
    debug "closing connection"
    conn' <- liftChat $ Conn.close conn
    modify $ \c -> c { clientConnection = conn' }
    return ()

-- | Send a command
say :: LB.ByteString -- ^ command
    -> Chat Client ()
say msg = do
    conn <- gets clientConnection
    send conn False msg
    modify $ \c -> c { clientLastCommand = Just msg
                     , clientLastReply   = Nothing
                     }
    return ()

-- | Send a command that shouldnt be logged
whisper :: LB.ByteString -- ^ command
        -> Chat Client ()
whisper msg = do
    conn <- gets clientConnection
    send conn True msg
    modify $ \c -> c { clientLastCommand = Nothing
                     , clientLastReply   = Nothing
                     }
    return ()

-- | Listen for a reply
listen :: NominalDiffTime -- ^ timeout
       -> Chat Client (Int, [B.ByteString])
listen ttl = do
    conn <- gets clientConnection
    reply <- recv conn False pReply ttl 512
    modify $ \c -> c { clientLastReply = Just reply }
    checkRC reply
    return reply
  where
    checkRC :: (Int, [B.ByteString]) -> Chat Client ()
    checkRC (rcode, _)
        | rcode >= 200 && rcode <= 399 = return ()
        | rcode >= 400 && rcode <= 499 = throwError TemporaryFailure
        | otherwise                    = throwError PermanentFailure
