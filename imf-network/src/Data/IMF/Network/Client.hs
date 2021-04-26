{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.IMF.Network.Client
  ( Client(..)
  , ClientSession(..)
  , newClient
  , setup
  , deliver
  , quit
  )
where

import           Control.Exception.Safe      (MonadMask)
import           Control.Monad               (forM_, unless, when)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Logger        (MonadLogger, logDebugN)
import           Control.Monad.Reader        (MonadReader, ask, asks)
import qualified Data.ByteString.Lazy        as LB
import           Data.List                   (uncons)
import           Data.Maybe                  (mapMaybe)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import qualified Data.Text.Encoding.Base64   as T
import           Data.Time                   (NominalDiffTime)
import qualified Network.TLS                 as TLS

import           Data.IMF                    (AddrSpec (..), Mailbox, format)
import           Data.IMF.Network.Connection (Connection)
import qualified Data.IMF.Network.Connection as Conn
import           Data.IMF.Network.Errors     (catchReply, cuttoff, throwReply)

type ClientEnv m = (MonadIO m, MonadLogger m, MonadMask m, MonadReader Client m)

data Client = Client
    { clientName        :: Text
    , clientConnection  :: Connection
    , clientTLSParams   :: TLS.ClientParams
    , clientCredentials :: Maybe (Text, Text)
    }

newClient :: Text -> Connection -> IO Client
newClient name conn =
    return $ Client
        { clientName        = name
        , clientConnection  = conn
        , clientTLSParams   = TLS.defaultParamsClient "localhost" ""
        , clientCredentials = Nothing
        }

data ClientSession = ClientSession
    { sessionBanner     :: Text
    , sessionGreeting   :: Text
    , sessionExtentions :: [Extention]
    , sessionUser       :: Maybe Text
    }
  deriving Show

type Extention = (Text, [Text])

recvReply :: ClientEnv m => NominalDiffTime -> m (Int, [Text])
recvReply ttl = do
    Client{..} <- ask
    (chunk, r@(rcode, _)) <- liftIO $ cuttoff ttl $ Conn.recvReply clientConnection
    forM_ (T.lines $ T.decodeUtf8 chunk) $ \line -> logDebugN $ "< " <> line
    when (rcode > 399) $ throwReply r
    return r

sendLine :: ClientEnv m => Text -> m ()
sendLine line = do
    Client{..} <- ask
    liftIO $ Conn.sendLine clientConnection $ T.encodeUtf8 line
    logDebugN $ "> " <> line

sendData :: ClientEnv m => LB.ByteString -> m ()
sendData msg = do
    Client{..} <- ask
    liftIO $ Conn.send clientConnection msg
    logDebugN $ "> [" <> T.pack (show $ LB.length msg) <> " bytes]"

secure :: ClientEnv m => m ()
secure = do
    Client{..} <- ask
    liftIO $ Conn.secure clientConnection clientTLSParams
    logDebugN "- [tls handshake]"

close :: ClientEnv m => m ()
close = do
    Client{..} <- ask
    liftIO $ Conn.close clientConnection
    logDebugN "- [closing connection]"

setup :: ClientEnv m => m ClientSession
setup = opening >>= opportunisticTLS >>= authenticate
  where
    opening = do
        sessionBanner <- banner
        (sessionGreeting, sessionExtentions) <- hello
        let sessionUser = Nothing
        return ClientSession{..}
    opportunisticTLS ClientSession{..} = do
        connIsSecure <- asks clientConnection >>= liftIO . Conn.isSecure
        case (connIsSecure, "starttls" `lookup` sessionExtentions) of
            (False, Just _) -> do
                startTLS
                (sessionGreeting, sessionExtentions) <- hello
                return ClientSession{..}
            _ ->
                return ClientSession{..}
    authenticate ClientSession{..} = do
        connIsSecure <- asks clientConnection >>= liftIO . Conn.isSecure
        creds <- asks clientCredentials
        case (connIsSecure, creds, "auth" `lookup` sessionExtentions) of
            (True, Just (user, pass), Just methods) -> do
                sessionUser <- auth methods (user, pass)
                return ClientSession{..}
            _ ->
                return ClientSession{..}

deliver :: ClientEnv m => Mailbox -> [Mailbox] -> LB.ByteString -> m ()
deliver returnPath recipients msg = do
    mailFrom returnPath
    mapM_ rcptTo recipients
    dataInit
    dataBlock msg
    dataTerm

-- | Verify the server banner
banner :: ClientEnv m => m Text
banner =
    recvReply 300 >>= \case
        (_, [])  -> return ""
        (_, l:_) -> return l

-- | Send the HELO command
helo :: ClientEnv m => m (Text, [Extention])
helo = do
    name <- asks clientName
    sendLine $ "HELO " <> name <> "\r"
    recvReply 300 >>= \case
        (_, [])         -> return ("", [])
        (_, greeting:_) -> return (greeting, [])

-- | Send the EHLO command
ehlo :: ClientEnv m => m (Text, [Extention])
ehlo = do
    name <- asks clientName
    sendLine $ "EHLO " <> name <> "\r"
    recvReply 300 >>= \case
        (_, [])          -> return ("", [])
        (_, greeting:ls) -> return (greeting, toExtentions ls)
  where
    toExtentions = mapMaybe (uncons . T.words . T.toLower)

-- | Try to send the EHLO command, fallback to HELO if necessary
hello :: ClientEnv m => m (Text, [Extention])
hello = ehlo `catchReply` const helo

-- | Send the STARTTLS command and negotiate TLS context
startTLS :: ClientEnv m => m ()
startTLS = do
    sendLine "STARTTLS\r"
    _ <- recvReply 120
    secure

auth :: ClientEnv m => [Text] -> (Text, Text) -> m (Maybe Text)
auth methods (username, password)
    | "login" `elem` methods = do
        sendLine "AUTH LOGIN\r"
        _ <- recvReply 120
        sendLine $ T.encodeBase64 username <> "\r"
        _ <- recvReply 120
        sendLine $ T.encodeBase64 password <> "\r"
        _ <- recvReply 120
        return $ Just username
    | "plain" `elem` methods = do
        sendLine $ "AUTH PLAIN " <> T.encodeBase64 (T.intercalate (T.singleton '\0') [username, username, password]) <> "\r"
        _ <- recvReply 120
        return $ Just username
    | otherwise =
        return Nothing

-- | Send the MAIL FROM command
mailFrom :: ClientEnv m => Mailbox -> m ()
mailFrom returnPath = do
    sendLine $ "MAIL FROM:<" <> format (AddrSpec returnPath) <> ">\r"
    _ <- recvReply 300
    return ()

-- | Send the RCPT TO command
rcptTo :: ClientEnv m => Mailbox -> m ()
rcptTo recipient = do
    sendLine $ "RCPT TO:<" <> format (AddrSpec recipient) <> ">\r"
    _ <- recvReply 300
    return ()

-- | Send the DATA command
dataInit :: ClientEnv m => m ()
dataInit = do
    sendLine "DATA\r"
    _ <- recvReply 120
    return ()

-- | Send the data block
dataBlock :: ClientEnv m => LB.ByteString -> m ()
dataBlock msg = do
    sendData msg
    unless (LB.last msg == 10) $ sendLine "\r"

-- | Terminate the data block
dataTerm :: ClientEnv m => m ()
dataTerm = do
    sendLine ".\r"
    _ <- recvReply 600
    return ()

-- | Send the QUIT command
quit :: ClientEnv m => m ()
quit = do
    sendLine "QUIT\r"
    _ <- recvReply 120
    close
