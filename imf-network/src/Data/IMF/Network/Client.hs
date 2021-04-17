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

import           Control.Monad               (forM_, when, unless)
import           Control.Monad.IO.Unlift     (liftIO)
import           Control.Monad.Reader        (ReaderT, ask, asks)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as B
import qualified Data.ByteString.Base64      as B64
import qualified Data.ByteString.Char8       as C
import qualified Data.ByteString.Lazy        as LB
import           Data.List                   (uncons)
import           Data.Maybe                  (mapMaybe)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import qualified Network.TLS                 as TLS
import           System.Log.FastLogger       (LogStr, ToLogStr (..))

import           Data.IMF
import           Data.IMF.Network.Connection (Connection)
import qualified Data.IMF.Network.Connection as Connection
import           Data.IMF.Network.Errors

type ClientM = ReaderT Client IO

data Client = Client
    { clientName        :: Text
    , clientConnection  :: Connection
    , clientTLSParams   :: TLS.ClientParams
    , clientLogger      :: LogStr -> IO ()
    , clientCredentials :: Maybe (Text, Text)
    }

newClient :: Text -> Connection -> IO Client
newClient name conn =
    return $ Client
        { clientName        = name
        , clientConnection  = conn
        , clientTLSParams   = TLS.defaultParamsClient "localhost" ""
        , clientLogger      = \_ -> return ()
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

recvReply :: ClientM (Int, [Text])
recvReply = do
    Client{..} <- ask
    (chunk, r@(rcode, _)) <- Connection.recvReply clientConnection
    forM_ (C.lines chunk) $ \line -> logger $ "< " <> line
    when (rcode > 399) $ throwReply r
    return r

sendLine :: ByteString -> ClientM ()
sendLine line = do
    Client{..} <- ask
    Connection.sendLine clientConnection line
    logger $ "> " <> line

sendData :: LB.ByteString -> ClientM ()
sendData msg = do
    Client{..} <- ask
    Connection.send clientConnection msg
    logger $ "> [" <> show (LB.length msg) <> " bytes]"

secure :: ClientM ()
secure = do
    Client{..} <- ask
    Connection.secure clientConnection clientTLSParams
    logger ("- [tls handshake]" :: String)

close :: ClientM ()
close = do
    Client{..} <- ask
    Connection.close clientConnection
    logger ("- [closing connection]" :: String)

logger :: ToLogStr msg => msg -> ClientM ()
logger msg = asks clientLogger >>= \log -> liftIO $ log $ toLogStr msg <> "\n"

setup :: ClientM ClientSession
setup = opening >>= opportunisticTLS >>= authenticate
  where
    opening = do
        sessionBanner <- banner
        (sessionGreeting, sessionExtentions) <- hello
        let sessionUser = Nothing
        return ClientSession{..}
    opportunisticTLS ClientSession{..} = do
        connIsSecure <- asks clientConnection >>= Connection.isSecure
        case (connIsSecure, "starttls" `lookup` sessionExtentions) of
            (False, Just _) -> do
                startTLS
                (sessionGreeting, sessionExtentions) <- hello
                return ClientSession{..}
            _ ->
                return ClientSession{..}
    authenticate ClientSession{..} = do
        connIsSecure <- asks clientConnection >>= Connection.isSecure
        creds <- asks clientCredentials
        case (connIsSecure, creds, "auth" `lookup` sessionExtentions) of
            (True, Just (user, pass), Just methods) -> do
                sessionUser <- auth methods (user, pass)
                return ClientSession{..}
            _ ->
                return ClientSession{..}

deliver :: Mailbox -> [Mailbox] -> LB.ByteString -> ClientM ()
deliver returnPath recipients msg = do
    mailFrom returnPath
    mapM_ rcptTo recipients
    dataInit
    dataBlock msg
    dataTerm

-- | Verify the server banner
banner :: ClientM Text
banner =
    cuttoff 300 recvReply >>= \case
        (_, [])  -> return ""
        (_, l:_) -> return l

-- | Send the HELO command
helo :: ClientM (Text, [Extention])
helo = do
    name <- asks clientName
    sendLine $ "HELO " <> T.encodeUtf8 name <> "\r"
    cuttoff 300 recvReply >>= \case
        (_, [])         -> return ("", [])
        (_, greeting:_) -> return (greeting, [])

-- | Send the EHLO command
ehlo :: ClientM (Text, [Extention])
ehlo = do
    name <- asks clientName
    sendLine $ "EHLO " <> T.encodeUtf8 name <> "\r"
    cuttoff 300 recvReply >>= \case
        (_, [])          -> return ("", [])
        (_, greeting:ls) -> return (greeting, toExtentions ls)
  where
    toExtentions = mapMaybe (uncons . T.words . T.toLower)

-- | Try to send the EHLO command, fallback to HELO if necessary
hello :: ClientM (Text, [Extention])
hello = ehlo `catchReply` const helo

-- | Send the STARTTLS command and negotiate TLS context
startTLS :: ClientM ()
startTLS = do
    sendLine "STARTTLS\r"
    _ <- cuttoff 120 recvReply
    secure

auth :: [Text] -> (Text, Text) -> ClientM (Maybe Text)
auth methods (username, password)
    | "login" `elem` methods = do
        sendLine "AUTH LOGIN\r"
        _ <- cuttoff 120 recvReply
        sendLine $ B64.encode (T.encodeUtf8 username) <> "\r"
        _ <- cuttoff 120 recvReply
        sendLine $ B64.encode (T.encodeUtf8 password) <> "\r"
        _ <- cuttoff 120 recvReply
        return $ Just username
    | "plain" `elem` methods = do
        sendLine $ "AUTH PLAIN " <> B64.encode (B.concat [T.encodeUtf8 username, C.singleton '\0', T.encodeUtf8 username, C.singleton '\0', T.encodeUtf8 password]) <> "\r"
        _ <- cuttoff 120 recvReply
        return $ Just username
    | otherwise =
        return Nothing

-- | Send the MAIL FROM command
mailFrom :: Mailbox -> ClientM ()
mailFrom returnPath = do
    sendLine $ "MAIL FROM:<" <> T.encodeUtf8 (format $ AddrSpec returnPath) <> ">\r"
    _ <- cuttoff 300 recvReply
    return ()

-- | Send the RCPT TO command
rcptTo :: Mailbox -> ClientM ()
rcptTo recipient = do
    sendLine $ "RCPT TO:<" <> T.encodeUtf8 (format $ AddrSpec recipient) <> ">\r"
    _ <- cuttoff 300 recvReply
    return ()

-- | Send the DATA command
dataInit :: ClientM ()
dataInit = do
    sendLine "DATA\r"
    _ <- cuttoff 120 recvReply
    return ()

-- | Send the data block
dataBlock :: LB.ByteString -> ClientM ()
dataBlock msg = do
    sendData msg
    unless (LB.last msg == 10) $ sendLine "\r"

-- | Terminate the data block
dataTerm :: ClientM ()
dataTerm = do
    sendLine ".\r"
    _ <- cuttoff 600 recvReply
    return ()

-- | Send the QUIT command
quit :: ClientM ()
quit = do
    sendLine "QUIT\r"
    _ <- cuttoff 120 recvReply
    close
