{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.IMF.Network.Client
  ( Client(..)
  , ClientSession(..)
  , setup
  , deliver
  , quit
  )
where

import           Control.Monad               (join, unless, when)
import           Control.Monad.IO.Unlift     (MonadUnliftIO, liftIO)
import           Control.Monad.Reader        (ReaderT, ask, asks)
import qualified Data.Attoparsec.ByteString  as B (IResult (..), Result, parse)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as B
import qualified Data.ByteString.Base64      as B64
import qualified Data.ByteString.Char8       as C
import qualified Data.ByteString.Lazy        as LB
import           Data.Char                   (toLower)
import           Data.List                   (uncons)
import           Data.Maybe                  (mapMaybe)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Text.Encoding          as T
import qualified Network.TLS                 as TLS
import           System.Log.FastLogger
import           UnliftIO.Exception          (throwIO)

import           Data.IMF
import           Data.IMF.Network.Connection (Connection)
import qualified Data.IMF.Network.Connection as Connection
import           Data.IMF.Network.Errors
import           Data.IMF.Network.Parsers

type ClientM = ReaderT Client IO

data Client = Client
    { clientName        :: Text
    , clientConnection  :: Connection
    , clientTLSParams   :: TLS.ClientParams
    , clientLogger      :: LogStr -> IO ()
    , clientCredentials :: Maybe (Text, Text)
    }

data ClientSession = ClientSession
    { sessionBanner     :: Text
    , sessionGreeting   :: Text
    , sessionExtentions :: [Extention]
    , sessionUser       :: Maybe Text
    }
  deriving Show

type Extention = (Text, [Text])

recvParsed :: (ByteString -> B.Result a) -> Int -> ClientM a
recvParsed parse limit = do
    when (limit < 0) $ throwIO ParseSizeExceeded
    join $ recv' $ \chunk -> case parse chunk of
        B.Fail chunk' _ err -> do
            logger . ("< " <>) $ B.take (B.length chunk - B.length chunk') chunk
            return (chunk', throwIO $ ParseFailure err)
        B.Partial parse' -> do
            logger . ("< " <>) $ chunk
            return ("", recvParsed parse' $ limit - B.length chunk)
        B.Done chunk' a -> do
            logger . ("< " <>) $ B.take (B.length chunk - B.length chunk') chunk
            return (chunk', return a)
  where
    recv' f = asks clientConnection >>= \conn -> Connection.recv' conn f

recvReply :: ClientM (Int, [Text])
recvReply = recvParsed (B.parse pReply) 4096 >>= checkReply

checkReply :: (Int, [Text]) -> ClientM (Int, [Text])
checkReply r@(code, _)
    | 200 <= code && code <= 399 = return r
    | otherwise                  = throwReply r

send :: ByteString -> ClientM ()
send chunk = do
    conn <- asks clientConnection
    Connection.send conn chunk
    logger . ("> " <>) $ chunk

logger :: ToLogStr msg => msg -> ClientM ()
logger msg = asks clientLogger >>= \log -> liftIO $ log $ toLogStr msg

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
                asks clientTLSParams >>= startTLS
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
banner = T.unwords . snd <$> cuttoff 300 recvReply

-- | Send the HELO command
helo :: ClientM (Text, [Extention])
helo = do
    name <- asks clientName
    send $ "HELO " <> T.encodeUtf8 name <> "\r\n"
    cuttoff 300 recvReply >>= \case
        (_, [])         -> return ("", [])
        (_, greeting:_) -> return (greeting, [])

-- | Send the EHLO command
ehlo :: ClientM (Text, [Extention])
ehlo = do
    name <- asks clientName
    send $ "EHLO " <> T.encodeUtf8 name <> "\r\n"
    cuttoff 300 recvReply >>= \case
        (_, [])             -> return ("", [])
        (_, greeting:lines) -> return (greeting, toExtentions lines)
  where
    toExtentions = mapMaybe (uncons . T.words . T.toLower)

-- | Try to send the EHLO command, fallback to HELO if necessary
hello :: ClientM (Text, [Extention])
hello = ehlo `catchReply` const helo

-- | Send the STARTTLS command and negotiate TLS context
startTLS :: TLS.ClientParams -> ClientM ()
startTLS tlsParams = do
    send "STARTTLS\r\n"
    _ <- cuttoff 120 recvReply
    logger (". [tls handshake]\r\n" :: String)
    conn <- asks clientConnection
    Connection.secure conn tlsParams

auth :: [Text] -> (Text, Text) -> ClientM (Maybe Text)
auth methods (username, password)
    | "login" `elem` methods = do
        send "AUTH LOGIN\r\n"
        _ <- cuttoff 120 recvReply
        send $ B64.encode (T.encodeUtf8 username) <> "\r\n"
        _ <- cuttoff 120 recvReply
        send $ B64.encode (T.encodeUtf8 password) <> "\r\n"
        _ <- cuttoff 120 recvReply
        return $ Just username
    | "plain" `elem` methods = do
        send $ "AUTH PLAIN " <> B64.encode (B.concat [T.encodeUtf8 username, C.singleton '\0', T.encodeUtf8 username, C.singleton '\0', T.encodeUtf8 password]) <> "\r\n"
        _ <- cuttoff 120 recvReply
        return $ Just username
    | otherwise =
        return Nothing

-- | Send the MAIL FROM command
mailFrom :: Mailbox -> ClientM ()
mailFrom returnPath = do
    send $ "MAIL FROM:<" <> T.encodeUtf8 (format $ AddrSpec returnPath) <> ">\r\n"
    _ <- cuttoff 300 recvReply
    return ()

-- | Send the RCPT TO command
rcptTo :: Mailbox -> ClientM ()
rcptTo recipient = do
    send $ "RCPT TO:<" <> T.encodeUtf8 (format $ AddrSpec recipient) <> ">\r\n"
    _ <- cuttoff 300 recvReply
    return ()

-- | Send the DATA command
dataInit :: ClientM ()
dataInit = do
    send "DATA\r\n"
    _ <- cuttoff 120 recvReply
    return ()

-- | Send the data block
dataBlock :: LB.ByteString -> ClientM ()
dataBlock msg = do
    mapM_ send $ LB.toChunks msg
    unless ("\n" `LB.isSuffixOf` msg) $ send "\r\n"

-- | Terminate the data block
dataTerm :: ClientM ()
dataTerm = do
    send ".\r\n"
    _ <- cuttoff 600 recvReply
    return ()

-- | Send the QUIT command
quit :: ClientM ()
quit = do
    send "QUIT\r\n"
    _ <- cuttoff 120 recvReply
    return ()
