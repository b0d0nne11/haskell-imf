{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TupleSections     #-}

module Data.IMF.Network.Client
  ( Client(..)
  , SessionProps(..)
  , initClient
  , deliver
  , finalizeClient
  , greeting
  , helo
  , ehlo
  , hello
  , startTLS
  , auth
  , mailFrom
  , rcptTo
  , dataInit
  , dataBlock
  , dataTerm
  , quit
  , Envelope(..)
  , parseWhileM
  , cuttoff
  )
where

import           Control.Monad              (when)
import           Control.Monad.IO.Unlift    (MonadUnliftIO, liftIO)
import           Control.Monad.Reader       (MonadReader, asks, runReaderT)
import           Data.Attoparsec.ByteString (IResult (..), Result, parse)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Base64     as B64
import qualified Data.ByteString.Char8      as C
import qualified Data.ByteString.Lazy       as LB
import           Data.Char                  (toLower)
import           Data.Default.Class         (def)
import           Data.List                  (uncons)
import           Data.Maybe                 (fromMaybe, mapMaybe)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Text.Encoding         as T
import           Data.Time.Clock            (NominalDiffTime)
import           Network.Connection
import           UnliftIO.Exception         (catchJust, catchAny, throwIO, throwString)
import           UnliftIO.Timeout           (timeout)

import           Data.IMF
import           Data.IMF.Network.Errors
import           Data.IMF.Parsers.Network       ( pReply )

data Client = Client
    { clientName        :: Text
    , clientConnection  :: Connection
    , clientLogger      :: ByteString -> IO ()
    , clientUseStartTLS :: Maybe TLSSettings
    , clientUseAuth     :: Maybe (String, String)
    }

data SessionProps = SessionProps
    { sessionGreeting        :: [Text]
    , sessionExtentions      :: [Extention]
    , sessionIsSecure        :: Bool
    , sessionIsAuthenticated :: Bool
    }

data Envelope = Envelope
    { returnPath :: Mailbox
    , recipient  :: Mailbox
    }

type Reply = (Int, [Text])

type Extention = (Text, [Text])

getReplies :: (MonadReader Client m, MonadUnliftIO m)
           => m [Reply]
getReplies = do
    replies <- parseWhileM (parse pReply) 4096 getChunk
    checkReply (last replies) >> return replies
  where
    checkReply (code, _)
        | 400 <= code && code <= 499 = throwIO TemporaryFailure
        | 500 <= code && code <= 599 = throwIO PermanentFailure
        | otherwise                  = return ()

getExtentions :: (MonadReader Client m, MonadUnliftIO m)
              => m [Extention]
getExtentions = mapMaybe (uncons . T.words . T.toLower) . drop 1 . snd . head <$> getReplies

putCommand :: (MonadReader Client m, MonadUnliftIO m)
           => Command
           -> m ()
putCommand = putChunk . T.encodeUtf8 . (`T.append` "\r\n") . format

getChunk :: (MonadReader Client m, MonadUnliftIO m)
         => m ByteString
getChunk = do
    conn <- asks clientConnection
    chunk <- liftIO $ connectionGetChunk conn
    _ <- logger $ "<- " <> chunk
    return chunk

putChunk :: (MonadReader Client m, MonadUnliftIO m)
         => ByteString
         -> m ()
putChunk chunk = do
    conn <- asks clientConnection
    _ <- logger $ "-> " <> chunk
    liftIO $ connectionPut conn chunk

logger :: (MonadReader Client m, MonadUnliftIO m)
       => ByteString
       -> m ()
logger msg = asks clientLogger >>= (\logger -> liftIO $ logger msg)

parseWhileM :: MonadUnliftIO m
            => (ByteString -> Result a)
            -> Int
            -> m ByteString
            -> m [a]
parseWhileM parse limit f = next parse limit f
  where
    next parse' limit' f' = do
        when (limit' <= 0) $ throwString "failed to parse: maximum number of bytes received"
        f' >>= \chunk' -> case parse' chunk' of
            Fail _ _ err    -> throwString $ "failed to parse: " <> err
            Partial parse'' -> next parse'' (limit' - B.length chunk') f
            Done "" a       -> return [a]
            Done chunk'' a  -> (a:) <$> next parse (limit' - B.length chunk' + B.length chunk'') (return chunk'')

cuttoff :: MonadUnliftIO m
        => NominalDiffTime
        -> m a
        -> m a
cuttoff t f = timeout (floor $ t * 1000000) f >>= maybe (throwIO Timeout) return

initClient :: Client
           -> ConnectionContext
           -> IO SessionProps
initClient client ctx =
    runReaderT chat client
  where
    chat = do
        sessionGreeting <- greeting
        exts <- hello
        (sessionIsSecure, tlsExts) <- startTLS ("starttls" `lookup` exts) ctx
        let sessionExtentions = fromMaybe exts tlsExts
        sessionIsAuthenticated <- auth ("auth" `lookup` sessionExtentions)
        return SessionProps{..}

deliver :: Client
        -> Envelope
        -> LB.ByteString
        -> IO ()
deliver client (Envelope returnPath recipient) msg =
    runReaderT chat client
  where
    chat = do
        mailFrom returnPath
        rcptTo recipient
        dataInit
        dataBlock msg
        dataTerm

finalizeClient :: Client -> IO ()
finalizeClient = runReaderT quit

-- | Verify the server greeting
greeting :: (MonadReader Client m, MonadUnliftIO m)
         => m [Text]
greeting =
    snd . head <$> cuttoff 300 getReplies

-- | Send the HELO command
helo :: (MonadReader Client m, MonadUnliftIO m)
     => m ()
helo = do
    name <- asks clientName
    putCommand $ HELO name
    _ <- cuttoff 300 getReplies
    return ()

-- | Send the EHLO command
ehlo :: (MonadReader Client m, MonadUnliftIO m)
     => m [Extention]
ehlo = do
    name <- asks clientName
    putCommand $ EHLO name
    cuttoff 300 getExtentions

-- | Try to send the EHLO command, fallback to HELO if necessary
hello :: (MonadReader Client m, MonadUnliftIO m)
      => m [Extention]
--hello = catchJust (\e -> if e `elem` [PermanentFailure, TemporaryFailure] then Just () else Nothing)
--                  ehlo (const $ [] <$ helo)
hello = ehlo `catchAny` const (helo >> return [])

-- | Send the STARTTLS command and negotiate TLS context
startTLS :: (MonadReader Client m, MonadUnliftIO m)
         => Maybe [Text]
         -> ConnectionContext
         -> m (Bool, Maybe [Extention])
startTLS Nothing _ = do
    conn <- asks clientConnection
    liftIO $ (, Nothing) <$> connectionIsSecure conn
startTLS (Just _) ctx = do
    conn <- asks clientConnection
    connIsSecure <- liftIO $ connectionIsSecure conn
    if connIsSecure then return (True, Nothing) else
        asks clientUseStartTLS >>= \case
            Just tlsSettings -> do
                -- issue command and wait for response
                putCommand STARTTLS
                _ <- cuttoff 120 getReplies
                -- negotiate TLS context
                logger "setting up tls"
                liftIO $ connectionSetSecure ctx conn tlsSettings
                -- test TLS context by re-issuing hello
                (True,) . Just <$> hello
            _ ->
                return (False, Nothing)

auth :: (MonadReader Client m, MonadUnliftIO m)
     => Maybe [Text]
     -> m Bool
auth Nothing = return False
auth (Just methods) = do
    conn <- asks clientConnection
    connIsSecure <- liftIO $ connectionIsSecure conn
    asks clientUseAuth >>= \case
        Just (user, pass)
            | not connIsSecure ->
                return False
            | "login" `elem` methods -> do
                putCommand $ AUTH "LOGIN"
                _ <- cuttoff 120 getReplies
                putChunk $ B64.encode (C.pack user) <> "\r\n"
                _ <- cuttoff 120 getReplies
                putChunk $ B64.encode (C.pack pass) <> "\r\n"
                _ <- cuttoff 120 getReplies
                return True
            | "plain" `elem` methods -> do
                putChunk $ "AUTH PLAIN " <> B64.encode (B.concat [C.pack user, C.singleton '\0', C.pack user, C.singleton '\0', C.pack pass]) <> "\r\n"
                _ <- cuttoff 120 getReplies
                return True
            | otherwise -> throwIO AuthMethodNotSupported
        _ ->
            return False

-- | Send the MAIL FROM command
mailFrom :: (MonadReader Client m, MonadUnliftIO m)
         => Mailbox
         -> m ()
mailFrom returnPath = do
    putCommand $ MAIL (format $ AddrSpec returnPath) ""
    _ <- cuttoff 300 getReplies
    return ()

-- | Send the RCPT TO command
rcptTo :: (MonadReader Client m, MonadUnliftIO m)
       => Mailbox
       -> m ()
rcptTo recipient = do
    putCommand $ RCPT (format $ AddrSpec recipient) ""
    _ <- cuttoff 300 getReplies
    return ()

-- | Send the DATA command
dataInit :: (MonadReader Client m, MonadUnliftIO m)
         => m ()
dataInit = do
    putCommand DATA
    _ <- cuttoff 120 getReplies
    return ()

-- | Send the data block
dataBlock :: (MonadReader Client m, MonadUnliftIO m)
          => LB.ByteString
          -> m ()
dataBlock msg = do
    putChunk $ LB.toStrict $ msg <> "\r\n"
    return ()

-- | Terminate the data block
dataTerm :: (MonadReader Client m, MonadUnliftIO m)
         => m ()
dataTerm = do
    putChunk ".\r\n"
    _ <- cuttoff 600 getReplies
    return ()

-- | Send the QUIT command
quit :: (MonadReader Client m, MonadUnliftIO m)
     => m ()
quit = do
    putCommand QUIT
    _ <- cuttoff 120 getReplies
    return ()

