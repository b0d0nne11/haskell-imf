{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module MailBin.MTA
  ( runMTA
  )
where

import           Control.Concurrent          (forkIO)
import           Control.Concurrent.Async    (async, cancel)
import           Control.Concurrent.STM      (TVar, atomically, check, modifyTVar', newTVarIO,
                                              readTVar)
import           Control.Exception.Safe      (bracket_, onException, throwString)
import           Control.Monad               (forever)
import           Control.Monad.IO.Class      (liftIO, MonadIO)
import           Control.Monad.Logger        (logInfo, runStdoutLoggingT, MonadLogger)
import           Control.Monad.Reader        (runReaderT)
import qualified Data.ByteString.Lazy        as LB
import           Data.IMF                    (Mailbox)
import           Data.IMF.Network            (PassFail (..), Server (..), newServer, run)
import           Data.IMF.Network.Connection (Connection, HostName, ServiceName, SockAddr)
import qualified Data.IMF.Network.Connection as Conn
import           Data.Int                    (Int64)
import           Data.Pool                   (Pool)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Database.SQLite.Simple      as DB
import qualified Network.TLS                 as TLS

import           MailBin.Config
import           MailBin.DB

data ConfigParams = ConfigParams
    { configHost           :: HostName
    , configPort           :: ServiceName
    , configCertificate    :: TLS.Credential
    , configCredentials    :: [(Text, Text)]
    , configMaxRecipients  :: Int
    , configMaxMessageSize :: Int64
    , configRequireTLS     :: Bool
    , configRequireAuth    :: Bool
    }

loadConfigParams :: Config -> IO ConfigParams
loadConfigParams config = do
    certPath <- lookupDefault "localhost.crt" config "certificate"
    pkeyPath <- lookupDefault "localhost.key" config "private_key"
    ConfigParams <$> lookupDefault "127.0.0.1" config "host"
                 <*> lookupDefault "2525" config "port"
                 <*> loadCertificate certPath pkeyPath
                 <*> lookupDefault [] config "credentials"
                 <*> lookupDefault 10 config "max_recipients"
                 <*> lookupDefault 4096 config "max_message_size"
                 <*> lookupDefault False config "require_tls"
                 <*> lookupDefault False config "require_auth"

loadCertificate :: FilePath -> FilePath -> IO TLS.Credential
loadCertificate cert pkey = TLS.credentialLoadX509 cert pkey >>= either throwString return

newServer' :: Text -> Connection -> Pool DB.Connection -> ConfigParams -> IO Server
newServer' name conn dbPool ConfigParams{..} = do
    server <- liftIO $ newServer name conn
    return $ server
        { serverTLSParams = Conn.tlsServerParams configCertificate
        , serverAuthenticate = auth configCredentials
        , serverVerifyReturnPath = const $ return Pass
        , serverVerifyRecipient = const $ return Pass
        , serverAcceptMessage = acceptMessage dbPool
        , serverMaxRecipients = configMaxRecipients
        , serverMaxMessageSize = configMaxMessageSize
        , serverReqTLS = configRequireTLS
        , serverReqAuth = configRequireAuth
        }

auth :: [(Text, Text)] -> Text -> Text -> IO PassFail
auth creds user pass = return $ if (user, pass) `elem` creds then Pass else PermFail

acceptMessage :: Pool DB.Connection -> Maybe Mailbox -> [Mailbox] -> LB.ByteString -> IO PassFail
acceptMessage dbPool rp rcpts msg = Pass <$ insertMessage dbPool rp rcpts msg

runMTA :: (MonadIO m, MonadLogger m) => Config -> Pool DB.Connection -> m (m ())
runMTA config dbPool = do
    config@ConfigParams{..} <- liftIO $ loadConfigParams config
    $(logInfo) $ "starting mta @ smtp://" <> T.pack configHost <> ":" <> T.pack configPort
    t <- liftIO $ async $
        serve (configHost, configPort) $ \(conn', _) ->
            newServer' "mailbin" conn' dbPool config >>= runStdoutLoggingT . runReaderT run
    return $ do
        liftIO $ cancel t
        $(logInfo) "stopped mta"

serve :: (HostName, ServiceName) -> ((Connection, SockAddr) -> IO ()) -> IO ()
serve (host, port) f = do
    conn <- Conn.listen (host, port)
    numThreads <- newCounter
    onException (forever $ Conn.accept conn >>= forkIO . bracket_ (inc numThreads) (dec numThreads) . f)
                (Conn.close conn >> waitForZero numThreads)

newtype Counter = Counter (TVar Int)

newCounter :: IO Counter
newCounter = Counter <$> newTVarIO 0

waitForZero :: Counter -> IO ()
waitForZero (Counter ref) = atomically $ readTVar ref >>= check . (== 0)

inc :: Counter -> IO ()
inc (Counter ref) = atomically $ modifyTVar' ref (+ 1)

dec :: Counter -> IO ()
dec (Counter ref) = atomically $ modifyTVar' ref (subtract 1)
