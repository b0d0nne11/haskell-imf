{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MailBin.MTA
  ( runMTA
  )
where

import           Control.Concurrent          (MVar, forkIO, modifyMVar_, myThreadId, newEmptyMVar,
                                              newMVar, putMVar, readMVar, takeMVar, threadDelay)
import           Control.Concurrent.Async    (Async, async, poll)
import           Control.Exception           (handle)
import           Control.Monad               (filterM, forever)
import           Control.Monad.Loops         (untilM_)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Lazy        as LB
import           Data.IMF                    (Mailbox)
import           Data.IMF.Network            (PassFail (..), Server (..), runServer)
import qualified Data.IMF.Network.Connection as Connection
import           Data.Maybe                  (isNothing)
import           Data.Pool                   (Pool)
import           Data.Text                   (Text)
import qualified Database.SQLite.Simple      as DB
import           GHC.IO.Exception            (IOException)
import           Network.Simple.TCP          (HostPreference, ServiceName, SockAddr, Socket, accept,
                                              bindSock, closeSock, listenSock)
import qualified Network.TLS                 as TLS
import           System.Log.FastLogger       (FastLogger, toLogStr)

import           MailBin.Config
import           MailBin.DB

data ConfigParams = ConfigParams
    { configHost           :: HostPreference
    , configPort           :: ServiceName
    , configCertificate    :: String
    , configPrivateKey     :: String
    , configCredentials    :: [(Text, Text)]
    , configMaxRecipients  :: Int
    , configMaxMessageSize :: Int
    , configRequireTLS     :: Bool
    , configRequireAuth    :: Bool
    }

loadConfigParams :: Config -> IO ConfigParams
loadConfigParams config =
    ConfigParams <$> lookupDefault "127.0.0.1" config "host"
                 <*> lookupDefault "2525" config "port"
                 <*> lookupDefault "localhost.crt" config "certificate"
                 <*> lookupDefault "localhost.key" config "private_key"
                 <*> lookupDefault [] config "credentials"
                 <*> lookupDefault 10 config "max_recipients"
                 <*> lookupDefault 4096 config "max_message_size"
                 <*> lookupDefault False config "require_tls"
                 <*> lookupDefault False config "require_auth"

auth :: [(Text, Text)] -> Text -> Text -> IO PassFail
auth creds user' pass' = return $ if (user', pass') `elem` creds then Pass else PermFail

acceptMessage :: Pool DB.Connection -> Maybe Mailbox -> [Mailbox] -> ByteString -> IO PassFail
acceptMessage dbPool rp rcpts msg = Pass <$ insertMessage dbPool rp rcpts (LB.fromStrict msg)

acceptSockLoop :: MVar [Async ()] -> Socket -> ((Socket, SockAddr) -> IO ()) -> IO ()
acceptSockLoop ts sock = handle (\(_ :: IOException) -> return ()) . forever . acceptSockFork ts sock

acceptSockFork :: MVar [Async ()] -> Socket -> ((Socket, SockAddr) -> IO ()) -> IO ()
acceptSockFork ts sock f =
    accept sock $ \a ->
        modifyMVar_ ts $ \ts' -> do
            t <- async $ f a
            return (t:ts')

pollAll :: MVar [Async ()] -> IO ()
pollAll ts = modifyMVar_ ts $ filterM (fmap isNothing . poll)

waitAll :: MVar [Async ()] -> IO ()
waitAll ts = threadDelay 250000 `untilM_` null <$> readMVar ts

runMTA :: Config -> FastLogger -> Pool DB.Connection -> IO (IO ())
runMTA config logger dbPool = do
    ConfigParams{..} <- loadConfigParams config
    cert <- either error id <$> TLS.credentialLoadX509 configCertificate configPrivateKey
    (sock, addr) <- bindSock configHost configPort
    listenSock sock 2048
    logger $ "starting mta @ smtp://" <> toLogStr (show addr) <> "\n"
    ts <- newMVar []
    done <- newEmptyMVar
    forkIO $
        forever $ do
            threadDelay 250000
            pollAll ts
    forkIO $ do
        acceptSockLoop ts sock $ \(sock', addr') -> do
            tid <- myThreadId
            let tlogger = logger . (<>) ("(" <> toLogStr (show tid) <> ") ")
            tlogger $ "accepted smtp connection from " <> toLogStr (show addr') <> "\n"
            conn <- Connection.fromSocket sock'
            runServer $ Server { serverName = "mailbin"
                               , serverConnection = conn
                               , serverTLSParams = Connection.tlsServerParams cert
                               , serverLogger = tlogger
                               , serverAuthenticate = auth configCredentials
                               , serverVerifyReturnPath = const $ return Pass
                               , serverVerifyRecipient = const $ return Pass
                               , serverAcceptMessage = acceptMessage dbPool
                               , serverMaxRecipients = configMaxRecipients
                               , serverMaxMessageSize = configMaxMessageSize
                               , serverReqTLS = configRequireTLS
                               , serverReqAuth = configRequireAuth
                               }
        waitAll ts
        logger "stopped mta\n"
        putMVar done ()
    return $ do
        closeSock sock
        takeMVar done
