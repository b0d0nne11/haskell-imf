{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module MailBin.MTA
  ( runMTA
  )
where

import           Control.Concurrent          (forkIO)
import           Control.Monad               (forever)
import           Control.Monad.Reader        (runReaderT)
import           Data.ByteString             (ByteString)
import           Data.Configurator           (lookupDefault)
import           Data.Configurator.Types     (Config)
import           Data.IMF                    (Mailbox (..), format, formatList)
import           Data.IMF.Network            (PassFail (..), Server (..), runServer)
import           Data.IMF.Network.Connection (Connection)
import qualified Data.IMF.Network.Connection as Connection
import           Data.Pool                   (Pool)
import           Data.Text                   (Text)
import qualified Data.Text.Encoding          as T
import qualified Database.SQLite.Simple      as DB
import qualified Network.TLS                 as TLS

import           MailBin.DB

newServer :: Connection -> TLS.ServerParams -> Pool DB.Connection -> Server
newServer conn tlsParams dbPool = Server
    { serverName = "mailbin"
    , serverConnection = conn
    , serverTLSParams = tlsParams
    , serverLogger = \_ -> return ()
    , serverAuthenticate = \_ _ -> return PermFail
    , serverVerifyReturnPath = \_ -> return Pass
    , serverVerifyRecipient = \_ -> return Pass
    , serverAcceptMessage = accept dbPool
    , serverMaxRecipients = 10
    , serverMaxMessageSize = 4096
    , serverReqTLS = False
    , serverReqAuth = False
    }

accept :: Pool DB.Connection -> Maybe Mailbox -> [Mailbox] -> ByteString -> IO PassFail
accept dbPool rp rcpts msg =
    Pass <$ insertMail dbPool (maybe "" format rp) (formatList rcpts) (T.decodeUtf8 msg)

loadConnection :: Config -> IO Connection
loadConnection config = do
    host <- lookupDefault "127.0.0.1" config "host"
    port <- lookupDefault "2525" config "port"
    Connection.listen (host, port)

loadTLSParams :: Config -> IO TLS.ServerParams
loadTLSParams config = do
    crt <- lookupDefault "./mailbin/test/Fixtures/localhost.crt" config "crt"
    key <- lookupDefault "./mailbin/test/Fixtures/localhost.key" config "key"
    TLS.credentialLoadX509 crt key >>= \case
        Left err   -> fail err
        Right cert -> return $ Connection.tlsServerParams cert

runMTA :: Config -> Pool DB.Connection -> IO ()
runMTA config dbPool = do
    conn <- loadConnection config
    tlsParams <- loadTLSParams config
    forever $ Connection.accept conn >>= \conn' ->
        forkIO $ runReaderT runServer $ newServer conn' tlsParams dbPool
