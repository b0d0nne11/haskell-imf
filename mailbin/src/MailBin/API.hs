{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module MailBin.API
  ( runAPI
  )
where

import           Control.Monad.IO.Class    (liftIO)
import           Data.Configurator         (lookupDefault)
import           Data.Configurator.Types   (Config)
import           Data.Maybe                (fromMaybe)
import           Data.Pool                 (Pool)
import           Data.String               (fromString)
import qualified Database.SQLite.Simple    as DB
import           Network.HTTP.Types.Status (Status (..))
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.Log.FastLogger     (FastLogger, toLogStr)

import           MailBin.DB

type MailAPI = "mail" :> QueryParam "limit" Integer :> QueryParam "offset" Integer :> Get '[JSON] [Mail]
          :<|> "mail" :> Capture "mailID" Integer :> DeleteNoContent

api :: Proxy MailAPI
api = Proxy

server :: Pool DB.Connection -> Server MailAPI
server dbPool = (\limit offset -> liftIO (selectMail dbPool (fromMaybe 10 limit) (fromMaybe 0 offset)))
           :<|> (\mailID -> NoContent <$ liftIO (deleteMail dbPool mailID))

app :: Pool DB.Connection -> Application
app dbPool = serve api (server dbPool)

runConfig :: FastLogger -> Config -> Application -> IO ()
runConfig logger config app = do
    host <- fromString <$> lookupDefault "127.0.0.1" config "host"
    port <- lookupDefault 8080 config "port"
    runSettings (setHost host $ setPort port $ setLogger (logRequest logger) defaultSettings) app

logRequest :: FastLogger -> Request -> Status -> Maybe Integer -> IO ()
logRequest logger req status size =
    logger $ "\""
          <> toLogStr (requestMethod req)
          <> " "
          <> toLogStr (rawPathInfo req <> rawQueryString req)
          <> " "
          <> toLogStr (show $ httpVersion req)
          <> "\" "
          <> toLogStr (show $ statusCode status)
          <> " "
          <> toLogStr (maybe "-" show size)
          <> "\n"

runAPI :: FastLogger -> Config -> Pool DB.Connection -> IO ()
runAPI logger config dbPool = runConfig logger config $ app dbPool
