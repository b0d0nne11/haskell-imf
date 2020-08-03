{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module MailBin.API
  ( runAPI
  )
where

import           Control.Monad.IO.Class   (liftIO)
import           Data.Configurator        (lookupDefault)
import           Data.Configurator.Types  (Config)
import           Data.Maybe               (fromMaybe)
import           Data.Pool                (Pool)
import           Data.String              (fromString)
import qualified Database.SQLite.Simple   as DB
import           Network.Wai.Handler.Warp
import           Servant

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

runConfig :: Config -> Application -> IO ()
runConfig config app = do
    host <- fromString <$> lookupDefault "127.0.0.1" config "host"
    port <- lookupDefault 8080 config "port"
    runSettings (setHost host $ setPort port defaultSettings) app

runAPI :: Config -> Pool DB.Connection -> IO ()
runAPI config dbPool = runConfig config $ app dbPool
