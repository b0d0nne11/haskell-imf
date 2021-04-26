{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TemplateHaskell   #-}

module MailBin.API
  ( runAPI
  )
where

import           Control.Concurrent.Async             (async, wait)
import           Control.Monad.IO.Class               (MonadIO, liftIO)
import           Control.Monad.Logger                 (MonadLogger, logInfo)
import           Data.Aeson                           (ToJSON (..), object, (.=))
import qualified Data.IMF                             as IMF
import qualified Data.IMF.Parsers                     as IMF
import           Data.Pool                            (Pool)
import qualified Data.Text                            as T
import           Data.Time.Clock                      (UTCTime)
import qualified Database.SQLite.Simple               as DB
import           Network.HTTP.Media                   ((//), (/:))
import           Network.Simple.TCP                   (HostPreference, ServiceName, bindSock,
                                                       closeSock, listenSock)
import           Network.Wai.Handler.Warp             (defaultSettings, runSettingsSocket)
import           Network.Wai.Middleware.RequestLogger (logStdout)
import Servant

import           MailBin.Config
import           MailBin.DB

or404 :: Handler (Maybe a) -> Handler a
or404 f = f >>= maybe (throwError $ err404 { errBody = "not found" }) return

instance FromHttpApiData IMF.Mailbox where
    parseQueryParam param = parseQueryParam param >>= either (const $ Left "invalid address") Right . IMF.parse

instance FromHttpApiData a => FromHttpApiData (Comparison a) where
    parseQueryParam t = case T.splitOn ":" t of
        [t1]           -> Equal <$> parseQueryParam t1
        ["lt", t1]     -> LessThan <$> parseQueryParam t1
        ["gt", t1]     -> GreaterThan <$> parseQueryParam t1
        ["lte", t1]    -> LessThanOrEqual <$> parseQueryParam t1
        ["gte", t1]    -> GreaterThanOrEqual <$> parseQueryParam t1
        ["eq", t1]     -> Equal <$> parseQueryParam t1
        ["neq", t1]    -> NotEqual <$> parseQueryParam t1
        ["b", t1, t2]  -> Between <$> parseQueryParam t1 <*> parseQueryParam t2
        ["nb", t1, t2] -> NotBetween <$> parseQueryParam t1 <*> parseQueryParam t2
        _              -> Left "invalid comparison"

instance FromHttpApiData Limit where
    parseQueryParam = fmap Limit . parseQueryParam

instance FromHttpApiData Offset where
    parseQueryParam = fmap Offset . parseQueryParam

instance ToJSON IMF.Mailbox where
    toJSON = toJSON . IMF.format

instance ToJSON Message where
    toJSON Message{..} =
        object [ "id"         .= messageId
               , "created_at" .= messageCreatedAt
               ]

instance ToJSON Envelope where
    toJSON Envelope{..} =
        object [ "id"          .= envelopeId
               , "message_id"  .= envelopeMessageId
               , "return_path" .= envelopeReturnPath
               , "recipient"   .= envelopeRecipient
               ]

data RFC822

instance Accept RFC822 where
    contentType _ = "message" // "rfc822" /: ("charset", "utf-8")

instance MimeRender RFC822 Message where
    mimeRender _ = messageContent

type MessageAPI = "message" :> QueryParam "id" (Comparison Int) :> QueryParam "created_at" (Comparison UTCTime) :> QueryParam "limit" Limit :> QueryParam "offset" Offset :> Get '[JSON] [Message]
             :<|> "message" :> Capture "id" Int :> Get '[JSON, RFC822] Message
             :<|> "message" :> QueryParam "id" (Comparison Int) :> QueryParam "created_at" (Comparison UTCTime) :> DeleteNoContent
             :<|> "envelope" :> QueryParam "id" (Comparison Int) :> QueryParam "message_id" (Comparison Int) :> QueryParam "return_path" (Comparison IMF.Mailbox) :> QueryParam "recipient" (Comparison IMF.Mailbox) :> QueryParam "limit" Limit :> QueryParam "offset" Offset :> Get '[JSON] [Envelope]
             :<|> "envelope" :> Capture "id" Int :> Get '[JSON] Envelope
             :<|> "envelope" :> QueryParam "id" (Comparison Int) :> QueryParam "message_id" (Comparison Int) :> QueryParam "return_path" (Comparison IMF.Mailbox) :> QueryParam "recipient" (Comparison IMF.Mailbox) :> DeleteNoContent

api :: Proxy MessageAPI
api = Proxy

server :: Pool DB.Connection -> Server MessageAPI
server dbPool = selectMessage dbPool
           :<|> or404 . getMessage dbPool
           :<|> (\filterById filterByCreatedAt -> NoContent <$ deleteMessage dbPool filterById filterByCreatedAt)
           :<|> selectEnvelope dbPool
           :<|> or404 . getEnvelope dbPool
           :<|> (\filterById filterByMessageId filterByReturnPath filterByRecipient -> NoContent <$ deleteEnvelope dbPool filterById filterByMessageId filterByReturnPath filterByRecipient)

app :: Pool DB.Connection -> Application
app dbPool = serve api $ server dbPool

data ConfigParams = ConfigParams
    { configHost :: HostPreference
    , configPort :: ServiceName
    }

loadConfigParams :: Config -> IO ConfigParams
loadConfigParams config =
    ConfigParams <$> lookupDefault "127.0.0.1" config "host"
                 <*> lookupDefault "3000" config "port"

runAPI :: (MonadIO m, MonadLogger m) => Config -> Pool DB.Connection -> m (m ())
runAPI config dbPool = do
    ConfigParams{..} <- liftIO $ loadConfigParams config
    $(logInfo) $ "starting api @ http://" <> T.pack (show configHost) <> ":" <> T.pack configPort
    (sock, addr) <- liftIO $ bindSock configHost configPort
    liftIO $ listenSock sock 2048
    t <- liftIO $ async $
        runSettingsSocket defaultSettings sock $ logStdout $ app dbPool
    return $ do
        liftIO $ closeSock sock
        liftIO $ wait t
        $(logInfo) "stopped api"
