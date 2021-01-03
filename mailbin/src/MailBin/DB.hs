{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module MailBin.DB
  ( setupDB
  , closeDB
  , Message(..)
  , selectMessage
  , getMessage
  , insertMessage
  , deleteMessage
  , Envelope(..)
  , selectEnvelope
  , getEnvelope
  , deleteEnvelope
  , Comparison(..)
  , Limit(..)
  , Offset(..)
  )
where

import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Data.ByteString.Lazy             (ByteString)
import qualified Data.IMF                         as IMF
import qualified Data.IMF.Parsers                 as IMF
import           Data.List                        (intersperse)
import           Data.Maybe                       (catMaybes)
import           Data.Pool                        (Pool, createPool, withResource, destroyAllResources)
import           Data.Time.Clock                  (UTCTime, NominalDiffTime)
import qualified Database.SQLite.Simple           as DB
import qualified Database.SQLite.Simple.FromField as DB
import           Database.SQLite.Simple.QQ
import qualified Database.SQLite.Simple.ToField   as DB

import           MailBin.Config

headMay :: [a] -> Maybe a
headMay as = if null as then Nothing else Just $ head as

instance DB.FromField IMF.Mailbox where
    fromField = fmap (either error id . IMF.parse) . DB.fromField

instance DB.ToField IMF.Mailbox where
    toField = DB.toField . IMF.format

data ConfigParams = ConfigParams
    { configFile        :: String
    , configIdleTimeout :: NominalDiffTime
    , configMaxConns    :: Int
    }

loadConfigParams :: Config -> IO ConfigParams
loadConfigParams config =
    ConfigParams <$> lookupDefault ":memory:" config "file"
                 <*> lookupDefault 86400 config "idle_timeout"
                 <*> lookupDefault 1 config "max_conns"

loadSchema :: Pool DB.Connection -> IO ()
loadSchema pool = withResource pool $ \conn -> do
    DB.execute_ conn [sql|
        CREATE TABLE IF NOT EXISTS message (
            id INTEGER PRIMARY KEY,
            created_at TEXT DEFAULT CURRENT_TIMESTAMP,
            content BLOB
        )
    |]
    DB.execute_ conn [sql|
        CREATE TABLE IF NOT EXISTS envelope (
            id INTEGER PRIMARY KEY,
            message_id INTEGER,
            return_path TEXT,
            recipient TEXT
        )
    |]

setupDB :: Config -> IO (Pool DB.Connection)
setupDB config = do
    ConfigParams{..} <- loadConfigParams config
    pool <- createPool (DB.open configFile) DB.close 1 configIdleTimeout configMaxConns
    loadSchema pool
    return pool

closeDB :: Pool DB.Connection -> IO ()
closeDB = destroyAllResources

data Message = Message
    { messageId        :: Integer
    , messageCreatedAt :: UTCTime
    , messageContent   :: ByteString
    }
  deriving (Show, Eq)

instance DB.FromRow Message where
    fromRow = Message <$> DB.field -- id
                      <*> DB.field -- created_at
                      <*> DB.field -- content

selectMessage :: MonadIO m => Pool DB.Connection -> Maybe (Comparison Int) -> Maybe (Comparison UTCTime) -> Maybe Limit -> Maybe Offset -> m [Message]
selectMessage pool filterMessageById filterMessageByCreatedAt limit offset =
    liftIO $ withResource pool $ \conn ->
        DB.query conn sql params
  where
    (sql, params) = mconcat
        [ ("SELECT id, created_at, content FROM message", [])
        , qWhere $ catMaybes [ qComparison "id" <$> filterMessageById
                             , qComparison "created_at" <$> filterMessageByCreatedAt
                             ]
        , qLimit limit
        , qOffset offset
        ]

getMessage :: MonadIO m => Pool DB.Connection -> Int -> m (Maybe Message)
getMessage pool oid =
    liftIO $ withResource pool $ \conn ->
        headMay <$> DB.query conn sql params
  where
    (sql, params) = mconcat
        [ ("SELECT id, created_at, content FROM message", [])
        , qWhere [ qComparison "id" $ Equal oid ]
        ]

deleteMessage :: MonadIO m => Pool DB.Connection -> Maybe (Comparison Int) -> Maybe (Comparison UTCTime) -> m ()
deleteMessage pool filterMessageById filterMessageByCreatedAt =
    liftIO $ withResource pool $ \conn ->
        DB.execute conn sql params
  where
    (sql, params) = mconcat
        [ ("DELETE FROM message", [])
        , qWhere $ catMaybes [ qComparison "id" <$> filterMessageById
                             , qComparison "created_at" <$> filterMessageByCreatedAt
                             ]
        ]

insertMessage :: MonadIO m => Pool DB.Connection -> Maybe IMF.Mailbox -> [IMF.Mailbox] -> ByteString -> m ()
insertMessage pool returnPath recipients content =
    liftIO $ withResource pool $ \conn -> DB.withTransaction conn $ do
        DB.execute conn [sql|
            INSERT INTO message (content)
            VALUES (?)
        |] (DB.Only content)
        messageId <- DB.lastInsertRowId conn
        DB.executeMany conn [sql|
            INSERT INTO envelope (message_id, return_path, recipient)
            VALUES (?, ?, ?)
        |] $ map (messageId, returnPath, ) recipients

data Envelope = Envelope
    { envelopeId         :: Integer
    , envelopeMessageId  :: Integer
    , envelopeReturnPath :: Maybe IMF.Mailbox
    , envelopeRecipient  :: IMF.Mailbox
    }
  deriving (Show, Eq)

instance DB.FromRow Envelope where
    fromRow = Envelope <$> DB.field -- id
                       <*> DB.field -- message_id
                       <*> DB.field -- return_path
                       <*> DB.field -- recipient

selectEnvelope :: MonadIO m => Pool DB.Connection -> Maybe (Comparison Int) -> Maybe (Comparison Int) -> Maybe (Comparison IMF.Mailbox) -> Maybe (Comparison IMF.Mailbox) -> Maybe Limit -> Maybe Offset -> m [Envelope]
selectEnvelope pool filterEnvelopeById filterEnvelopeByMessageId filterEnvelopeByReturnPath filterEnvelopeByRecipient limit offset =
    liftIO $ withResource pool $ \conn ->
        DB.query conn sql params
  where
    (sql, params) = mconcat
        [ ("SELECT id, message_id, return_path, recipient FROM envelope", [])
        , qWhere $ catMaybes [ qComparison "id" <$> filterEnvelopeById
                             , qComparison "message_id" <$> filterEnvelopeByMessageId
                             , qComparison "return_path" <$> filterEnvelopeByReturnPath
                             , qComparison "recipient" <$> filterEnvelopeByRecipient
                             ]
        , qLimit limit
        , qOffset offset
        ]

getEnvelope :: MonadIO m => Pool DB.Connection -> Int -> m (Maybe Envelope)
getEnvelope pool oid =
    liftIO $ withResource pool $ \conn ->
        headMay <$> DB.query conn sql params
  where
    (sql, params) = mconcat
        [ ("SELECT id, message_id, return_path, recipient FROM envelope", [])
        , qWhere [ qComparison "id" $ Equal oid ]
        ]

deleteEnvelope :: MonadIO m => Pool DB.Connection -> Maybe (Comparison Int) -> Maybe (Comparison Int) -> Maybe (Comparison IMF.Mailbox) -> Maybe (Comparison IMF.Mailbox) -> m ()
deleteEnvelope pool filterEnvelopeById filterEnvelopeByMessageId filterEnvelopeByReturnPath filterEnvelopeByRecipient =
    liftIO $ withResource pool $ \conn ->
        DB.execute conn sql params
  where
    (sql, params) = mconcat
        [ ("DELETE FROM envelope", [])
        , qWhere $ catMaybes [ qComparison "id" <$> filterEnvelopeById
                             , qComparison "message_id" <$> filterEnvelopeByMessageId
                             , qComparison "return_path" <$> filterEnvelopeByReturnPath
                             , qComparison "recipient" <$> filterEnvelopeByRecipient
                             ]
        ]

qWhere :: [(DB.Query, [DB.SQLData])] -> (DB.Query, [DB.SQLData])
qWhere [] = ("", [])
qWhere qs = mconcat $ (" WHERE", []) : intersperse (" AND", []) qs

data Comparison a = LessThan a
                  | GreaterThan a
                  | LessThanOrEqual a
                  | GreaterThanOrEqual a
                  | Equal a
                  | NotEqual a
                  | Between a a
                  | NotBetween a a

qComparison :: DB.ToField a => DB.Query -> Comparison a -> (DB.Query, [DB.SQLData])
qComparison field (LessThan a)           = (" " <> field <> " < ?", [DB.toField a])
qComparison field (GreaterThan a)        = (" " <> field <> " > ?", [DB.toField a])
qComparison field (LessThanOrEqual a)    = (" " <> field <> " <= ?", [DB.toField a])
qComparison field (GreaterThanOrEqual a) = (" " <> field <> " >= ?", [DB.toField a])
qComparison field (Equal a)              = (" " <> field <> " = ?", [DB.toField a])
qComparison field (NotEqual a)           = (" " <> field <> " <> ?", [DB.toField a])
qComparison field (Between a b)          = (" " <> field <> " BETWEEN ? AND ?", [DB.toField a, DB.toField b])
qComparison field (NotBetween a b)       = (" " <> field <> " NOT BETWEEN ? AND ?", [DB.toField a, DB.toField b])

newtype Limit = Limit Int

qLimit :: Maybe Limit -> (DB.Query, [DB.SQLData])
qLimit Nothing          = ("", [])
qLimit (Just (Limit a)) = (" LIMIT ?", [DB.toField a])

newtype Offset = Offset Int

qOffset :: Maybe Offset -> (DB.Query, [DB.SQLData])
qOffset Nothing           = ("", [])
qOffset (Just (Offset a)) = (" OFFSET ?", [DB.toField a])
