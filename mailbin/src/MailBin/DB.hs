{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module MailBin.DB
  ( setupDB
  , Mail(..)
  , selectMail
  , insertMail
  , deleteMail
  )
where

import           Data.Aeson
import           Data.Configurator              (lookupDefault)
import           Data.Configurator.Types        (Config)
import           Data.Pool                      (Pool, createPool, withResource)
import           Data.Text                      (Text)
import           Data.Time.Clock                (UTCTime)
import qualified Database.SQLite.Simple         as DB
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.ToRow

data Mail = Mail
    { mailID         :: Integer
    , mailCreatedAt  :: UTCTime
    , mailReturnPath :: Text
    , mailRecipient  :: Text
    , mailMessage    :: Text
    }
  deriving (Show, Eq)

instance FromRow Mail where
    fromRow = Mail <$> field -- id
                   <*> field -- created_at
                   <*> field -- return_path
                   <*> field -- recipient
                   <*> field -- message

instance ToJSON Mail where
    toJSON Mail{..} =
        object [ "id"          .= mailID
               , "created_at"  .= mailCreatedAt
               , "return_path" .= mailReturnPath
               , "recipient"   .= mailRecipient
               ]

setupDB :: Config -> IO (Pool DB.Connection)
setupDB config = do
    uri <- lookupDefault ":memory:" config "uri"
    idleTimeout <- fromRational <$> lookupDefault 86400 config "idle_timeout"
    maxConns <- lookupDefault 1 config "max_conns"
    pool <- createPool (DB.open uri) DB.close 1 idleTimeout maxConns
    withResource pool $ \conn ->
        DB.execute_ conn "CREATE TABLE IF NOT EXISTS mail (id INTEGER PRIMARY KEY, created_at TEXT DEFAULT CURRENT_TIMESTAMP, return_path TEXT, recipient TEXT, message BLOB)"
    return pool

selectMail :: Pool DB.Connection -> Integer -> Integer -> IO [Mail]
selectMail pool limit offset = withResource pool $ \conn ->
    DB.query conn "SELECT * FROM mail ORDER BY id LIMIT ? OFFSET ?" (limit, offset)

insertMail :: Pool DB.Connection -> Text -> Text -> Text -> IO ()
insertMail pool returnPath recipient message = withResource pool $ \conn ->
    DB.execute conn "INSERT INTO mail (return_path, recipient, message) VALUES (?, ?, ?)" (returnPath, recipient, message)

deleteMail :: Pool DB.Connection -> Integer -> IO ()
deleteMail pool oid = withResource pool $ \conn ->
    DB.execute conn "DELETE FROM mail WHERE id = ?" (DB.Only oid)
