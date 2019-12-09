{-# LANGUAGE OverloadedStrings #-}

module Data.IMF.MessageId
  ( MessageId(..)
  , newMessageId
  )
where

import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Text.Base36      (encode)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.Word             (Word32)
import           System.Random         (randomIO)

import           Data.IMF.Types

data MessageId = MessageId Text Text
  deriving (Show, Eq)

instance HasFormatter MessageId where
    format (MessageId leftid rightid) = "<" <> leftid <> "@" <> rightid <> ">"

instance HasListFormatter MessageId where
    formatList = T.unwords . map format

newMessageId :: Text -> IO MessageId
newMessageId domain = do
    t <- truncate . (1e6 *) <$> getPOSIXTime
    r <- randomIO :: IO Word32
    return $ MessageId (encode t <> "." <> encode r) domain

