{-# LANGUAGE OverloadedStrings #-}

module Data.IMF.MessageId
  ( MessageId(..)
  , newMessageId
  )
where

import           Data.Char             (chr, ord)
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.Word             (Word32)
import           Numeric               (showIntAtBase)
import           System.Random         (randomIO)

import           Data.IMF.Types

base36 :: (Integral a, Show a) => a -> Text
base36 a = T.pack $ showIntAtBase 36 intToDigit a ""
  where
    intToDigit i
      | i >= 0  && i <=  9 = chr $ ord '0' + i
      | i >= 10 && i <= 35 = chr $ ord 'a' + i - 10
      | otherwise = error $ "invalid base 36 digit: " ++ show i

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
    return $ MessageId (base36 t <> "." <> base36 r) domain
