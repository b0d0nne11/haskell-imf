{-# LANGUAGE OverloadedStrings #-}

module Data.IMF.Network.Parsers
  ( pLine
  , pTextLine
  , pEOL
  , pData
  , pReply
  )
where

import           Prelude                          hiding (take)

import           Control.Applicative              (liftA2, many, (<|>))
import           Data.Attoparsec.ByteString       (Parser, manyTill, take, takeTill)
import           Data.Attoparsec.ByteString.Char8 (decimal)
import           Data.Attoparsec.Combinator       (lookAhead)
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString                  as B
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as T

-- | Parser for SMTP line
pLine :: Parser ByteString
pLine = liftA2 B.append (takeTill (== 10)) (take 1)

-- | Parser for SMTP line converted into stripped-text
pTextLine :: Parser Text
pTextLine = T.strip . T.decodeUtf8 <$> pLine

-- | Parser for end of line
pEOL :: Parser ByteString
pEOL = "\n" <|> "\r\n"

-- | Parser for SMTP data
pData :: Parser ByteString
pData = B.concat <$> manyTill pLine ("." *> pEOL)

-- | Parser for SMTP reply
pReply :: Parser (Int, [Text])
pReply =
    liftA2 (,) (lookAhead decimal) $
        liftA2 snoc (many $ decimal *> "-" *> pTextLine)
                    (decimal *> " " *> pTextLine)
  where
    snoc xs x = xs ++ [x]
