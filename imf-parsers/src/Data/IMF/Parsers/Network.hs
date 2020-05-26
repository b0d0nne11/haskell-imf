{-# LANGUAGE OverloadedStrings #-}

module Data.IMF.Parsers.Network
  ( pLine
  , pBody
  , pReply
  )
where

import           Control.Applicative        (liftA2, liftA3, many, (<|>))
import           Data.Attoparsec.ByteString (Parser, anyWord8, manyTill, satisfy, string)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as C
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T

-- | Parser for SMTP message line
pLine :: Parser T.Text
pLine = T.decodeUtf8 . B.pack <$> manyTill anyWord8 (string "\r\n")

-- | Parser for SMTP message body
pBody :: Parser T.Text
pBody = (`T.append` "\r\n") . T.intercalate "\r\n" <$> manyTill pLine (string ".\r\n")

-- | Parser for SMTP reply
pReply :: Parser (Int, [T.Text])
pReply =
    liftA2 (,) pCode (pEmptyLine <|> pSingleLine <|> pMultiLine)
  where
    pCode = read . C.unpack . B.pack <$> sequence [pFirstDigit, pDigit, pDigit]
    pFirstDigit = satisfy $ \c -> c >= 50 && c <= 53
    pDigit = satisfy $ \c -> c >= 48 && c <= 57
    pEmptyLine = [] <$ string " \r\n"
    pSingleLine = pure <$> (string " " *> pLine)
    pMultiLine = liftA3 (\a b c -> [a] ++ b ++ [c])
                        (string "-" *> pLine)
                        (many $ pCode *> string "-" *> pLine)
                        (pCode *> string " " *> pLine)
