{-# LANGUAGE OverloadedStrings #-}

module Text.IMF.Parsers.Client
  ( pReply
  )
where

import           Prelude                   hiding ( takeWhile )

import           Control.Applicative              ( (<|>)
                                                  , liftA2
                                                  , liftA3
                                                  , many
                                                  )
import           Data.Attoparsec.ByteString       ( Parser
                                                  , try
                                                  )
import           Data.Attoparsec.ByteString.Char8 ( satisfy
                                                  , takeWhile
                                                  , string
                                                  )
import           Data.ByteString                  ( ByteString )
import           Data.Char                        ( isDigit
                                                  , isPrint
                                                  )

-- | Parser for SMTP reply
pReply :: Parser (Int, [ByteString])
pReply =
    liftA2 (,) pCode (pEmptyLine <|> pSingleLine <|> pMultiLine)
  where
    pCode = read <$> sequence [pFirstDigit, pDigit, pDigit]
    pFirstDigit = satisfy $ \c -> c >= '2' && c <= '5'
    pDigit = satisfy isDigit
    pEmptyLine = [] <$ string "\r\n"
    pSingleLine = pure <$> (string " " *> pText <* string "\r\n")
    pMultiLine = liftA3 (\a b c -> [a] ++ b ++ [c])
                        (string "-" *> pText <* string "\r\n")
                        (many $ try $ pCode *> string "-" *> pText <* string "\r\n")
                        (pCode *> string " " *> pText <* string "\r\n")
    pText = takeWhile $ \c -> isPrint c || c == '\t'

