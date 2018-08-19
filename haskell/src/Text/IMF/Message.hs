{-# LANGUAGE OverloadedStrings #-}

module Text.IMF.Message
  ( Message(..)
  , Body
  , pMessage
  )
where

import           Control.Applicative            ( liftA2 )
import           Text.Parsec                    ( many1
                                                , (<|>)
                                                , (<?>)
                                                , parse
                                                , eof
                                                )
import           Text.Parsec.Char               ( satisfy )

import           Text.IMF.Primatives
import           Text.IMF.Format
import           Text.IMF.Header

type Body = String

data Message = Message Header Body
  deriving (Show, Eq)

instance ParseMessage Message where
    parseMessage = parse (pMessage <* eof) ""

instance FormatMessage Message where
    formatMessage (Message h b) = formatMessage h ++ "\r\n" ++ b

-- | Message
--
-- > message = fields [CRLF body]
-- > body    = *(*998text CRLF) *998tex)
-- > text    = %d1-9 / %d11 / %d12 / %d14-127
--
pMessage :: Parser Message
pMessage = liftA2 Message pHeader (eol *> pBody)
  where
    pBody = concatMany (pText <|> eol)
    pText = many1 (satisfy isText) <?> "text"

-- | Match text characters
isText :: Char -> Bool
isText c | c == '\0' = False
         | c == '\r' = False
         | c == '\n' = False
         | otherwise = True
{-# INLINE isText #-}
