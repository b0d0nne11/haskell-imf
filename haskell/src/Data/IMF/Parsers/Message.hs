{-# LANGUAGE OverloadedStrings #-}

module Data.IMF.Parsers.Message
  ( pMessage
  )
where

import           Control.Applicative            ( liftA2 )
import           Data.Attoparsec.Text           ( Parser
                                                , (<?>)
                                                , endOfInput
                                                , many1
                                                , parseOnly
                                                , takeWhile1
                                                )
import           Data.String                    ( IsString
                                                , fromString
                                                )
import qualified Data.Text                     as T

import           Data.IMF.Types
import           Data.IMF.Parsers.Core
import           Data.IMF.Parsers.Header

instance IsString Message where
    fromString = either (fail "invalid message") id . parseOnly (pMessage <* endOfInput) . T.pack

---------------------------------------------------------------------------
-- Overall Message Syntax <https://tools.ietf.org/html/rfc5322#section-3.5>
---------------------------------------------------------------------------

-- | Message
--
-- > message = fields [CRLF body]
--
pMessage :: Parser Message
pMessage = liftA2 Message (many1 pHeader) (optional $ pEOL *> pBody)

-- | Body
--
-- > body = *(*998text CRLF) *998tex)
--
pBody :: Parser T.Text
pBody = foldMany (optional pText `append` pEOL) `append` optional pText

-- | Text
--
-- > text = %d1-9 / %d11 / %d12 / %d14-127
--
pText :: Parser T.Text
pText = takeWhile1 isText <?> "text"

-- | Match text characters
isText :: Char -> Bool
isText c | c == '\0' = False
         | c == '\r' = False
         | c == '\n' = False
         | otherwise = True
{-# INLINE isText #-}
