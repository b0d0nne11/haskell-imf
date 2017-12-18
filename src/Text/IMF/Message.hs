{-# LANGUAGE OverloadedStrings #-}

module Text.IMF.Message
    ( Message(..)
    , message
    ) where

import           Control.Applicative (liftA2)
import           Text.Parsec         (ParsecT, many, many1)
import           Text.Parsec.Char    (crlf, oneOf, string)

import           Text.IMF.Combinator
import           Text.IMF.Format
import           Text.IMF.Header

data Message = Message { msgHeaders :: [Header] -- ^ Headers
                       , msgBody    :: String   -- ^ Body
                       }
  deriving (Show, Eq)

instance ParseMessage Message where
  msgParser = message

instance FormatMessage Message where
  formatMessage (Message hs b) = concatMap formatMessage hs ++ "\r\n" ++ b

-- | Parser for message.
--
-- > message = fields [CRLF body]
-- > body    = *(*998text CRLF) *998tex)
-- > text    = %d1-9 / %d11 / %d12 / %d14-127
--
message :: Monad m => ParsecT String u m Message
message = liftA2 Message (many1 header) (crlf *> body)

body :: Monad m => ParsecT String u m String
body = collect [collectMany [many text, string "\r\n"], many text]

text :: Monad m => ParsecT String u m Char
text = oneOf $ ['\01'..'\09'] ++ ['\11', '\12'] ++ ['\14'..'\127']
