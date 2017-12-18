{-# LANGUAGE OverloadedStrings #-}

module Text.IMF.MsgId
    ( MsgId(..)
    , msgId
    ) where

import           Control.Applicative (liftA2)
import           Text.Parsec         (ParsecT, many, optional, try, (<|>))
import           Text.Parsec.Char    (string)

import           Text.IMF.Combinator
import           Text.IMF.Format
import           Text.IMF.Mailbox    (dtext)
import           Text.IMF.Primatives

data MsgId = MsgId String String
  deriving (Eq, Show)

instance ParseMessage MsgId where
  msgParser = msgId

instance FormatMessage MsgId where
  formatMessage (MsgId leftId rightId) = concat ["<", leftId, "@", rightId, ">"]

-- | Parser for message ID.
--
-- > msg-id          = [CFWS] "<" id-left "@" id-right ">" [CFWS]
-- > id-left         = dot-atom-text
-- > id-right        = dot-atom-text / no-fold-literal
-- > no-fold-literal = "[" *dtext "]"
--
msgId :: Monad m => ParsecT String u m MsgId
msgId = liftA2 MsgId (optional cfws *> string "<" *> leftId)
                     (string "@" *> rightId <* string ">" <* optional cfws)
  where
    leftId        = dotAtomText
    rightId       = try dotAtomText <|> noFoldLiteral
    noFoldLiteral = collect [string "[", many dtext, string "]"]
