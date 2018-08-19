{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.IMF.MsgId
  ( MsgId(..)
  , LeftId
  , pMsgId
  )
where

import           Data.Foldable                  ( fold )
import           Control.Applicative            ( liftA2 )
import           Text.Parsec                    ( optional
                                                , option
                                                , try
                                                , (<|>)
                                                , parse
                                                , eof
                                                , many1
                                                )
import           Text.Parsec.Char               ( string )

import           Text.IMF.Format
import           Text.IMF.Mailbox
import           Text.IMF.Primatives

type LeftId = String

data MsgId = MsgId LeftId Domain
  deriving (Eq, Show)

instance ParseMessage MsgId where
    parseMessage = parse (pMsgId <* eof) ""

instance FormatMessage MsgId where
    formatMessage (MsgId leftId rightId) = concat ["<", leftId, "@", rightId, ">"]

instance ParseMessage [MsgId] where
    parseMessage = parse (many1 pMsgId <* eof) ""

instance FormatMessage [MsgId] where
    formatMessage = unwords . map formatMessage

-- | Message ID
--
-- > msg-id          = [CFWS] "<" id-left "@" id-right ">" [CFWS]
-- > id-left         = dot-atom-text
-- > id-right        = dot-atom-text / no-fold-literal
-- > no-fold-literal = "[" *dtext "]"
--
pMsgId :: Parser MsgId
pMsgId = liftA2 MsgId
                (optional pCFWS *> string "<" *> pLeftId)
                (string "@" *> pRightId <* string ">" <* optional pCFWS)
  where
    pLeftId        = pDotAtomText
    pRightId       = try pDotAtomText <|> pNoFoldLiteral
    pNoFoldLiteral = fold [string "[", option "" pDText, string "]"]
