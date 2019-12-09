{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.IMF.Parsers.MessageId
  ( pMessageId
  )
where

import           Control.Applicative      (liftA2, (<|>))
import           Data.Attoparsec.Text     (Parser, endOfInput, many1, parseOnly)
import           Data.String              (IsString, fromString)
import qualified Data.Text                as T

import           Data.IMF
import           Data.IMF.Parsers.Core
import           Data.IMF.Parsers.Mailbox
import           Data.IMF.Parsers.Types

instance HasParser MessageId where
    parser = pMessageId

instance HasListParser MessageId where
    listParser = many1 pMessageId

instance IsString MessageId where
    fromString = either (error "invalid message id") id . parse . T.pack

----------------------------------------------------------------------------
-- Identification Fields <https://tools.ietf.org/html/rfc5322#section-3.6.4>
----------------------------------------------------------------------------

-- | Message ID
--
-- > msg-id = [CFWS] "<" id-left "@" id-right ">" [CFWS]
--
pMessageId :: Parser MessageId
pMessageId = liftA2 MessageId (optional pCFWS *> "<" *> pLeftId) ("@" *> pRightId <* ">" <* optional pCFWS)

-- | Left ID
--
-- > id-left = dot-atom-text / obs-id-left
--
pLeftId :: Parser T.Text
pLeftId = pDotAtomText

-- | Right ID
--
-- > id-right = dot-atom-text / no-fold-literal / obs-id-right
--
pRightId :: Parser T.Text
pRightId = pDotAtomText <|> pNoFoldLiteral

-- | No-fold literal
--
-- > no-fold-literal = "[" *dtext "]"
--
pNoFoldLiteral :: Parser T.Text
pNoFoldLiteral = "[" `append` optional pDText `append` "]"
