{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.IMF.Parsers.MsgId
  ( pMsgId
  )
where

import           Control.Applicative            ( (<|>)
                                                , liftA2
                                                )
import           Data.Attoparsec.Text           ( Parser
                                                , endOfInput
                                                , parseOnly
                                                )
import           Data.String                    ( IsString
                                                , fromString
                                                )
import qualified Data.Text                     as T

import           Data.IMF.Types
import           Data.IMF.Parsers.Core
import           Data.IMF.Parsers.Mailbox

instance IsString MsgId where
    fromString = either (fail "invalid message id") id . parseOnly (pMsgId <* endOfInput) . T.pack

----------------------------------------------------------------------------
-- Identification Fields <https://tools.ietf.org/html/rfc5322#section-3.6.4>
----------------------------------------------------------------------------

-- | Message ID
--
-- > msg-id = [CFWS] "<" id-left "@" id-right ">" [CFWS]
--
pMsgId :: Parser MsgId
pMsgId = liftA2 MsgId (optional pCFWS *> "<" *> pLeftId) ("@" *> pRightId <* ">" <* optional pCFWS)

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
