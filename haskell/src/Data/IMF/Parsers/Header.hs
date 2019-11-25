{-# LANGUAGE OverloadedStrings #-}

module Data.IMF.Parsers.Header
  ( pHeader
  )
where

import           Control.Applicative            ( (<|>)
                                                )
import           Data.Attoparsec.Text           ( Parser
                                                , (<?>)
                                                , endOfInput
                                                , many1
                                                , parseOnly
                                                , sepBy1
                                                , takeWhile1
                                                )
import           Data.String                    ( IsString
                                                , fromString
                                                )
import qualified Data.Text                     as T

import           Data.IMF.Types
import           Data.IMF.Parsers.Core
import           Data.IMF.Parsers.DateTime
import           Data.IMF.Parsers.Mailbox
import           Data.IMF.Parsers.MsgId

instance IsString Header where
    fromString = either (fail "invalid header") id . parseOnly (pHeader <* endOfInput) . T.pack

-----------------------------------------------------------------------------
-- Header Field Definitions <https://tools.ietf.org/html/rfc5322#section-3.6>
-----------------------------------------------------------------------------

-- | Header
--
-- > orig-date      = "Date:"        date-time             CRLF
-- > from           = "From:"        mailbox-list          CRLF
-- > sender         = "Sender:"      mailbox               CRLF
-- > reply-to       = "Reply-To:"    address-list          CRLF
-- > to             = "To:"          address-list          CRLF
-- > cc             = "Cc:"          address-list          CRLF
-- > bcc            = "Bcc:"         [address-list / CFWS] CRLF
-- > message-id     = "Message-ID:"  msg-id                CRLF
-- > in-reply-to    = "In-Reply-To:" 1*msg-id              CRLF
-- > references     = "References:"  1*msg-id              CRLF
-- > subject        = "Subject:"     unstructured          CRLF
-- > comments       = "Comments:"    unstructured          CRLF
-- > keywords       = "Keywords:"    phrase *("," phrase)  CRLF
-- > optional-field = field-name ":" unstructured          CRLF
--
pHeader :: Parser Header
pHeader = (pFieldName <* ":" <* optional pWSP) >>= (\name -> pFieldValue name <* pEOL)

pFieldValue :: T.Text -> Parser Header
pFieldValue "Date"        = Date <$> pDateTime
pFieldValue "From"        = From <$> pMailboxList
pFieldValue "Sender"      = Sender <$> pMailbox
pFieldValue "Reply-To"    = ReplyTo <$> pAddressList
pFieldValue "To"          = To <$> pAddressList
pFieldValue "Cc"          = Cc <$> pAddressList
pFieldValue "Bcc"         = Bcc <$> (pAddressList <|> ([] <$ optional pCFWS))
pFieldValue "Message-ID"  = MessageId <$> pMsgId
pFieldValue "In-Reply-To" = InReplyTo <$> many1 pMsgId
pFieldValue "References"  = References <$> many1 pMsgId
pFieldValue "Subject"     = Subject <$> pUnstructured
pFieldValue "Comments"    = Comments <$> pUnstructured
pFieldValue "Keywords"    = Keywords <$> pPhrase `sepBy1` ","
pFieldValue name          = Optional . (,) name <$> pUnstructured

-- | Field name
--
-- > field-name = 1*ftext
--
pFieldName :: Parser T.Text
pFieldName = pFText

-- | Field name text
--
-- > Printable US-ASCII characters not including ":".
-- > ftext = %d33-57 / %d59-126
--
pFText :: Parser T.Text
pFText = takeWhile1 isFText <?> "field name text"

-- | Match field name text
isFText :: Char -> Bool
isFText c | c == ':'  = False
          | isVCHAR c = True
          | otherwise = False
{-# INLINE isFText #-}
