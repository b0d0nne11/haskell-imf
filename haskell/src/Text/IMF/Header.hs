{-# LANGUAGE OverloadedStrings #-}

module Text.IMF.Header
  ( Header(..)
  , pHeader
  , HeaderField(..)
  , pHeaderField
  , fieldName
  , fieldValue
  )
where

import           Data.List                      ( intercalate )
import           Text.Parsec                    ( many1
                                                , option
                                                , sepBy1
                                                , try
                                                , (<|>)
                                                , (<?>)
                                                , parse
                                                , eof
                                                )
import           Text.Parsec.Char               ( satisfy
                                                , string
                                                )

import           Text.IMF.DateTime
import           Text.IMF.Format
import           Text.IMF.Mailbox
import           Text.IMF.MsgId
import           Text.IMF.Primatives

newtype Header = Header [HeaderField]
  deriving (Eq, Show)

instance ParseMessage Header where
    parseMessage = parse (pHeader <* eof) ""

instance FormatMessage Header where
    formatMessage (Header fields) = concatMap formatMessage fields

-- | Header
pHeader :: Parser Header
pHeader = Header <$> many1 pHeaderField

data HeaderField = Date DateTime
                 | From [Mailbox]
                 | Sender Mailbox
                 | ReplyTo [Mailbox]
                 | To [Mailbox]
                 | Cc [Mailbox]
                 | Bcc [Mailbox]
                 | MessageId MsgId
                 | InReplyTo [MsgId]
                 | References [MsgId]
                 | Subject String
                 | Comments String
                 | Keywords [String]
                 | Optional (String, String)
  deriving (Eq, Show)

instance ParseMessage HeaderField where
    parseMessage = parse (pHeaderField <* eof) ""

instance FormatMessage HeaderField where
    formatMessage f = fieldName f ++ ": " ++ fieldValue f ++ "\r\n"

-- | Get field name from header
fieldName :: HeaderField -> String
fieldName (Date       _) = "Date"
fieldName (From       _) = "From"
fieldName (Sender     _) = "Sender"
fieldName (ReplyTo    _) = "Reply-To"
fieldName (To         _) = "To"
fieldName (Cc         _) = "Cc"
fieldName (Bcc        _) = "Bcc"
fieldName (MessageId  _) = "Message-ID"
fieldName (InReplyTo  _) = "In-Reply-To"
fieldName (References _) = "References"
fieldName (Subject    _) = "Subject"
fieldName (Comments   _) = "Comments"
fieldName (Keywords   _) = "Keywords"
fieldName (Optional   a) = fst a

-- | Get field value from header as a string
fieldValue :: HeaderField -> String
fieldValue (Date       a ) = formatMessage a
fieldValue (From       a ) = formatMessage a
fieldValue (Sender     a ) = formatMessage a
fieldValue (ReplyTo    a ) = formatMessage a
fieldValue (To         []) = "Empty:;"
fieldValue (To         a ) = formatMessage a
fieldValue (Cc         []) = "Empty:;"
fieldValue (Cc         a ) = formatMessage a
fieldValue (Bcc        []) = ""
fieldValue (Bcc        a ) = formatMessage a
fieldValue (MessageId  a ) = formatMessage a
fieldValue (InReplyTo  a ) = formatMessage a
fieldValue (References a ) = formatMessage a
fieldValue (Subject    a ) = a
fieldValue (Comments   a ) = a
fieldValue (Keywords   a ) = intercalate ", " a
fieldValue (Optional   a ) = snd a

-- | Header field
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
pHeaderField :: Parser HeaderField
pHeaderField = do
    name <- pFieldName <* string ":"
    pFieldValue name <* eol

-- | Field name
pFieldName :: Parser String
pFieldName = many1 (satisfy isFieldNameText) <?> "field name"

-- | Field value, varies depending on field name
pFieldValue :: String -> Parser HeaderField
pFieldValue "Date"        = Date <$> pDateTime
pFieldValue "From"        = From <$> pMailboxList
pFieldValue "Sender"      = Sender <$> pMailbox
pFieldValue "Reply-To"    = ReplyTo <$> pAddressList
pFieldValue "To"          = To <$> pAddressList
pFieldValue "Cc"          = Cc <$> pAddressList
pFieldValue "Bcc"         = Bcc <$> (try pAddressList <|> ([] <$ option [] pCFWS))
pFieldValue "Message-ID"  = MessageId <$> pMsgId
pFieldValue "In-Reply-To" = InReplyTo <$> many1 pMsgId
pFieldValue "References"  = References <$> many1 pMsgId
pFieldValue "Subject"     = Subject <$> pUnstructured
pFieldValue "Comments"    = Comments <$> pUnstructured
pFieldValue "Keywords"    = Keywords <$> pPhrase `sepBy1` string ","
pFieldValue a             = Optional . (,) a <$> pUnstructured

-- | Match field name text
isFieldNameText :: Char -> Bool
isFieldNameText c | c == ':'    = False
                  | isVisible c = True
                  | otherwise   = False
{-# INLINE isFieldNameText #-}
