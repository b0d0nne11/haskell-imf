{-# LANGUAGE OverloadedStrings #-}

module Text.IMF.Header
    ( Header(..)
    , header
    ) where

import           Control.Applicative (liftA2)
import           Data.List           (delete, intercalate)
import           Text.Parsec         (ParsecT, choice, many1, option, sepBy1,
                                      try, (<|>))
import           Text.Parsec.Char    (crlf, oneOf, string)

import           Text.IMF.DateTime
import           Text.IMF.Format
import           Text.IMF.Mailbox    (Mailbox, addressList, mailbox,
                                      mailboxList)
import           Text.IMF.MsgId
import           Text.IMF.Primatives

data Header = Date DateTime
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

instance ParseMessage Header where
  msgParser = header

instance FormatMessage Header where
  formatMessage (Date a)          = concat ["Date: ",        formatMessage a,                         "\r\n"]
  formatMessage (From as)         = concat ["From: ",        intercalate ", " (map formatMessage as), "\r\n"]
  formatMessage (Sender a)        = concat ["Sender: ",      formatMessage a,                         "\r\n"]
  formatMessage (ReplyTo as)      = concat ["Reply-To: ",    intercalate ", " (map formatMessage as), "\r\n"]
  formatMessage (To [])           = "To: Empty:;\r\n"
  formatMessage (To as)           = concat ["To: ",          intercalate ", " (map formatMessage as), "\r\n"]
  formatMessage (Cc [])           = "Cc: Empty:;\r\n"
  formatMessage (Cc as)           = concat ["Cc: ",          intercalate ", " (map formatMessage as), "\r\n"]
  formatMessage (Bcc [])          = "Bcc:\r\n"
  formatMessage (Bcc as)          = concat ["Bcc: ",         intercalate ", " (map formatMessage as), "\r\n"]
  formatMessage (MessageId a)     = concat ["Message-ID: ",  formatMessage a,                         "\r\n"]
  formatMessage (InReplyTo as)    = concat ["In-Reply-To: ", unwords (map formatMessage as),          "\r\n"]
  formatMessage (References as)   = concat ["References: ",  unwords (map formatMessage as),          "\r\n"]
  formatMessage (Subject a)       = concat ["Subject: ",     a,                                       "\r\n"]
  formatMessage (Comments a)      = concat ["Comments: ",    a,                                       "\r\n"]
  formatMessage (Keywords as)     = concat ["Keywords: ",    intercalate ", " as,                     "\r\n"]
  formatMessage (Optional (a, b)) = concat [a, ": ",         b,                                       "\r\n"]

-- | Parser for header.
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
header :: Monad m => ParsecT String u m Header
header = choice
    [ originatorDateField
    , originatorField
    , destinationAddressField
    , identificationField
    , informationalField
    , optionalField
    ]

-- https://tools.ietf.org/html/rfc5322#section-3.6.1
originatorDateField :: Monad m => ParsecT String u m Header
originatorDateField = choice
    [ Date <$> try (string "Date:" *> date <* crlf)
    ]
  where
    date = dateTime

-- https://tools.ietf.org/html/rfc5322#section-3.6.2
originatorField :: Monad m => ParsecT String u m Header
originatorField = choice
    [ From    <$> try (string "From:"     *> from    <* crlf)
    , Sender  <$> try (string "Sender:"   *> sender  <* crlf)
    , ReplyTo <$> try (string "Reply-To:" *> replyTo <* crlf)
    ]
  where
    from    = mailboxList
    sender  = mailbox
    replyTo = addressList

-- https://tools.ietf.org/html/rfc5322#section-3.6.3
destinationAddressField :: Monad m => ParsecT String u m Header
destinationAddressField = choice
    [ To  <$> try (string "To:"  *> to            <* crlf)
    , Cc  <$> try (string "Cc:"  *> cc            <* crlf)
    , Bcc <$> try (string "Bcc:" *> option [] bcc <* crlf)
    ]
  where
    to  = addressList
    cc  = addressList
    bcc = try addressList <|> ([] <$ cfws)

-- https://tools.ietf.org/html/rfc5322#section-3.6.4
identificationField :: Monad m => ParsecT String u m Header
identificationField = choice
    [ MessageId  <$> try (string "Message-ID:"  *> messageId  <* crlf)
    , InReplyTo  <$> try (string "In-Reply-To:" *> inReplyTo  <* crlf)
    , References <$> try (string "References:"  *> references <* crlf)
    ]
  where
    messageId  = msgId
    inReplyTo  = many1 msgId
    references = many1 msgId

-- https://tools.ietf.org/html/rfc5322#section-3.6.5
informationalField :: Monad m => ParsecT String u m Header
informationalField = choice
    [ Subject  <$> try (string "Subject:"  *> subject  <* crlf)
    , Comments <$> try (string "Comments:" *> comments <* crlf)
    , Keywords <$> try (string "Keywords:" *> keywords <* crlf)
    ]
  where
    subject  = unstructured
    comments = unstructured
    keywords = phrase `sepBy1` string ","


-- TODO: https://tools.ietf.org/html/rfc5322#section-3.6.6
-- TODO: https://tools.ietf.org/html/rfc5322#section-3.6.7

-- https://tools.ietf.org/html/rfc5322#section-3.6.8
optionalField :: Monad m => ParsecT String u m Header
optionalField =
    Optional <$> liftA2 (,) (fname <* string ":") (ftext <* crlf)
  where
    fname = many1 $ oneOf $ delete ':' ['\x21'..'\x7E']
    ftext = unstructured
