{-# LANGUAGE OverloadedStrings #-}

module Data.IMF.Types
  ( format
  , formatList
  , Command(..)
  , Param(..)
  , Header(..)
  , Mailbox(..)
  , formatAddrSpec
  , Message(..)
  , MsgId(..)
  , ZonedTime(..)
  , zonedTimeNanoseconds
  , zonedTimeTZOffset
  , zonedTimeFromNanoseconds
  )
where

import           Data.Int                       ( Int64 )
import qualified Data.Text                     as T
import           Data.Time.Clock.POSIX          ( utcTimeToPOSIXSeconds
                                                , posixSecondsToUTCTime
                                                )
import           Data.Time.Format               ( TimeLocale(..)
                                                , formatTime
                                                )
import           Data.Time.LocalTime            ( TimeZone(..)
                                                , ZonedTime(..)
                                                , minutesToTimeZone
                                                , utcToZonedTime
                                                , zonedTimeToUTC
                                                )
import           Data.Ratio                     ( (%) )

class Format a where
    -- | Format a message component into text
    format :: a -> T.Text

    -- | Format a list of message component into text
    formatList :: [a] -> T.Text

instance Format T.Text where
    format = id
    formatList = T.intercalate ", "

data Command = EHLO T.Text
             | HELO T.Text
             | MAIL T.Text T.Text
             | RCPT T.Text T.Text
             | DATA
             | RSET
             | VRFY T.Text T.Text
             | NOOP
             | QUIT
  deriving (Show, Eq)

data Param = SIZE Int
           | BODY T.Text
           | SMTPUTF8
           | Unsupported (T.Text, T.Text)
  deriving (Show, Eq)

data Header = Date ZonedTime
            | From [Mailbox]
            | Sender Mailbox
            | ReplyTo [Mailbox]
            | To [Mailbox]
            | Cc [Mailbox]
            | Bcc [Mailbox]
            | MessageId MsgId
            | InReplyTo [MsgId]
            | References [MsgId]
            | Subject T.Text
            | Comments T.Text
            | Keywords [T.Text]
            | Optional (T.Text, T.Text)
  deriving (Show, Eq)

instance Format Header where
    format (Date t)            = "Date: " <> format t                 <> "\r\n"
    format (From mboxes)       = "From: " <> formatList mboxes        <> "\r\n"
    format (Sender mbox)       = "Sender: " <> format mbox            <> "\r\n"
    format (ReplyTo mboxes)    = "Reply-To: " <> formatList mboxes    <> "\r\n"
    format (To [])             = "To: Empty:;"                        <> "\r\n"
    format (To mboxes)         = "To: " <> formatList mboxes          <> "\r\n"
    format (Cc [])             = "Cc: Empty:;"                        <> "\r\n"
    format (Cc mboxes)         = "Cc: " <> formatList mboxes          <> "\r\n"
    format (Bcc [])            = "Bcc: "                              <> "\r\n"
    format (Bcc mboxes)        = "Bcc: " <> formatList mboxes         <> "\r\n"
    format (MessageId msgid)   = "Message-ID: " <> format msgid       <> "\r\n"
    format (InReplyTo msgids)  = "In-Reply-To: " <> formatList msgids <> "\r\n"
    format (References msgids) = "References: " <> formatList msgids  <> "\r\n"
    format (Subject subject)   = "Subject: " <> subject               <> "\r\n"
    format (Comments comments) = "Comments: " <> comments             <> "\r\n"
    format (Keywords keywords) = "Keywords: " <> formatList keywords  <> "\r\n"
    format (Optional (k,v))    = k <> ": " <> v                       <> "\r\n"

    formatList = T.concat . map format

data Mailbox = Mailbox
    { mboxDisplay :: T.Text
    , mboxLocal   :: T.Text
    , mboxDomain  :: T.Text
    }
  deriving (Show, Eq)

instance Format Mailbox where
    format (Mailbox "" local domain) = local <> "@" <> domain
    format (Mailbox display local domain) = display <> " <" <> local <> "@" <> domain <> ">"
    formatList = T.intercalate ", " . map format

formatAddrSpec :: Mailbox -> T.Text
formatAddrSpec mbox = format $ mbox { mboxDisplay = "" }

data Message = Message [Header] T.Text
  deriving (Show, Eq)

instance Format Message where
    format (Message headers body) = formatList headers <> "\r\n" <> body
    formatList = fail "not implemented"

data MsgId = MsgId T.Text T.Text
  deriving (Show, Eq)

instance Format MsgId where
    format (MsgId leftid rightid) = "<" <> leftid <> "@" <> rightid <> ">"
    formatList = T.unwords . map format

instance Eq ZonedTime where
    t1 == t2 = zonedTimeToUTC t1 == zonedTimeToUTC t2

instance Format ZonedTime where
    format t = T.pack $ formatTime rfc5322DateFormat "%a, %c" t
    formatList = fail "not implemented"

-- | Time locale according to RFC 5322
rfc5322DateFormat :: TimeLocale
rfc5322DateFormat = TimeLocale
    { wDays          = [ ("Sunday"   , "Sun")
                       , ("Monday"   , "Mon")
                       , ("Tuesday"  , "Tue")
                       , ("Wednesday", "Wed")
                       , ("Thursday" , "Thu")
                       , ("Friday"   , "Fri")
                       , ("Saturday" , "Sat")
                       ]
    , months         = [ ("January"  , "Jan")
                       , ("February" , "Feb")
                       , ("March"    , "Mar")
                       , ("April"    , "Apr")
                       , ("May"      , "May")
                       , ("June"     , "Jun")
                       , ("July"     , "Jul")
                       , ("August"   , "Aug")
                       , ("September", "Sep")
                       , ("October"  , "Oct")
                       , ("November" , "Nov")
                       , ("December" , "Dec")
                       ]
    , amPm           = ("AM", "PM")
    , dateTimeFmt    = "%e %b %Y %H:%M:%S %z"
    , dateFmt        = "%e %b %Y"
    , timeFmt        = "%H:%M:%S"
    , time12Fmt      = "%I:%M:%S %p"
    , knownTimeZones = []
    }

-- | Get an number of nanoseconds since Jan 1 1970 UTC
zonedTimeNanoseconds :: ZonedTime -> Int64
zonedTimeNanoseconds t = ns
  where
    utcTime = zonedTimeToUTC t
    sec = utcTimeToPOSIXSeconds utcTime
    ns = truncate $ sec * 1000000000

-- | Get a the timezone offset from UTC in minutes
zonedTimeTZOffset :: ZonedTime -> Int
zonedTimeTZOffset (ZonedTime _ (TimeZone offset _ _)) = offset

-- | Build a datetime from nanoseconds since Jan 1 1970 UTC and timezone offset
zonedTimeFromNanoseconds :: Int64 -> Int -> ZonedTime
zonedTimeFromNanoseconds ns tzoffset = t
  where
    sec = fromRational $ (toInteger ns) % 1000000000
    tz  = minutesToTimeZone tzoffset
    utcTime = posixSecondsToUTCTime sec
    t = utcToZonedTime tz utcTime
