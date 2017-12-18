{-# LANGUAGE OverloadedStrings #-}

module Text.IMF.DateTime
    ( DateTime(..)
    , dateTime
    , rfc5322DateFormat
    , buildDateTime
    ) where

import           Data.Fixed          (Pico)
import           Data.Time.Calendar  (fromGregorian)
import           Data.Time.Format    (TimeLocale (..), formatTime, parseTimeM)
import           Data.Time.LocalTime (LocalTime (..), TimeOfDay (..),
                                      ZonedTime (..), minutesToTimeZone,
                                      zonedTimeToUTC)
import           Text.Parsec         (ParsecT, choice, count, option, optional)
import           Text.Parsec.Char    (digit, string)

import           Text.IMF.Combinator
import           Text.IMF.Format
import           Text.IMF.Primatives

newtype DateTime = DateTime ZonedTime
  deriving (Show)

instance Eq DateTime where
  (DateTime t1) == (DateTime t2) = zonedTimeToUTC t1 == zonedTimeToUTC t2

instance ParseMessage DateTime where
  msgParser = dateTime

instance FormatMessage DateTime where
  formatMessage (DateTime t) = formatTime rfc5322DateFormat "%a, %c" t

-- | @buildDateTime year month day hour minute second tzMinutes@ builds a
-- DateTime object without having to worry about all the nested time classes.
-- This is provided mostly for testing.
buildDateTime :: Integer -> Int -> Int -> Int -> Int -> Pico -> Int -> DateTime
buildDateTime _year _month _day _hour _minute _second _tzMinutes = DateTime $ ZonedTime _datetime _timezone
  where
    _date = fromGregorian _year _month _day
    _time = TimeOfDay _hour _minute _second
    _datetime = LocalTime _date _time
    _timezone = minutesToTimeZone _tzMinutes

-- | Format string according to RFC 5322.
rfc5322DateFormat :: TimeLocale
rfc5322DateFormat = TimeLocale {
    wDays  = [ ("Sunday",    "Sun")
             , ("Monday",    "Mon")
             , ("Tuesday",   "Tue")
             , ("Wednesday", "Wed")
             , ("Thursday",  "Thu")
             , ("Friday",    "Fri")
             , ("Saturday",  "Sat")
             ],
    months = [ ("January",   "Jan")
             , ("February",  "Feb")
             , ("March",     "Mar")
             , ("April",     "Apr")
             , ("May",       "May")
             , ("June",      "Jun")
             , ("July",      "Jul")
             , ("August",    "Aug")
             , ("September", "Sep")
             , ("October",   "Oct")
             , ("November",  "Nov")
             , ("December",  "Dec")
             ],
    amPm = ("AM", "PM"),
    dateTimeFmt = "%e %b %Y %H:%M:%S %z",
    dateFmt = "%e %b %Y",
    timeFmt = "%H:%M:%S",
    time12Fmt = "%I:%M:%S %p",
    knownTimeZones = []
}

-- | Parser for local time with time zone.
--
-- > date-time   = [ [FWS] day-name "," ] [FWS] date FWS time [CFWS]
-- > day-name    = "Mon" / "Tue" / "Wed" / "Thu" /
-- >               "Fri" / "Sat" / "Sun"
-- > date        = day FWS month FWS year
-- > day         = 1*2DIGIT
-- > month       = "Jan" / "Feb" / "Mar" / "Apr" /
-- >               "May" / "Jun" / "Jul" / "Aug" /
-- >               "Sep" / "Oct" / "Nov" / "Dec"
-- > year        = 4*DIGIT
-- > time        = time-of-day FWS zone
-- > time-of-day = hour ":" minute [ ":" second ]
-- > hour        = 2DIGIT
-- > minute      = 2DIGIT
-- > second      = 2DIGIT
-- > zone        = ( "+" / "-" ) 4DIGIT
--
dateTime :: Monad m => ParsecT String u m DateTime
dateTime = choice
    [ DateTime <$> (parseTime "%a, %c" =<< optional fws *> collect (a ++ c) <* optional cfws)
    , DateTime <$> (parseTime     "%c" =<< optional fws *> collect c        <* optional cfws)
    ]
  where
    parseTime = parseTimeM False rfc5322DateFormat
    a = [dayName, string ",", option "" fws]
    c = [date, fws, time]

dayName :: Monad m => ParsecT String u m String
dayName = strings ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]

date :: Monad m => ParsecT String u m String
date = collect [day, fws, month, fws, year]

day :: Monad m => ParsecT String u m String
day = range [2,1] digit

month :: Monad m => ParsecT String u m String
month = strings [ "Jan", "Feb", "Mar", "Apr"
                , "May", "Jun", "Jul", "Aug"
                , "Sep", "Oct", "Nov", "Dec"
                ]

year :: Monad m => ParsecT String u m String
year = count 4 digit

time :: Monad m => ParsecT String u m String
time = collect [timeOfDay, fws, zone]

timeOfDay :: Monad m => ParsecT String u m String
timeOfDay = collect [hour, string ":", minute, option ":00" $ collect [string ":", second]]

hour :: Monad m => ParsecT String u m String
hour =  count 2 digit

minute :: Monad m => ParsecT String u m String
minute =  count 2 digit

second :: Monad m => ParsecT String u m String
second =  count 2 digit

zone :: Monad m => ParsecT String u m String
zone = collect [strings ["+", "-"], hour, minute]
