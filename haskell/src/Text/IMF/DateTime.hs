{-# LANGUAGE OverloadedStrings #-}

module Text.IMF.DateTime
  ( DateTime(..)
  , pDateTime
  , rfc5322DateFormat
  , buildDateTime
  , datetimeNanoseconds
  , datetimeTZOffset
  , datetimeFromNanoseconds
  )
where

import           Control.Applicative            ( liftA3 )
import           Data.Fixed                     ( Pico )
import           Data.Int                       ( Int64 )
import           Data.Ratio                     ( (%) )
import           Data.Time.Calendar             ( Day
                                                , fromGregorian
                                                )
import           Data.Time.Clock.POSIX          ( utcTimeToPOSIXSeconds
                                                , posixSecondsToUTCTime
                                                )
import           Data.Time.Format               ( TimeLocale(..)
                                                , formatTime
                                                )
import           Data.Time.LocalTime            ( LocalTime(..)
                                                , TimeOfDay(..)
                                                , ZonedTime(..)
                                                , minutesToTimeZone
                                                , zonedTimeToUTC
                                                , utcToZonedTime
                                                , TimeZone(..)
                                                )
import           Text.Parsec                    ( parse
                                                , (<|>)
                                                , count
                                                , option
                                                , optional
                                                , try
                                                , eof
                                                )
import           Text.Parsec.Char               ( digit
                                                , string
                                                )

import           Text.IMF.Format
import           Text.IMF.Primatives

newtype DateTime = DateTime ZonedTime
  deriving (Show)

instance Eq DateTime where
    (DateTime t1) == (DateTime t2) = zonedTimeToUTC t1 == zonedTimeToUTC t2

instance ParseMessage DateTime where
    parseMessage = parse (pDateTime <* eof) ""

instance FormatMessage DateTime where
    formatMessage (DateTime t) = formatTime rfc5322DateFormat "%a, %c" t

-- | Get an number of nanoseconds since Jan 1 1970 UTC
datetimeNanoseconds :: DateTime -> Int64
datetimeNanoseconds (DateTime zonedTime) = ns
  where
    utcTime = zonedTimeToUTC zonedTime
    sec = utcTimeToPOSIXSeconds utcTime
    ns = truncate $ sec * 1000000000

-- | Get a the timezone offset from UTC in minutes
datetimeTZOffset :: DateTime -> Int
datetimeTZOffset (DateTime (ZonedTime _ (TimeZone offset _ _))) = offset

-- | Build a datetime from nanoseconds since Jan 1 1970 UTC and timezone offset
datetimeFromNanoseconds :: Int64 -> Int -> DateTime
datetimeFromNanoseconds ns tzoffset = DateTime zonedTime
  where
    sec = fromRational $ (toInteger ns) % 1000000000
    tz  = minutesToTimeZone tzoffset
    utcTime = posixSecondsToUTCTime sec
    zonedTime = utcToZonedTime tz utcTime

-- | Builds a datetime from subcomponents
buildDateTime :: Integer -- ^ year
              -> Int     -- ^ month (1-12)
              -> Int     -- ^ day (1-31)
              -> Int     -- ^ hour (0-59)
              -> Int     -- ^ minute (0-59)
              -> Pico    -- ^ second
              -> Int     -- ^ timezone offset
              -> DateTime
buildDateTime _year _month _day _hour _minute _second _tzMinutes =
    DateTime (ZonedTime (LocalTime _date _time) _tz)
  where
    _date = fromGregorian _year _month _day
    _time = TimeOfDay _hour _minute _second
    _tz   = minutesToTimeZone _tzMinutes

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

-- | Date and time
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
pDateTime :: Parser DateTime
pDateTime = do
    _ <- optional $ try (optional pFWS *> pDayName <* string ",")
    liftA3 datetime' (optional pFWS *> pDate)
                     (pFWS *> pTimeOfDay)
                     (pFWS *> pZone <* optional pCFWS)
  where
    datetime' d t z = DateTime (ZonedTime (LocalTime d t) z)

-- | Day name
--
-- > day-name = "Mon" / "Tue" / "Wed" / "Thu" /
-- >            "Fri" / "Sat" / "Sun"
--
pDayName :: Parser String
pDayName = try (string "Sun")
       <|> try (string "Mon")
       <|> try (string "Tue")
       <|> try (string "Wed")
       <|> try (string "Thu")
       <|> try (string "Fri")
       <|> try (string "Sat")

-- | Date
--
-- > date = day FWS month FWS year
--
pDate :: Parser Day
pDate = liftA3 fromGregorian' pDay (pFWS *> pMonth) (pFWS *> pYear)
  where
    fromGregorian' d m y = fromGregorian y m d

-- | Day
--
-- > day = 1*2DIGIT
--
pDay :: Parser Int
pDay = read <$> (try (count 2 digit) <|> count 1 digit)

-- | Month
--
-- > month = "Jan" / "Feb" / "Mar" / "Apr" /
-- >         "May" / "Jun" / "Jul" / "Aug" /
-- >         "Sep" / "Oct" / "Nov" / "Dec"
--
pMonth :: Parser Int
pMonth = 1 <$ try (string "Jan")
    <|>  2 <$ try (string "Feb")
    <|>  3 <$ try (string "Mar")
    <|>  4 <$ try (string "Apr")
    <|>  5 <$ try (string "May")
    <|>  6 <$ try (string "Jun")
    <|>  7 <$ try (string "Jul")
    <|>  8 <$ try (string "Aug")
    <|>  9 <$ try (string "Sep")
    <|> 10 <$ try (string "Oct")
    <|> 11 <$ try (string "Nov")
    <|> 12 <$ try (string "Dec")

-- | Year
--
-- > year = 4*DIGIT
--
pYear :: Parser Integer
pYear = read <$> count 4 digit

-- | Time of day
--
-- > time-of-day = hour ":" minute [ ":" second ]
--
pTimeOfDay :: Parser TimeOfDay
pTimeOfDay = liftA3 TimeOfDay pHour (string ":" *> pMinute) (option 0 $ string ":" *> pSecond)

-- | Hour
--
-- > hour = 2DIGIT
--
pHour :: Parser Int
pHour = read <$> count 2 digit

-- | Minute
--
-- > minute = 2DIGIT
--
pMinute :: Parser Int
pMinute = read <$> count 2 digit

-- | Second
--
-- > second = 2DIGIT
--
pSecond :: Parser Pico
pSecond = read <$> count 2 digit

-- | Timezone
--
-- > zone = ( "+" / "-" ) 4DIGIT
--
pZone :: Parser TimeZone
pZone = liftA3 minutesToTimeZone' pSign pHour pMinute
  where
    pSign = (1 <$ string "+") <|> (-1 <$ string "-")
    minutesToTimeZone' s h m = minutesToTimeZone $ s * (h * 60 + m)
