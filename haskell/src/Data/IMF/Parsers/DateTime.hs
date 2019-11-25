{-# LANGUAGE OverloadedStrings #-}

module Data.IMF.Parsers.DateTime
  ( pDateTime
  )
where

import           Control.Applicative            ( (<|>)
                                                , liftA3
                                                )
import           Data.Fixed                     ( Pico )
import           Data.Attoparsec.Text           ( Parser
                                                , count
                                                , digit
                                                , endOfInput
                                                , option
                                                , parseOnly
                                                )
import           Data.String                    ( IsString
                                                , fromString
                                                )
import           Data.Time.Calendar             ( Day
                                                , fromGregorian
                                                )
import           Data.Time.LocalTime            ( LocalTime(..)
                                                , TimeOfDay(..)
                                                , TimeZone(..)
                                                , ZonedTime(..)
                                                , minutesToTimeZone
                                                )
import qualified Data.Text                     as T

import           Data.IMF.Parsers.Core

instance IsString ZonedTime where
    fromString = either (fail "invalid datetime") id . parseOnly (pDateTime <* endOfInput) . T.pack

--------------------------------------------------------------------------------
-- Date and Time Specification <https://tools.ietf.org/html/rfc5322#section-3.3>
--------------------------------------------------------------------------------

-- | Date and time
--
-- > date-time = [ [FWS] day-name "," ] [FWS] date FWS time-of-day FWS zone [CFWS]
--
pDateTime :: Parser ZonedTime
pDateTime = liftA3 (\d t z -> ZonedTime (LocalTime d t) z)
                   (optional (optional pFWS *> pDayName *> ",") *> optional pFWS *> pDate)
                   (pFWS *> pTimeOfDay)
                   (pFWS *> pZone <* optional pCFWS)

-- | Day name
--
-- > day-name = "Mon" / "Tue" / "Wed" / "Thu" /
-- >            "Fri" / "Sat" / "Sun"
--
pDayName :: Parser T.Text
pDayName = "Sun"
       <|> "Mon"
       <|> "Tue"
       <|> "Wed"
       <|> "Thu"
       <|> "Fri"
       <|> "Sat"

-- | Date
--
-- > date = day FWS month FWS year
--
pDate :: Parser Day
pDate = liftA3 (\d m y -> fromGregorian y m d)
               pDay (pFWS *> pMonth) (pFWS *> pYear)

-- | Day
--
-- > day = 1*2DIGIT
--
pDay :: Parser Int
pDay = read <$> (count 2 digit <|> count 1 digit)

-- | Month
--
-- > month = "Jan" / "Feb" / "Mar" / "Apr" /
-- >         "May" / "Jun" / "Jul" / "Aug" /
-- >         "Sep" / "Oct" / "Nov" / "Dec"
--
pMonth :: Parser Int
pMonth = 1 <$ "Jan"
    <|>  2 <$ "Feb"
    <|>  3 <$ "Mar"
    <|>  4 <$ "Apr"
    <|>  5 <$ "May"
    <|>  6 <$ "Jun"
    <|>  7 <$ "Jul"
    <|>  8 <$ "Aug"
    <|>  9 <$ "Sep"
    <|> 10 <$ "Oct"
    <|> 11 <$ "Nov"
    <|> 12 <$ "Dec"

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
pTimeOfDay = liftA3 TimeOfDay pHour (":" *> pMinute) (option 0 $ ":" *> pSecond)

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
pZone = liftA3 (\sign h m -> minutesToTimeZone $ sign * (60 * h + m))
               ((1 <$ "+") <|> (-1 <$ "-")) pHour pMinute
