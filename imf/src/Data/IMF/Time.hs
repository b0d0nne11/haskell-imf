module Data.IMF.Time
  ( ZonedTime(..)
  )
where

import qualified Data.Text           as T
import           Data.Time.Format    (TimeLocale (..), formatTime)
import           Data.Time.LocalTime (ZonedTime (..), zonedTimeToUTC)

import           Data.IMF.Types

instance Eq ZonedTime where
    t1 == t2 = zonedTimeToUTC t1 == zonedTimeToUTC t2

instance HasFormatter ZonedTime where
    format t = T.pack $ formatTime rfc5322DateFormat "%a, %c" t

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

