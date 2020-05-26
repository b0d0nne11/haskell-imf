module Data.IMF.Time
  ( ZonedTime(..)
  , getZonedTime
  , epoch
  )
where

import qualified Data.Text           as T
import           Data.Time.Calendar  (Day (..))
import           Data.Time.Format    (defaultTimeLocale, formatTime, rfc822DateFormat)
import           Data.Time.LocalTime (LocalTime (..), ZonedTime (..), getZonedTime, midnight, utc,
                                      zonedTimeToUTC)

import           Data.IMF.Types

instance Eq ZonedTime where
    t1 == t2 = zonedTimeToUTC t1 == zonedTimeToUTC t2

instance Ord ZonedTime where
    compare t1 t2 = compare (zonedTimeToUTC t1) (zonedTimeToUTC t2)

instance HasFormatter ZonedTime where
    format = T.pack . formatTime defaultTimeLocale rfc822DateFormat

-- | Midnight UTC on November 17, 1858
epoch :: ZonedTime
epoch = ZonedTime (LocalTime (ModifiedJulianDay 0) midnight) utc
