{-# LANGUAGE OverloadedStrings #-}

module Test.Time where

import Data.Attoparsec.Text (endOfInput, parseOnly)
import Data.Time.Calendar   (fromGregorian)
import Data.Time.LocalTime  (LocalTime (..), ZonedTime (..), midnight, utc)
import Test.Tasty
import Test.Tasty.HUnit

import Data.IMF
import Data.IMF.Parsers.Time

tests :: TestTree
tests = testGroup "time parsers"
    [ testTime
    ]

testTime = testCase "time" $ do
    parse "1 Jan 1970 00:00 +0000"         @?= Right epoch
    parse "Thu, 1 Jan 1970 00:00:00 +0000" @?= Right epoch
    parse " 1 Jan 1970 00:00 +0000\r\n "   @?= Right epoch
  where
    parse = parseOnly $ pTime <* endOfInput
    epoch = ZonedTime (LocalTime (fromGregorian 1970 1 1) midnight) utc
