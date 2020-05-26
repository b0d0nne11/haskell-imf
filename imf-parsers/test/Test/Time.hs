{-# LANGUAGE OverloadedStrings #-}

module Test.Time where

import Data.Attoparsec.Text  (endOfInput, parseOnly)
import Test.Tasty
import Test.Tasty.HUnit

import Data.IMF
import Data.IMF.Parsers.Time

tests :: TestTree
tests = testGroup "time parsers"
    [ testTime
    ]

testTime = testCase "time" $ do
    parse "17 Nov 1858 00:00 +0000"         @?= Right epoch
    parse "Thu, 17 Nov 1858 00:00:00 +0000" @?= Right epoch
    parse " 17 Nov 1858 00:00 +0000\r\n "   @?= Right epoch
  where
    parse = parseOnly $ pTime <* endOfInput
