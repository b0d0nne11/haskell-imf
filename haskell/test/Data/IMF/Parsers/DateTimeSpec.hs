{-# LANGUAGE OverloadedStrings #-}

module Data.IMF.Parsers.DateTimeSpec where

import           Data.Attoparsec.Text           ( parseOnly
                                                , endOfInput
                                                )
import           Data.Time.Calendar             ( fromGregorian
                                                )
import           Data.Time.LocalTime            ( LocalTime(..)
                                                , ZonedTime(..)
                                                , midnight
                                                , utc
                                                )
import           Test.Hspec

import           Data.IMF.Types                 ()
import           Data.IMF.Parsers.DateTime

epoch :: ZonedTime
epoch = ZonedTime (LocalTime (fromGregorian 1970 1 1) midnight) utc

spec :: Spec
spec = do
    let parse = parseOnly $ pDateTime <* endOfInput
    describe "datetime" $ it "parses" $ do
        parse "1 Jan 1970 00:00 +0000" `shouldBe` Right epoch
        parse "Thu, 1 Jan 1970 00:00:00 +0000" `shouldBe` Right epoch
        parse " 1 Jan 1970 00:00 +0000\r\n " `shouldBe` Right epoch
