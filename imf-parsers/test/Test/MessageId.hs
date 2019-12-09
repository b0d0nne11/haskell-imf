{-# LANGUAGE OverloadedStrings #-}

module Test.MessageId where

import Data.Attoparsec.Text       (endOfInput, parseOnly)
import Test.Tasty
import Test.Tasty.HUnit

import Data.IMF
import Data.IMF.Parsers.MessageId

tests :: TestTree
tests = testGroup "message id parsers"
    [ testMessageId
    ]

testMessageId = testCase "message id" $ do
    parse "<1234@example.com>"       @?= Right (MessageId "1234" "example.com")
    parse " <1234@example.com>\r\n " @?= Right (MessageId "1234" "example.com")
    parse "<1234@[1.2.3.4]>"         @?= Right (MessageId "1234" "[1.2.3.4]")
  where
    parse = parseOnly $ pMessageId <* endOfInput
