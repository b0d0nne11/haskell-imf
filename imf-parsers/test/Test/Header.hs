{-# LANGUAGE OverloadedStrings #-}

module Test.Header where

import Data.Attoparsec.Text (endOfInput, parseOnly)
import Data.Either          (isLeft)
import Data.Time.Calendar   (fromGregorian)
import Data.Time.LocalTime  (LocalTime (..), ZonedTime (..), midnight, utc)
import Test.Tasty
import Test.Tasty.HUnit

import Data.IMF
import Data.IMF.Parsers.Header

tests :: TestTree
tests = testGroup "header parsers"
    [ testHeader
    ]

testHeader = testCase "header" $ do
    parse "Date: Thu, 1 Jan 1970 00:00:00 +0000" @?= Right (HeaderDate epoch)
    parse "From: foo@example.com, bar@example.com" @?= Right (HeaderFrom [Mailbox "" "foo" "example.com", Mailbox "" "bar" "example.com"])
    parse "Sender: Foo <foo@example.com>" @?= Right (HeaderSender $ Mailbox "Foo" "foo" "example.com")
    parse "Reply-To: foo@example.com, bar@example.com" @?= Right (HeaderReplyTo [Mailbox "" "foo" "example.com", Mailbox "" "bar" "example.com"])
    parse "To: foo@example.com, bar@example.com" @?= Right (HeaderTo [Mailbox "" "foo" "example.com", Mailbox "" "bar" "example.com"])
    parse "Cc: foo@example.com, bar@example.com" @?= Right (HeaderCc [Mailbox "" "foo" "example.com", Mailbox "" "bar" "example.com"])
    parse "Bcc: foo@example.com, bar@example.com" @?= Right (HeaderBcc [Mailbox "" "foo" "example.com", Mailbox "" "bar" "example.com"])
    parse "Bcc:" @?= Right (HeaderBcc [])
    parse "Bcc: " @?= Right (HeaderBcc [])
    parse "Bcc: Undisclosed recipients:;" @?= Right (HeaderBcc [])
    parse "Message-ID: <1234@example.com>" @?= Right (HeaderMessageId $ MessageId "1234" "example.com")
    parse "In-Reply-To: <1@foo.com> <2@bar.com>" @?= Right (HeaderInReplyTo [MessageId "1" "foo.com", MessageId "2" "bar.com"])
    parse "References: <1@foo.com> <2@bar.com>" @?= Right (HeaderReferences [MessageId "1" "foo.com", MessageId "2" "bar.com"])
    parse "Subject: Hello world!" @?= Right (HeaderSubject "Hello world!")
    parse "Comments: A comment." @?= Right (HeaderComments "A comment.")
    parse "Keywords: Foo, Bar, Baz" @?= Right (HeaderKeywords ["Foo", "Bar", "Baz"])
    parse "X-Custom-Id: 1" @?= Right (HeaderOptional ("X-Custom-Id", "1"))
    isLeft (parse "Date: foo@example.com") @? "invalid header"
  where
    parse = parseOnly $ pHeader <* endOfInput
    epoch = ZonedTime (LocalTime (fromGregorian 1970 1 1) midnight) utc
