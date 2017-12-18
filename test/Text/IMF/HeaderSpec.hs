{-# LANGUAGE OverloadedStrings #-}

module Text.IMF.HeaderSpec where

import           Data.Either (isLeft, isRight)
import           Test.Hspec

import           Text.IMF

spec :: Spec
spec = describe "ParseMessage Header" $ do
  checkHeader "Date: Thu,  1 Jan 1970 00:00:00 +0000"      $ Date $ buildDateTime 1970 1 1 0 0 0 0
  checkHeader "From: foo@example.com, bar@example.com"     $ From [Mailbox Nothing "foo" "example.com", Mailbox Nothing "bar" "example.com"]
  checkHeader "Sender: Foo <foo@example.com>"              $ Sender (Mailbox (Just "Foo") "foo" "example.com")
  checkHeader "Reply-To: foo@example.com, bar@example.com" $ ReplyTo [Mailbox Nothing "foo" "example.com", Mailbox Nothing "bar" "example.com"]
  checkHeader "To: foo@example.com, bar@example.com"       $ To [Mailbox Nothing "foo" "example.com", Mailbox Nothing "bar" "example.com"]
  checkHeader "Cc: foo@example.com, bar@example.com"       $ Cc [Mailbox Nothing "foo" "example.com", Mailbox Nothing "bar" "example.com"]
  checkHeader "Bcc: foo@example.com, bar@example.com"      $ Bcc [Mailbox Nothing "foo" "example.com", Mailbox Nothing "bar" "example.com"]
  checkHeader "Bcc:"                                       $ Bcc []
  checkHeader "Bcc: Undisclosed recipients:;"              $ Bcc []
  checkHeader "Message-ID: <1234@example.com>"             $ MessageId (MsgId "1234" "example.com")
  checkHeader "In-Reply-To: <1@foo.com> <2@bar.com>"       $ InReplyTo [MsgId "1" "foo.com", MsgId "2" "bar.com"]
  checkHeader "References: <1@foo.com> <2@bar.com>"        $ References [MsgId "1" "foo.com", MsgId "2" "bar.com"]
  checkHeader "Subject: Hello world!"                      $ Subject "Hello world!"
  checkHeader "Comments: A comment."                       $ Comments "A comment."
  checkHeader "Keywords: Foo, Bar, Baz"                    $ Keywords ["Foo", "Bar", "Baz"]
  checkHeader "X-Custom-Id: 1"                             $ Optional ("X-Custom-Id", "1")

checkHeader :: String -> Header -> SpecWith ()
checkHeader raw expected = context raw $ do
  let parsed = parseMessage (raw ++ "\r\n") :: Either ParseError Header
      formatted = formatMessage <$> parsed
      reParsed = formatted >>= parseMessage :: Either ParseError Header
      reFormatted = formatMessage <$> reParsed
  it "parsing should return a valid result" $
    parsed `shouldSatisfy` isRight
  it "parsing should return the expected result" $
    parsed `shouldBe` Right expected
  it "parsing should be idempotent" $ do
    parsed `shouldBe` reParsed
    formatted `shouldBe` reFormatted

checkInvalidHeader :: String -> SpecWith ()
checkInvalidHeader raw = context raw $ do
  let parsed = parseMessage raw :: Either ParseError Header
  it "parsing should return an error" $
    parsed `shouldSatisfy` isLeft
