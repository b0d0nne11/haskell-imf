{-# LANGUAGE OverloadedStrings #-}

module Text.IMF.HeaderSpec where

import           Data.Foldable                  ( for_ )
import           Data.Either                    ( isLeft )
import           Test.Hspec

import           Text.IMF

spec :: Spec
spec = for_ fixtures $ \(input, mExpected) ->
  context ("given header field " ++ input) $ case mExpected of
    Just expected -> do
      let parsed = parseMessage (input ++ "\r\n") :: Either ParseError HeaderField
      it "should parse" $ parsed `shouldBe` Right expected
      let validated = parsed >>= validateMessage
      it "parsing should be idempotent" $ parsed `shouldBe` validated
    Nothing -> do
      let parsed = parseMessage (input ++ "\r\n") :: Either ParseError HeaderField
      it "should not parse" $ parsed `shouldSatisfy` isLeft

fixtures :: [(String, Maybe HeaderField)]
fixtures =
  -- Date
  [ ("Date: 1 Jan 1970 00:00 +0000"   , Just $ Date $ buildDateTime 1970 1 1 0 0 0 0)
  , ("Date: 1 Jan 1970 00:00:00 +0000", Just $ Date $ buildDateTime 1970 1 1 0 0 0 0)
  , ( "Date: Thu, 1 Jan 1970 00:00:00 +0000"
    , Just $ Date $ buildDateTime 1970 1 1 0 0 0 0
    )
  -- From
  , ( "From: foo@example.com, bar@example.com"
    , Just $ From [Mailbox "" "foo" "example.com", Mailbox "" "bar" "example.com"]
    )
  -- Sender
  , ( "Sender: Foo <foo@example.com>"
    , Just $ Sender $ Mailbox "Foo" "foo" "example.com"
    )
  -- Reply To
  , ( "Reply-To: foo@example.com, bar@example.com"
    , Just $ ReplyTo [Mailbox "" "foo" "example.com", Mailbox "" "bar" "example.com"]
    )
  -- To
  , ( "To: foo@example.com, bar@example.com"
    , Just $ To [Mailbox "" "foo" "example.com", Mailbox "" "bar" "example.com"]
    )
  -- Cc
  , ( "Cc: foo@example.com, bar@example.com"
    , Just $ Cc [Mailbox "" "foo" "example.com", Mailbox "" "bar" "example.com"]
    )
  -- Bcc
  , ( "Bcc: foo@example.com, bar@example.com"
    , Just $ Bcc [Mailbox "" "foo" "example.com", Mailbox "" "bar" "example.com"]
    )
  , ("Bcc:"                         , Just $ Bcc [])
  , ("Bcc: "                        , Just $ Bcc [])
  , ("Bcc: Undisclosed recipients:;", Just $ Bcc [])
  -- Message Id
  , ( "Message-ID: <1234@example.com>"
    , Just $ MessageId $ MsgId "1234" "example.com"
    )
  -- In Reply To
  , ( "In-Reply-To: <1@foo.com> <2@bar.com>"
    , Just $ InReplyTo [MsgId "1" "foo.com", MsgId "2" "bar.com"]
    )
  -- References
  , ( "References: <1@foo.com> <2@bar.com>"
    , Just $ References [MsgId "1" "foo.com", MsgId "2" "bar.com"]
    )
  -- Subject
  , ( "Subject: Hello world!"
    , Just $ Subject "Hello world!"
    )
  -- Comments
  , ( "Comments: A comment."
    , Just $ Comments "A comment."
    )
  -- Keywords
  , ( "Keywords: Foo, Bar, Baz"
    , Just $ Keywords ["Foo", "Bar", "Baz"]
    )
  -- Optional
  , ("X-Custom-Id: 1", Just $ Optional ("X-Custom-Id", "1"))
  -- Mismatched name/value
  , ("Date: foo@example.com", Nothing)
  ]
