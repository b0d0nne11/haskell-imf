{-# LANGUAGE OverloadedStrings #-}

module Data.IMF.Parsers.HeaderSpec where

import           Data.Attoparsec.Text           ( parseOnly
                                                , endOfInput
                                                )
import           Data.Either                    ( isLeft )
import           Data.Foldable                  ( for_ )
import qualified Data.Text                     as T
import           Data.Time.Calendar             ( fromGregorian
                                                )
import           Data.Time.LocalTime            ( LocalTime(..)
                                                , ZonedTime(..)
                                                , midnight
                                                , utc
                                                )
import           Test.Hspec

import           Data.IMF.Types
import           Data.IMF.Parsers.Header

epoch :: ZonedTime
epoch = ZonedTime (LocalTime (fromGregorian 1970 1 1) midnight) utc

spec :: Spec
spec = do
    let parse = parseOnly $ pHeader <* endOfInput
    for_ fixtures $ \(input, output) -> case output of
        Just expected -> do
            describe ("valid header " ++ T.unpack input) $ do
                let parsed = parse $ input `T.append` "\r\n"
                it "should parse" $ parsed `shouldBe` Right expected
                let reparsed = parse . format =<< parsed
                it "parsing should be idempotent" $ reparsed `shouldBe` parsed
        Nothing -> do
            describe ("invalid header " ++ T.unpack input) $ do
                let parsed = parse $ input `T.append` "\r\n"
                it "should not parse" $ parsed `shouldSatisfy` isLeft

fixtures :: [(T.Text, Maybe Header)]
fixtures =
    [ -- Date
      ( "Date: Thu, 1 Jan 1970 00:00:00 +0000"
      , Just $ Date epoch
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
