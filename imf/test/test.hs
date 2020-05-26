{-# LANGUAGE OverloadedStrings #-}

import Data.Text (intercalate)
import Test.Tasty
import Test.Tasty.HUnit

import Data.IMF.Header
import Data.IMF.Mailbox
import Data.IMF.MessageId
import Data.IMF.Message
import Data.IMF.Time
import Data.IMF.Types

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ testFormatHeader
    , testFormatMailbox
    , testFormatAddrSpec
    , testFormatMessageId
    , testFormatMessage
    , testFormatTime
    ]

testFormatHeader = testCase "format header" $ do
    format (HeaderDate epoch) @?= "Date: Wed, 17 Nov 1858 00:00:00 UTC"
    format (HeaderFrom [jsmith]) @?= "From: John Smith <jsmith@example.com>"
    format (HeaderSender jsmith) @?= "Sender: John Smith <jsmith@example.com>"
    format (HeaderReplyTo [jsmith]) @?= "Reply-To: John Smith <jsmith@example.com>"
    format (HeaderTo []) @?= "To: Empty:;"
    format (HeaderTo [jsmith]) @?= "To: John Smith <jsmith@example.com>"
    format (HeaderCc []) @?= "Cc: Empty:;"
    format (HeaderCc [jsmith]) @?= "Cc: John Smith <jsmith@example.com>"
    format (HeaderBcc []) @?= "Bcc: "
    format (HeaderBcc [jsmith]) @?= "Bcc: John Smith <jsmith@example.com>"
    format (HeaderMessageId msgid) @?= "Message-ID: <1234@example.com>"
    format (HeaderInReplyTo [msgid]) @?= "In-Reply-To: <1234@example.com>"
    format (HeaderReferences [msgid]) @?= "References: <1234@example.com>"
    format (HeaderSubject "Hello world!") @?= "Subject: Hello world!"
    format (HeaderComments "foo") @?= "Comments: foo"
    format (HeaderKeywords ["foo"]) @?= "Keywords: foo"
    format (HeaderOptional ("X-Custom", "foo")) @?= "X-Custom: foo"
  where
    msgid = MessageId "1234" "example.com"
    jsmith = Mailbox "John Smith" "jsmith" "example.com"

testFormatMailbox = testCase "format mailbox" $ do
    format (Mailbox "" "jsmith" "example.com") @?= "jsmith@example.com"
    format (Mailbox "John Smith" "jsmith" "example.com") @?= "John Smith <jsmith@example.com>"

testFormatDomain = testCase "format domain" $
    format (Domain "example.com") @?= "example.com"

testFormatAddrSpec = testCase "format address spec" $ do
    format (AddrSpec $ Mailbox "" "jsmith" "example.com") @?= "jsmith@example.com"
    format (AddrSpec $ Mailbox "John Smith" "jsmith" "example.com") @?= "jsmith@example.com"

testFormatMessageId = testCase "format message id" $
    format (MessageId "1234" "example.com") @?= "<1234@example.com>"

testFormatMessage = testCase "format message" $
    format msg @?= expected
  where
    msgid = MessageId "1234" "example.com"
    jsmith = Mailbox "John Smith" "jsmith" "example.com"
    msg = Message {
        msgHeaders =
            [ HeaderMessageId msgid
            , HeaderDate epoch
            , HeaderFrom [jsmith]
            , HeaderTo [jsmith]
            , HeaderSubject "Hello world!"
            ],
        msgBody = "This is a test message."
    }
    expected = intercalate "\r\n"
        [ "Message-ID: <1234@example.com>"
        , "Date: Wed, 17 Nov 1858 00:00:00 UTC"
        , "From: John Smith <jsmith@example.com>"
        , "To: John Smith <jsmith@example.com>"
        , "Subject: Hello world!"
        , ""
        , "This is a test message."
        ]

testFormatTime = testCase "format time" $
    format epoch @?= "Wed, 17 Nov 1858 00:00:00 UTC"
