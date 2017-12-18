{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Text.IMF.MessageSpec where

import           Data.Either    (isLeft, isRight)
import           Data.String.QQ
import           Test.Hspec

import           Text.IMF

spec :: Spec
spec = describe "ParseMessage Message" $ do

    context "Simple Addressing (1/2)" $
      let raw = escape [s|
From: John Doe <jdoe@machine.example>
To: Mary Smith <mary@example.net>
Subject: Saying Hello
Date: Fri, 21 Nov 1997 09:55:06 -0600
Message-ID: <1234@local.machine.example>

This is a message just to say hello.
So, "Hello".
|]
          expected = Message {
                       msgHeaders = [
                         From [Mailbox (Just "John Doe") "jdoe" "machine.example"],
                         To [Mailbox (Just "Mary Smith") "mary" "example.net"],
                         Subject "Saying Hello",
                         Date $ buildDateTime 1997 11 21 9 55 06 (-360),
                         MessageId (MsgId "1234" "local.machine.example")
                       ],
                       msgBody = escape [s|
This is a message just to say hello.
So, "Hello".
|]
                     }
      in checkMessage raw expected

    context "Simple Addressing (2/2)" $
      let raw = escape [s|
From: John Doe <jdoe@machine.example>
Sender: Michael Jones <mjones@machine.example>
To: Mary Smith <mary@example.net>
Subject: Saying Hello
Date: Fri, 21 Nov 1997 09:55:06 -0600
Message-ID: <1234@local.machine.example>

This is a message just to say hello.
So, "Hello".
|]
          expected = Message {
                       msgHeaders = [
                         From [Mailbox (Just "John Doe") "jdoe" "machine.example"],
                         Sender (Mailbox (Just "Michael Jones") "mjones" "machine.example"),
                         To [Mailbox (Just "Mary Smith") "mary" "example.net"],
                         Subject "Saying Hello",
                         Date $ buildDateTime 1997 11 21 9 55 06 (-360),
                         MessageId (MsgId "1234" "local.machine.example")
                       ],
                       msgBody = escape [s|
This is a message just to say hello.
So, "Hello".
|]
                     }
      in checkMessage raw expected

    context "Different Types of Mailboxes" $
      let raw = escape [s|
From: "Joe Q. Public" <john.q.public@example.com>
To: Mary Smith <mary@x.test>, jdoe@example.org, Who? <one@y.test>
Cc: <boss@nil.test>, "Giant; \"Big\" Box" <sysservices@example.net>
Date: Tue, 1 Jul 2003 10:52:37 +0200
Message-ID: <5678.21-Nov-1997@example.com>

Hi everyone.|]
          expected = Message {
                       msgHeaders = [
                         From [Mailbox (Just "\"Joe Q. Public\"") "john.q.public" "example.com"],
                         To [Mailbox (Just "Mary Smith") "mary" "x.test", Mailbox Nothing "jdoe" "example.org", Mailbox (Just "Who?") "one" "y.test"],
                         Cc [Mailbox Nothing "boss" "nil.test", Mailbox (Just "\"Giant; \\\"Big\\\" Box\"") "sysservices" "example.net"],
                         Date $ buildDateTime 2003 7 1 10 52 37 120,
                         MessageId (MsgId "5678.21-Nov-1997" "example.com")
                       ],
                       msgBody = "Hi everyone."
                     }
      in checkMessage raw expected

    context "Group Addresses" $
      let raw = escape [s|
From: Pete <pete@silly.example>
To: A Group:Ed Jones <c@a.test>,joe@where.test,John <jdoe@one.test>;
Cc: Undisclosed recipients:;
Date: Thu, 13 Feb 1969 23:32:54 -0330
Message-ID: <testabcd.1234@silly.example>

Testing.|]
          expected = Message {
                       msgHeaders = [
                         From [Mailbox (Just "Pete") "pete" "silly.example"],
                         To [Mailbox (Just "Ed Jones") "c" "a.test", Mailbox Nothing "joe" "where.test", Mailbox (Just "John") "jdoe" "one.test"],
                         Cc [],
                         Date $ buildDateTime 1969 2 13 23 32 54 (-210),
                         MessageId (MsgId "testabcd.1234" "silly.example")
                       ],
                       msgBody = "Testing."
                     }
      in checkMessage raw expected

    context "Reply Messages (1/3)" $
      let raw = escape [s|
From: John Doe <jdoe@machine.example>
To: Mary Smith <mary@example.net>
Subject: Saying Hello
Date: Fri, 21 Nov 1997 09:55:06 -0600
Message-ID: <1234@local.machine.example>

This is a message just to say hello.
So, "Hello".
|]
          expected = Message {
                       msgHeaders = [
                         From [Mailbox (Just "John Doe") "jdoe" "machine.example"],
                         To [Mailbox (Just "Mary Smith") "mary" "example.net"],
                         Subject "Saying Hello",
                         Date $ buildDateTime 1997 11 21 9 55 06 (-360),
                         MessageId (MsgId "1234" "local.machine.example")
                       ],
                       msgBody = escape [s|
This is a message just to say hello.
So, "Hello".
|]
                     }
      in checkMessage raw expected

    context "Reply Messages (2/3)" $
      let raw = escape [s|
From: Mary Smith <mary@example.net>
To: John Doe <jdoe@machine.example>
Reply-To: "Mary Smith: Personal Account" <smith@home.example>
Subject: Re: Saying Hello
Date: Fri, 21 Nov 1997 10:01:10 -0600
Message-ID: <3456@example.net>
In-Reply-To: <1234@local.machine.example>
References: <1234@local.machine.example>

This is a reply to your hello.|]
          expected = Message {
                       msgHeaders = [
                         From [Mailbox (Just "Mary Smith") "mary" "example.net"],
                         To [Mailbox (Just "John Doe") "jdoe" "machine.example"],
                         ReplyTo [Mailbox (Just "\"Mary Smith: Personal Account\"") "smith" "home.example"],
                         Subject "Re: Saying Hello",
                         Date $ buildDateTime 1997 11 21 10 1 10 (-360),
                         MessageId (MsgId "3456" "example.net"),
                         InReplyTo [MsgId "1234" "local.machine.example"],
                         References [MsgId "1234" "local.machine.example"]
                       ],
                       msgBody = "This is a reply to your hello."
                     }
      in checkMessage raw expected

    context "Reply Messages (3/3)" $
      let raw = escape [s|
To: "Mary Smith: Personal Account" <smith@home.example>
From: John Doe <jdoe@machine.example>
Subject: Re: Saying Hello
Date: Fri, 21 Nov 1997 11:00:00 -0600
Message-ID: <abcd.1234@local.machine.test>
In-Reply-To: <3456@example.net>
References: <1234@local.machine.example> <3456@example.net>

This is a reply to your reply.|]
          expected = Message {
                       msgHeaders = [
                         To [Mailbox (Just "\"Mary Smith: Personal Account\"") "smith" "home.example"],
                         From [Mailbox (Just "John Doe") "jdoe" "machine.example"],
                         Subject "Re: Saying Hello",
                         Date $ buildDateTime 1997 11 21 11 0 0 (-360),
                         MessageId (MsgId "abcd.1234" "local.machine.test"),
                         InReplyTo [MsgId "3456" "example.net"],
                         References [MsgId "1234" "local.machine.example", MsgId "3456" "example.net"]
                       ],
                       msgBody = "This is a reply to your reply."
                     }
      in checkMessage raw expected

    context "White Space, Comments, and Other Oddities" $
      let raw = escape [s|
From: Pete(A nice \) chap) <pete(his account)@silly.test(his host)>
To:A Group(Some people)
     :Chris Jones <c@(Chris's host.)public.example>,
         joe@example.org,
  John <jdoe@one.test> (my dear friend); (the end of the group)
Cc:(Empty list)(start)Hidden recipients  :(nobody(that I know))  ;
Date: Thu,
      13
        Feb
          1969
      23:32
               -0330 (Newfoundland Time)
Message-ID:              <testabcd.1234@silly.test>

Testing.|]
          expected = Message {
                       msgHeaders = [
                         From [Mailbox (Just "Pete") "pete" "silly.test"],
                         To [Mailbox (Just "Chris Jones") "c" "public.example", Mailbox Nothing "joe" "example.org", Mailbox (Just "John") "jdoe" "one.test"],
                         Cc [],
                         Date $ buildDateTime 1969 2 13 23 32 0 (-210),
                         MessageId (MsgId "testabcd.1234" "silly.test")
                       ],
                       msgBody = "Testing."
                     }
      in checkMessage raw expected

escape :: String -> String
escape = concatMap escapeChar
  where
    escapeChar '\n' = "\r\n"
    escapeChar c = [c]

checkMessage :: String -> Message -> SpecWith ()
checkMessage raw expected = do
  let parsed = parseMessage raw :: Either ParseError Message
      formatted = formatMessage <$> parsed
      reParsed = formatted >>= parseMessage
      reFormatted = formatMessage <$> reParsed
  it "parsing should return a valid result" $
    parsed `shouldSatisfy` isRight
  it "parsing should return the expected result" $
    parsed `shouldBe` Right expected
  it "parsing should be idempotent" $ do
    parsed `shouldBe` reParsed
    formatted `shouldBe` reFormatted

checkInvalidMessage :: String -> SpecWith ()
checkInvalidMessage raw = context raw $ do
  let parsed = parseMessage raw :: Either ParseError Message
  it "should return an error" $
    parsed `shouldSatisfy` isLeft
