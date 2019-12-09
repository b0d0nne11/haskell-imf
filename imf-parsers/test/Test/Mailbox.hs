{-# LANGUAGE OverloadedStrings #-}

module Test.Mailbox where

import Data.Attoparsec.Text     (endOfInput, parseOnly)
import Data.Either              (isLeft)
import Test.Tasty
import Test.Tasty.HUnit

import Data.IMF
import Data.IMF.Parsers.Mailbox

tests :: TestTree
tests = testGroup "mailbox parsers"
    [ testDomainLiteral
    , testMailbox
    ]

testDomainLiteral = testCase "domain literal" $ do
    parse "[1.2.3.4]"         @?= Right "[1.2.3.4]"
    parse "[ 1 . 2 . 3 . 4 ]" @?= Right "[ 1 . 2 . 3 . 4 ]"
    parse " [1.2.3.4]\r\n "   @?= Right "[1.2.3.4]"
  where
    parse = parseOnly $ pDomainLiteral <* endOfInput

testMailbox = testCase "mailbox" $ do
    -- from https://en.wikipedia.org/wiki/email_address
    -- generic valid
    parse "simple@example.com" @?= Right (Mailbox "" "simple" "example.com")
    parse "very.common@example.com" @?= Right (Mailbox "" "very.common" "example.com")
    parse "disposable.style.email.with+symbol@example.com" @?= Right (Mailbox "" "disposable.style.email.with+symbol" "example.com")
    parse "other.email-with-hyphen@example.com" @?= Right (Mailbox "" "other.email-with-hyphen" "example.com")
    parse "fully-qualified-domain@example.com" @?= Right (Mailbox "" "fully-qualified-domain" "example.com")
    parse "user.name+tag+sorting@example.com" @?= Right (Mailbox "" "user.name+tag+sorting" "example.com")
    parse "x@example.com" @?= Right (Mailbox "" "x" "example.com")
    parse "example-indeed@strange-example.com" @?= Right (Mailbox "" "example-indeed" "strange-example.com")
    parse "admin@mailserver1" @?= Right (Mailbox "" "admin" "mailserver1")
    parse "example@s.example" @?= Right (Mailbox "" "example" "s.example")
    parse "\" \"@example.org" @?= Right (Mailbox "" "\" \"" "example.org")
    parse "\"john..doe\"@example.org" @?= Right (Mailbox "" "\"john..doe\"" "example.org")
    parse "mailhost!username@example.org" @?= Right (Mailbox "" "mailhost!username" "example.org")
    parse "user%example.com@example.org" @?= Right (Mailbox "" "user%example.com" "example.org")
    -- generic invalid
    isLeft (parse "Abc.example.com") @? "invalid mailbox"
    isLeft (parse "A@b@c@example.com") @? "invalid mailbox"
    isLeft (parse "a\"b(c)d,e:f;g<h>i[j\\k]l@example.com") @? "invalid mailbox"
    isLeft (parse "just\"not\"right@example.com") @? "invalid mailbox"
    isLeft (parse "this is\"not\\allowed@example.com") @? "invalid mailbox"
    isLeft (parse "this\\ still\\\"not\\\\allowed@example.com") @? "invalid mailbox"
    -- international
    parse "Pelé@example.com" @?= Right (Mailbox "" "Pelé" "example.com")
    parse "медведь@с-балалайкой.рф" @?= Right (Mailbox "" "медведь" "с-балалайкой.рф")
    -- consecutive dots
    isLeft (parse "John..Doe@example.com") @? "invalid mailbox"
    parse "\"John..Doe\"@example.com" @?= Right (Mailbox "" "\"John..Doe\"" "example.com")
    -- domain literals
    parse "jsmith@[192.168.2.1]" @?= Right (Mailbox "" "jsmith" "[192.168.2.1]")
    parse "jsmith@[IPv6:2001:db8::1]" @?= Right (Mailbox "" "jsmith" "[IPv6:2001:db8::1]")
    -- comments
    parse "john.smith@(comment)example.com" @?= Right (Mailbox "" "john.smith" "example.com")
    parse "john.smith@example.com(comment)" @?= Right (Mailbox "" "john.smith" "example.com")
    -- subaddressing
    parse "joeuser+tag@example.com" @?= Right (Mailbox "" "joeuser+tag" "example.com")
  where
    parse = parseOnly $ pMailbox <* endOfInput
