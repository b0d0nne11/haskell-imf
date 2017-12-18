{-# LANGUAGE OverloadedStrings #-}

module Text.IMF.MailboxSpec where

import           Data.Either (isLeft, isRight)
import           Test.Hspec

import           Text.IMF

spec :: Spec
spec = describe "ParseMessage Mailbox" $ do
  -- Mailboxes
  -- Taken from https://en.wikipedia.org/wiki/Email_address
  checkMailbox "prettyandsimple@example.com"
  checkMailbox "very.common@example.com"
  checkMailbox "disposable.style.email.with+symbol@example.com"
  checkMailbox "other.email-with-dash@example.com"
  checkMailbox "x@example.com"
  checkMailbox "\"much.more unusual\"@example.com"
  checkMailbox "\"very.unusual.@.unusual.com\"@example.com"
  checkMailbox "\"very.(),:;<>[]\\\".VERY.\\\"very@\\\\ \\\"very\\\".unusual\"@strange.example.com"
  checkMailbox "example-indeed@strange-example.com"
  checkMailbox "admin@mailserver1"
  checkMailbox "#!$%&'*+-/=?^_`{}|~@example.org"
  checkMailbox "\"()<>[]:,;@\\\"!#$%&'-/=?^_`{}| ~.a\"@example.org"
  checkMailbox "\" \"@example.org"
  checkMailbox "example@localhost"
  checkMailbox "example@s.solutions"
  checkMailbox "user@localserver"
  checkMailbox "user@tt"
  checkMailbox "user@[IPv6:2001:DB8::1]"
  checkInvalidMailbox "Abc.example.com"
  checkInvalidMailbox "A@b@c@example.com"
  checkInvalidMailbox "a\"b(c)d,e:f;g<h>i[j\\k]l@example.com"
  checkInvalidMailbox "just\"not\"right@example.com"
  checkInvalidMailbox "this is\"not\\allowed@example.com"
  checkInvalidMailbox "this\\ still\\\"not\\\\allowed@example.com"
  -- TODO: Add mailbox validation
  -- checkInvalidMailbox "1234567890123456789012345678901234567890123456789012345678901234+x@example.com"
  checkInvalidMailbox "john..doe@example.com"
  checkInvalidMailbox "john.doe@example..com"

  -- Internationalization examples
  -- Taken from https://en.wikipedia.org/wiki/Email_address
  checkMailbox "Pelé@example.com"                -- Latin alphabet (with diacritics)
  checkMailbox "δοκιμή@παράδειγμα.δοκιμή"        -- Greek alphabet
  checkMailbox "我買@屋企.香港"                  -- Traditional Chinese characters
  checkMailbox "甲斐@黒川.日本"                  -- Japanese characters
  checkMailbox "чебурашка@ящик-с-апельсинами.рф" -- Cyrillic characters

  -- Named Addresses
  checkMailbox "<user@example.com>"
  checkMailbox "User<user@example.com>"
  checkMailbox "Sample User <user@example.com>"
  -- TODO: Add obs-phrase to display name
  -- checkMailbox "A. B. C. <user@example.com>"
  checkMailbox "\"Sample User\" <user@example.com>"

  -- Comments
  checkMailbox "(comment) Sample User <user@example.com>"
  checkMailbox "Sample User (comment) <user@example.com>"
  checkMailbox "Sample User <user@example.com> (comment)"
  checkMailbox "(missing display name) <user@example.com>"
  checkMailbox "(multipule)(comments) Sample User <user@example.com>"
  checkMailbox "((nested)comments) Sample User <user@example.com>"

  -- Domain Literals
  checkMailbox "user@[1.2.3.4]"
  checkMailbox "user@[IPv6:2001:DB8::1]"

checkMailbox :: String -> SpecWith ()
checkMailbox raw = context raw $ do
  let parsed = parseMessage raw :: Either ParseError Mailbox
      formatted = formatMessage <$> parsed
      reParsed = formatted >>= parseMessage
      reFormatted = formatMessage <$> reParsed
  it "parsing should return a valid result" $
    parsed `shouldSatisfy` isRight
  it "parsing should be idempotent" $ do
    parsed `shouldBe` reParsed
    formatted `shouldBe` reFormatted

checkInvalidMailbox :: String -> SpecWith ()
checkInvalidMailbox raw = context raw $ do
  let parsed = parseMessage raw :: Either ParseError Mailbox
  it "should return an error" $
    parsed `shouldSatisfy` isLeft
