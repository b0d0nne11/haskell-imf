{-# LANGUAGE OverloadedStrings #-}

import           Data.Either      (isLeft)
import           Data.List        (intercalate)
import qualified Data.Text        as T
import           Test.HUnit
import           Text.Printf      (printf)

import           Text.IMF.Mailbox

assertMailbox :: T.Text -> Assertion
assertMailbox raw =
    either failMsg (\mbox -> assertEqual "" (Right mbox) (reparse mbox)) (parseMailbox raw)
  where
    failMsg = assertFailure . printf "failed to parse address: %v: %v" raw . show
    reparse = parseMailbox . T.pack . show

assertMailboxList :: T.Text -> Assertion
assertMailboxList raw =
    either failMsg (\mlist -> assertEqual "" (Right mlist) (reparse mlist)) (parseMailboxList raw)
  where
    failMsg = assertFailure . printf "failed to parse address list: %v: %v" raw . show
    reparse = parseMailboxList . T.pack . intercalate ", " . map show

assertInvalidMailbox :: T.Text -> Assertion
assertInvalidMailbox raw =
    assertBool failMsg (isLeft $ parseMailbox raw)
  where
    failMsg = printf "parsed invalid address: %v" raw

assertInvalidMailboxList :: T.Text -> Assertion
assertInvalidMailboxList raw =
    assertBool failMsg (isLeft $ parseMailboxList raw)
  where
    failMsg = printf "parsed invalid address list: %v" raw

testAddrSpec :: Test
testAddrSpec = TestCase $ do
    -- Examples taken from https://en.wikipedia.org/wiki/Email_address
    -- Valid email addresses
    assertMailbox "prettyandsimple@example.com"
    assertMailbox "very.common@example.com"
    assertMailbox "disposable.style.email.with+symbol@example.com"
    assertMailbox "other.email-with-dash@example.com"
    assertMailbox "x@example.com"
    assertMailbox "\"much.more unusual\"@example.com"
    assertMailbox "\"very.unusual.@.unusual.com\"@example.com"
    assertMailbox "\"very.(),:;<>[]\\\".VERY.\\\"very@\\\\ \\\"very\\\".unusual\"@strange.example.com"
    assertMailbox "example-indeed@strange-example.com"
    assertMailbox "admin@mailserver1"
    assertMailbox "#!$%&'*+-/=?^_`{}|~@example.org"
    assertMailbox "\"()<>[]:,;@\\\"!#$%&'-/=?^_`{}| ~.a\"@example.org"
    assertMailbox "\" \"@example.org"
    assertMailbox "example@localhost"
    assertMailbox "example@s.solutions"
    assertMailbox "user@localserver"
    assertMailbox "user@tt"
    assertMailbox "user@[IPv6:2001:DB8::1]"
    -- Invalid email addresses
    assertInvalidMailbox "Abc.example.com"
    assertInvalidMailbox "A@b@c@example.com"
    assertInvalidMailbox "a\"b(c)d,e:f;g<h>i[j\\k]l@example.com"
    assertInvalidMailbox "just\"not\"right@example.com"
    assertInvalidMailbox "this is\"not\\allowed@example.com"
    assertInvalidMailbox "this\\ still\\\"not\\\\allowed@example.com"
    assertInvalidMailbox "1234567890123456789012345678901234567890123456789012345678901234+x@example.com"
    assertInvalidMailbox "john..doe@example.com"
    assertInvalidMailbox "john.doe@example..com"
    -- Internationalization examples
    assertMailbox "Pelé@example.com"                -- Latin alphabet (with diacritics)
    assertMailbox "δοκιμή@παράδειγμα.δοκιμή"        -- Greek alphabet
    assertMailbox "我買@屋企.香港"                  -- Traditional Chinese characters
    assertMailbox "甲斐@黒川.日本"                  -- Japanese characters
    assertMailbox "чебурашка@ящик-с-апельсинами.рф" -- Cyrillic characters

testNameAddr :: Test
testNameAddr = TestCase $ do
    -- Valid email addresses
    assertMailbox "<user@example.com>"
    assertMailbox "User<user@example.com>"
    assertMailbox "Sample User <user@example.com>"
    assertMailbox "A. B. C. <user@example.com>"
    assertMailbox "\"Sample User!\" <user@example.com>"
    assertMailbox "\"\\\"\" <user@example.com>"
    assertMailbox "\"\\\\\" <user@example.com>"
    -- Invalid email addresses
    assertInvalidMailbox "Sample User user@example.com"
    assertInvalidMailbox "\"\"\" <user@example.com>"
    assertInvalidMailbox "\"\\\" <user@example.com>"

testComment :: Test
testComment = TestCase $ do
    -- Valid email addresses
    assertMailbox "(comment) Sample User <user@example.com>"
    assertMailbox "Sample User (comment) <user@example.com>"
    assertMailbox "Sample User <(comment) user@example.com>"
    assertMailbox "Sample User <user(comment) @example.com>"
    assertMailbox "Sample User <user (comment)@example.com>"
    assertMailbox "Sample User <user@(comment) example.com>"
    assertMailbox "Sample User <user@ (comment)example.com>"
    assertMailbox "Sample User <user@example.com (comment)>"
    assertMailbox "Sample User <user@example.com> (comment)"
    assertMailbox "(missing display name) <user@example.com>"
    assertMailbox "(multipule)(comments) Sample User <user@example.com>"
    assertMailbox "((nested)comments) Sample User <user@example.com>"
    assertMailbox "(\\() Sample User <user@example.com>"
    assertMailbox "(\\)) Sample User <user@example.com>"
    assertMailbox "(\\\\) Sample User <user@example.com>"
    -- Invalid email addresses
    assertInvalidMailbox "(() Sample User <user@example.com>"
    assertInvalidMailbox "()) Sample User <user@example.com>"
    assertInvalidMailbox "(\\) Sample User <user@example.com>"

testDomainLiteral :: Test
testDomainLiteral = TestCase $ do
    -- Valid email addresses
    assertMailbox "user@[1.2.3.4]"
    assertMailbox "user@[IPv6:2001:DB8::1]"
    -- Invalid email addresses
    assertInvalidMailbox "user@[\\[]"
    assertInvalidMailbox "user@[\\]]"
    assertInvalidMailbox "user@[\\\\]"
    assertInvalidMailbox "user@[[]"
    assertInvalidMailbox "user@[]]"
    assertInvalidMailbox "user@[\\]"

testList :: Test
testList = TestCase $ do
    -- Valid email addresses list
    assertMailboxList "user@example.com, user@example.com, user@example.com"
    assertMailboxList "user@example.com, \"Sample User!\" <user@example.com>, Sample User <user@example.com>"
    -- Invalid email addresses list
    assertInvalidMailboxList "user@example.com, user@example.com, user@example.com, "
    assertInvalidMailboxList "user@example.com, , user@example.com, user@example.com"

tests :: Test
tests = TestList [testAddrSpec, testNameAddr, testComment, testDomainLiteral, testList]

main :: IO ()
main = do
  _ <- runTestTT tests
  return ()
