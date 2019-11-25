{-# LANGUAGE OverloadedStrings #-}

module Data.IMF.Parsers.MailboxSpec where

import           Data.Attoparsec.Text           ( parseOnly
                                                , endOfInput
                                                )
import           Data.Either                    ( isLeft )
import           Data.Foldable                  ( for_ )
import qualified Data.Text                     as T
import           Test.Hspec

import           Data.IMF.Types
import           Data.IMF.Parsers.Mailbox

spec :: Spec
spec = do
    let parse = parseOnly $ pDomainLiteral <* endOfInput
    describe "domain literal" $ it "parses" $ do
        parse "[1.2.3.4]" `shouldBe` Right "[1.2.3.4]"
        parse "[ 1 . 2 . 3 . 4 ]" `shouldBe` Right "[ 1 . 2 . 3 . 4 ]"
        parse " [1.2.3.4]\r\n " `shouldBe` Right "[1.2.3.4]"

    let parse = parseOnly $ pMailbox <* endOfInput
    for_ fixtures $ \(input, output) -> case output of
        Just expected -> do
            describe ("valid mailbox " ++ T.unpack input) $ do
                let parsed = parse input
                it "should parse" $ parsed `shouldBe` Right expected
                let reparsed = parse . format =<< parsed
                it "parsing should be idempotent" $ reparsed `shouldBe` parsed
        Nothing -> do
            describe ("invalid mailbox " ++ T.unpack input) $
                it "should not parse" $ parse input `shouldSatisfy` isLeft

fixtures :: [(T.Text, Maybe Mailbox)]
fixtures =
  -- Taken from https://en.wikipedia.org/wiki/Email_address
  [ -- valid email addresses
    ("simple@example.com", Just $ Mailbox "" "simple" "example.com")
  , ("very.common@example.com", Just $ Mailbox "" "very.common" "example.com" )
  , ("disposable.style.email.with+symbol@example.com", Just $ Mailbox "" "disposable.style.email.with+symbol" "example.com" )
  , ("other.email-with-hyphen@example.com", Just $ Mailbox "" "other.email-with-hyphen" "example.com" )
  , ("fully-qualified-domain@example.com", Just $ Mailbox "" "fully-qualified-domain" "example.com" )
  , ("user.name+tag+sorting@example.com", Just $ Mailbox "" "user.name+tag+sorting" "example.com" )
  , ("x@example.com", Just $ Mailbox "" "x" "example.com" )
  , ("example-indeed@strange-example.com", Just $ Mailbox "" "example-indeed" "strange-example.com" )
  , ("admin@mailserver1", Just $ Mailbox "" "admin" "mailserver1" )
  , ("example@s.example", Just $ Mailbox "" "example" "s.example" )
  , ("\" \"@example.org", Just $ Mailbox "" "\" \"" "example.org" )
  , ("\"john..doe\"@example.org", Just $ Mailbox "" "\"john..doe\"" "example.org" )
  , ("mailhost!username@example.org", Just $ Mailbox "" "mailhost!username" "example.org" )
  , ("user%example.com@example.org", Just $ Mailbox "" "user%example.com" "example.org" )
    -- invalid email addresses
  , ("Abc.example.com", Nothing )
  , ("A@b@c@example.com", Nothing )
  , ("a\"b(c)d,e:f;g<h>i[j\\k]l@example.com", Nothing )
  , ("just\"not\"right@example.com", Nothing )
  , ("this is\"not\\allowed@example.com", Nothing )
  , ("this\\ still\\\"not\\\\allowed@example.com", Nothing )
    -- international
  , ("Pelé@example.com", Just $ Mailbox "" "Pelé" "example.com" )
  , ("медведь@с-балалайкой.рф", Just $ Mailbox "" "медведь" "с-балалайкой.рф" )
    -- consecutive dots
  , ("John..Doe@example.com", Nothing )
  , ("\"John..Doe\"@example.com", Just $ Mailbox "" "\"John..Doe\"" "example.com" )
    -- domain literals
  , ("jsmith@[192.168.2.1]", Just $ Mailbox "" "jsmith" "[192.168.2.1]" )
  , ("jsmith@[IPv6:2001:db8::1]", Just $ Mailbox "" "jsmith" "[IPv6:2001:db8::1]" )
    -- comments
  , ("john.smith@(comment)example.com", Just $ Mailbox "" "john.smith" "example.com" )
  , ("john.smith@example.com(comment)", Just $ Mailbox "" "john.smith" "example.com" )
    -- subaddressing
  , ("joeuser+tag@example.com", Just $ Mailbox "" "joeuser+tag" "example.com" )
  ]
