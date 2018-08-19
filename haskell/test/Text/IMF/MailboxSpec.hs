{-# LANGUAGE OverloadedStrings #-}

module Text.IMF.MailboxSpec where

import           Data.Either                    ( isLeft )
import           Test.Hspec
import           Data.Foldable                  ( for_ )

import           Text.IMF

spec :: Spec
spec = for_ fixtures $ \(input, mExpected) ->
  context ("given mailbox " ++ input) $ case mExpected of
    Just expected -> do
      let parsed = parseMessage input :: Either ParseError Mailbox
      it "should parse" $ parsed `shouldBe` Right expected
      let validated = parsed >>= validateMessage
      it "parsing should be idempotent" $ parsed `shouldBe` validated
    Nothing -> do
      let parsed = parseMessage input :: Either ParseError Mailbox
      it "should not parse" $ parsed `shouldSatisfy` isLeft

fixtures :: [(String, Maybe Mailbox)]
fixtures =
    -- Generic examples
    -- Taken from https://en.wikipedia.org/wiki/Email_address
  [ ("prettyandsimple@example.com", Just $ Mailbox "" "prettyandsimple" "example.com")
  , ("very.common@example.com"    , Just $ Mailbox "" "very.common" "example.com")
  , ( "disposable.style.email.with+symbol@example.com"
    , Just $ Mailbox "" "disposable.style.email.with+symbol" "example.com"
    )
  , ( "other.email-with-dash@example.com"
    , Just $ Mailbox "" "other.email-with-dash" "example.com"
    )
  , ("x@example.com", Just $ Mailbox "" "x" "example.com")
  , ( "\"much.more unusual\"@example.com"
    , Just $ Mailbox "" "\"much.more unusual\"" "example.com"
    )
  , ( "\"very.unusual.@.unusual.com\"@example.com"
    , Just $ Mailbox "" "\"very.unusual.@.unusual.com\"" "example.com"
    )
  , ( "\"very.(),:;<>[]\\\".VERY.\\\"very@\\\\ \\\"very\\\".unusual\"@strange.example.com"
    , Just $ Mailbox ""
                     "\"very.(),:;<>[]\\\".VERY.\\\"very@\\\\ \\\"very\\\".unusual\""
                     "strange.example.com"
    )
  , ( "example-indeed@strange-example.com"
    , Just $ Mailbox "" "example-indeed" "strange-example.com"
    )
  , ("admin@mailserver1", Just $ Mailbox "" "admin" "mailserver1")
  , ( "#!$%&'*+-/=?^_`{}|~@example.org"
    , Just $ Mailbox "" "#!$%&'*+-/=?^_`{}|~" "example.org"
    )
  , ( "\"()<>[]:,;@\\\"!#$%&'-/=?^_`{}| ~.a\"@example.org"
    , Just $ Mailbox "" "\"()<>[]:,;@\\\"!#$%&'-/=?^_`{}| ~.a\"" "example.org"
    )
  , ("\" \"@example.org", Just $ Mailbox "" "\" \"" "example.org")
  , ("example@localhost", Just $ Mailbox "" "example" "localhost")
  , ("example@s.solutions", Just $ Mailbox "" "example" "s.solutions")
  , ("user@localserver", Just $ Mailbox "" "user" "localserver")
  , ("user@tt"                              , Just $ Mailbox "" "user" "tt")
  , ("user@[IPv6:2001:DB8::1]", Just $ Mailbox "" "user" "[IPv6:2001:DB8::1]")
  , ("Abc.example.com"                      , Nothing)
  , ("A@b@c@example.com"                    , Nothing)
  , ("a\"b(c)d,e:f;g<h>i[j\\k]l@example.com", Nothing)
  , ("just\"not\"right@example.com"         , Nothing)
  , ("this is\"not\\allowed@example.com"    , Nothing)
  , ( "this\\ still\\\"not\\\\allowed@example.com"
    , Nothing
    )
    -- TODO: Add mailbox validation
    -- , ("1234567890123456789012345678901234567890123456789012345678901234+x@example.com", Nothing)
  , ("john..doe@example.com", Nothing)
  , ( "john.doe@example..com"
    , Nothing
    )
    -- Internationalization examples
    -- Taken from https://en.wikipedia.org/wiki/Email_address
  , ("Pelé@example.com"        , Just $ Mailbox "" "Pelé" "example.com")
  , ("δοκιμή@παράδειγμα.δοκιμή", Just $ Mailbox "" "δοκιμή" "παράδειγμα.δοκιμή")
  , ("我買@屋企.香港"                , Just $ Mailbox "" "我買" "屋企.香港")
  , ("甲斐@黒川.日本"                , Just $ Mailbox "" "甲斐" "黒川.日本")
  , ( "чебурашка@ящик-с-апельсинами.рф"
    , Just $ Mailbox "" "чебурашка" "ящик-с-апельсинами.рф"
    )
    -- Named Addresses
  , ("<user@example.com>"    , Just $ Mailbox "" "user" "example.com")
  , ("User<user@example.com>", Just $ Mailbox "User" "user" "example.com")
  , ( "Sample User <user@example.com>"
    , Just $ Mailbox "Sample User" "user" "example.com"
    )
    -- TODO: Add obs-phrase to display name
    -- , ("A. B. C. <user@example.com>", Just $ Mailbox "A. B. C." "user" "example.com")
  , ( "\"Sample User\" <user@example.com>"
    , Just $ Mailbox "\"Sample User\"" "user" "example.com"
    )
    -- Comments
  , ( "(comment) Sample User <user@example.com>"
    , Just $ Mailbox "Sample User" "user" "example.com"
    )
  , ( "Sample User (comment) <user@example.com>"
    , Just $ Mailbox "Sample User" "user" "example.com"
    )
  , ( "Sample User <user@example.com> (comment)"
    , Just $ Mailbox "Sample User" "user" "example.com"
    )
  , ( "(missing display name) <user@example.com>"
    , Just $ Mailbox "" "user" "example.com"
    )
  , ( "(multipule)(comments) Sample User <user@example.com>"
    , Just $ Mailbox "Sample User" "user" "example.com"
    )
  , ( "((nested)comments) Sample User <user@example.com>"
    , Just $ Mailbox "Sample User" "user" "example.com"
    )
    -- Domain Literals
  , ("user@[1.2.3.4]"         , Just $ Mailbox "" "user" "[1.2.3.4]")
  , ("user@[IPv6:2001:DB8::1]", Just $ Mailbox "" "user" "[IPv6:2001:DB8::1]")
  ]
