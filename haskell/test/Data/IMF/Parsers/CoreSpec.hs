{-# LANGUAGE OverloadedStrings #-}

module Data.IMF.Parsers.CoreSpec where

import           Data.Attoparsec.Text           ( parseOnly
                                                , endOfInput
                                                )
import           Test.Hspec

import           Data.IMF.Parsers.Core

spec :: Spec
spec = do
    let parse = parseOnly $ pFWS <* endOfInput
    describe "folding whitespace" $ it "parses" $ do
        parse " " `shouldBe` Right " "
        parse "\t" `shouldBe` Right "\t"
        parse " \t " `shouldBe` Right " \t "
        parse "\r\n " `shouldBe` Right "\r\n "
        parse " \r\n " `shouldBe` Right " \r\n "

    let parse = parseOnly $ pComment <* endOfInput
    describe "comment" $ it "parses" $ do
        parse "(comment)" `shouldBe` Right "(comment)"
        parse "( foo bar baz )" `shouldBe` Right "( foo bar baz )"
        parse "((nested)comment)" `shouldBe` Right "((nested)comment)"
        parse "(\\comment)" `shouldBe` Right "(\\comment)"

    let parse = parseOnly $ pCFWS <* endOfInput
    describe "comment or folding whitespace" $ it "parses" $ do
        parse " " `shouldBe` Right " "
        parse "(comment)" `shouldBe` Right "(comment)"
        parse " (comment)\r\n " `shouldBe` Right " (comment)\r\n "

    let parse = parseOnly $ pAtom <* endOfInput
    describe "atom" $ it "parses" $ do
        parse "example" `shouldBe` Right "example"
        parse " example\r\n " `shouldBe` Right "example"

    let parse = parseOnly $ pDotAtom <* endOfInput
    describe "dot-atom" $ it "parses" $ do
        parse "mx1.example.com" `shouldBe` Right "mx1.example.com"
        parse " mx1.example.com\r\n " `shouldBe` Right "mx1.example.com"

    let parse = parseOnly $ pQuotedString <* endOfInput
    describe "quote" $ it "parses" $ do
        parse "\"quote\"" `shouldBe` Right "\"quote\""
        parse "\" foo bar baz \"" `shouldBe` Right "\" foo bar baz \""
        parse "\"\\\"nested\\\"quote\"" `shouldBe` Right "\"\\\"nested\\\"quote\""
        parse " \"quote\"\r\n " `shouldBe` Right "\"quote\""
