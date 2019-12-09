{-# LANGUAGE OverloadedStrings #-}

module Test.Core where

import Data.Attoparsec.Text  (endOfInput, parseOnly)
import Test.Tasty
import Test.Tasty.HUnit

import Data.IMF.Parsers.Core

tests :: TestTree
tests = testGroup "core parsers"
    [ testFWS
    , testComment
    , testCFWS
    , testAtom
    , testDotAtom
    , testQuote
    ]

testFWS = testCase "folding whitespace" $ do
    parse " "      @?= Right " "
    parse "\t"     @?= Right "\t"
    parse " \t "   @?= Right " \t "
    parse "\r\n "  @?= Right "\r\n "
    parse " \r\n " @?= Right " \r\n "
  where
    parse = parseOnly $ pFWS <* endOfInput

testComment = testCase "comment" $ do
    parse "(comment)"         @?= Right "(comment)"
    parse "( foo bar baz )"   @?= Right "( foo bar baz )"
    parse "((nested)comment)" @?= Right "((nested)comment)"
    parse "(\\comment)"       @?= Right "(\\comment)"
  where
    parse = parseOnly $ pComment <* endOfInput

testCFWS = testCase "comment or folding whitespace" $ do
    parse " "               @?= Right " "
    parse "(comment)"       @?= Right "(comment)"
    parse " (comment)\r\n " @?= Right " (comment)\r\n "
  where
    parse = parseOnly $ pCFWS <* endOfInput

testAtom = testCase "atom" $ do
    parse "example"       @?= Right "example"
    parse " example\r\n " @?= Right "example"
  where
    parse = parseOnly $ pAtom <* endOfInput

testDotAtom = testCase "dot-atom" $ do
    parse "mx1.example.com"       @?= Right "mx1.example.com"
    parse " mx1.example.com\r\n " @?= Right "mx1.example.com"
  where
    parse = parseOnly $ pDotAtom <* endOfInput

testQuote = testCase "quoted string" $ do
    parse "\"quote\""               @?= Right "\"quote\""
    parse "\" foo bar baz \""       @?= Right "\" foo bar baz \""
    parse "\"\\\"nested\\\"quote\"" @?= Right "\"\\\"nested\\\"quote\""
    parse " \"quote\"\r\n "         @?= Right "\"quote\""
  where
    parse = parseOnly $ pQuotedString <* endOfInput
