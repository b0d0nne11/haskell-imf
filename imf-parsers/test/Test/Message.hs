{-# LANGUAGE OverloadedStrings #-}

module Test.Message where

import           Data.Attoparsec.Text     (endOfInput, parseOnly)
import           Data.Either              (isRight)
import           Data.Foldable            (for_)
import           Data.List                (sort)
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import           System.Directory         (listDirectory)
import           System.FilePath          ((</>))
import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.IMF.Parsers.Message

fixtures :: FilePath
fixtures = "./test/Fixtures/Messages"

tests :: TestTree
tests = testGroup "message parsers"
    [ testMessage
    ]

testMessage = testCase "message" $
    sort <$> listDirectory fixtures >>= \files -> for_ files $ \file -> do
        contents <- T.replace "\n" "\r\n" <$> T.readFile (fixtures </> file)
        isRight (parse contents) @? "message should parse"
  where
    parse = parseOnly $ pMessage <* endOfInput
