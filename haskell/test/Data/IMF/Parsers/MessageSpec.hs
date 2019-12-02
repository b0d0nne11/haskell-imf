{-# LANGUAGE OverloadedStrings #-}

module Data.IMF.Parsers.MessageSpec where

import           Data.Attoparsec.Text           ( parseOnly
                                                , endOfInput
                                                )
import           Data.Either                    ( isRight )
import           Data.Foldable                  ( for_ )
import           Data.List                      ( sort )
import qualified Data.Text                     as T
import           System.Directory               ( listDirectory )
import           System.FilePath                ( (</>) )
import           Test.Hspec

import           Data.IMF.Parsers.Message

fixturesDir :: FilePath
fixturesDir = "haskell/test/Fixtures/Messages"

spec :: Spec
spec = do
    let parse = parseOnly $ pMessage <* endOfInput
    files <- sort <$> runIO (listDirectory fixturesDir)
    for_ files $ \file -> do
        input <- runIO $ readFile $ fixturesDir </> file
        describe ("valid message " ++ fixturesDir </> file) $ do
            let parsed = parse $ T.replace "\n" "\r\n" $ T.pack input
            it "should parse" $ parsed `shouldSatisfy` isRight
