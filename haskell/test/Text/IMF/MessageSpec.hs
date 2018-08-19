{-# LANGUAGE OverloadedStrings #-}

module Text.IMF.MessageSpec where

import           Data.Either                    ( isRight )
import           Data.Foldable                  ( for_ )
import           Data.List                      ( sort )
import           System.Directory               ( listDirectory )
import           System.FilePath                ( (</>) )
import           Test.Hspec

import           Text.IMF

fixturesDir :: FilePath
fixturesDir = "haskell/test/Fixtures/Messages"

spec :: Spec
spec = do
  files <- sort <$> runIO (listDirectory fixturesDir)
  for_ files $ \file -> context ("given " ++ fixturesDir </> file) $ do
    input <- runIO $ readFile $ fixturesDir </> file
    let parsed = parseMessage input :: Either ParseError Message
    it "should parse" $ parsed `shouldSatisfy` isRight
