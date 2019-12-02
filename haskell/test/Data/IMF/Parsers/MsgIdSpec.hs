{-# LANGUAGE OverloadedStrings #-}

module Data.IMF.Parsers.MsgIdSpec where

import           Data.Attoparsec.Text           ( parseOnly
                                                , endOfInput
                                                )
import           Test.Hspec

import           Data.IMF.Types
import           Data.IMF.Parsers.MsgId

spec :: Spec
spec = do
    let parse = parseOnly $ pMsgId <* endOfInput
    describe "message id" $ it "parses" $ do
        parse "<1234@example.com>" `shouldBe` Right (MsgId "1234" "example.com")
        parse " <1234@example.com>\r\n " `shouldBe` Right (MsgId "1234" "example.com")
        parse "<1234@[1.2.3.4]>" `shouldBe` Right (MsgId "1234" "[1.2.3.4]")
