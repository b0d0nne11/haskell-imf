module Main where

import           Control.Monad (forever)
import           System.IO     (BufferMode (..), hSetBuffering, stdout)
import           Text.Parsec   (parseTest)

import           Text.IMF.Mailbox

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    forever $ do
        putStr "Enter email address: "
        input <- getLine
        putStr "\n"
        parseTest mailbox input
        putStr "\n"
