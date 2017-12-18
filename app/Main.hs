module Main where

import           Control.Monad (forever)
import           System.IO     (BufferMode (..), hSetBuffering, stdout)

import           Text.IMF

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    forever $ do
        putStr "Enter email address: "
        input <- getLine
        case parseMessage input :: Either ParseError Mailbox of
            Left err -> putStrLn $ "Error:\n" ++ show err
            Right parsed -> putStrLn $ "Parsed: " ++ show parsed
        putStr "\n"
