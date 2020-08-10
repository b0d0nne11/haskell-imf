{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent    (forkIO)
import Data.Configurator     (Worth (..), load, subconfig)
import System.Log.FastLogger (LogType' (..), newFastLogger)

import MailBin.API
import MailBin.DB
import MailBin.MTA

main :: IO ()
main = do
    (logger, _) <- newFastLogger $ LogStdout 0
    config <- load [Optional "mailbin.cfg"]
    dbPool <- setupDB $ subconfig "db" config
    _ <- forkIO $ runMTA (logger . ("[mailbin.mta] " <>)) (subconfig "mta" config) dbPool
    runAPI (logger . ("[mailbin.api] " <>)) (subconfig "api" config) dbPool
