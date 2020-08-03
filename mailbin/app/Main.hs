{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO)
import Data.Configurator  (Worth (..), load, subconfig)

import MailBin.API
import MailBin.DB
import MailBin.MTA

main :: IO ()
main = do
    config <- load [Optional "mailbin.cfg"]
    dbPool <- setupDB $ subconfig "db" config
    _ <- forkIO $ runMTA (subconfig "mta" config) dbPool
    runAPI (subconfig "api" config) dbPool
