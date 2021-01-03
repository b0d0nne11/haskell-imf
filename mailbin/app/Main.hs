{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent    (newEmptyMVar, putMVar, takeMVar)
import System.Log.FastLogger (LogType' (..), withFastLogger)
import System.Posix.Signals  (Handler (..), Signal, installHandler, sigINT)

import MailBin.API
import MailBin.Config
import MailBin.DB
import MailBin.MTA

main :: IO ()
main =  withFastLogger (LogStdout 4096) $ \logger -> do
    config <- loadConfig
    dbPool <- setupDB $ subconfig "db" config
    closeMTA <- runMTA (subconfig "mta" config) logger dbPool
    closeAPI <- runAPI (subconfig "api" config) logger dbPool
    logger "ready\n"
    waitFor sigINT
    logger "caught interrupt signal, shutting down\n"
    closeAPI
    closeMTA
    closeDB dbPool
    logger "done\n"

waitFor :: Signal -> IO ()
waitFor sig = do
    done <- newEmptyMVar
    installHandler sig (CatchOnce $ putMVar done ()) Nothing
    takeMVar done
