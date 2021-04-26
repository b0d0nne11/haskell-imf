{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import Control.Concurrent     (newEmptyMVar, putMVar, takeMVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger   (logInfo, runStdoutLoggingT)
import System.Posix.Signals   (Handler (..), Signal, installHandler, sigINT)

import MailBin.API
import MailBin.Config
import MailBin.DB
import MailBin.MTA

main :: IO ()
main =  runStdoutLoggingT $ do
    config <- liftIO loadConfig
    dbPool <- setupDB $ subconfig "db" config
    closeMTA <- runMTA (subconfig "mta" config) dbPool
    closeAPI <- runAPI (subconfig "api" config) dbPool
    $(logInfo) "ready"
    liftIO $ waitFor sigINT
    $(logInfo) "caught interrupt signal, shutting down"
    closeAPI
    closeMTA
    closeDB dbPool
    $(logInfo) "done"

waitFor :: Signal -> IO ()
waitFor sig = do
    done <- newEmptyMVar
    installHandler sig (CatchOnce $ putMVar done ()) Nothing
    takeMVar done
