{-# LANGUAGE OverloadedStrings #-}

module Data.IMF.Network.DNS
  ( lookupMX
  , lookupA
  )
where

import           Control.Exception              ( throwIO )
import           Data.List                      ( sortOn )
import qualified Data.ByteString.Char8         as C
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( encodeUtf8 )
import           Network.Socket                 ( HostName )
import qualified Network.DNS.Lookup            as DNS
import qualified Network.DNS.Resolver          as DNS
import           System.IO.Unsafe               ( unsafePerformIO )

import           Data.IMF.Network.Errors

rseed :: DNS.ResolvSeed
rseed = unsafePerformIO $ DNS.makeResolvSeed DNS.defaultResolvConf

-- | Lookup MX records for a domain sorted by priority
lookupMX :: T.Text -> IO [HostName]
lookupMX "localhost" = return ["localhost"]
lookupMX domain = DNS.withResolver rseed $ \resolver -> do
    records <- DNS.lookupMX resolver $ encodeUtf8 domain
    case records of
        Left  _  -> throwIO DNSLookupFailed
        Right [] -> return [T.unpack domain] -- return implicit record
        Right rs -> return $ map (C.unpack . fst) $ sortOn snd rs

-- | Lookup a single A record for a domain
lookupA :: HostName -> IO HostName
lookupA "localhost" = return "127.0.0.1"
lookupA host = DNS.withResolver rseed $ \resolver -> do
    records <- DNS.lookupA resolver $ C.pack host
    case records of
        Left  _     -> throwIO DNSLookupFailed
        Right []    -> throwIO DNSLookupNotFound
        Right (r:_) -> return $ show r
