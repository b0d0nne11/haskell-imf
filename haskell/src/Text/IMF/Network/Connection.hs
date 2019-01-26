{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Text.IMF.Network.Connection
  ( Connection
  , open
  , close
  , negotiateTLS
  , isTLS
  , send
  , recv
  )
where

import           Data.ByteString                 ( ByteString )
import qualified Data.ByteString.Lazy           as LB
import           Data.Default.Class              ( def )
import           Network.Socket                  ( AddrInfo(..)
                                                 , SocketType(..)
                                                 , Socket
                                                 , getAddrInfo
                                                 , socket
                                                 , connect
                                                 , defaultHints
                                                 )
import qualified Network.Socket                 as Socket hiding (sendAll, recv)
import qualified Network.Socket.ByteString      as Socket (recv)
import qualified Network.Socket.ByteString.Lazy as Socket (sendAll)
import qualified Network.TLS                    as TLS
import qualified Network.TLS.Extra.Cipher       as TLS

data Connection = Plain Socket | TLS TLS.Context

instance Show Connection where
    show (Plain sock) = show sock
    show (TLS ctx) = "TLS"

instance Eq Connection where
    (Plain sock1) == (Plain sock2) = sock1 == sock2
    (TLS _) == (TLS _) = True
    _ == _ = False

tlsParams :: String -> Bool -> TLS.ClientParams
tlsParams hostname reqValidate =
    (TLS.defaultParamsClient hostname "")
        { TLS.clientShared = def { TLS.sharedValidationCache = validationCache }
        , TLS.clientSupported = def { TLS.supportedCiphers = TLS.ciphersuite_default }
        }
  where
    validationCache
      | reqValidate = def
      | otherwise   = TLS.ValidationCache (\_ _ _ -> return TLS.ValidationCachePass)
                                          (\_ _ _ -> return ())

-- | Open a new connection
open :: String -- ^ ip address
     -> String -- ^ port
     -> IO Connection
open ip port = do
    let hints = defaultHints { addrSocketType = Stream }
    addr:_ <- getAddrInfo (Just hints) (Just ip) (Just port)
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    connect sock $ addrAddress addr
    return $ Plain sock

-- | Close a connection
close :: Connection -> IO ()
close (Plain sock) = Socket.close sock
close (TLS ctx) = TLS.bye ctx >> TLS.contextClose ctx

negotiateTLS :: Connection -> String -> Bool -> IO Connection
negotiateTLS (Plain sock) hostname reqValidate = do
    ctx <- TLS.contextNew sock $ tlsParams hostname reqValidate
    TLS.handshake ctx
    return $ TLS ctx
negotiateTLS (TLS ctx) _ _ =
    return $ TLS ctx

isTLS :: Connection -> Bool
isTLS (Plain _) = False
isTLS (TLS _)   = True

send :: Connection -> LB.ByteString -> IO ()
send (Plain sock) msg = Socket.sendAll sock msg
send (TLS ctx) msg    = TLS.sendData ctx msg

recv :: Connection -> IO ByteString
recv (Plain sock) = Socket.recv sock 4096
recv (TLS ctx)    = TLS.recvData ctx

