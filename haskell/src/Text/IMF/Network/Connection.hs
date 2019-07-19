{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

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

import           Control.Exception              (catch, handle, throwIO)
import           Data.ByteString                (ByteString)
import qualified Data.ByteString.Lazy           as LB
import           Data.Default.Class             (def)
import           Network.Socket                 (AddrInfo (..),
                                                 AddrInfoFlag (..), Family (..),
                                                 Socket, SocketType (..), bind,
                                                 connect, defaultHints,
                                                 defaultProtocol, getAddrInfo,
                                                 socket)
import qualified Network.Socket                 as Socket hiding (recv, sendAll)
import qualified Network.Socket.ByteString      as Socket (recv)
import qualified Network.Socket.ByteString.Lazy as Socket (sendAll)
import qualified Network.TLS                    as TLS
import qualified Network.TLS.Extra.Cipher       as TLS
import           System.X509                    (getSystemCertificateStore)

import           Text.IMF.Network.Errors        (ClientException (..))

data Connection = Plain Socket | TLS Socket TLS.Context

instance Show Connection where
    show (Plain _) = "connection"
    show (TLS _ _) = "tls connection"

instance Eq Connection where
    (Plain sock1) == (Plain sock2) = sock1 == sock2
    (TLS sock1 _) == (TLS sock2 _) = sock1 == sock2
    _ == _ = False

-- | Open a new connection
open :: Maybe String -- ^ source ip address
     -> String       -- ^ destination ip address
     -> String       -- ^ destination port
     -> IO Connection
open Nothing dstIP port =
    handle (throwIO . ConnectFailure) $ do
        dstAddr:_ <- getAddrInfo hints (Just dstIP) (Just port)
        sock <- getSocket
        connect sock $ addrAddress dstAddr
        return $ Plain sock
  where
    getSocket = socket AF_INET Stream defaultProtocol
    hints = Just $ defaultHints
        { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV]
        , addrFamily = AF_INET
        , addrSocketType = Stream
        }
open (Just srcIP) dstIP port =
    handle (throwIO . ConnectFailure) $ do
        srcAddr:_ <- getAddrInfo hints (Just srcIP) (Just "0")
        dstAddr:_ <- getAddrInfo hints (Just dstIP) (Just port)
        sock <- getSocket
        bind sock $ addrAddress srcAddr
        connect sock $ addrAddress dstAddr
        return $ Plain sock
  where
    getSocket = socket AF_INET Stream defaultProtocol
    hints = Just $ defaultHints
        { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV]
        , addrFamily = AF_INET
        , addrSocketType = Stream
        }

-- | Close a connection
close :: Connection -> IO ()
close (Plain sock) =
    handle (throwIO . SocketFailure) $
        Socket.close sock
close (TLS _ ctx) =
    handle (throwIO . TLSSocketFailure) $ do
        TLS.bye ctx
        TLS.contextClose ctx

negotiateTLS :: Connection
             -> String     -- ^ hostname
             -> Bool       -- ^ validate certificate?
             -> IO Connection
negotiateTLS (Plain sock) hostname isValidated =
    handle (throwIO . TLSNegotiationFailure) $ do
        params <- getParams
        ctx <- TLS.contextNew sock params
        TLS.handshake ctx
        return $ TLS sock ctx
  where
    getParams = do
        caStore <- getSystemCertificateStore
        return $ defaultParams
            { TLS.clientShared = def
                { TLS.sharedCAStore = caStore
                , TLS.sharedValidationCache = validationCache
                }
            , TLS.clientSupported = def
                { TLS.supportedCiphers = ciphers
                }
            }
    defaultParams = TLS.defaultParamsClient hostname ""
    validationCache
        | isValidated = def
        | otherwise   = TLS.ValidationCache (\_ _ _ -> return TLS.ValidationCachePass)
                                            (\_ _ _ -> return ())
    ciphers = TLS.ciphersuite_default
negotiateTLS (TLS sock ctx) _ _ =
    handle (throwIO . TLSNegotiationFailure) $ do
        TLS.handshake ctx
        return $ TLS sock ctx

isTLS :: Connection -> Bool
isTLS (Plain _) = False
isTLS (TLS _ _) = True

send :: Connection -> LB.ByteString -> IO ()
send (Plain sock) msg = Socket.sendAll sock msg `catch` (throwIO . SocketFailure)
send (TLS _ ctx) msg  = TLS.sendData ctx msg `catch` (throwIO . TLSSocketFailure)

recv :: Connection -> IO ByteString
recv (Plain sock) = Socket.recv sock 4096 `catch` (throwIO . SocketFailure)
recv (TLS _ ctx)  = TLS.recvData ctx `catch` (throwIO . TLSSocketFailure)

