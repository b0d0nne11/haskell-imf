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
                                                 getSocketName, socket)
import qualified Network.Socket                 as Socket hiding (recv, sendAll)
import qualified Network.Socket.ByteString      as Socket (recv)
import qualified Network.Socket.ByteString.Lazy as Socket (sendAll)
import qualified Network.TLS                    as TLS
import qualified Network.TLS.Extra.Cipher       as TLS
import           System.X509                    (getSystemCertificateStore)

import           Text.IMF.Network.Errors        (ClientException (..))

data Connection = Plain String String Socket
                | TLS String String TLS.Context
                | Closed String String

instance Show Connection where
    show (Plain src dst _) = src ++ "<->" ++ dst
    show (TLS src dst _)   = src ++ "<~>" ++ dst
    show (Closed src dst)  = src ++ "<x>" ++ dst

getSocket :: IO Socket
getSocket = socket AF_INET Stream defaultProtocol

hints :: AddrInfo
hints = defaultHints
    { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV]
    , addrFamily = AF_INET
    , addrSocketType = Stream
    }

-- | Open a new connection
open :: Maybe String -- ^ source ip address
     -> String       -- ^ destination ip address
     -> String       -- ^ destination port
     -> IO Connection
open Nothing dstIP port =
    handle (throwIO . ConnectFailure) $ do
        dstAddr <- addrAddress . head <$> getAddrInfo (Just hints) (Just dstIP) (Just port)
        sock <- getSocket
        connect sock dstAddr
        srcAddr <- getSocketName sock
        return $ Plain (show srcAddr) (show dstAddr) sock
open (Just srcIP) dstIP port =
    handle (throwIO . ConnectFailure) $ do
        srcAddr <- addrAddress . head <$> getAddrInfo (Just hints) (Just srcIP) (Just "0")
        dstAddr <- addrAddress . head <$> getAddrInfo (Just hints) (Just dstIP) (Just port)
        sock <- getSocket
        bind sock srcAddr
        connect sock dstAddr
        return $ Plain (show srcAddr) (show dstAddr) sock

-- | Close a connection
close :: Connection -> IO Connection
close (Plain src dst sock) =
    handle (throwIO . SocketFailure) $ do
        Socket.close sock
        return $ Closed src dst
close (TLS src dst ctx) =
    handle (throwIO . TLSSocketFailure) $ do
        TLS.bye ctx
        TLS.contextClose ctx
        return $ Closed src dst
close conn@(Closed _ _) = return conn

-- | Upgrade a plain connection to a TLS connection
negotiateTLS :: Connection
             -> String     -- ^ hostname
             -> Bool       -- ^ validate certificate?
             -> IO Connection
negotiateTLS (Plain src dst sock) hostname isValidated =
    handle (throwIO . TLSNegotiationFailure) $ do
        params <- getParams
        ctx <- TLS.contextNew sock params
        TLS.handshake ctx
        return $ TLS src dst ctx
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
negotiateTLS (TLS src dst ctx) _ _ =
    handle (throwIO . TLSNegotiationFailure) $ do
        TLS.handshake ctx
        return $ TLS src dst ctx
negotiateTLS (Closed _ _) _ _ = fail "cannot negotiate TLS on a closed connection"

-- | Is this a TLS connection?
isTLS :: Connection -> Bool
isTLS (TLS _ _ _) = True
isTLS _           = False

-- | Send over a connection
send :: Connection -> LB.ByteString -> IO ()
send (Plain _ _ sock) msg = Socket.sendAll sock msg `catch` (throwIO . SocketFailure)
send (TLS _ _ ctx) msg    = TLS.sendData ctx msg `catch` (throwIO . TLSSocketFailure)
send (Closed _ _) _       = fail "cannot send on a closed connection"

-- | Receive over a connection
recv :: Connection -> IO ByteString
recv (Plain _ _ sock) = Socket.recv sock 4096 `catch` (throwIO . SocketFailure)
recv (TLS _ _ ctx)    = TLS.recvData ctx `catch` (throwIO . TLSSocketFailure)
recv (Closed _ _)     = fail "cannot receive on a closed connection"

