{-# LANGUAGE OverloadedStrings #-}

module Data.IMF.Network.Connection
  ( Connection
  , blank
  , connect
  , listen
  , accept
  , secure
  , secureServer
  , isSecure
  , send
  , recv
  , close
   -- * Re-exports
  , HostName
  , ServiceName
  )
where

import           Control.Monad.Catch            ( catch
                                                , handle
                                                , throwM
                                                )
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as LB
import           Data.Default.Class             ( def )
import           Network.Socket                 ( AddrInfo(..)
                                                , AddrInfoFlag(..)
                                                , Family(..)
                                                , HostName
                                                , ServiceName
                                                , SockAddr
                                                , Socket
                                                , SocketOption(..)
                                                , SocketType(..)
                                                )
import qualified Network.Socket                as Socket
                                         hiding ( recv )
import qualified Network.Socket.ByteString     as Socket
                                                ( recv )
import qualified Network.Socket.ByteString.Lazy
                                               as Socket
                                                ( sendAll )
import qualified Network.TLS                   as TLS
import qualified Network.TLS.Extra.Cipher      as TLS
import qualified Network.TLS.Extra.FFDHE       as TLS
import           System.X509                    ( getSystemCertificateStore )

import           Data.IMF.Network.Errors

data Connection = Listening SockAddr Socket
                | Connected SockAddr SockAddr Socket
                | Secured SockAddr SockAddr TLS.Context
                | Closed

instance Show Connection where
    show (Listening srcAddr _)         = show srcAddr
    show (Connected srcAddr dstAddr _) = show srcAddr ++ "<->" ++ show dstAddr
    show (Secured srcAddr dstAddr _)   = show srcAddr ++ "<~>" ++ show dstAddr
    show Closed                        = "closed"

getSocket :: IO Socket
getSocket = Socket.socket AF_INET Stream 0

hints :: AddrInfo
hints = Socket.defaultHints
    { addrFamily     = AF_INET
    , addrFlags      = [AI_NUMERICHOST, AI_NUMERICSERV]
    , addrSocketType = Stream
    }

-- | A blank connection, meant as a placeholder value
blank :: Connection
blank = Closed

-- | Open a new connection
connect :: (HostName, ServiceName) -- ^ source ip address and port
        -> (HostName, ServiceName) -- ^ destination ip address and port
        -> IO Connection
connect (srcIP, srcPort) (dstIP, dstPort) =
    handle (throwM . ConnectFailure) $ do
        srcAddr <- Socket.addrAddress . head <$> Socket.getAddrInfo (Just hints) (Just srcIP) (Just srcPort)
        dstAddr <- Socket.addrAddress . head <$> Socket.getAddrInfo (Just hints) (Just dstIP) (Just dstPort)
        sock <- getSocket
        Socket.bind sock srcAddr
        Socket.connect sock dstAddr
        return $ Connected srcAddr dstAddr sock

-- | Listen on a new connection
listen :: (HostName, ServiceName) -- ^ source ip address and port
       -> IO Connection
listen (srcIP, srcPort) =
    handle (throwM . ConnectFailure) $ do
        srcAddr <- Socket.addrAddress . head <$> Socket.getAddrInfo (Just hints) (Just srcIP) (Just srcPort)
        sock <- getSocket
        Socket.setSocketOption sock ReuseAddr 1
        Socket.bind sock srcAddr
        Socket.listen sock 1
        return $ Listening srcAddr sock

-- | Accept a connection
accept :: Connection -> IO Connection
accept (Listening srcAddr sock) =
    handle (throwM . ConnectFailure) $ do
        (sock', dstAddr) <- Socket.accept sock
        return $ Connected srcAddr dstAddr sock'
accept _ = throwM InvalidSocketOperation

-- | Upgrade a connection to a secure connection
secure :: Connection -- ^ connection
       -> HostName   -- ^ hostname
       -> Bool       -- ^ validate certificate?
       -> IO Connection
secure (Connected src dst sock) hostname isValidated =
    handle (throwM . TLSNegotiationFailure) $ do
        params <- getParams
        ctx <- TLS.contextNew sock params
        TLS.handshake ctx
        return $ Secured src dst ctx
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
secure (Secured src dst ctx) _ _ =
    handle (throwM . TLSNegotiationFailure) $ do
        TLS.handshake ctx
        return $ Secured src dst ctx
secure _ _ _ = throwM InvalidSocketOperation

-- | Upgrade a server connection to a secure connection
secureServer :: Connection -- ^ connection
             -> IO Connection
secureServer (Connected src dst sock) =
    handle (throwM . TLSNegotiationFailure) $ do
        params <- getParams
        ctx <- TLS.contextNew sock params
        TLS.handshake ctx
        return $ Secured src dst ctx
 where
    getParams = do
        creds <- TLS.credentialLoadX509 "haskell/test/Fixtures/localhost.crt" "haskell/test/Fixtures/localhost.key" >>= either fail return
        caStore <- getSystemCertificateStore
        return $ def
            { TLS.serverDHEParams = Just TLS.ffdhe4096
            , TLS.serverShared = def
                { TLS.sharedCredentials = TLS.Credentials [creds] }
            , TLS.serverSupported = def
                { TLS.supportedCiphers = ciphers }
            }
    ciphers = TLS.ciphersuite_strong
secureServer (Secured src dst ctx) =
    handle (throwM . TLSNegotiationFailure) $ do
        TLS.handshake ctx
        return $ Secured src dst ctx
secureServer _ = throwM InvalidSocketOperation

-- | Is this a secure connection?
isSecure :: Connection -> Bool
isSecure (Secured _ _ _) = True
isSecure _               = False

-- | Send over a connection
send :: Connection -> LB.ByteString -> IO ()
send (Connected _ _ sock) msg = Socket.sendAll sock msg `catch` (throwM . SocketFailure)
send (Secured _ _ ctx) msg    = TLS.sendData ctx msg `catch` (throwM . TLSSocketFailure)
send _ _                      = throwM InvalidSocketOperation

-- | Receive over a connection
recv :: Connection -> IO B.ByteString
recv (Connected _ _ sock) = Socket.recv sock 4096 `catch` (throwM . SocketFailure)
recv (Secured _ _ ctx)    = TLS.recvData ctx `catch` (throwM . TLSSocketFailure)
recv _                    = throwM InvalidSocketOperation

-- | Close a connection
close :: Connection -> IO Connection
close (Listening _ sock) =
    handle (throwM . SocketFailure) $ do
        Socket.close sock
        return $ Closed
close (Connected _ _ sock) =
    handle (throwM . SocketFailure) $ do
        Socket.close sock
        return $ Closed
close (Secured _ _ ctx) =
    handle (throwM . TLSSocketFailure) $ do
        TLS.bye ctx
        TLS.contextClose ctx
        return $ Closed
close Closed = return Closed
