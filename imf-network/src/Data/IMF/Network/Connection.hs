{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Data.IMF.Network.Connection
  ( -- * Connection Management
    Connection
  , connect
  , listen
  , accept
  , secure
  , isSecure
  , tlsInfo
  , tlsClientParams
  , tlsServerParams
  , close
    -- * Mail eXchange Specific
  , connectMX
    -- * Sending and Receiving
  , send
  , put
  , flush
  , recv
  , recv'
  , lookAhead
    -- * Exceptions
  , ConnectionException(..)
    -- * Re-exports
  , HostName
  , ServiceName
  )
where

import           Control.Monad                  (when)
import           Control.Monad.IO.Unlift        (MonadIO, MonadUnliftIO, liftIO)
import           Data.ByteString                (ByteString)
import qualified Data.ByteString                as B
import qualified Data.ByteString.Char8          as C
import qualified Data.ByteString.Lazy           as LB
import           Data.Default.Class             (def)
import           Data.List                      (groupBy)
import           Data.Maybe                     (fromMaybe)
import           Data.Traversable               (for)
import           Data.X509.CertificateStore     (CertificateStore)
import qualified Network.DNS                    as DNS
import           Network.Socket                 (HostName, ServiceName, Socket)
import qualified Network.Socket                 as Socket hiding (recv, sendAll)
import qualified Network.Socket.ByteString      as Socket (recv, sendAll)
import qualified Network.TLS                    as TLS
import qualified Network.TLS.Extra.Cipher       as TLS
import qualified Network.TLS.Extra.FFDHE        as TLS
import           System.IO.Unsafe               (unsafePerformIO)
import           System.X509                    (getSystemCertificateStore)
import           UnliftIO.Exception
import           UnliftIO.MVar
import           UnliftIO.Timeout

data Connection = Connection
    { connBackend    :: MVar Backend
    , connRecvBuffer :: MVar ByteString
    , connSendBuffer :: MVar ByteString
    , connSocketAddr :: Socket.SockAddr
    , connPeerAddr   :: Maybe Socket.SockAddr
    }

data Backend = ConnectionSocket Socket
             | ConnectionTLS TLS.Context

rawRecv :: MonadUnliftIO m => MVar Backend -> m ByteString
rawRecv backend =
    withMVar backend $ \case
        ConnectionSocket sock -> liftIO $ Socket.recv sock 4096
        ConnectionTLS ctx     -> liftIO $ TLS.recvData ctx

rawSend :: MonadUnliftIO m => MVar Backend -> ByteString -> m ()
rawSend _ "" = return ()
rawSend backend chunk =
    withMVar backend $ \case
        ConnectionSocket sock -> liftIO $ Socket.sendAll sock chunk
        ConnectionTLS ctx     -> liftIO $ TLS.sendData ctx $ LB.fromStrict chunk

getAddr :: MonadIO m => HostName -> ServiceName -> m Socket.SockAddr
getAddr host port = liftIO $ Socket.addrAddress . head <$> Socket.getAddrInfo (Just hints) (Just host) (Just port)

getSocket :: MonadIO m => m Socket
getSocket = liftIO $ Socket.socket (Socket.addrFamily hints) (Socket.addrSocketType hints) (Socket.addrProtocol hints)

hints :: Socket.AddrInfo
hints = Socket.defaultHints
    { Socket.addrFamily     = Socket.AF_INET
    , Socket.addrSocketType = Socket.Stream
    }

-- | Open a new connection
connect :: MonadUnliftIO m => (HostName, ServiceName) -> (HostName, ServiceName) -> m Connection
connect (socketHost, socketPort) (peerHost, peerPort) = do
    sock <- getSocket
    socketAddr <- getAddr socketHost socketPort
    liftIO $ Socket.bind sock socketAddr
    peerAddr <- getAddr peerHost peerPort
    liftIO $ Socket.connect sock peerAddr
    Connection <$> newMVar (ConnectionSocket sock)
               <*> newMVar ""
               <*> newMVar ""
               <*> return socketAddr
               <*> return (Just peerAddr)

-- | Listen on a new connection
listen :: MonadUnliftIO m => (HostName, ServiceName) -> m Connection
listen (socketHost, socketPort) = do
    sock <- getSocket
    socketAddr <- getAddr socketHost socketPort
    liftIO $ Socket.setSocketOption sock Socket.ReuseAddr 1
    liftIO $ Socket.bind sock socketAddr
    liftIO $ Socket.listen sock 1
    Connection <$> newMVar (ConnectionSocket sock)
               <*> newMVar ""
               <*> newMVar ""
               <*> return socketAddr
               <*> return Nothing

-- | Accept a connection
accept :: MonadUnliftIO m => Connection -> m Connection
accept Connection{..} =
    withMVar connBackend $ \case
        ConnectionSocket sock -> do
            (sock', peerAddr) <- liftIO $ Socket.accept sock
            Connection <$> newMVar (ConnectionSocket sock')
                       <*> newMVar ""
                       <*> newMVar ""
                       <*> return connSocketAddr
                       <*> return (Just peerAddr)
        _ -> throwIO $ OperationRefused "accept requires a socket backend"

resolvSeed :: DNS.ResolvSeed
resolvSeed = unsafePerformIO $ DNS.makeResolvSeed DNS.defaultResolvConf
{-# NOINLINE resolvSeed #-}

lookupMX :: MonadIO m => HostName -> m [HostName]
lookupMX "localhost"   = return ["localhost"]
lookupMX "example.com" = throwIO $ OperationRefused "example.com is for documentation purposes only"
lookupMX "example.net" = throwIO $ OperationRefused "example.net is for documentation purposes only"
lookupMX "example.org" = throwIO $ OperationRefused "example.org is for documentation purposes only"
lookupMX domain        =
    liftIO $ DNS.withResolver resolvSeed $ \resolver ->
        DNS.lookupMX resolver (DNS.normalize $ C.pack domain) >>= \case
            Left e   -> throwIO e
            Right [] -> return [domain]
            Right rs -> return $ map (C.unpack . fst) $ concat $ groupBy (\(_, p1) (_, p2) -> p1 == p2) rs

tryEach :: MonadUnliftIO m => [m a] -> m a
tryEach []     = throwIO $ OperationRefused "tryEach requires a non-empty list"
tryEach [a]    = a
tryEach (a:as) = a `catchAny` \_ -> tryEach as

-- | Open a new connection to a MX domain
connectMX :: MonadUnliftIO m => (HostName, ServiceName) -> HostName -> m Connection
connectMX src domain = do
    hosts <- lookupMX domain
    tryEach $ flip map hosts $ \host ->
        tryEach $ flip map ["465", "587", "25", "2525"] $ \port -> do
            conn <- connect src (host, port)
            when (port == "465") $ secure conn $ tlsClientParams host True
            return conn

-- | Upgrade a plain socket to a TLS connection
secure :: (MonadUnliftIO m, TLS.TLSParams params) => Connection -> params -> m ()
secure Connection{..} params =
    modifyMVar_ connBackend $ \case
        ConnectionSocket sock -> do
            ctx <- liftIO $ TLS.contextNew sock params
            liftIO $ TLS.handshake ctx
            return $ ConnectionTLS ctx
        ConnectionTLS ctx ->
            return $ ConnectionTLS ctx

-- | Is this a TLS connection?
isSecure :: MonadUnliftIO m => Connection -> m Bool
isSecure Connection{..} =
    withMVar connBackend $ \case
        ConnectionTLS _ -> return True
        _               -> return False

-- | Get detailed TLS information for TLS connections
tlsInfo :: MonadUnliftIO m => Connection -> m (Maybe TLS.Information)
tlsInfo Connection{..} =
    withMVar connBackend $ \case
        ConnectionTLS ctx -> liftIO $ TLS.contextGetInformation ctx
        _                 -> return Nothing

systemCertificateStore :: CertificateStore
systemCertificateStore = unsafePerformIO getSystemCertificateStore
{-# NOINLINE systemCertificateStore #-}

-- | Helper to setup TLS client parameters
tlsClientParams :: HostName -> Bool -> TLS.ClientParams
tlsClientParams hostname enableValidation =
    (TLS.defaultParamsClient hostname "")
        { TLS.clientShared = def
            { TLS.sharedCAStore = systemCertificateStore
            , TLS.sharedValidationCache = validationCache
            }
        , TLS.clientSupported = def
            { TLS.supportedCiphers = TLS.ciphersuite_default
            }
        }
  where
    validationCache
        | enableValidation = def
        | otherwise = TLS.ValidationCache
            (\_ _ _ -> return TLS.ValidationCachePass)
            (\_ _ _ -> return ())

-- | Helper to setup TLS server parameters
tlsServerParams :: TLS.Credential -> TLS.ServerParams
tlsServerParams cred =
    def
        { TLS.serverDHEParams = Just TLS.ffdhe4096
        , TLS.serverShared = def
            { TLS.sharedCredentials = TLS.Credentials [cred]
            }
        , TLS.serverSupported = def
            { TLS.supportedCiphers = TLS.ciphersuite_strong
            }
        }

-- | Close a connection
close :: MonadUnliftIO m => Connection -> m ()
close Connection{..} =
    withMVar connBackend $ \case
        ConnectionSocket sock -> liftIO $ Socket.close sock
        ConnectionTLS ctx     -> liftIO $ TLS.bye ctx >> TLS.contextClose ctx

-- | Send bytestring over a connection
send :: MonadUnliftIO m => Connection -> ByteString -> m ()
send Connection{..} chunk =
    modifyMVar_ connSendBuffer $ \buffer -> do
        rawSend connBackend $ buffer `B.append` chunk
        return ""

-- | Put a bytestring into the send buffer
put :: MonadUnliftIO m => Connection -> ByteString -> m ()
put Connection{..} chunk =
    modifyMVar_ connSendBuffer $ \buffer ->
        return $ buffer `B.append` chunk

-- | Flush the send buffer
flush :: MonadUnliftIO m => Connection -> m ()
flush Connection{..} =
    modifyMVar_ connSendBuffer $ \buffer -> do
        rawSend connBackend buffer
        return ""

-- | Receive bytestring over a connection
recv :: MonadUnliftIO m => Connection -> m ByteString
recv conn = recv' conn $ return . ("", )

-- | Receive bytestring over a connection and put the unused portion back on the buffer
recv' :: MonadUnliftIO m => Connection -> (ByteString -> m (ByteString, a)) -> m a
recv' Connection{..} f =
    modifyMVar connRecvBuffer $ \buffer ->
        if not $ B.null buffer then f buffer else do
            chunk <- rawRecv connBackend
            when (B.null chunk) $ throwIO PeerConnectionClosed
            f chunk

-- | Look ahead without consuming the recv buffer or blocking forever
lookAhead :: MonadUnliftIO m => Connection -> m ByteString
lookAhead Connection{..} =
    modifyMVar connRecvBuffer $ \buffer ->
        if not $ B.null buffer then return (buffer, buffer) else do
            chunk <- fromMaybe "" <$> timeout 50 (rawRecv connBackend)
            return (chunk, chunk)

-- | Extra connection exceptions in addition to those thrown by Socket/TLS/DNS/etc libraries
data ConnectionException = OperationRefused String
                         | PeerConnectionClosed
  deriving (Show, Eq)

instance Exception ConnectionException
