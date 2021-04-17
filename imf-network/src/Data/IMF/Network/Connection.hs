{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE TupleSections #-}
module Data.IMF.Network.Connection
  ( Connection
  , fromSocket
    -- * Connection Management
  , connect
  , listen
  , accept
  , close
    -- * Mail eXchange Specific
  , lookupMX
  , sortMX
  , connectMX
    -- * TLS
  , secure
  , isSecure
  , tlsInfo
  , tlsClientParams
  , tlsServerParams
    -- * Sending and Receiving
  , send
  , sendLine
  , sendLines
  , recv
  , recvLine
  , recvLines
  , recvData
  , recvReply
    -- * Exceptions
  , ConnectionException(..)
    -- * Re-exports
  , HostName
  , ServiceName
  , Socket
  , SockAddr
  )
where

import qualified Data.Attoparsec.ByteString as AP
import qualified Data.Attoparsec.ByteString.Char8 as AP (decimal)
import qualified Data.Attoparsec.Combinator as AP
import           Control.Applicative        (many, liftA2)
import           Control.Monad.Loops        (unfoldWhileM)
import           Control.Monad              (join, when)
import           Control.Monad.IO.Unlift    (MonadIO, MonadUnliftIO, liftIO)
import           Data.Bifunctor             (first)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Builder    as BB
import qualified Data.ByteString.Char8      as C
import qualified Data.ByteString.Lazy       as LB
import           Data.Default.Class         (def)
import           Data.Foldable              (asum)
import           Data.List                  (intercalate, groupBy, sortOn)
import           Data.Text                  (Text)
import qualified Data.Text.Encoding         as T
import           Data.X509.CertificateStore (CertificateStore)
import qualified Network.DNS                as DNS
import           Network.Socket             (HostName, ServiceName, Socket, SockAddr)
import qualified Network.Socket             as Socket hiding (recv, sendAll)
import qualified Network.Socket.ByteString  as Socket (recv, sendAll)
import qualified Network.TLS                as TLS
import qualified Network.TLS.Extra.Cipher   as TLS
import qualified Network.TLS.Extra.FFDHE    as TLS
import           System.IO.Unsafe           (unsafePerformIO)
import           System.Random.Shuffle      (shuffleM)
import           System.X509                (getSystemCertificateStore)
import           UnliftIO.Exception         (Exception, throwIO)
import           UnliftIO.MVar              (MVar, modifyMVar, modifyMVar_, newMVar, withMVar)
import           UnliftIO.Timeout           (timeout)

data Connection = Connection
    { connBackend    :: MVar Backend
    , connRecvBuffer :: MVar ByteString
    }

data Backend = ConnectionSocket Socket
             | ConnectionTLS TLS.Context

fromSocket :: MonadIO m => Socket -> m Connection
fromSocket sock =
    Connection <$> newMVar (ConnectionSocket sock)
               <*> newMVar ""

rawRecv :: MonadUnliftIO m => MVar Backend -> m ByteString
rawRecv backend = do
    chunk <- liftIO $ withMVar backend $ \case
        ConnectionSocket sock -> Socket.recv sock 4096
        ConnectionTLS ctx     -> TLS.recvData ctx
    when (B.null chunk) $ throwIO PeerConnectionClosed
    return chunk

rawSend :: MonadUnliftIO m => MVar Backend -> ByteString -> m ()
rawSend _ "" = return ()
rawSend backend chunk =
    liftIO $ withMVar backend $ \case
        ConnectionSocket sock -> Socket.sendAll sock chunk
        ConnectionTLS ctx     -> TLS.sendData ctx $ LB.fromStrict chunk

rawClose :: MonadUnliftIO m => MVar Backend -> m ()
rawClose backend =
    liftIO $ withMVar backend $ \case
        ConnectionSocket sock -> Socket.close sock
        ConnectionTLS ctx     -> TLS.bye ctx >> TLS.contextClose ctx

dialTimeout :: MonadUnliftIO m => m a -> m a
dialTimeout f = timeout 3000000 f >>= maybe (throwIO ConnectionTimeout) return

-- | Open a new connection
connect :: (HostName, ServiceName) -> (HostName, ServiceName) -> IO Connection
connect (lhost, lport) (rhost, rport) =
    dialTimeout $ do
        laddr:_ <- Socket.getAddrInfo (Just hints) (Just lhost) (Just lport)
        raddr:_ <- Socket.getAddrInfo (Just hints) (Just rhost) (Just rport)
        sock <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
        Socket.setSocketOption sock Socket.KeepAlive 1
        Socket.setSocketOption sock Socket.NoDelay 1
        Socket.bind sock $ Socket.addrAddress laddr
        Socket.connect sock $ Socket.addrAddress raddr
        fromSocket sock
  where
    hints = Socket.defaultHints { Socket.addrFamily     = Socket.AF_INET
                                , Socket.addrSocketType = Socket.Stream
                                }

-- | Listen on a new socket
listen :: (HostName, ServiceName) -> IO Connection
listen (lhost, lport) =
    dialTimeout $ do
        laddr:_ <- Socket.getAddrInfo (Just hints) (Just lhost) (Just lport)
        sock <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
        Socket.setSocketOption sock Socket.KeepAlive 1
        Socket.setSocketOption sock Socket.NoDelay 1
        Socket.setSocketOption sock Socket.ReuseAddr 1
        Socket.bind sock $ Socket.addrAddress laddr
        Socket.listen sock 1
        fromSocket sock
  where
    hints = Socket.defaultHints { Socket.addrFamily     = Socket.AF_INET
                                , Socket.addrSocketType = Socket.Stream
                                }

-- | Accept a connection
accept :: Connection -> IO (Connection, SockAddr)
accept Connection{..} =
    withMVar connBackend $ \case
        ConnectionSocket sock -> do
            (sock', addr') <- Socket.accept sock
            conn' <- fromSocket sock'
            return (conn', addr')
        _ ->
            fail "accept requires a socket backend"

-- | Close a connection
close :: MonadUnliftIO m => Connection -> m ()
close Connection{..} = rawClose connBackend

resolvSeed :: DNS.ResolvSeed
resolvSeed = unsafePerformIO $ DNS.makeResolvSeed DNS.defaultResolvConf
{-# NOINLINE resolvSeed #-}

lookupMX :: HostName -> IO [(HostName, Int)]
lookupMX domain =
    DNS.withResolver resolvSeed $ \resolver ->
        DNS.lookupMX resolver (DNS.normalize $ C.pack domain) >>= \case
            Left e           -> throwIO e
            Right [(".", _)] -> throwIO NullMX
            Right []         -> return [(domain, 0)] -- implicit MX
            Right rs         -> return $ map (first C.unpack) rs

sortMX :: [(HostName, Int)] -> IO [(HostName, Int)]
sortMX = fmap concat . mapM shuffleM . groupOn snd . sortOn snd
  where
    groupOn :: Ord b => (a -> b) -> [a] -> [[a]]
    groupOn f = groupBy (\a1 a2 -> f a1 == f a2)

-- | Open a new connection to a MX domain
connectMX :: HostName -> HostName -> IO Connection
connectMX lhost domain = do
    rhosts <- map fst <$> (lookupMX domain >>= sortMX)
    asum $ flip map rhosts $ \rhost ->
        asum $ flip map ["465", "587", "25", "2525"] $ \rport -> do
            conn <- connect (lhost, "0") (rhost, rport)
            when (rport == "465") $ secure conn $ tlsClientParams rhost True
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

-- | Send a bytestring over a connection
send :: MonadUnliftIO m => Connection -> LB.ByteString -> m ()
send Connection{..} = mapM_ (rawSend connBackend) . LB.toChunks

-- | Send a single line appending the newline
sendLine :: MonadUnliftIO m => Connection -> ByteString -> m ()
sendLine conn = send conn . LB.fromStrict . (`C.snoc` '\n')

-- | Send a set of lines
sendLines :: MonadUnliftIO m => Connection -> [ByteString] -> m ()
sendLines conn = send conn . LB.fromStrict . C.unlines

-- | Receive a bytestring over a connection and put the unused portion back on the buffer
recv :: MonadUnliftIO m => Connection -> (ByteString -> m (ByteString, a)) -> m a
recv Connection{..} f =
    modifyMVar connRecvBuffer $ \buffer ->
        if B.null buffer then rawRecv connBackend >>= f else f buffer

-- | Receive the next line minus the newline
recvLine :: (MonadUnliftIO m, MonadFail m) => Connection -> m ByteString
recvLine conn = recvLoop ""
  where
    recvLoop acc = join $ recv conn $ \chunk ->
        let (acc', chunk') = first (acc <>) $ C.break (== '\n') chunk in
        if chunk' == ""
            then return ("", recvLoop acc')
            else return (B.drop 1 chunk', return acc')

-- | Receive the next set of full lines
recvLines :: (MonadUnliftIO m, MonadFail m) => Connection -> m [ByteString]
recvLines conn = C.lines <$> recvLoop ""
  where
    recvLoop acc = join $ recv conn $ \chunk ->
        let acc' = acc <> chunk in
        if C.last chunk == '\n'
            then return ("", return acc')
            else return ("", recvLoop acc')

-- | Receive a message minus the data termination sequence
recvData :: (MonadUnliftIO m, MonadFail m) => Connection -> m LB.ByteString
recvData conn = BB.toLazyByteString . mconcat . map (\l -> BB.byteString l <> "\n") <$> unfoldWhileM (\l -> l /= "." && l /= ".\r") (recvLine conn)

-- | Receive a tuple of a bytestring and a parsed object it corresponds to
recvParsed :: (MonadUnliftIO m, MonadFail m) => Connection -> AP.Parser a -> m (ByteString, a)
recvParsed conn parser = recvLoop "" $ AP.parse parser
  where
    recvLoop acc parser = do
        join $ recv conn $ \chunk ->
            case parser chunk of
                AP.Fail i [] err   -> return (i, fail $ "recv: " ++ err)
                AP.Fail i ctxs err -> return (i, fail $ "recv: " ++ intercalate " > " ctxs ++ ": " ++ err)
                AP.Done i a        -> return (i, return (acc <> B.take (B.length chunk - B.length i) chunk, a))
                AP.Partial parser' -> return ("", recvLoop (acc <> chunk) parser')

type Reply = (Int, [Text])

-- | Receive a tuple of a bytestring and matching reply
recvReply :: (MonadUnliftIO m, MonadFail m) => Connection -> m (ByteString, Reply)
recvReply conn = recvParsed conn pReply
  where
    pReply =
        liftA2 (,) (AP.lookAhead AP.decimal) $
            liftA2 snoc (many $ AP.take 3 *> AP.string "-" *> pLine)
                        (AP.take 3 *> AP.string " " *> pLine)
    pLine = T.decodeUtf8 <$> AP.takeTill (== 10) <* AP.take 1
    snoc as a = as ++ [a]

-- | Extra connection exceptions in addition to those thrown by Socket/TLS/DNS/etc libraries
data ConnectionException = ConnectionTimeout
                         | PeerConnectionClosed
                         | NullMX
  deriving (Show, Eq)

instance Exception ConnectionException
