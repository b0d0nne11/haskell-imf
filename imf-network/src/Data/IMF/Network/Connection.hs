{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import           Control.Applicative              (liftA2, many)
import           Control.Concurrent.MVar          (MVar, modifyMVar, modifyMVar_, newMVar, withMVar)
import           Control.Exception.Safe           (Exception, throw)
import           Control.Monad                    (join, when)
import           Control.Monad.Loops              (unfoldWhileM)
import qualified Data.Attoparsec.ByteString       as AP
import qualified Data.Attoparsec.ByteString.Char8 as AP (decimal)
import qualified Data.Attoparsec.Combinator       as AP
import           Data.Bifunctor                   (first)
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Builder          as BB
import qualified Data.ByteString.Char8            as C
import qualified Data.ByteString.Lazy             as LB
import           Data.Default.Class               (def)
import           Data.Foldable                    (asum)
import           Data.List                        (groupBy, intercalate, sortOn)
import           Data.Text                        (Text)
import qualified Data.Text.Encoding               as T
import           Data.X509.CertificateStore       (CertificateStore)
import qualified Network.DNS                      as DNS
import           Network.Socket                   (HostName, ServiceName, SockAddr, Socket)
import qualified Network.Socket                   as Socket hiding (recv, sendAll)
import qualified Network.Socket.ByteString        as Socket (recv, sendAll)
import qualified Network.TLS                      as TLS
import qualified Network.TLS.Extra.Cipher         as TLS
import qualified Network.TLS.Extra.FFDHE          as TLS
import           System.IO.Unsafe                 (unsafePerformIO)
import           System.Random.Shuffle            (shuffleM)
import           System.Timeout                   (timeout)
import           System.X509                      (getSystemCertificateStore)

data Connection = Connection
    { connBackend    :: MVar Backend
    , connRecvBuffer :: MVar ByteString
    }

data Backend = ConnectionSocket Socket
             | ConnectionTLS TLS.Context

fromSocket :: Socket -> IO Connection
fromSocket sock =
    Connection <$> newMVar (ConnectionSocket sock)
               <*> newMVar ""

rawRecv :: MVar Backend -> IO ByteString
rawRecv backend = do
    chunk <- withMVar backend $ \case
        ConnectionSocket sock -> Socket.recv sock 4096
        ConnectionTLS ctx     -> TLS.recvData ctx
    when (B.null chunk) $ throw PeerConnectionClosed
    return chunk

rawSend :: MVar Backend -> ByteString -> IO ()
rawSend _ "" = return ()
rawSend backend chunk =
    withMVar backend $ \case
        ConnectionSocket sock -> Socket.sendAll sock chunk
        ConnectionTLS ctx     -> TLS.sendData ctx $ LB.fromStrict chunk

rawClose :: MVar Backend -> IO ()
rawClose backend =
    withMVar backend $ \case
        ConnectionSocket sock -> Socket.close sock
        ConnectionTLS ctx     -> TLS.bye ctx >> TLS.contextClose ctx

dialTimeout :: IO a -> IO a
dialTimeout f = timeout 3000000 f >>= maybe (throw ConnectionTimeout) return

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
close :: Connection -> IO ()
close Connection{..} = rawClose connBackend

resolvSeed :: DNS.ResolvSeed
resolvSeed = unsafePerformIO $ DNS.makeResolvSeed DNS.defaultResolvConf
{-# NOINLINE resolvSeed #-}

lookupMX :: HostName -> IO [(HostName, Int)]
lookupMX domain =
    DNS.withResolver resolvSeed $ \resolver ->
        DNS.lookupMX resolver (DNS.normalize $ C.pack domain) >>= \case
            Left e           -> throw e
            Right [(".", _)] -> throw NullMX
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
secure :: TLS.TLSParams params => Connection -> params -> IO ()
secure Connection{..} params =
    modifyMVar_ connBackend $ \case
        ConnectionSocket sock -> do
            ctx <- TLS.contextNew sock params
            TLS.handshake ctx
            return $ ConnectionTLS ctx
        ConnectionTLS ctx ->
            return $ ConnectionTLS ctx

-- | Is this a TLS connection?
isSecure :: Connection -> IO Bool
isSecure Connection{..} =
    withMVar connBackend $ \case
        ConnectionTLS _ -> return True
        _               -> return False

-- | Get detailed TLS information for TLS connections
tlsInfo :: Connection -> IO (Maybe TLS.Information)
tlsInfo Connection{..} =
    withMVar connBackend $ \case
        ConnectionTLS ctx -> TLS.contextGetInformation ctx
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
send :: Connection -> LB.ByteString -> IO ()
send Connection{..} = mapM_ (rawSend connBackend) . LB.toChunks

-- | Send a single line appending the newline
sendLine :: Connection -> ByteString -> IO ()
sendLine conn = send conn . LB.fromStrict . (`C.snoc` '\n')

-- | Send a set of lines
sendLines :: Connection -> [ByteString] -> IO ()
sendLines conn = send conn . LB.fromStrict . C.unlines

-- | Receive a bytestring over a connection and put the unused portion back on the buffer
recv :: Connection -> (ByteString -> IO (ByteString, a)) -> IO a
recv Connection{..} f =
    modifyMVar connRecvBuffer $ \buffer ->
        if B.null buffer then rawRecv connBackend >>= f else f buffer

-- | Receive the next line minus the newline
recvLine :: Connection -> IO ByteString
recvLine conn = recvLoop ""
  where
    recvLoop acc = join $ recv conn $ \chunk ->
        let (acc', chunk') = first (acc <>) $ C.break (== '\n') chunk in
        if chunk' == ""
            then return ("", recvLoop acc')
            else return (B.drop 1 chunk', return acc')

-- | Receive the next set of full lines
recvLines :: Connection -> IO [ByteString]
recvLines conn = C.lines <$> recvLoop ""
  where
    recvLoop acc = join $ recv conn $ \chunk ->
        let acc' = acc <> chunk in
        if C.last chunk == '\n'
            then return ("", return acc')
            else return ("", recvLoop acc')

-- | Receive a message minus the data termination sequence
recvData :: Connection -> IO LB.ByteString
recvData conn = BB.toLazyByteString . mconcat . map (\l -> BB.byteString l <> "\n") <$> unfoldWhileM (\l -> l /= "." && l /= ".\r") (recvLine conn)

-- | Receive a tuple of a bytestring and a parsed object it corresponds to
recvParsed :: Connection -> AP.Parser a -> IO (ByteString, a)
recvParsed conn parser = recvLoop "" $ AP.parse parser
  where
    recvLoop acc parser = do
        join $ recv conn $ \chunk ->
            case parser chunk of
                AP.Fail i [] err   -> return (i, fail $ "parse error: " ++ err)
                AP.Fail i ctxs err -> return (i, fail $ "parse error: " ++ intercalate " > " ctxs ++ ": " ++ err)
                AP.Done i a        -> return (i, return (acc <> B.take (B.length chunk - B.length i) chunk, a))
                AP.Partial parser' -> return ("", recvLoop (acc <> chunk) parser')

type Reply = (Int, [Text])

-- | Receive a tuple of a bytestring and matching reply
recvReply :: Connection -> IO (ByteString, Reply)
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
