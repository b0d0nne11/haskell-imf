{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.IMF.Network.Server
  ( Server(..)
  , PassFail(..)
  , newServer
  , run
  )
where

import           Control.Concurrent.MVar     (MVar, modifyMVar_, newMVar, swapMVar)
import           Control.Exception.Safe      (MonadMask)
import           Control.Monad               (forM_, join, unless, when)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Logger        (MonadLogger, logDebugN)
import           Control.Monad.Reader        (MonadReader, ask, asks)
import qualified Data.Attoparsec.Text        as T (IResult (..), parseWith)
import qualified Data.ByteString.Lazy        as LB
import           Data.Default.Class          (def)
import           Data.Foldable               (foldlM)
import           Data.Int                    (Int64)
import           Data.Maybe                  (isJust, isNothing)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import qualified Data.Text.Encoding.Base64   as T
import           Data.Time                   (NominalDiffTime)
import qualified Network.TLS                 as TLS

import           Data.IMF                    (Mailbox, Message, format)
import           Data.IMF.Network.Command    (Command (..), pCommand)
import           Data.IMF.Network.Connection (Connection)
import qualified Data.IMF.Network.Connection as Conn
import           Data.IMF.Network.Errors     (cuttoff, handleReply, throwReply)
import           Data.IMF.Parsers.Mailbox    (pMailbox)
import           Data.IMF.Parsers.Message    (pMessage)

type ServerEnv m = (MonadIO m, MonadLogger m, MonadMask m, MonadReader Server m)

data Server = Server
    { serverName             :: Text
    , serverConnection       :: Connection
    , serverPendingLines     :: MVar [Text]
    , serverTLSParams        :: TLS.ServerParams
    , serverAuthenticate     :: Text -> Text -> IO PassFail
    , serverVerifyReturnPath :: Mailbox -> IO PassFail
    , serverVerifyRecipient  :: Mailbox -> IO PassFail
    , serverAcceptMessage    :: Maybe Mailbox -> [Mailbox] -> LB.ByteString -> IO PassFail
    , serverMaxRecipients    :: Int
    , serverMaxMessageSize   :: Int64
    , serverReqTLS           :: Bool
    , serverReqAuth          :: Bool
    }

data PassFail = Pass | TempFail | PermFail

serverExtensions :: ServerEnv m => Server -> m [Text]
serverExtensions Server{..} = do
    secure <- liftIO $ Conn.isSecure serverConnection
    if secure then return tlsExtensions
              else return defExtensions
  where
    defExtensions = [ "SIZE " `T.append` T.pack (show serverMaxMessageSize)
                    , "8BITMIME"
                    , "SMTPUTF8"
                    , "PIPELINING"
                    , "STARTTLS"
                    ]
    tlsExtensions = [ "SIZE " `T.append` T.pack (show serverMaxMessageSize)
                    , "8BITMIME"
                    , "SMTPUTF8"
                    , "PIPELINING"
                    , "AUTH PLAIN LOGIN"
                    ]

newServer :: Text -> Connection -> IO Server
newServer name conn = do
    pendingLines <- newMVar []
    return $ Server
        { serverName             = name
        , serverConnection       = conn
        , serverPendingLines     = pendingLines
        , serverTLSParams        = def
        , serverAuthenticate     = \_ _ -> return PermFail
        , serverVerifyReturnPath = \_ -> return Pass
        , serverVerifyRecipient  = \_ -> return Pass
        , serverAcceptMessage    = \_ _ _ -> return PermFail
        , serverMaxRecipients    = 10
        , serverMaxMessageSize   = 4096
        , serverReqTLS           = False
        , serverReqAuth          = False
        }

data ServerSession = ServerSession
    { sessionGreeting   :: Maybe Text
    , sessionUser       :: Maybe Text
    , sessionReturnPath :: Maybe Mailbox
    , sessionRecipients :: [Mailbox]
    , sessionClose      :: Bool
    }
  deriving Show

newSession :: ServerSession
newSession = ServerSession
    { sessionGreeting   = Nothing
    , sessionUser       = Nothing
    , sessionReturnPath = Nothing
    , sessionRecipients = []
    , sessionClose      = False
    }

recvLines :: ServerEnv m => NominalDiffTime -> m [Text]
recvLines ttl = do
    Server{..} <- ask
    lines <- map T.decodeUtf8 <$> liftIO (cuttoff ttl $ Conn.recvLines serverConnection)
    forM_ lines $ \line -> logDebugN $ "< " <> line
    return lines

recvCredential :: ServerEnv m => NominalDiffTime -> m Text
recvCredential ttl = do
    Server{..} <- ask
    line <- T.decodeUtf8 <$> liftIO (cuttoff ttl $ Conn.recvLine serverConnection)
    logDebugN "< [...]"
    return line

recvData :: ServerEnv m => NominalDiffTime -> m LB.ByteString
recvData ttl = do
    Server{..} <- ask
    msg <- liftIO $ cuttoff ttl $ Conn.recvData serverConnection
    logDebugN $ "< [" <> T.pack (show $ LB.length msg) <> " bytes]"
    return msg

reply :: ServerEnv m => (Int, [Text]) -> m ()
reply r = do
    Server{..} <- ask
    liftIO $ modifyMVar_ serverPendingLines $ return . (++ formatReply r)
  where
    formatReply (code, [])   = [T.pack (show code) <> " \r"]
    formatReply (code, [l])  = [T.pack (show code) <> " " <> l <> "\r"]
    formatReply (code, l:ls) = (T.pack (show code) <> "-" <> l <> "\r") : formatReply (code, ls)

sendLines :: ServerEnv m => m ()
sendLines = do
    Server{..} <- ask
    lines <- liftIO $ swapMVar serverPendingLines []
    liftIO $ Conn.sendLines serverConnection $ map T.encodeUtf8 lines
    forM_ lines $ \line -> logDebugN $ "> " <> line

secure :: ServerEnv m => m ()
secure = do
    Server{..} <- ask
    liftIO $ Conn.secure serverConnection serverTLSParams
    logDebugN "- [tls handshake]"

close :: ServerEnv m => m ()
close = do
    Server{..} <- ask
    liftIO $ Conn.close serverConnection
    logDebugN "- [closing connection]"

run :: ServerEnv m => m ()
run = opening >>= loop >> close
  where
    opening = do
        Server{..} <- ask
        reply (220, [serverName <> " Service ready"])
        sendLines
        return newSession
    loop session = do
        lines <- recvLines 10
        session@ServerSession{..} <- foldlM step session lines
        sendLines
        unless sessionClose $ loop session
    step session line =
        handleReply (\r -> session <$ reply r) $
            parseCommand line >>= runCommand session

parseCommand :: ServerEnv m => Text -> m (Command, Text)
parseCommand i =
    T.parseWith (return "") pCommand (T.strip i) >>= \case
        T.Done i' r -> return (r, i')
        _           -> throwReply (500, ["Syntax error, command unrecognized"])

parseMailbox :: ServerEnv m => Text -> m (Mailbox, Text)
parseMailbox i =
    T.parseWith (return "") pMailbox (T.strip i) >>= \case
        T.Done i' r -> return (r, i')
        _           -> throwReply (550, ["Syntax error, mailbox " <> i <> " unrecognized"])

parseMessage :: ServerEnv m => Text -> m (Message, Text)
parseMessage i =
    T.parseWith (return "") pMessage (T.strip i) >>= \case
        T.Done i' r -> return (r, i')
        _           -> throwReply (550, ["Syntax error, message unrecognized"])

parseBase64 :: ServerEnv m => Text -> m Text
parseBase64 i =
    case T.decodeBase64 $ T.strip i of
        Right i' -> return i'
        _        -> throwReply (501, ["Syntax error, credentials unrecognized"])

runCommand :: ServerEnv m => ServerSession -> (Command, Text) -> m ServerSession
runCommand session (HELO, line) = do
    reply (250, ["OK"])
    return $ session { sessionGreeting = Just $ T.strip line }
runCommand session (EHLO, line) = do
    Server{..} <- ask
    serverExtensions <- join $ asks serverExtensions
    reply (250, serverName : serverExtensions)
    return $ session { sessionGreeting = Just $ T.strip line }
runCommand session (RSET, _) = do
    reply (250, ["OK"])
    return $ session { sessionReturnPath = Nothing, sessionRecipients = [] }
runCommand session (VRFY, line) = do
    (_, line') <- parseMailbox line
    forM_ (map (T.breakOn "=") $ T.words $ T.strip line') $ \case
        ("SMTPUTF8", "") -> return ()
        _                -> throwReply (504, ["Command parameter or argument not implemented"])
    reply (252, ["Cannot VRFY user, but will accept message and attempt delivery"])
    return session
runCommand session (NOOP, _) = do
    reply (250, ["OK"])
    return session
runCommand session (QUIT, _) = do
    Server{..} <- ask
    reply (221, [serverName <> " Service closing transmission session"])
    return $ session { sessionClose = True }
runCommand session@ServerSession{..} (MAIL, line) = do
    Server{..} <- ask
    when (isNothing sessionGreeting || isJust sessionReturnPath) $
        throwReply (503, ["Bad sequence of commands"])
    liftIO (Conn.isSecure serverConnection) >>= \secure -> when (serverReqTLS && not secure) $
        throwReply (530, ["Must issue a STARTTLS command first"])
    when (serverReqAuth && isNothing sessionUser) $
        throwReply (530, ["Authentication required"])
    (mbox, line') <- parseMailbox line
    forM_ (map (T.breakOn "=") $ T.words $ T.strip line') $ \case
        ("SIZE", size)       -> when (read (T.unpack size) > serverMaxMessageSize) $ throwReply (552, ["Message exceeds fixed maximum message size"])
        ("BODY", "7BIT")     -> return ()
        ("BODY", "8BITMIME") -> return ()
        ("SMTPUTF8", "")     -> return ()
        ("AUTH", _)          -> return ()
        _                    -> throwReply (504, ["Command parameter or argument not implemented"])
    liftIO (serverVerifyReturnPath mbox) >>= \case
        Pass      -> reply      (250, ["Mailbox <" <> format mbox <> "> OK"])
        TempFail  -> throwReply (450, ["Mailbox <" <> format mbox <> "> unavailable"])
        PermFail  -> throwReply (550, ["Mailbox <" <> format mbox <> "> unavailable"])
    return $ session { sessionReturnPath = Just mbox }
runCommand session@ServerSession{..} (RCPT, line) = do
    Server{..} <- ask
    when (isNothing sessionReturnPath) $
        throwReply (503, ["Bad sequence of commands"])
    when (length sessionRecipients >= serverMaxRecipients) $
        throwReply (452, ["Too many recipients"])
    (mbox, line') <- parseMailbox line
    forM_ (map (T.breakOn "=") $ T.words $ T.strip line') $ \_ ->
        throwReply (504, ["Command parameter or argument not implemented"])
    liftIO (serverVerifyRecipient mbox) >>= \case
        Pass      -> reply      (250, ["Mailbox <" <> format mbox <> "> OK"])
        TempFail  -> throwReply (450, ["Mailbox <" <> format mbox <> "> unavailable"])
        PermFail  -> throwReply (550, ["Mailbox <" <> format mbox <> "> unavailable"])
    return $ session { sessionRecipients = mbox : sessionRecipients }
runCommand session@ServerSession{..} (DATA, _) = do
    Server{..} <- ask
    when (isNothing sessionReturnPath) $
        throwReply (503, ["Bad sequence of commands"])
    when (null sessionRecipients) $
        throwReply (554, ["No valid recipients"])
    reply (354, ["End data with <CR><LF>.<CR><LF>"]) >> sendLines
    msg <- recvData 600
    when (LB.length msg > serverMaxMessageSize) $
        throwReply (552, ["Message exceeds fixed maximum message size"])
    _ <- parseMessage $ T.decodeUtf8 $ LB.toStrict msg
    liftIO (serverAcceptMessage sessionReturnPath sessionRecipients msg) >>= \case
        Pass      -> reply      (250, ["OK"])
        TempFail  -> throwReply (451, ["Local error in processing"])
        PermFail  -> throwReply (554, ["Transaction failed"])
    return $ session { sessionReturnPath = Nothing, sessionRecipients = [] }
runCommand _ (STARTTLS, _) = do
    Server{..} <- ask
    liftIO (Conn.isSecure serverConnection) >>= \secure -> when secure $
        throwReply (502, ["Command not implemented"])
    reply (220, ["Go ahead"]) >> sendLines
    secure
    return newSession
runCommand session (AUTH, line) = case T.words $ T.strip line of
    ["PLAIN"] -> do
        Server{..} <- ask
        liftIO (Conn.isSecure serverConnection) >>= \secure -> unless secure $
            throwReply (502, ["Command not implemented"])
        reply (334, ["Go ahead"]) >> sendLines
        creds <- recvCredential 10
        runCommand session (AUTH, "PLAIN " <> creds)
    ["PLAIN", creds] -> do
        Server{..} <- ask
        liftIO (Conn.isSecure serverConnection) >>= \secure -> unless secure $
            throwReply (502, ["Command not implemented"])
        creds <- parseBase64 creds
        (user, pass) <- case T.split (== '\0') creds of
            [_, user, pass] -> return (user, pass)
            _               -> throwReply (501, ["Syntax error, credentials unrecognized"])
        liftIO (serverAuthenticate user pass) >>= \case
            Pass      -> reply      (235, ["Authentication succeeded"])
            TempFail  -> throwReply (454, ["Temporary authentication failure"])
            PermFail  -> throwReply (535, ["Authentication credentials invalid"])
        return $ session { sessionUser = Just user }
    ["LOGIN"] -> do
        Server{..} <- ask
        liftIO (Conn.isSecure serverConnection) >>= \secure -> unless secure $
            throwReply (502, ["Command not implemented"])
        reply (334, ["VXNlcm5hbWU6"]) >> sendLines
        user <- recvCredential 10
        runCommand session (AUTH, "LOGIN " <> user)
    ["LOGIN", user] -> do
        Server{..} <- ask
        liftIO (Conn.isSecure serverConnection) >>= \secure -> unless secure $
            throwReply (502, ["Command not implemented"])
        reply (334, ["UGFzc3dvcmQ6"]) >> sendLines
        user <- parseBase64 user
        pass <- recvCredential 10 >>= parseBase64
        liftIO (serverAuthenticate user pass) >>= \case
            Pass      -> reply      (235, ["Authentication succeeded"])
            TempFail  -> throwReply (454, ["Temporary authentication failure"])
            PermFail  -> throwReply (535, ["Authentication credentials invalid"])
        return $ session { sessionUser = Just user }
    _ ->
        throwReply (504, ["Command parameter or argument not implemented"])
