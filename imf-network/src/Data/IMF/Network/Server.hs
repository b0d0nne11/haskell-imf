{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}

module Data.IMF.Network.Server
  ( Server(..)
  , PassFail(..)
  , runServer
  )
where

import           Control.Monad               (forM_, join, unless, void, when)
import           Control.Monad.IO.Unlift     (MonadUnliftIO, liftIO)
import           Control.Monad.Loops         (iterateUntilM, untilM)
import           Control.Monad.Reader        (ReaderT, ask, asks, local)
import qualified Data.Attoparsec.ByteString  as B (IResult (..), Result, parse, parseWith)
import qualified Data.Attoparsec.Text        as T (IResult (..), Result, parse, parseWith)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as B
import qualified Data.ByteString.Base64      as B64
import qualified Data.ByteString.Char8       as C
import           Data.Foldable               (foldlM)
import           Data.Maybe                  (isJust, isNothing)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import qualified Network.TLS                 as TLS
import           System.Log.FastLogger
import           UnliftIO.Exception          (catchJust, throwIO)

import           Data.IMF
import           Data.IMF.Network.Command
import           Data.IMF.Network.Connection (Connection)
import qualified Data.IMF.Network.Connection as Connection
import           Data.IMF.Network.Errors
import           Data.IMF.Network.Parsers
import           Data.IMF.Parsers.Mailbox
import           Data.IMF.Parsers.Message

type ServerM = ReaderT Server IO

data Server = Server
    { serverName             :: Text
    , serverConnection       :: Connection
    , serverTLSParams        :: TLS.ServerParams
    , serverLogger           :: LogStr -> IO ()
    , serverAuthenticate     :: Text -> Text -> IO PassFail
    , serverVerifyReturnPath :: Mailbox -> IO PassFail
    , serverVerifyRecipient  :: Mailbox -> IO PassFail
    , serverAcceptMessage    :: Maybe Mailbox -> [Mailbox] -> ByteString -> IO PassFail
    , serverMaxRecipients    :: Int
    , serverMaxMessageSize   :: Int
    , serverReqTLS           :: Bool
    , serverReqAuth          :: Bool
    }

data PassFail = Pass | TempFail | PermFail

serverExtensions :: Server -> ServerM [Text]
serverExtensions Server{..} = do
    secure <- Connection.isSecure serverConnection
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

recvParsed :: (ByteString -> B.Result a) -> Int -> ServerM a
recvParsed parse limit = do
    when (limit <= 0) $ throwIO ParseSizeExceeded
    join $ recv' $ \chunk -> case parse chunk of
        B.Fail chunk' _ err -> do
            logger . ("< " <>) $ B.take (B.length chunk - B.length chunk') chunk
            return (chunk', throwIO $ ParseFailure err)
        B.Partial parse' -> do
            logger . ("< " <>) $ chunk
            return ("", recvParsed parse' $ limit - B.length chunk)
        B.Done chunk' a -> do
            logger . ("< " <>) $ B.take (B.length chunk - B.length chunk') chunk
            return (chunk', return a)
  where
    recv' f = asks serverConnection >>= \conn -> Connection.recv' conn f

recvLine :: ServerM Text
recvLine = catchJust fromException (recvParsed (B.parse pTextLine) 4096) throwReply
  where
    fromException ParseSizeExceeded = Just (500, ["Syntax error, command unrecognized"])
    fromException (ParseFailure _)  = Just (500, ["Syntax error, command unrecognized"])

recvLines :: ServerM [Text]
recvLines = recvLine `untilM` done
  where
    done = asks serverConnection >>= fmap B.null . Connection.lookAhead

recvData :: ServerM ByteString
recvData = catchJust fromException (asks serverMaxMessageSize >>= recvParsed (B.parse pData)) throwReply
  where
    fromException ParseSizeExceeded = Just (552, ["Message exceeds fixed maximum message size"])
    fromException (ParseFailure _)  = Just (500, ["Syntax error, message unrecognized"])

putReply :: (Int, [Text]) -> ServerM ()
putReply reply = do
    conn <- asks serverConnection
    Connection.put conn $ formatReply reply
    logger . ("> " <>) $ formatReply reply

sendReply :: (Int, [Text]) -> ServerM ()
sendReply reply = do
    conn <- asks serverConnection
    Connection.send conn $ formatReply reply
    logger . ("> " <>) $ formatReply reply

formatReply :: (Int, [Text]) -> ByteString
formatReply (code, [])   = C.pack (show code) <> " " <> "\r\n"
formatReply (code, l:[]) = C.pack (show code) <> " " <> T.encodeUtf8 l <> "\r\n"
formatReply (code, l:ls) = C.pack (show code) <> "-" <> T.encodeUtf8 l <> "\r\n" <> formatReply (code, ls)

logger :: ToLogStr msg => msg -> ServerM ()
logger msg = asks serverLogger >>= \log -> liftIO $ log $ toLogStr msg

redact :: LogStr -> ServerM a -> ServerM a
redact msg = local $ \s -> s { serverLogger = const $ serverLogger s msg }

runServer :: ServerM ()
runServer = opening >>= void . iterateUntilM sessionClose loop
  where
    opening = do
        Server{..} <- ask
        sendReply (220, [serverName <> " Service ready"])
        return newSession
    loop session = do
        lines <- cuttoff 10 recvLines
        session <- foldlM step session lines
        asks serverConnection >>= Connection.flush
        return session
    step session line =
        handleReply (\r -> putReply r >> return session) $
            parseCommand line >>= runCommand session

parseCommand :: Text -> ServerM (Command, Text)
parseCommand i =
    T.parseWith (return "") pCommand i >>= \case
        T.Done i' r -> return (r, i')
        _           -> throwReply (500, ["Syntax error, command unrecognized"])

parseMailbox :: Text -> ServerM (Mailbox, Text)
parseMailbox i =
    T.parseWith (return "") pMailbox i >>= \case
        T.Done i' r -> return (r, i')
        _           -> throwReply (550, ["Syntax error, mailbox " <> i <> " unrecognized"])

parseMessage :: Text -> ServerM (Message, Text)
parseMessage i =
    T.parseWith (return "") pMessage i >>= \case
        T.Done i' r -> return (r, i')
        _           -> throwReply (550, ["Syntax error, message unrecognized"])

parseBase64 :: Text -> ServerM Text
parseBase64 i =
    case B64.decode $ T.encodeUtf8 i of
        Right i' -> return $ T.decodeUtf8 i'
        _        -> throwReply (501, ["Syntax error, credentials unrecognized"])

runCommand :: ServerSession -> (Command, Text) -> ServerM ServerSession
runCommand session (HELO, line) = do
    sendReply (250, ["OK"])
    return $ session { sessionGreeting = Just $ T.strip line }
runCommand session (EHLO, line) = do
    Server{..} <- ask
    serverExtensions <- join $ asks serverExtensions
    sendReply (250, serverName : serverExtensions)
    return $ session { sessionGreeting = Just $ T.strip line }
runCommand session (RSET, _) = do
    sendReply (250, ["OK"])
    return $ session { sessionReturnPath = Nothing, sessionRecipients = [] }
runCommand session (VRFY, line) = do
    (_, line') <- parseMailbox line
    forM_ (map (T.breakOn "=") $ T.words $ T.strip line') $ \case
        ("SMTPUTF8", "") -> return ()
        _                -> throwReply (504, ["Command parameter or argument not implemented"])
    sendReply (252, ["Cannot VRFY user, but will accept message and attempt delivery"])
    return session
runCommand session (NOOP, _) = do
    sendReply (250, ["OK"])
    return session
runCommand session (QUIT, _) = do
    Server{..} <- ask
    sendReply (221, [serverName <> " Service closing transmission session"])
    return $ session { sessionClose = True }
runCommand session@ServerSession{..} (MAIL, line) = do
    Server{..} <- ask
    when (isNothing sessionGreeting || isJust sessionReturnPath) $
        throwReply (503, ["Bad sequence of commands"])
    Connection.isSecure serverConnection >>= \secure -> when (serverReqTLS && not secure) $
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
        Pass      -> putReply   (250, ["Mailbox <" <> format mbox <> "> OK"])
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
        Pass      -> putReply   (250, ["Mailbox <" <> format mbox <> "> OK"])
        TempFail  -> throwReply (450, ["Mailbox <" <> format mbox <> "> unavailable"])
        PermFail  -> throwReply (550, ["Mailbox <" <> format mbox <> "> unavailable"])
    return $ session { sessionRecipients = mbox : sessionRecipients }
runCommand session@ServerSession{..} (DATA, _) = do
    Server{..} <- ask
    when (isNothing sessionReturnPath) $
        throwReply (503, ["Bad sequence of commands"])
    when (null sessionRecipients) $
        throwReply (554, ["No valid recipients"])
    sendReply (354, ["End data with <CR><LF>.<CR><LF>"])
    logger ("< [...]\r\n" :: LogStr)
    msg <- redact "" $ cuttoff 600 recvData
    when (B.length msg > serverMaxMessageSize) $
        throwReply (552, ["Message exceeds fixed maximum message size"])
    _ <- parseMessage $ T.decodeUtf8 msg
    liftIO (serverAcceptMessage sessionReturnPath sessionRecipients msg) >>= \case
        Pass      -> putReply   (250, ["OK"])
        TempFail  -> throwReply (451, ["Local error in processing"])
        PermFail  -> throwReply (554, ["Transaction failed"])
    return $ session { sessionReturnPath = Nothing, sessionRecipients = [] }
runCommand session (STARTTLS, _) = do
    Server{..} <- ask
    Connection.isSecure serverConnection >>= \secure -> when secure $
        throwReply (502, ["Command not implemented"])
    sendReply (220, ["Go ahead"])
    Connection.secure serverConnection serverTLSParams
    logger (". [tls handshake]\r\n" :: LogStr)
    return newSession
runCommand session (AUTH, line) = case T.words $ T.strip line of
    ["PLAIN"] -> do
        Server{..} <- ask
        Connection.isSecure serverConnection >>= \secure -> unless secure $
            throwReply (502, ["Command not implemented"])
        sendReply (334, ["Go ahead"])
        creds <- redact "< [...]\r\n" recvLine
        runCommand session (AUTH, "PLAIN " <> creds)
    ["PLAIN", creds] -> do
        Server{..} <- ask
        Connection.isSecure serverConnection >>= \secure -> unless secure $
            throwReply (502, ["Command not implemented"])
        (user, pass) <- T.split (== '\0') <$> parseBase64 creds >>= \case
            [_, user, pass] -> return (user, pass)
            _               -> throwReply (501, ["Syntax error, credentials unrecognized"])
        liftIO (serverAuthenticate user pass) >>= \case
            Pass      -> putReply   (235, ["Authentication succeeded"])
            TempFail  -> throwReply (454, ["Temporary authentication failure"])
            PermFail  -> throwReply (535, ["Authentication credentials invalid"])
        return $ session { sessionUser = Just user }
    ["LOGIN"] -> do
        Server{..} <- ask
        Connection.isSecure serverConnection >>= \secure -> unless secure $
            throwReply (502, ["Command not implemented"])
        sendReply (334, ["VXNlcm5hbWU6"])
        user <- redact "< [...]\r\n" recvLine
        runCommand session (AUTH, "LOGIN " <> user)
    ["LOGIN", user] -> do
        Server{..} <- ask
        Connection.isSecure serverConnection >>= \secure -> unless secure $
            throwReply (502, ["Command not implemented"])
        sendReply (334, ["UGFzc3dvcmQ6"])
        user <- parseBase64 user
        pass <- redact "< [...]\r\n" recvLine >>= parseBase64
        liftIO (serverAuthenticate user pass) >>= \case
            Pass      -> putReply   (235, ["Authentication succeeded"])
            TempFail  -> throwReply (454, ["Temporary authentication failure"])
            PermFail  -> throwReply (535, ["Authentication credentials invalid"])
        return $ session { sessionUser = Just user }
    _ ->
        throwReply (504, ["Command parameter or argument not implemented"])
