{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.IMF.Network.Server
  ( ServerParams(..)
  , Hooks(..)
  , server
  , Session(..)
  , Buffer(..)
  , newSession
  , runSession
  )
where

import qualified Control.Concurrent.Thread     as Thread
import qualified Control.Concurrent.Thread.Group
                                               as ThreadGroup
import           Control.Monad                  ( when )
import           Control.Monad.Except           ( catchError
                                                , throwError
                                                )
import           Control.Monad.State            ( gets
                                                , modify
                                                )
import           Data.Attoparsec.Text           ( endOfInput
                                                , parseOnly
                                                )
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.Lazy          as LB
import           Data.IORef                     ( newIORef
                                                , readIORef
                                                , writeIORef
                                                )
import           Data.Maybe                     ( isNothing )
import           Data.Time.Clock                ( UTCTime
                                                , getCurrentTime
                                                )
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( decodeUtf8
                                                , encodeUtf8
                                                )
import           Data.Traversable               ( for )
import           System.IO                      ( hPutStrLn
                                                , stderr
                                                )
import           System.Timeout                 ( timeout )

import           Data.IMF.Types
import           Data.IMF.Network.Chat
import qualified Data.IMF.Network.Connection   as Conn
import           Data.IMF.Network.Errors
import           Data.IMF.Parsers.Command
import           Data.IMF.Parsers.Mailbox
import           Data.IMF.Parsers.Message
import           Data.IMF.Parsers.Network

whenM :: Monad m => m Bool -> m () -> m ()
whenM test f = do res <- test; if res then f else return ()

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM test = whenM $ not <$> test

whenJustM :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
whenJustM ma f = do a <- ma; maybe (return ()) f a

whileM :: Monad m => m Bool -> m () -> m ()
whileM test f = do result <- test; if result then f >> whileM test f else return ()

data ServerParams = ServerParams
    { serverName        :: T.Text
    , serverIP          :: String
    , serverPort        :: String
    , serverMaxSessions :: Int
    , serverMaxMsgSize  :: Int
    , serverMaxRcpts    :: Int
    }
  deriving (Eq, Show)

data Hooks = Hooks
    { hookLogSession       :: (Session, ChatLog) -> IO ()
    , hookVerifyReturnPath :: Mailbox -> IO ()
    , hookVerifyRcpt       :: Mailbox -> IO ()
    , hookSaveMessage      :: Buffer -> T.Text -> IO ()
    }

server :: ServerParams -- ^ server parameters
       -> Hooks        -- ^ server hooks
       -> IO (IO ())
server params@ServerParams{..} hooks@Hooks{..}= do
    sessions <- ThreadGroup.new
    isRunning <- newIORef True
    hPutStrLn stderr $ "starting server with " ++ show params
    (_, waitForMainThread) <- Thread.forkIO $ do
        conn <- Conn.listen (serverIP, serverPort)
        whileM (readIORef isRunning) $ do
            whenJustM (timeout 1000000 $ Conn.accept conn) $ \conn' -> do
                _ <- ThreadGroup.forkIO sessions $
                    newSession conn' >>= runSession params hooks (readIORef isRunning) >>= hookLogSession
                ThreadGroup.waitN serverMaxSessions sessions
        Conn.close conn
    hPutStrLn stderr $ "up and running"
    return $ do
        hPutStrLn stderr $ "sending shutdown signal"
        writeIORef isRunning False
        hPutStrLn stderr $ "waiting for main thread to finish"
        _ <- waitForMainThread
        hPutStrLn stderr $ "waiting for sessions to finish"
        ThreadGroup.wait sessions

data Session = Session
    { sessionConnection  :: Conn.Connection
    , sessionError       :: ChatException
    , sessionCreatedAt   :: UTCTime
    , sessionClientName  :: T.Text
    , sessionInitialized :: Bool
    , sessionBuffer      :: Maybe Buffer
    }
  deriving Show

newSession :: Conn.Connection -> IO Session
newSession conn = do
    now <- getCurrentTime
    return Session
        { sessionConnection  = conn
        , sessionError       = OK
        , sessionCreatedAt   = now
        , sessionClientName  = ""
        , sessionInitialized = False
        , sessionBuffer      = Nothing
        }

data Buffer = Buffer
    { returnPath  :: Mailbox
    , rcpts       :: [Mailbox]
    , failedRcpts :: [T.Text]
    }
  deriving (Eq, Show)

newBuffer :: Mailbox -> Buffer
newBuffer returnPath =
    Buffer
        { returnPath  = returnPath
        , rcpts       = []
        , failedRcpts = []
        }

runSession :: ServerParams -- ^ server parameters
           -> Hooks        -- ^ server hooks
           -> IO Bool      -- ^ keep running?
           -> Session      -- ^ session
           -> IO (Session, ChatLog)
runSession ServerParams{..} Hooks{..} isRunning = runChat $ chat `catchError` handleError
  where
    chat :: Chat Session ()
    chat = do
        sendReply (220, [encodeUtf8 serverName <> " Service ready"])
        runCommands
    handleError :: ChatException -> Chat Session ()
    handleError e = do
        modify $ \s -> s { sessionError = e }
        debug $ "exception: " <> (C.pack $ show e)
        closeConnection
        return ()
    runCommands :: Chat Session ()
    runCommands = do
        recvLine >>= parseCommand >>= \cmd -> case cmd of
            EHLO clientName -> do
                modify $ \s -> s { sessionInitialized = True
                                 , sessionClientName = clientName
                                 , sessionBuffer = Nothing
                                 }
                sendReply (250, [ encodeUtf8 serverName
                                , "SIZE " <> (C.pack $ show serverMaxMsgSize)
                                , "8BITMIME"
                                , "SMTPUTF8"
                                ])
                runCommands
            HELO clientName -> do
                modify $ \s -> s { sessionInitialized = True
                                 , sessionClientName  = clientName
                                 , sessionBuffer = Nothing
                                 }
                sendReply (250, ["OK"])
                runCommands
            RSET -> do
                modify $ \s -> s { sessionBuffer = Nothing }
                sendReply (250, ["OK"])
                runCommands
            VRFY _ paramsText -> do
                params <- parseVrfyParams paramsText
                for params $ \param -> case param of
                    SMTPUTF8 -> do
                        return ()
                    _ -> do
                        throwError ParameterNotImplemented
                sendReply (252, ["Cannot VRFY user, but will accept message and attempt delivery"])
                runCommands
            NOOP -> do
                sendReply (250, ["OK"])
                runCommands
            QUIT -> do
                sendReply (221, [encodeUtf8 serverName <> " Service closing transmission session"])
                closeConnection
                return ()
            MAIL mboxText paramsText ->
                do
                    checkReadyForMail
                    returnPath <- parseMailbox mboxText
                    params <- parseMailParams paramsText
                    for params $ \param -> case param of
                        SIZE size -> do
                            when (size > serverMaxMsgSize) $ throwError MessageSizeExceeded
                        BODY _ -> do
                            return ()
                        SMTPUTF8 -> do
                            return ()
                        _ -> do
                            throwError ParameterNotImplemented
                    liftChat $ hookVerifyReturnPath returnPath
                    modify $ \s -> s { sessionBuffer = Just $ newBuffer returnPath }
                    sendReply (250, ["OK"])
                    runCommands
                `catchError` \e -> case e of
                    TemporaryFailure -> do
                        sendReply (450, ["Mailbox unavailable"])
                        runCommands
                    PermanentFailure -> do
                        sendReply (550, ["Mailbox unavailable"])
                        runCommands
                    _ -> do
                        throwError e
            RCPT mboxText paramsText ->
                do
                    buffer@Buffer{..} <- checkReadyForData
                    when (length rcpts >= serverMaxRcpts) $ throwError TooManyRecipients
                    rcpt <- parseMailbox mboxText
                    params <- parseRcptParams paramsText
                    for params $ \param -> case param of
                        _ -> do
                            throwError ParameterNotImplemented
                    liftChat $ hookVerifyRcpt rcpt
                    modify $ \s -> s { sessionBuffer = Just $ buffer { rcpts = rcpt:rcpts }}
                    sendReply (250, ["OK"])
                    runCommands
                `catchError` \e -> case e of
                    TemporaryFailure -> do
                        buffer@Buffer{..} <- gets sessionBuffer >>= maybe (throwError CommandOutOfOrder) return
                        modify $ \s -> s { sessionBuffer = Just $ buffer { failedRcpts = mboxText:failedRcpts }}
                        sendReply (450, ["Mailbox unavailable"])
                        runCommands
                    PermanentFailure -> do
                        buffer@Buffer{..} <- gets sessionBuffer >>= maybe (throwError CommandOutOfOrder) return
                        modify $ \s -> s { sessionBuffer = Just $ buffer { failedRcpts = mboxText:failedRcpts }}
                        sendReply (550, ["Mailbox unavailable"])
                        runCommands
                    _ -> do
                        buffer@Buffer{..} <- gets sessionBuffer >>= maybe (throwError CommandOutOfOrder) return
                        modify $ \s -> s { sessionBuffer = Just $ buffer { failedRcpts = mboxText:failedRcpts }}
                        throwError e
            DATA ->
                do
                    buffer <- checkReadyForData
                    sendReply (354, ["End data with <CR><LF>.<CR><LF>"])
                    messageText <- recvData serverMaxMsgSize
                    _ <- parseMessage messageText
                    liftChat $ hookSaveMessage buffer messageText
                    modify $ \s -> s { sessionBuffer = Nothing }
                    sendReply (250, ["OK"])
                    runCommands
                `catchError` \e -> case e of
                    TemporaryFailure -> do
                        sendReply (451, ["Transaction failed"])
                        runCommands
                    PermanentFailure -> do
                        sendReply (554, ["Transaction failed"])
                        runCommands
                    _ -> do
                        throwError e
        `catchError` \e -> case e of
            CommandParseFailure -> do
                sendReply (500, ["Syntax error, command unrecognized"])
                runCommands
            CommandSizeExceeded -> do
                sendReply (500, ["Syntax error, command unrecognized"])
                runCommands
            ParameterParseFailure -> do
                sendReply (501, ["Syntax error, command parameter or argument unrecognized"])
                runCommands
            CommandNotImplemented -> do
                sendReply (502, ["Command not implemented"])
                runCommands
            CommandOutOfOrder -> do
                sendReply (503, ["Bad sequence of commands"])
                runCommands
            ParameterNotImplemented -> do
                sendReply (504, ["Command parameter or argument not implemented"])
                runCommands
            MailboxParseFailure -> do
                sendReply (550, ["Syntax error, mailbox unrecognized"])
                runCommands
            MessageParseFailure -> do
                sendReply (550, ["Syntax error, message unrecognized"])
                runCommands
            MessageSizeExceeded -> do
                sendReply (552, ["Message exceeds fixed maximum message size"])
                runCommands
            ShutdownImminent -> do
                sendReply (421, [encodeUtf8 serverName <> " Service not available, shutting down"])
                runCommands
            TooManyRecipients -> do
                sendReply (452, ["Too many recipients"])
                runCommands
            Timeout -> do
                handleError e
            PeerConnectionClosed -> do
                handleError e
            _ -> do
                debug $ "unhandled exception: " <> (C.pack $ show e)
                sendReply (451, ["Requested action aborted, local error in processing"])
                runCommands
    checkReadyForMail :: Chat Session ()
    checkReadyForMail = do
        unlessM (liftChat isRunning) $ throwError ShutdownImminent
        unlessM (gets sessionInitialized) $ throwError CommandOutOfOrder
        unlessM (isNothing <$> gets sessionBuffer) $ throwError CommandOutOfOrder
    checkReadyForData :: Chat Session Buffer
    checkReadyForData = do
        unlessM (liftChat isRunning) $ throwError ShutdownImminent
        unlessM (gets sessionInitialized) $ throwError CommandOutOfOrder
        gets sessionBuffer >>= maybe (throwError CommandOutOfOrder) return

closeConnection :: Chat Session ()
closeConnection = do
    conn <- gets sessionConnection
    conn' <- liftChat $ Conn.close conn
    modify $ \s -> s { sessionConnection = conn' }

recvLine :: Chat Session T.Text
recvLine = do
    conn <- gets sessionConnection
    decodeUtf8 <$> recv conn False pLine 300 512 `catchError`
        \e -> case e of
            SizeExceeded -> throwError CommandSizeExceeded
            _            -> throwError e

recvData :: Int -> Chat Session T.Text
recvData maxMsgSize = do
    conn <- gets sessionConnection
    decodeUtf8 <$> recv conn True pBody 300 maxMsgSize `catchError`
        \e -> case e of
            SizeExceeded -> throwError MessageSizeExceeded
            _            -> throwError e

sendReply :: (Int, [B.ByteString]) -> Chat Session ()
sendReply (code, lines) = do
    conn <- gets sessionConnection
    send conn False $ LB.fromStrict $ format (code, lines)
  where
    format :: (Int, [B.ByteString]) -> B.ByteString
    format (_, [])            = "\r\n"
    format (code, line:[])    = (C.pack $ show code) <> " " <> line <> "\r\n"
    format (code, line:lines) = (C.pack $ show code) <> "-" <> line <> "\r\n" <> format (code, lines)

parseCommand :: T.Text -> Chat Session Command
parseCommand = either (const $ throwError CommandParseFailure) return . parseOnly (pCommand <* endOfInput)

parseVrfyParams :: T.Text -> Chat Session [Param]
parseVrfyParams = either (const $ throwError ParameterParseFailure) return . parseOnly (pVrfyParams <* endOfInput)

parseMailParams :: T.Text -> Chat Session [Param]
parseMailParams = either (const $ throwError ParameterParseFailure) return . parseOnly (pMailParams <* endOfInput)

parseRcptParams :: T.Text -> Chat Session [Param]
parseRcptParams = either (const $ throwError ParameterParseFailure) return . parseOnly (pRcptParams <* endOfInput)

parseMailbox :: T.Text -> Chat Session Mailbox
parseMailbox = either (const $ throwError MailboxParseFailure) return . parseOnly (pMailbox <* endOfInput)

parseMessage :: T.Text -> Chat Session Message
parseMessage = either (const $ throwError MessageParseFailure) return . parseOnly (pMessage <* endOfInput)
