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
import           Control.Monad                  ( forever
                                                , when
                                                )
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
import qualified Data.ByteString.Base64        as B64
import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.Lazy          as LB
import           Data.IORef                     ( newIORef
                                                , readIORef
                                                , writeIORef
                                                )
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

import           Data.IMF
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
    , serverMaxRcpts    :: Int
    , serverMaxMsgSize  :: Int
    , serverAuthReq     :: Bool
    }
  deriving (Eq, Show)

data Hooks = Hooks
    { hookLogSession       :: (Session, ChatLog) -> IO ()
    , hookVerifyReturnPath :: Mailbox -> IO ()
    , hookVerifyRcpt       :: Mailbox -> IO ()
    , hookSaveMessage      :: Buffer -> T.Text -> IO ()
    , hookAuthenticate     :: T.Text -> T.Text -> IO ()
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
    { sessionConnection      :: Conn.Connection
    , sessionError           :: ChatException
    , sessionCreatedAt       :: UTCTime
    , sessionClientName      :: T.Text
    , sessionInitialized     :: Bool
    , sessionAuthenticated   :: Bool
    , sessionBuffer          :: Maybe Buffer
    , sessionMsgsReceived    :: Int
    , sessionPendingReplies  :: [(Int, [B.ByteString])]
    }
  deriving Show

newSession :: Conn.Connection -> IO Session
newSession conn = do
    now <- getCurrentTime
    return Session
        { sessionConnection      = conn
        , sessionError           = OK
        , sessionCreatedAt       = now
        , sessionClientName      = ""
        , sessionInitialized     = False
        , sessionAuthenticated   = False
        , sessionBuffer          = Nothing
        , sessionMsgsReceived    = 0
        , sessionPendingReplies  = []
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
runSession ServerParams{..} Hooks{..} isRunning = runChat $ chat `catchError` closeSession
  where
    chat :: Chat Session ()
    chat = do
        saveReply (220, [encodeUtf8 serverName <> " Service ready"]) >> sendReplies
        forever $ (recvLines >>= runLines) `catchError` handleGenericError >> sendReplies

    closeSession :: ChatException -> Chat Session ()
    closeSession e = do
        if e == Shutdown
            then saveReply (221, [encodeUtf8 serverName <> " Service closing transmission session"]) >> sendReplies
            else debug $ "exception: " <> C.pack (show e)
        conn' <- gets sessionConnection >>= liftChat . Conn.close
        modify $ \s -> s { sessionConnection = conn'
                         , sessionError = e
                         }
        return ()

    runLines :: [T.Text] -> Chat Session ()
    runLines = foldl1 (>>) . map runLine

    runLine :: T.Text -> Chat Session ()
    runLine line = (parseCommand line >>= runCommand) `catchError` handleGenericError

    runCommand :: Command -> Chat Session ()
    runCommand (EHLO clientName) = do
        modify $ \s -> s { sessionInitialized = True
                         , sessionClientName = clientName
                         , sessionBuffer = Nothing
                         }
        saveReply ( 250
                  , [ encodeUtf8 serverName
                    , "SIZE " <> C.pack (show serverMaxMsgSize)
                    , "8BITMIME"
                    , "SMTPUTF8"
                    , "PIPELINING"
                    , "STARTTLS"
                    , "AUTH PLAIN LOGIN"
                    ]
                  )
    runCommand (HELO clientName) = do
        modify $ \s -> s { sessionInitialized = True
                         , sessionClientName  = clientName
                         , sessionBuffer = Nothing
                         }
        saveReply (250, ["OK"])
    runCommand RSET = (`catchError` handleGenericError) $ do
        modify $ \s -> s { sessionBuffer = Nothing }
        saveReply (250, ["OK"])
    runCommand (VRFY _ paramsText) = do
        params <- parseVrfyParams paramsText
        for params $ \param -> case param of
            SMTPUTF8 -> return ()
            _        -> throwError ParameterNotImplemented
        saveReply (252, ["Cannot VRFY user, but will accept message and attempt delivery"])
    runCommand NOOP =
        saveReply (250, ["OK"])
    runCommand QUIT =
        throwError Shutdown
    runCommand (MAIL mboxText paramsText) = (`catchError` handleMailError mboxText) $ do
        checkShutdownInProgress
        checkInitialized
        checkAuthenticated
        checkReadyForMail
        returnPath <- parseMailbox mboxText
        params <- parseMailParams paramsText
        for params $ \param -> case param of
            SIZE n   -> when (n > serverMaxMsgSize) $ throwError MessageSizeExceeded
            BODY _   -> return ()
            SMTPUTF8 -> return ()
            AUTHP _  -> return ()
            _        -> throwError ParameterNotImplemented
        liftChat $ hookVerifyReturnPath returnPath
        modify $ \s -> s { sessionBuffer = Just $ newBuffer returnPath }
        saveReply (250, ["Mailbox " <> encodeUtf8 mboxText <> " OK"])
    runCommand (RCPT mboxText paramsText) = (`catchError` handleRcptError mboxText) $ do
        checkShutdownInProgress
        checkInitialized
        checkAuthenticated
        buffer@Buffer{..} <- checkReadyForRcpt
        rcpt <- parseMailbox mboxText
        params <- parseRcptParams paramsText
        for params $ \param -> case param of
            _ -> throwError ParameterNotImplemented
        liftChat $ hookVerifyRcpt rcpt
        modify $ \s -> s { sessionBuffer = Just $ buffer { rcpts = rcpt:rcpts }}
        saveReply (250, ["Mailbox " <> encodeUtf8 mboxText <> " OK"])
    runCommand DATA = (`catchError` handleDataError) $ do
        -- data init
        checkShutdownInProgress
        checkInitialized
        checkAuthenticated
        buffer@Buffer{..} <- checkReadyForData
        saveReply (354, ["End data with <CR><LF>.<CR><LF>"])
        sendReplies
        -- data block
        messageText <- recvMessage serverMaxMsgSize
        -- data term
        _ <- parseMessage messageText
        liftChat $ hookSaveMessage buffer messageText
        modify $ \s -> s { sessionBuffer = Nothing
                         , sessionMsgsReceived = sessionMsgsReceived s + 1
                         }
        saveReply (250, ["OK"])
    runCommand STARTTLS = do
        -- starttls init
        checkShutdownInProgress
        checkInitialized
        saveReply (220, ["Go ahead"])
        sendReplies
        -- starttls negotiate
        debug "setting up tls"
        conn' <- gets sessionConnection >>= liftChat . Conn.secureServer
        modify $ \s -> s { sessionConnection = conn' }
    runCommand (AUTH paramsText) = (`catchError` handleAuthError) $ do
        checkShutdownInProgress
        checkInitialized
        checkSecureConnection
        param <- parseAuthParams paramsText
        case param of
            PLAIN "" -> do
                saveReply (334, ["Go ahead"])
                sendReplies
                creds <- recvCredentials >>= decodeCredentials
                case T.split (== '\0') creds of
                    [_, user, pass] -> liftChat $ hookAuthenticate user pass
                    _               -> throwError CredentialsParseFailure
            PLAIN initial -> do
                creds <- decodeCredentials initial
                case T.split (== '\0') creds of
                    [_, user, pass] -> liftChat $ hookAuthenticate user pass
                    _               -> throwError CredentialsParseFailure
            LOGIN "" -> do
                saveReply (334, ["VXNlcm5hbWU6"])
                sendReplies
                user <- recvCredentials >>= decodeCredentials
                saveReply (334, ["UGFzc3dvcmQ6"])
                sendReplies
                pass <- recvCredentials >>= decodeCredentials
                liftChat $ hookAuthenticate user pass
            LOGIN initial -> do
                user <- decodeCredentials initial
                saveReply (334, ["UGFzc3dvcmQ6"])
                sendReplies
                pass <- recvCredentials >>= decodeCredentials
                liftChat $ hookAuthenticate user pass
            _ ->
                throwError ParameterNotImplemented
        modify $ \s -> s { sessionAuthenticated = True }
        saveReply (235, ["Authentication successful"])

    checkShutdownInProgress :: Chat Session ()
    checkShutdownInProgress = unlessM (liftChat isRunning) $ throwError ShutdownInProgress

    checkInitialized :: Chat Session ()
    checkInitialized = unlessM (gets sessionInitialized) $ throwError CommandOutOfOrder

    checkAuthenticated :: Chat Session ()
    checkAuthenticated = when serverAuthReq $ unlessM (gets sessionAuthenticated) $ throwError AuthenticationRequired

    checkSecureConnection :: Chat Session ()
    checkSecureConnection = unlessM (Conn.isSecure <$> gets sessionConnection) $ throwError EncryptionRequired

    checkReadyForMail :: Chat Session ()
    checkReadyForMail = unlessM ((== Nothing) <$> gets sessionBuffer) $ throwError CommandOutOfOrder

    checkReadyForRcpt :: Chat Session Buffer
    checkReadyForRcpt = do
        buffer@Buffer{..} <- gets sessionBuffer >>= maybe (throwError CommandOutOfOrder) return
        when (length rcpts >= serverMaxRcpts) $ throwError TooManyRecipients
        return buffer

    checkReadyForData :: Chat Session Buffer
    checkReadyForData = do
        buffer@Buffer{..} <- gets sessionBuffer >>= maybe (throwError CommandOutOfOrder) return
        when (null rcpts) $ throwError NoValidRecipients
        return buffer

    handleGenericError :: ChatException -> Chat Session ()
    handleGenericError CommandParseFailure     = saveReply (500, ["Syntax error, command unrecognized"])
    handleGenericError CommandSizeExceeded     = saveReply (500, ["Syntax error, command unrecognized"])
    handleGenericError ParameterParseFailure   = saveReply (501, ["Syntax error, command parameter or argument unrecognized"])
    handleGenericError CredentialsParseFailure = saveReply (501, ["Syntax error, credentials unrecognized"])
    handleGenericError CredentialsSizeExceeded = saveReply (501, ["Syntax error, credentials unrecognized"])
    handleGenericError CommandNotImplemented   = saveReply (502, ["Command not implemented"])
    handleGenericError CommandOutOfOrder       = saveReply (503, ["Bad sequence of commands"])
    handleGenericError ParameterNotImplemented = saveReply (504, ["Command parameter or argument not implemented"])
    handleGenericError AuthenticationRequired  = saveReply (530, ["Authentication required"])
    handleGenericError EncryptionRequired      = saveReply (538, ["Encryption required"])
    handleGenericError MailboxParseFailure     = saveReply (550, ["Syntax error, mailbox unrecognized"])
    handleGenericError MessageParseFailure     = saveReply (550, ["Syntax error, message unrecognized"])
    handleGenericError MessageSizeExceeded     = saveReply (552, ["Message exceeds fixed maximum message size"])
    handleGenericError NoValidRecipients       = saveReply (554, ["No valid recipients"])
    handleGenericError ShutdownInProgress      = saveReply (421, [encodeUtf8 serverName <> " Service not available, shutting down"])
    handleGenericError TooManyRecipients       = saveReply (452, ["Too many recipients"])
    handleGenericError e                       = throwError e

    handleMailError :: T.Text -> ChatException -> Chat Session ()
    handleMailError mboxText TemporaryFailure    = saveReply (450, ["Mailbox " <> encodeUtf8 mboxText <> " unavailable"])
    handleMailError mboxText PermanentFailure    = saveReply (550, ["Mailbox " <> encodeUtf8 mboxText <> " unavailable"])
    handleMailError mboxText MailboxParseFailure = saveReply (550, ["Syntax error, mailbox " <> encodeUtf8 mboxText <> " unrecognized"])
    handleMailError _        e                   = throwError e

    handleRcptError :: T.Text -> ChatException -> Chat Session ()
    handleRcptError mboxText TemporaryFailure = do
        buffer@Buffer{..} <- gets sessionBuffer >>= maybe (throwError CommandOutOfOrder) return
        modify $ \s -> s { sessionBuffer = Just $ buffer { failedRcpts = mboxText:failedRcpts }}
        saveReply (450, ["Mailbox " <> encodeUtf8 mboxText <> " unavailable"])
    handleRcptError mboxText PermanentFailure = do
        buffer@Buffer{..} <- gets sessionBuffer >>= maybe (throwError CommandOutOfOrder) return
        modify $ \s -> s { sessionBuffer = Just $ buffer { failedRcpts = mboxText:failedRcpts }}
        saveReply (550, ["Mailbox " <> encodeUtf8 mboxText <> " unavailable"])
    handleRcptError mboxText MailboxParseFailure = do
        buffer@Buffer{..} <- gets sessionBuffer >>= maybe (throwError CommandOutOfOrder) return
        modify $ \s -> s { sessionBuffer = Just $ buffer { failedRcpts = mboxText:failedRcpts }}
        saveReply (550, ["Syntax error, mailbox " <> encodeUtf8 mboxText <> " unrecognized"])
    handleRcptError mboxText e = do
        throwError e

    handleDataError :: ChatException -> Chat Session ()
    handleDataError TemporaryFailure = saveReply (451, ["Requested action aborted, local error in processing"])
    handleDataError PermanentFailure = saveReply (554, ["Transaction failed"])
    handleDataError e                = throwError e

    handleAuthError :: ChatException -> Chat Session ()
    handleAuthError TemporaryFailure = saveReply (454, ["Temporary authentication failure"])
    handleAuthError PermanentFailure = saveReply (535, ["Authentication failed"])
    handleAuthError e                = throwError e

    recvLines :: Chat Session [T.Text]
    recvLines = do
        conn <- gets sessionConnection
        recv False pLine 300 102400 conn `catchError`
            \e -> case e of
                ParseFailure -> throwError CommandParseFailure
                SizeExceeded -> throwError CommandSizeExceeded
                _            -> throwError e

    recvMessage :: Int -> Chat Session T.Text
    recvMessage maxMsgSize = do
        conn <- gets sessionConnection
        T.concat <$> recv True pBody 600 maxMsgSize conn `catchError`
            \e -> case e of
                ParseFailure -> throwError MessageParseFailure
                SizeExceeded -> throwError MessageSizeExceeded
                _            -> throwError e

    recvCredentials :: Chat Session T.Text
    recvCredentials = do
        conn <- gets sessionConnection
        T.concat <$> recv True pLine 300 1024 conn `catchError`
            \e -> case e of
                ParseFailure -> throwError CredentialsParseFailure
                SizeExceeded -> throwError CredentialsSizeExceeded
                _            -> throwError e

    decodeCredentials :: T.Text -> Chat Session T.Text
    decodeCredentials "=" = return ""
    decodeCredentials creds =
        case B64.decode $ encodeUtf8 creds of
            Left _      -> throwError CredentialsParseFailure
            Right creds -> return $ decodeUtf8 creds

    saveReply :: (Int, [B.ByteString]) -> Chat Session ()
    saveReply r = do
        rs <- gets sessionPendingReplies
        modify $ \s -> s { sessionPendingReplies = rs ++ [r] }

    sendReplies :: Chat Session ()
    sendReplies = do
        rs <- gets sessionPendingReplies
        modify $ \s -> s { sessionPendingReplies = [] }
        conn <- gets sessionConnection
        send conn False $ LB.fromStrict $ B.concat $ map formatReply rs

    formatReply :: (Int, [B.ByteString]) -> B.ByteString
    formatReply (code, [])         = C.pack (show code) <> " " <> "\r\n"
    formatReply (code, line:[])    = C.pack (show code) <> " " <> line <> "\r\n"
    formatReply (code, line:lines) = C.pack (show code) <> "-" <> line <> "\r\n" <> formatReply (code, lines)

    parseCommand :: T.Text -> Chat Session Command
    parseCommand = either (const $ throwError CommandParseFailure) return . parseOnly (pCommand <* endOfInput)

    parseVrfyParams :: T.Text -> Chat Session [Param]
    parseVrfyParams = either (const $ throwError ParameterParseFailure) return . parseOnly (pVrfyParams <* endOfInput)

    parseMailParams :: T.Text -> Chat Session [Param]
    parseMailParams = either (const $ throwError ParameterParseFailure) return . parseOnly (pMailParams <* endOfInput)

    parseRcptParams :: T.Text -> Chat Session [Param]
    parseRcptParams = either (const $ throwError ParameterParseFailure) return . parseOnly (pRcptParams <* endOfInput)

    parseAuthParams :: T.Text -> Chat Session Param
    parseAuthParams = either (const $ throwError ParameterParseFailure) return . parseOnly (pAuthParams <* endOfInput)

    parseMailbox :: T.Text -> Chat Session Mailbox
    parseMailbox = either (const $ throwError MailboxParseFailure) return . parseOnly (pMailbox <* endOfInput)

    parseMessage :: T.Text -> Chat Session Message
    parseMessage = either (const $ throwError MessageParseFailure) return . parseOnly (pMessage <* endOfInput)
