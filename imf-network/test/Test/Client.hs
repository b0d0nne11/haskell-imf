{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{- HLINT ignore "Reduce duplication" -}

module Test.Client where

import           Control.Concurrent           (forkIO)
import           Control.Concurrent.MVar      (newEmptyMVar, putMVar, takeMVar)
import           Control.Concurrent.STM.TChan (TChan, isEmptyTChan, newTChanIO, readTChan,
                                               writeTChan)
import           Control.Exception.Safe       (bracket)
import           Control.Monad.IO.Class       (MonadIO)
import           Control.Monad.Logger         (LogLine, LoggingT, runLoggingT)
import           Control.Monad.Loops          (untilM)
import           Control.Monad.Reader         (runReaderT)
import           Control.Monad.STM            (atomically)
import qualified Data.ByteString.Lazy         as LB
import           Data.Text                    (Text)
import qualified Network.TLS                  as TLS
import           System.IO.Unsafe             (unsafePerformIO)
import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.IMF
import           Data.IMF.Network.Client
import           Data.IMF.Network.Connection

tests :: TestTree
tests = testGroup "client"
    [ testHello
    , testHelloFallback
    , testStartTLS
    , testAuthLogin
    , testAuthPlain
    , testDeliver
    , testQuit
    ]

clientTest :: (Client -> IO ()) -> (Connection -> IO ()) -> IO ()
clientTest clientAct serverAct = do
    sync <- newEmptyMVar
    _ <- forkIO $ server sync
    client sync
  where
    server sync = bracket (listen ("127.0.0.1", "2525")) close $ \conn -> do
        putMVar sync ()
        bracket (fst <$> accept conn) close serverAct
    client sync = do
        takeMVar sync
        connect ("0.0.0.0", "0") ("127.0.0.1", "2525") >>= newClient' "relay.example.com" >>= clientAct

newClient' :: Text -> Connection -> IO Client
newClient' name conn = do
    client <- newClient name conn
    return $ client
        { clientTLSParams = tlsClientParams "localhost" False
        }

testMessage :: LB.ByteString
testMessage = unsafePerformIO $ LB.readFile "./test/Fixtures/Messages/simple_addressing_1.txt"
{-# NOINLINE testMessage #-}

testCertificate :: TLS.Credential
testCertificate = unsafePerformIO $ TLS.credentialLoadX509 "./test/Fixtures/localhost.crt" "./test/Fixtures/localhost.key" >>= either fail return
{-# NOINLINE testCertificate #-}

runTChanLoggingT :: MonadIO m => TChan LogLine -> LoggingT m a -> m a
runTChanLoggingT chan = (`runLoggingT` \loc src lvl msg -> atomically $ writeTChan chan (loc, src, lvl, msg))

withLogging :: LoggingT IO a -> IO (a, [LogLine])
withLogging f = do
    chan <- newTChanIO
    a <- runTChanLoggingT chan f
    logs <- atomically $ readTChan chan `untilM` isEmptyTChan chan
    return (a, logs)

testHello = testCase "init (ehlo)" $
    clientTest clientAct serverAct
  where
    serverAct conn = do
        send conn "220 smtp.example.com ESMTP Postfix\r\n"
        recvLine conn >>= (@?= "EHLO relay.example.com\r")
        send conn "250-smtp.example.com\r\n250-SIZE 14680064\r\n250 HELP\r\n"
    clientAct client = do
        (ClientSession{..}, log) <- withLogging $ runReaderT setup client
        sessionExtentions @?= [("size",["14680064"]),("help",[])]
        isSecure (clientConnection client) >>= (@?= False)
        sessionUser @?= Nothing
        map (\(_, _, _, a) -> a) log @?=
            [ "< 220 smtp.example.com ESMTP Postfix\r"
            , "> EHLO relay.example.com\r"
            , "< 250-smtp.example.com\r"
            , "< 250-SIZE 14680064\r"
            , "< 250 HELP\r"
            ]

testHelloFallback = testCase "init (helo)" $
    clientTest clientAct serverAct
  where
    serverAct conn = do
        send conn "220 smtp.example.com ESMTP Postfix\r\n"
        recvLine conn >>= (@?= "EHLO relay.example.com\r")
        send conn "502 Command not implemented\r\n"
        recvLine conn >>= (@?= "HELO relay.example.com\r")
        send conn "250 smtp.example.com, I am glad to meet you\r\n"
    clientAct client = do
        (ClientSession{..}, log) <- withLogging $ runReaderT setup client
        sessionExtentions @?= []
        isSecure (clientConnection client) >>= (@?= False)
        sessionUser @?= Nothing
        map (\(_, _, _, a) -> a) log @?=
            [ "< 220 smtp.example.com ESMTP Postfix\r"
            , "> EHLO relay.example.com\r"
            , "< 502 Command not implemented\r"
            , "> HELO relay.example.com\r"
            , "< 250 smtp.example.com, I am glad to meet you\r"
            ]

testStartTLS = testCase "init (ehlo + starttls)" $
    clientTest clientAct serverAct
  where
    serverAct conn = do
        send conn "220 smtp.example.com ESMTP Postfix\r\n"
        recvLine conn >>= (@?= "EHLO relay.example.com\r")
        send conn "250-smtp.example.com\r\n250 STARTTLS\r\n"
        recvLine conn >>= (@?= "STARTTLS\r")
        send conn "220 Go ahead\r\n"
        secure conn $ tlsServerParams testCertificate
        recvLine conn >>= (@?= "EHLO relay.example.com\r")
        send conn "250-smtp.example.com\r\n250 STARTTLS\r\n"
    clientAct client = do
        (ClientSession{..}, log) <- withLogging $ runReaderT setup client
        sessionExtentions @?= [("starttls", [])]
        isSecure (clientConnection client) >>= (@?= True)
        sessionUser @?= Nothing
        map (\(_, _, _, a) -> a) log @?=
            [ "< 220 smtp.example.com ESMTP Postfix\r"
            , "> EHLO relay.example.com\r"
            , "< 250-smtp.example.com\r"
            , "< 250 STARTTLS\r"
            , "> STARTTLS\r"
            , "< 220 Go ahead\r"
            , "- [tls handshake]"
            , "> EHLO relay.example.com\r"
            , "< 250-smtp.example.com\r"
            , "< 250 STARTTLS\r"
            ]

testAuthLogin = testCase "init (ehlo + starttls + login)" $
    clientTest clientAct serverAct
  where
    serverAct conn = do
        send conn "220 smtp.example.com ESMTP Postfix\r\n"
        recvLine conn >>= (@?= "EHLO relay.example.com\r")
        send conn "250-smtp.example.com\r\n250 STARTTLS\r\n"
        recvLine conn >>= (@?= "STARTTLS\r")
        send conn "220 Go ahead\r\n"
        secure conn $ tlsServerParams testCertificate
        recvLine conn >>= (@?= "EHLO relay.example.com\r")
        send conn "250-smtp.example.com\r\n250-STARTTLS\r\n250 AUTH LOGIN\r\n"
        recvLine conn >>= (@?= "AUTH LOGIN\r")
        send conn "334 VXNlcm5hbWU6\r\n"
        recvLine conn >>= (@?= "dXNlcm5hbWU=\r")
        send conn "334 UGFzc3dvcmQ6\r\n"
        recvLine conn >>= (@?= "cGFzc3dvcmQ=\r")
        send conn "235 Authentication successful.\r\n"
    clientAct client = do
        (ClientSession{..}, log) <- withLogging $ runReaderT setup $ client { clientCredentials = Just ("username", "password") }
        sessionExtentions @?= [("starttls", []), ("auth", ["login"])]
        isSecure (clientConnection client) >>= (@?= True)
        sessionUser @?= Just "username"
        map (\(_, _, _, a) -> a) log @?=
            [ "< 220 smtp.example.com ESMTP Postfix\r"
            , "> EHLO relay.example.com\r"
            , "< 250-smtp.example.com\r"
            , "< 250 STARTTLS\r"
            , "> STARTTLS\r"
            , "< 220 Go ahead\r"
            , "- [tls handshake]"
            , "> EHLO relay.example.com\r"
            , "< 250-smtp.example.com\r"
            , "< 250-STARTTLS\r"
            , "< 250 AUTH LOGIN\r"
            , "> AUTH LOGIN\r"
            , "< 334 VXNlcm5hbWU6\r"
            , "> dXNlcm5hbWU=\r"
            , "< 334 UGFzc3dvcmQ6\r"
            , "> cGFzc3dvcmQ=\r"
            , "< 235 Authentication successful.\r"
            ]

testAuthPlain = testCase "init (ehlo + starttls + plain)" $
    clientTest clientAct serverAct
  where
    serverAct conn = do
        send conn "220 smtp.example.com ESMTP Postfix\r\n"
        recvLine conn >>= (@?= "EHLO relay.example.com\r")
        send conn "250-smtp.example.com\r\n250 STARTTLS\r\n"
        recvLine conn >>= (@?= "STARTTLS\r")
        send conn "220 Go ahead\r\n"
        secure conn $ tlsServerParams testCertificate
        recvLine conn >>= (@?= "EHLO relay.example.com\r")
        send conn "250-smtp.example.com\r\n250-STARTTLS\r\n250 AUTH PLAIN\r\n"
        recvLine conn >>= (@?= "AUTH PLAIN dXNlcm5hbWUAdXNlcm5hbWUAcGFzc3dvcmQ=\r")
        send conn "235 Authentication successful.\r\n"
    clientAct client = do
        (ClientSession{..}, log) <- withLogging $ runReaderT setup $ client { clientCredentials = Just ("username", "password") }
        sessionExtentions @?= [("starttls", []), ("auth", ["plain"])]
        isSecure (clientConnection client) >>= (@?= True)
        sessionUser @?= Just "username"
        map (\(_, _, _, a) -> a) log @?=
            [ "< 220 smtp.example.com ESMTP Postfix\r"
            , "> EHLO relay.example.com\r"
            , "< 250-smtp.example.com\r"
            , "< 250 STARTTLS\r"
            , "> STARTTLS\r"
            , "< 220 Go ahead\r"
            , "- [tls handshake]"
            , "> EHLO relay.example.com\r"
            , "< 250-smtp.example.com\r"
            , "< 250-STARTTLS\r"
            , "< 250 AUTH PLAIN\r"
            , "> AUTH PLAIN dXNlcm5hbWUAdXNlcm5hbWUAcGFzc3dvcmQ=\r"
            , "< 235 Authentication successful.\r"
            ]

testDeliver = testCase "deliver" $
    clientTest clientAct serverAct
  where
    serverAct conn = do
        recvLine conn >>= (@?= "MAIL FROM:<matt@localhost>\r")
        send conn "250 Ok\r\n"
        recvLine conn >>= (@?= "RCPT TO:<mary@localhost>\r")
        send conn "250 Ok\r\n"
        recvLine conn >>= (@?= "DATA\r")
        send conn "354 End data with <CR><LF>.<CR><LF>\r\n"
        _ <- recvData conn
        send conn "250 Ok: queued as 12345\r\n"
    clientAct client = do
        (_, log) <- withLogging $ runReaderT (deliver (Mailbox "" "matt" "localhost") [Mailbox "" "mary" "localhost"] testMessage) client
        map (\(_, _, _, a) -> a) log @?=
            [ "> MAIL FROM:<matt@localhost>\r"
            , "< 250 Ok\r"
            , "> RCPT TO:<mary@localhost>\r"
            , "< 250 Ok\r"
            , "> DATA\r"
            , "< 354 End data with <CR><LF>.<CR><LF>\r"
            , "> [224 bytes]"
            , "> .\r"
            , "< 250 Ok: queued as 12345\r"
            ]

testQuit = testCase "quit" $
    clientTest clientAct serverAct
  where
    serverAct conn = do
        recvLine conn >>= (@?= "QUIT\r")
        send conn "221 Bye\r\n"
    clientAct client = do
        (_, log) <- withLogging $ runReaderT quit client
        map (\(_, _, _, a) -> a) log @?=
            [ "> QUIT\r"
            , "< 221 Bye\r"
            , "- [closing connection]"
            ]
