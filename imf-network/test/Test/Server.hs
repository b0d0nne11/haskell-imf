{-# LANGUAGE OverloadedStrings #-}

{- HLINT ignore "Reduce duplication" -}

module Test.Server where

import           Control.Concurrent           (forkIO)
import           Control.Concurrent.MVar      (newEmptyMVar, putMVar, takeMVar)
import           Control.Concurrent.STM.TChan (TChan, isEmptyTChan, newTChanIO, readTChan,
                                               writeTChan)
import           Control.Exception.Safe       (bracket, try)
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

import           Data.IMF.Network.Connection
import           Data.IMF.Network.Server
import Control.Monad.IO.Class

tests :: TestTree
tests = testGroup "server"
    [ testConnect
    , testSimpleChat
    , testPipelinedChat
    , testSecuredChat
    , testAuthLogin
    , testAuthLoginInit
    , testAuthPlain
    , testAuthPlainInit
    , testPeerConnectionClosed
    , testCommandUnrecognized
    , testCommandOutOfOrder
    , testParameterNotImplemented
    , testReturnPathUnrecognized
    , testReturnPathUnavailable
    , testRecipientUnrecognized
    , testRecipientUnavailable
    , testTooManyRecipients
    , testMessageUnrecognized
    , testMessageTooLarge
    , testMessageTansactionFailed
    , testNoValidRecipients
    , testCredentialsUnrecognized
    , testCredentialsInvalid
    , testAuthenticationRequired
    , testEncryptionRequired
    ]

serverTest :: (Connection -> IO ()) -> (Server -> IO ()) -> IO ()
serverTest clientAct serverAct = do
    sync <- newEmptyMVar
    _ <- forkIO $ client sync
    server sync
  where
    server sync = bracket (listen ("127.0.0.1", "2525")) close $ \conn -> do
        putMVar sync ()
        accept conn >>= newServer' "mx1.example.com" . fst >>= serverAct
    client sync = do
        takeMVar sync
        bracket (connect ("0.0.0.0", "0") ("127.0.0.1", "2525")) close clientAct

newServer' :: Text -> Connection -> IO Server
newServer' name conn = do
    server <- newServer name conn
    return $ server
        { serverTLSParams        = tlsServerParams testCertificate
        , serverAuthenticate     = \_ _ -> return Pass
        , serverVerifyReturnPath = \_ -> return Pass
        , serverVerifyRecipient  = \_ -> return Pass
        , serverAcceptMessage    = \_ _ _ -> return Pass
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

testConnect = testCase "connect/disconnect" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        recvReply conn >>= (@?= 220) . fst . snd
        send conn "QUIT\r\n"
        recvReply conn >>= (@?= 221) . fst . snd
    serverAct server = do
        (_, log) <- withLogging $ runReaderT run server
        map (\(_, _, _, a) -> a) log @?=
            [ "> 220 mx1.example.com Service ready\r"
            , "< QUIT\r"
            , "> 221 mx1.example.com Service closing transmission session\r"
            , "- [closing connection]"
            ]

testSimpleChat = testCase "simple chat" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        recvReply conn >>= (@?= 220) . fst . snd
        send conn "EHLO relay.example.com\r\n"
        recvReply conn >>= (@?= 250) . fst . snd
        send conn "MAIL FROM:<no-reply@example.com>\r\n"
        recvReply conn >>= (@?= 250) . fst . snd
        send conn "RCPT TO:<jsmith@example.com>\r\n"
        recvReply conn >>= (@?= 250) . fst . snd
        send conn "DATA\r\n"
        recvReply conn >>= (@?= 354) . fst . snd
        send conn testMessage
        send conn "\r\n.\r\n"
        recvReply conn >>= (@?= 250) . fst . snd
        send conn "QUIT\r\n"
        recvReply conn >>= (@?= 221) . fst . snd
    serverAct server = do
        (_, log) <- withLogging $ runReaderT run server
        map (\(_, _, _, a) -> a) log @?=
            [ "> 220 mx1.example.com Service ready\r"
            , "< EHLO relay.example.com\r"
            , "> 250-mx1.example.com\r"
            , "> 250-SIZE 4096\r"
            , "> 250-8BITMIME\r"
            , "> 250-SMTPUTF8\r"
            , "> 250-PIPELINING\r"
            , "> 250 STARTTLS\r"
            , "< MAIL FROM:<no-reply@example.com>\r"
            , "> 250 Mailbox <no-reply@example.com> OK\r"
            , "< RCPT TO:<jsmith@example.com>\r"
            , "> 250 Mailbox <jsmith@example.com> OK\r"
            , "< DATA\r"
            , "> 354 End data with <CR><LF>.<CR><LF>\r"
            , "< [226 bytes]"
            , "> 250 OK\r"
            , "< QUIT\r"
            , "> 221 mx1.example.com Service closing transmission session\r"
            , "- [closing connection]"
            ]

testPipelinedChat = testCase "pipelined chat" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        recvReply conn >>= (@?= 220) . fst . snd
        send conn "EHLO relay.example.com\r\n"
        recvReply conn >>= (@?= 250) . fst . snd
        send conn "MAIL FROM:<no-reply@example.com>\r\nRCPT TO:<jsmith@example.com>\r\nDATA\r\n"
        recvReply conn >>= (@?= 250) . fst . snd
        recvReply conn >>= (@?= 250) . fst . snd
        recvReply conn >>= (@?= 354) . fst . snd
        send conn testMessage
        send conn "\r\n.\r\n"
        recvReply conn >>= (@?= 250) . fst . snd
        send conn "QUIT\r\n"
        recvReply conn >>= (@?= 221) . fst . snd
    serverAct server = do
        (_, log) <- withLogging $ runReaderT run server
        map (\(_, _, _, a) -> a) log @?=
            [ "> 220 mx1.example.com Service ready\r"
            , "< EHLO relay.example.com\r"
            , "> 250-mx1.example.com\r"
            , "> 250-SIZE 4096\r"
            , "> 250-8BITMIME\r"
            , "> 250-SMTPUTF8\r"
            , "> 250-PIPELINING\r"
            , "> 250 STARTTLS\r"
            , "< MAIL FROM:<no-reply@example.com>\r"
            , "< RCPT TO:<jsmith@example.com>\r"
            , "< DATA\r"
            , "> 250 Mailbox <no-reply@example.com> OK\r"
            , "> 250 Mailbox <jsmith@example.com> OK\r"
            , "> 354 End data with <CR><LF>.<CR><LF>\r"
            , "< [226 bytes]"
            , "> 250 OK\r"
            , "< QUIT\r"
            , "> 221 mx1.example.com Service closing transmission session\r"
            , "- [closing connection]"
            ]

testSecuredChat = testCase "secured chat" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        recvReply conn >>= (@?= 220) . fst . snd
        send conn "EHLO relay.example.com\r\n"
        recvReply conn >>= (@?= 250) . fst . snd
        send conn "STARTTLS\r\n"
        recvReply conn >>= (@?= 220) . fst . snd
        secure conn $ tlsClientParams "localhost" False
        send conn "EHLO relay.example.com\r\n"
        recvReply conn >>= (@?= 250) . fst . snd
        send conn "MAIL FROM:<no-reply@example.com>\r\n"
        recvReply conn >>= (@?= 250) . fst . snd
        send conn "RCPT TO:<jsmith@example.com>\r\n"
        recvReply conn >>= (@?= 250) . fst . snd
        send conn "DATA\r\n"
        recvReply conn >>= (@?= 354) . fst . snd
        send conn testMessage
        send conn "\r\n.\r\n"
        recvReply conn >>= (@?= 250) . fst . snd
        send conn "QUIT\r\n"
        recvReply conn >>= (@?= 221) . fst . snd
    serverAct server = do
        (_, log) <- withLogging $ runReaderT run server
        map (\(_, _, _, a) -> a) log @?=
            [ "> 220 mx1.example.com Service ready\r"
            , "< EHLO relay.example.com\r"
            , "> 250-mx1.example.com\r"
            , "> 250-SIZE 4096\r"
            , "> 250-8BITMIME\r"
            , "> 250-SMTPUTF8\r"
            , "> 250-PIPELINING\r"
            , "> 250 STARTTLS\r"
            , "< STARTTLS\r"
            , "> 220 Go ahead\r"
            , "- [tls handshake]"
            , "< EHLO relay.example.com\r"
            , "> 250-mx1.example.com\r"
            , "> 250-SIZE 4096\r"
            , "> 250-8BITMIME\r"
            , "> 250-SMTPUTF8\r"
            , "> 250-PIPELINING\r"
            , "> 250 AUTH PLAIN LOGIN\r"
            , "< MAIL FROM:<no-reply@example.com>\r"
            , "> 250 Mailbox <no-reply@example.com> OK\r"
            , "< RCPT TO:<jsmith@example.com>\r"
            , "> 250 Mailbox <jsmith@example.com> OK\r"
            , "< DATA\r"
            , "> 354 End data with <CR><LF>.<CR><LF>\r"
            , "< [226 bytes]"
            , "> 250 OK\r"
            , "< QUIT\r"
            , "> 221 mx1.example.com Service closing transmission session\r"
            , "- [closing connection]"
            ]

testAuthPlain = testCase "authenticate (plain)" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        recvReply conn >>= (@?= 220) . fst . snd
        send conn "EHLO relay.example.com\r\n"
        recvReply conn >>= (@?= 250) . fst . snd
        send conn "STARTTLS\r\n"
        recvReply conn >>= (@?= 220) . fst . snd
        secure conn $ tlsClientParams "localhost" False
        send conn "EHLO relay.example.com\r\n"
        recvReply conn >>= (@?= 250) . fst . snd
        send conn "AUTH PLAIN\r\n"
        recvReply conn >>= (@?= 334) . fst . snd
        send conn "Zm9vAGJhcgBiYXI=\r\n"
        recvReply conn >>= (@?= 235) . fst . snd
        send conn "QUIT\r\n"
        recvReply conn >>= (@?= 221) . fst . snd
    serverAct server = do
        (_, log) <- withLogging $ runReaderT run server
        map (\(_, _, _, a) -> a) log @?=
            [ "> 220 mx1.example.com Service ready\r"
            , "< EHLO relay.example.com\r"
            , "> 250-mx1.example.com\r"
            , "> 250-SIZE 4096\r"
            , "> 250-8BITMIME\r"
            , "> 250-SMTPUTF8\r"
            , "> 250-PIPELINING\r"
            , "> 250 STARTTLS\r"
            , "< STARTTLS\r"
            , "> 220 Go ahead\r"
            , "- [tls handshake]"
            , "< EHLO relay.example.com\r"
            , "> 250-mx1.example.com\r"
            , "> 250-SIZE 4096\r"
            , "> 250-8BITMIME\r"
            , "> 250-SMTPUTF8\r"
            , "> 250-PIPELINING\r"
            , "> 250 AUTH PLAIN LOGIN\r"
            , "< AUTH PLAIN\r"
            , "> 334 Go ahead\r"
            , "< [...]"
            , "> 235 Authentication succeeded\r"
            , "< QUIT\r"
            , "> 221 mx1.example.com Service closing transmission session\r"
            , "- [closing connection]"
            ]

testAuthPlainInit = testCase "authenticate (plain with initial)" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        recvReply conn >>= (@?= 220) . fst . snd
        send conn "EHLO relay.example.com\r\n"
        recvReply conn >>= (@?= 250) . fst . snd
        send conn "STARTTLS\r\n"
        recvReply conn >>= (@?= 220) . fst . snd
        secure conn $ tlsClientParams "localhost" False
        send conn "EHLO relay.example.com\r\n"
        recvReply conn >>= (@?= 250) . fst . snd
        send conn "AUTH PLAIN Zm9vAGJhcgBiYXI=\r\n"
        recvReply conn >>= (@?= 235) . fst . snd
        send conn "QUIT\r\n"
        recvReply conn >>= (@?= 221) . fst . snd
        return ()
    serverAct server = do
        (_, log) <- withLogging $ runReaderT run server
        map (\(_, _, _, a) -> a) log @?=
            [ "> 220 mx1.example.com Service ready\r"
            , "< EHLO relay.example.com\r"
            , "> 250-mx1.example.com\r"
            , "> 250-SIZE 4096\r"
            , "> 250-8BITMIME\r"
            , "> 250-SMTPUTF8\r"
            , "> 250-PIPELINING\r"
            , "> 250 STARTTLS\r"
            , "< STARTTLS\r"
            , "> 220 Go ahead\r"
            , "- [tls handshake]"
            , "< EHLO relay.example.com\r"
            , "> 250-mx1.example.com\r"
            , "> 250-SIZE 4096\r"
            , "> 250-8BITMIME\r"
            , "> 250-SMTPUTF8\r"
            , "> 250-PIPELINING\r"
            , "> 250 AUTH PLAIN LOGIN\r"
            , "< AUTH PLAIN Zm9vAGJhcgBiYXI=\r"
            , "> 235 Authentication succeeded\r"
            , "< QUIT\r"
            , "> 221 mx1.example.com Service closing transmission session\r"
            , "- [closing connection]"
            ]

testAuthLogin = testCase "authenticate (login)" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        recvReply conn >>= (@?= 220) . fst . snd
        send conn "EHLO relay.example.com\r\n"
        recvReply conn >>= (@?= 250) . fst . snd
        send conn "STARTTLS\r\n"
        recvReply conn >>= (@?= 220) . fst . snd
        secure conn $ tlsClientParams "localhost" False
        send conn "EHLO relay.example.com\r\n"
        recvReply conn >>= (@?= 250) . fst . snd
        send conn "AUTH LOGIN\r\n"
        recvReply conn >>= (@?= 334) . fst . snd
        send conn "Zm9v\r\n"
        recvReply conn >>= (@?= 334) . fst . snd
        send conn "YmFy\r\n"
        recvReply conn >>= (@?= 235) . fst . snd
        send conn "QUIT\r\n"
        recvReply conn >>= (@?= 221) . fst . snd
    serverAct server = do
        (_, log) <- withLogging $ runReaderT run server
        map (\(_, _, _, a) -> a) log @?=
            [ "> 220 mx1.example.com Service ready\r"
            , "< EHLO relay.example.com\r"
            , "> 250-mx1.example.com\r"
            , "> 250-SIZE 4096\r"
            , "> 250-8BITMIME\r"
            , "> 250-SMTPUTF8\r"
            , "> 250-PIPELINING\r"
            , "> 250 STARTTLS\r"
            , "< STARTTLS\r"
            , "> 220 Go ahead\r"
            , "- [tls handshake]"
            , "< EHLO relay.example.com\r"
            , "> 250-mx1.example.com\r"
            , "> 250-SIZE 4096\r"
            , "> 250-8BITMIME\r"
            , "> 250-SMTPUTF8\r"
            , "> 250-PIPELINING\r"
            , "> 250 AUTH PLAIN LOGIN\r"
            , "< AUTH LOGIN\r"
            , "> 334 VXNlcm5hbWU6\r"
            , "< [...]"
            , "> 334 UGFzc3dvcmQ6\r"
            , "< [...]"
            , "> 235 Authentication succeeded\r"
            , "< QUIT\r"
            , "> 221 mx1.example.com Service closing transmission session\r"
            , "- [closing connection]"
            ]

testAuthLoginInit = testCase "authenticate (login with initial)" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        recvReply conn >>= (@?= 220) . fst . snd
        send conn "EHLO relay.example.com\r\n"
        recvReply conn >>= (@?= 250) . fst . snd
        send conn "STARTTLS\r\n"
        recvReply conn >>= (@?= 220) . fst . snd
        secure conn $ tlsClientParams "localhost" False
        send conn "EHLO relay.example.com\r\n"
        recvReply conn >>= (@?= 250) . fst . snd
        send conn "AUTH LOGIN Zm9v\r\n"
        recvReply conn >>= (@?= 334) . fst . snd
        send conn "YmFy\r\n"
        recvReply conn >>= (@?= 235) . fst . snd
        send conn "QUIT\r\n"
        recvReply conn >>= (@?= 221) . fst . snd
    serverAct server = do
        (_, log) <- withLogging $ runReaderT run server
        map (\(_, _, _, a) -> a) log @?=
            [ "> 220 mx1.example.com Service ready\r"
            , "< EHLO relay.example.com\r"
            , "> 250-mx1.example.com\r"
            , "> 250-SIZE 4096\r"
            , "> 250-8BITMIME\r"
            , "> 250-SMTPUTF8\r"
            , "> 250-PIPELINING\r"
            , "> 250 STARTTLS\r"
            , "< STARTTLS\r"
            , "> 220 Go ahead\r"
            , "- [tls handshake]"
            , "< EHLO relay.example.com\r"
            , "> 250-mx1.example.com\r"
            , "> 250-SIZE 4096\r"
            , "> 250-8BITMIME\r"
            , "> 250-SMTPUTF8\r"
            , "> 250-PIPELINING\r"
            , "> 250 AUTH PLAIN LOGIN\r"
            , "< AUTH LOGIN Zm9v\r"
            , "> 334 UGFzc3dvcmQ6\r"
            , "< [...]"
            , "> 235 Authentication succeeded\r"
            , "< QUIT\r"
            , "> 221 mx1.example.com Service closing transmission session\r"
            , "- [closing connection]"
            ]

testPeerConnectionClosed = testCase "peer connection closed" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        recvReply conn >>= (@?= 220) . fst . snd
    serverAct server = do
        (a, log) <- withLogging $ runReaderT (try run) server
        a @?= Left PeerConnectionClosed
        map (\(_, _, _, a) -> a) log @?=
            [ "> 220 mx1.example.com Service ready\r"
            ]

testCommandUnrecognized = testCase "command unrecognized" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        recvReply conn >>= (@?= 220) . fst . snd
        send conn "FOO\r\n"
        recvReply conn >>= (@?= 500) . fst . snd
        send conn "QUIT\r\n"
        recvReply conn >>= (@?= 221) . fst . snd
    serverAct server = do
        (_, log) <- withLogging $ runReaderT run server
        map (\(_, _, _, a) -> a) log @?=
            [ "> 220 mx1.example.com Service ready\r"
            , "< FOO\r"
            , "> 500 Syntax error, command unrecognized\r"
            , "< QUIT\r"
            , "> 221 mx1.example.com Service closing transmission session\r"
            , "- [closing connection]"
            ]

testCommandOutOfOrder = testCase "command out of order" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        recvReply conn >>= (@?= 220) . fst . snd
        send conn "MAIL FROM:<no-reply@example.com>\r\n"
        recvReply conn >>= (@?= 503) . fst . snd
        send conn "QUIT\r\n"
        recvReply conn >>= (@?= 221) . fst . snd
    serverAct server = do
        (_, log) <- withLogging $ runReaderT run server
        map (\(_, _, _, a) -> a) log @?=
            [ "> 220 mx1.example.com Service ready\r"
            , "< MAIL FROM:<no-reply@example.com>\r"
            , "> 503 Bad sequence of commands\r"
            , "< QUIT\r"
            , "> 221 mx1.example.com Service closing transmission session\r"
            , "- [closing connection]"
            ]

testParameterNotImplemented = testCase "parameter not implemented" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        recvReply conn >>= (@?= 220) . fst . snd
        send conn "EHLO relay.example.com\r\n"
        recvReply conn >>= (@?= 250) . fst . snd
        send conn "MAIL FROM:<no-reply@example.com> FOO=BAR\r\n"
        recvReply conn >>= (@?= 504) . fst . snd
        send conn "QUIT\r\n"
        recvReply conn >>= (@?= 221) . fst . snd
    serverAct server = do
        (_, log) <- withLogging $ runReaderT run server
        map (\(_, _, _, a) -> a) log @?=
            [ "> 220 mx1.example.com Service ready\r"
            , "< EHLO relay.example.com\r"
            , "> 250-mx1.example.com\r"
            , "> 250-SIZE 4096\r"
            , "> 250-8BITMIME\r"
            , "> 250-SMTPUTF8\r"
            , "> 250-PIPELINING\r"
            , "> 250 STARTTLS\r"
            , "< MAIL FROM:<no-reply@example.com> FOO=BAR\r"
            , "> 504 Command parameter or argument not implemented\r"
            , "< QUIT\r"
            , "> 221 mx1.example.com Service closing transmission session\r"
            , "- [closing connection]"
            ]

testReturnPathUnrecognized = testCase "return path mailbox unrecognized" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        recvReply conn >>= (@?= 220) . fst . snd
        send conn "EHLO relay.example.com\r\n"
        recvReply conn >>= (@?= 250) . fst . snd
        send conn "MAIL FROM:<foo>\r\n"
        recvReply conn >>= (@?= 550) . fst . snd
        send conn "QUIT\r\n"
        recvReply conn >>= (@?= 221) . fst . snd
    serverAct server = do
        (_, log) <- withLogging $ runReaderT run $
            server { serverVerifyReturnPath = \_ -> return PermFail }
        map (\(_, _, _, a) -> a) log @?=
            [ "> 220 mx1.example.com Service ready\r"
            , "< EHLO relay.example.com\r"
            , "> 250-mx1.example.com\r"
            , "> 250-SIZE 4096\r"
            , "> 250-8BITMIME\r"
            , "> 250-SMTPUTF8\r"
            , "> 250-PIPELINING\r"
            , "> 250 STARTTLS\r"
            , "< MAIL FROM:<foo>\r"
            , "> 550 Syntax error, mailbox <foo> unrecognized\r"
            , "< QUIT\r"
            , "> 221 mx1.example.com Service closing transmission session\r"
            , "- [closing connection]"
            ]

testReturnPathUnavailable = testCase "return path mailbox unavailable" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        recvReply conn >>= (@?= 220) . fst . snd
        send conn "EHLO relay.example.com\r\n"
        recvReply conn >>= (@?= 250) . fst . snd
        send conn "MAIL FROM:<no-reply@example.com>\r\n"
        recvReply conn >>= (@?= 550) . fst . snd
        send conn "QUIT\r\n"
        recvReply conn >>= (@?= 221) . fst . snd
    serverAct server = do
        (_, log) <- withLogging $ runReaderT run $
            server { serverVerifyReturnPath = \_ -> return PermFail }
        map (\(_, _, _, a) -> a) log @?=
            [ "> 220 mx1.example.com Service ready\r"
            , "< EHLO relay.example.com\r"
            , "> 250-mx1.example.com\r"
            , "> 250-SIZE 4096\r"
            , "> 250-8BITMIME\r"
            , "> 250-SMTPUTF8\r"
            , "> 250-PIPELINING\r"
            , "> 250 STARTTLS\r"
            , "< MAIL FROM:<no-reply@example.com>\r"
            , "> 550 Mailbox <no-reply@example.com> unavailable\r"
            , "< QUIT\r"
            , "> 221 mx1.example.com Service closing transmission session\r"
            , "- [closing connection]"
            ]

testRecipientUnrecognized = testCase "recipient mailbox unrecognized" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        recvReply conn >>= (@?= 220) . fst . snd
        send conn "EHLO relay.example.com\r\n"
        recvReply conn >>= (@?= 250) . fst . snd
        send conn "MAIL FROM:<no-reply@example.com>\r\n"
        recvReply conn >>= (@?= 250) . fst . snd
        send conn "RCPT TO:<foo>\r\n"
        recvReply conn >>= (@?= 550) . fst . snd
        send conn "QUIT\r\n"
        recvReply conn >>= (@?= 221) . fst . snd
    serverAct server = do
        (_, log) <- withLogging $ runReaderT run $
            server { serverVerifyRecipient = \_ -> return PermFail }
        map (\(_, _, _, a) -> a) log @?=
            [ "> 220 mx1.example.com Service ready\r"
            , "< EHLO relay.example.com\r"
            , "> 250-mx1.example.com\r"
            , "> 250-SIZE 4096\r"
            , "> 250-8BITMIME\r"
            , "> 250-SMTPUTF8\r"
            , "> 250-PIPELINING\r"
            , "> 250 STARTTLS\r"
            , "< MAIL FROM:<no-reply@example.com>\r"
            , "> 250 Mailbox <no-reply@example.com> OK\r"
            , "< RCPT TO:<foo>\r"
            , "> 550 Syntax error, mailbox <foo> unrecognized\r"
            , "< QUIT\r"
            , "> 221 mx1.example.com Service closing transmission session\r"
            , "- [closing connection]"
            ]

testRecipientUnavailable = testCase "recipient mailbox unavailable" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        recvReply conn >>= (@?= 220) . fst . snd
        send conn "EHLO relay.example.com\r\n"
        recvReply conn >>= (@?= 250) . fst . snd
        send conn "MAIL FROM:<no-reply@example.com>\r\n"
        recvReply conn >>= (@?= 250) . fst . snd
        send conn "RCPT TO:<jsmith@example.com>\r\n"
        recvReply conn >>= (@?= 550) . fst . snd
        send conn "QUIT\r\n"
        recvReply conn >>= (@?= 221) . fst . snd
    serverAct server = do
        (_, log) <- withLogging $ runReaderT run $
            server { serverVerifyRecipient = \_ -> return PermFail }
        map (\(_, _, _, a) -> a) log @?=
            [ "> 220 mx1.example.com Service ready\r"
            , "< EHLO relay.example.com\r"
            , "> 250-mx1.example.com\r"
            , "> 250-SIZE 4096\r"
            , "> 250-8BITMIME\r"
            , "> 250-SMTPUTF8\r"
            , "> 250-PIPELINING\r"
            , "> 250 STARTTLS\r"
            , "< MAIL FROM:<no-reply@example.com>\r"
            , "> 250 Mailbox <no-reply@example.com> OK\r"
            , "< RCPT TO:<jsmith@example.com>\r"
            , "> 550 Mailbox <jsmith@example.com> unavailable\r"
            , "< QUIT\r"
            , "> 221 mx1.example.com Service closing transmission session\r"
            , "- [closing connection]"
            ]

testTooManyRecipients = testCase "too many recipients" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        recvReply conn >>= (@?= 220) . fst . snd
        send conn "EHLO relay.example.com\r\n"
        recvReply conn >>= (@?= 250) . fst . snd
        send conn "MAIL FROM:<no-reply@example.com>\r\n"
        recvReply conn >>= (@?= 250) . fst . snd
        send conn "RCPT TO:<jsmith@example.com>\r\n"
        recvReply conn >>= (@?= 250) . fst . snd
        send conn "RCPT TO:<msmith@example.com>\r\n"
        recvReply conn >>= (@?= 452) . fst . snd
        send conn "QUIT\r\n"
        recvReply conn >>= (@?= 221) . fst . snd
    serverAct server = do
        (_, log) <- withLogging $ runReaderT run $
            server { serverMaxRecipients = 1 }
        map (\(_, _, _, a) -> a) log @?=
            [ "> 220 mx1.example.com Service ready\r"
            , "< EHLO relay.example.com\r"
            , "> 250-mx1.example.com\r"
            , "> 250-SIZE 4096\r"
            , "> 250-8BITMIME\r"
            , "> 250-SMTPUTF8\r"
            , "> 250-PIPELINING\r"
            , "> 250 STARTTLS\r"
            , "< MAIL FROM:<no-reply@example.com>\r"
            , "> 250 Mailbox <no-reply@example.com> OK\r"
            , "< RCPT TO:<jsmith@example.com>\r"
            , "> 250 Mailbox <jsmith@example.com> OK\r"
            , "< RCPT TO:<msmith@example.com>\r"
            , "> 452 Too many recipients\r"
            , "< QUIT\r"
            , "> 221 mx1.example.com Service closing transmission session\r"
            , "- [closing connection]"
            ]

testMessageUnrecognized = testCase "message unrecognized" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        recvReply conn >>= (@?= 220) . fst . snd
        send conn "EHLO relay.example.com\r\n"
        recvReply conn >>= (@?= 250) . fst . snd
        send conn "MAIL FROM:<no-reply@example.com>\r\n"
        recvReply conn >>= (@?= 250) . fst . snd
        send conn "RCPT TO:<jsmith@example.com>\r\n"
        recvReply conn >>= (@?= 250) . fst . snd
        send conn "DATA\r\n"
        recvReply conn >>= (@?= 354) . fst . snd
        send conn "foo\r\n.\r\n"
        recvReply conn >>= (@?= 550) . fst . snd
        send conn "QUIT\r\n"
        recvReply conn >>= (@?= 221) . fst . snd
    serverAct server = do
        (_, log) <- withLogging $ runReaderT run server
        map (\(_, _, _, a) -> a) log @?=
            [ "> 220 mx1.example.com Service ready\r"
            , "< EHLO relay.example.com\r"
            , "> 250-mx1.example.com\r"
            , "> 250-SIZE 4096\r"
            , "> 250-8BITMIME\r"
            , "> 250-SMTPUTF8\r"
            , "> 250-PIPELINING\r"
            , "> 250 STARTTLS\r"
            , "< MAIL FROM:<no-reply@example.com>\r"
            , "> 250 Mailbox <no-reply@example.com> OK\r"
            , "< RCPT TO:<jsmith@example.com>\r"
            , "> 250 Mailbox <jsmith@example.com> OK\r"
            , "< DATA\r"
            , "> 354 End data with <CR><LF>.<CR><LF>\r"
            , "< [5 bytes]"
            , "> 550 Syntax error, message unrecognized\r"
            , "< QUIT\r"
            , "> 221 mx1.example.com Service closing transmission session\r"
            , "- [closing connection]"
            ]

testMessageTooLarge = testCase "message too large" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        recvReply conn >>= (@?= 220) . fst . snd
        send conn "EHLO relay.example.com\r\n"
        recvReply conn >>= (@?= 250) . fst . snd
        send conn "MAIL FROM:<no-reply@example.com>\r\n"
        recvReply conn >>= (@?= 250) . fst . snd
        send conn "RCPT TO:<jsmith@example.com>\r\n"
        recvReply conn >>= (@?= 250) . fst . snd
        send conn "DATA\r\n"
        recvReply conn >>= (@?= 354) . fst . snd
        send conn testMessage
        send conn "\r\n.\r\n"
        recvReply conn >>= (@?= 552) . fst . snd
        send conn "QUIT\r\n"
        recvReply conn >>= (@?= 221) . fst . snd
    serverAct server = do
        (_, log) <- withLogging $ runReaderT run $
            server { serverMaxMessageSize = 1 }
        map (\(_, _, _, a) -> a) log @?=
            [ "> 220 mx1.example.com Service ready\r"
            , "< EHLO relay.example.com\r"
            , "> 250-mx1.example.com\r"
            , "> 250-SIZE 1\r"
            , "> 250-8BITMIME\r"
            , "> 250-SMTPUTF8\r"
            , "> 250-PIPELINING\r"
            , "> 250 STARTTLS\r"
            , "< MAIL FROM:<no-reply@example.com>\r"
            , "> 250 Mailbox <no-reply@example.com> OK\r"
            , "< RCPT TO:<jsmith@example.com>\r"
            , "> 250 Mailbox <jsmith@example.com> OK\r"
            , "< DATA\r"
            , "> 354 End data with <CR><LF>.<CR><LF>\r"
            , "< [226 bytes]"
            , "> 552 Message exceeds fixed maximum message size\r"
            , "< QUIT\r"
            , "> 221 mx1.example.com Service closing transmission session\r"
            , "- [closing connection]"
            ]

testMessageTansactionFailed = testCase "message transaction failed" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        recvReply conn >>= (@?= 220) . fst . snd
        send conn "EHLO relay.example.com\r\n"
        recvReply conn >>= (@?= 250) . fst . snd
        send conn "MAIL FROM:<no-reply@example.com>\r\n"
        recvReply conn >>= (@?= 250) . fst . snd
        send conn "RCPT TO:<jsmith@example.com>\r\n"
        recvReply conn >>= (@?= 250) . fst . snd
        send conn "DATA\r\n"
        recvReply conn >>= (@?= 354) . fst . snd
        send conn testMessage
        send conn "\r\n.\r\n"
        recvReply conn >>= (@?= 554) . fst . snd
        send conn "QUIT\r\n"
        recvReply conn >>= (@?= 221) . fst . snd
    serverAct server = do
        (_, log) <- withLogging $ runReaderT run $
            server { serverAcceptMessage = \_ _ _ -> return PermFail }
        map (\(_, _, _, a) -> a) log @?=
            [ "> 220 mx1.example.com Service ready\r"
            , "< EHLO relay.example.com\r"
            , "> 250-mx1.example.com\r"
            , "> 250-SIZE 4096\r"
            , "> 250-8BITMIME\r"
            , "> 250-SMTPUTF8\r"
            , "> 250-PIPELINING\r"
            , "> 250 STARTTLS\r"
            , "< MAIL FROM:<no-reply@example.com>\r"
            , "> 250 Mailbox <no-reply@example.com> OK\r"
            , "< RCPT TO:<jsmith@example.com>\r"
            , "> 250 Mailbox <jsmith@example.com> OK\r"
            , "< DATA\r"
            , "> 354 End data with <CR><LF>.<CR><LF>\r"
            , "< [226 bytes]"
            , "> 554 Transaction failed\r"
            , "< QUIT\r"
            , "> 221 mx1.example.com Service closing transmission session\r"
            , "- [closing connection]"
            ]

testNoValidRecipients = testCase "no valid recipients" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        recvReply conn >>= (@?= 220) . fst . snd
        send conn "EHLO relay.example.com\r\n"
        recvReply conn >>= (@?= 250) . fst . snd
        send conn "MAIL FROM:<no-reply@example.com>\r\n"
        recvReply conn >>= (@?= 250) . fst . snd
        send conn "DATA\r\n"
        recvReply conn >>= (@?= 554) . fst . snd
        send conn "QUIT\r\n"
        recvReply conn >>= (@?= 221) . fst . snd
    serverAct server = do
        (_, log) <- withLogging $ runReaderT run server
        map (\(_, _, _, a) -> a) log @?=
            [ "> 220 mx1.example.com Service ready\r"
            , "< EHLO relay.example.com\r"
            , "> 250-mx1.example.com\r"
            , "> 250-SIZE 4096\r"
            , "> 250-8BITMIME\r"
            , "> 250-SMTPUTF8\r"
            , "> 250-PIPELINING\r"
            , "> 250 STARTTLS\r"
            , "< MAIL FROM:<no-reply@example.com>\r"
            , "> 250 Mailbox <no-reply@example.com> OK\r"
            , "< DATA\r"
            , "> 554 No valid recipients\r"
            , "< QUIT\r"
            , "> 221 mx1.example.com Service closing transmission session\r"
            , "- [closing connection]"
            ]

testCredentialsUnrecognized = testCase "credentials unrecognized" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        recvReply conn >>= (@?= 220) . fst . snd
        send conn "EHLO relay.example.com\r\n"
        recvReply conn >>= (@?= 250) . fst . snd
        send conn "STARTTLS\r\n"
        recvReply conn >>= (@?= 220) . fst . snd
        secure conn $ tlsClientParams "localhost" False
        send conn "EHLO relay.example.com\r\n"
        recvReply conn >>= (@?= 250) . fst . snd
        send conn "AUTH PLAIN Zm9v\r\n"
        recvReply conn >>= (@?= 501) . fst . snd
        send conn "QUIT\r\n"
        recvReply conn >>= (@?= 221) . fst . snd
    serverAct server = do
        (_, log) <- withLogging $ runReaderT run server
        map (\(_, _, _, a) -> a) log @?=
            [ "> 220 mx1.example.com Service ready\r"
            , "< EHLO relay.example.com\r"
            , "> 250-mx1.example.com\r"
            , "> 250-SIZE 4096\r"
            , "> 250-8BITMIME\r"
            , "> 250-SMTPUTF8\r"
            , "> 250-PIPELINING\r"
            , "> 250 STARTTLS\r"
            , "< STARTTLS\r"
            , "> 220 Go ahead\r"
            , "- [tls handshake]"
            , "< EHLO relay.example.com\r"
            , "> 250-mx1.example.com\r"
            , "> 250-SIZE 4096\r"
            , "> 250-8BITMIME\r"
            , "> 250-SMTPUTF8\r"
            , "> 250-PIPELINING\r"
            , "> 250 AUTH PLAIN LOGIN\r"
            , "< AUTH PLAIN Zm9v\r"
            , "> 501 Syntax error, credentials unrecognized\r"
            , "< QUIT\r"
            , "> 221 mx1.example.com Service closing transmission session\r"
            , "- [closing connection]"
            ]

testCredentialsInvalid = testCase "credentials invalid" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        recvReply conn >>= (@?= 220) . fst . snd
        send conn "EHLO relay.example.com\r\n"
        recvReply conn >>= (@?= 250) . fst . snd
        send conn "STARTTLS\r\n"
        recvReply conn >>= (@?= 220) . fst . snd
        secure conn $ tlsClientParams "localhost" False
        send conn "EHLO relay.example.com\r\n"
        recvReply conn >>= (@?= 250) . fst . snd
        send conn "AUTH PLAIN dXNlcm5hbWUAdXNlcm5hbWUAcGFzc3dvcmQ=\r\n"
        recvReply conn >>= (@?= 535) . fst . snd
        send conn "QUIT\r\n"
        recvReply conn >>= (@?= 221) . fst . snd
    serverAct server = do
        (_, log) <- withLogging $ runReaderT run $
            server { serverAuthenticate = \_ _ -> return PermFail }
        map (\(_, _, _, a) -> a) log @?=
            [ "> 220 mx1.example.com Service ready\r"
            , "< EHLO relay.example.com\r"
            , "> 250-mx1.example.com\r"
            , "> 250-SIZE 4096\r"
            , "> 250-8BITMIME\r"
            , "> 250-SMTPUTF8\r"
            , "> 250-PIPELINING\r"
            , "> 250 STARTTLS\r"
            , "< STARTTLS\r"
            , "> 220 Go ahead\r"
            , "- [tls handshake]"
            , "< EHLO relay.example.com\r"
            , "> 250-mx1.example.com\r"
            , "> 250-SIZE 4096\r"
            , "> 250-8BITMIME\r"
            , "> 250-SMTPUTF8\r"
            , "> 250-PIPELINING\r"
            , "> 250 AUTH PLAIN LOGIN\r"
            , "< AUTH PLAIN dXNlcm5hbWUAdXNlcm5hbWUAcGFzc3dvcmQ=\r"
            , "> 535 Authentication credentials invalid\r"
            , "< QUIT\r"
            , "> 221 mx1.example.com Service closing transmission session\r"
            , "- [closing connection]"
            ]

testAuthenticationRequired = testCase "authentication required" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        recvReply conn >>= (@?= 220) . fst . snd
        send conn "EHLO relay.example.com\r\n"
        recvReply conn >>= (@?= 250) . fst . snd
        send conn "MAIL FROM:<no-reply@example.com>\r\n"
        recvReply conn >>= (@?= 530) . fst . snd
        send conn "QUIT\r\n"
        recvReply conn >>= (@?= 221) . fst . snd
    serverAct server = do
        (_, log) <- withLogging $ runReaderT run $
            server { serverReqAuth = True }
        map (\(_, _, _, a) -> a) log @?=
            [ "> 220 mx1.example.com Service ready\r"
            , "< EHLO relay.example.com\r"
            , "> 250-mx1.example.com\r"
            , "> 250-SIZE 4096\r"
            , "> 250-8BITMIME\r"
            , "> 250-SMTPUTF8\r"
            , "> 250-PIPELINING\r"
            , "> 250 STARTTLS\r"
            , "< MAIL FROM:<no-reply@example.com>\r"
            , "> 530 Authentication required\r"
            , "< QUIT\r"
            , "> 221 mx1.example.com Service closing transmission session\r"
            , "- [closing connection]"
            ]

testEncryptionRequired = testCase "encryption required" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        recvReply conn >>= (@?= 220) . fst . snd
        send conn "EHLO relay.example.com\r\n"
        recvReply conn >>= (@?= 250) . fst . snd
        send conn "MAIL FROM:<no-reply@example.com>\r\n"
        recvReply conn >>= (@?= 530) . fst . snd
        send conn "QUIT\r\n"
        recvReply conn >>= (@?= 221) . fst . snd
    serverAct server = do
        (_, log) <- withLogging $ runReaderT run $
            server { serverReqTLS = True }
        map (\(_, _, _, a) -> a) log @?=
            [ "> 220 mx1.example.com Service ready\r"
            , "< EHLO relay.example.com\r"
            , "> 250-mx1.example.com\r"
            , "> 250-SIZE 4096\r"
            , "> 250-8BITMIME\r"
            , "> 250-SMTPUTF8\r"
            , "> 250-PIPELINING\r"
            , "> 250 STARTTLS\r"
            , "< MAIL FROM:<no-reply@example.com>\r"
            , "> 530 Must issue a STARTTLS command first\r"
            , "< QUIT\r"
            , "> 221 mx1.example.com Service closing transmission session\r"
            , "- [closing connection]"
            ]
