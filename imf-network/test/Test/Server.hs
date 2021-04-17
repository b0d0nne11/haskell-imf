{-# LANGUAGE OverloadedStrings #-}

{- HLINT ignore "Reduce duplication" -}

module Test.Server where

import           Control.Concurrent          (forkIO)
import           Control.Monad               (replicateM)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as B
import qualified Data.ByteString.Lazy        as LB
import           Data.Text                   (Text)
import qualified Network.TLS                 as TLS
import           System.IO.Unsafe            (unsafePerformIO)
import           System.Log.FastLogger
import           Test.Tasty
import           Test.Tasty.HUnit
import           UnliftIO.Exception          (bracket, throwString, try)
import           UnliftIO.IORef              (atomicModifyIORef', newIORef, readIORef)
import           UnliftIO.MVar               (newMVar, newEmptyMVar, putMVar, takeMVar)

import           Data.IMF.Network.Connection
import           Data.IMF.Network.Server

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
        accept conn >>= newServer' "mx1.example.com" >>= serverAct
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
testCertificate = unsafePerformIO $ TLS.credentialLoadX509 "./test/Fixtures/localhost.crt" "./test/Fixtures/localhost.key" >>= either throwString return
{-# NOINLINE testCertificate #-}

newMemLogger :: IO (LogStr -> IO (), IO ByteString)
newMemLogger = do
    ref <- newIORef mempty
    return (logger ref, reader ref)
  where
    logger ref new = atomicModifyIORef' ref $ \old -> (old <> new, ())
    reader ref = fromLogStr <$> readIORef ref

withMemLogger :: ((LogStr -> IO ()) -> IO ()) -> IO ByteString
withMemLogger f = do
    (logger, reader) <- newMemLogger
    f logger >> reader

capabilities = B.concat
    [ "> 250-SIZE 4096\r\n"
    , "> 250-8BITMIME\r\n"
    , "> 250-SMTPUTF8\r\n"
    , "> 250-PIPELINING\r\n"
    , "> 250 STARTTLS\r\n"
    ]

tlsCapabilities = B.concat
    [ "> 250-SIZE 4096\r\n"
    , "> 250-8BITMIME\r\n"
    , "> 250-SMTPUTF8\r\n"
    , "> 250-PIPELINING\r\n"
    , "> 250 AUTH PLAIN LOGIN\r\n"
    ]

testConnect = testCase "connect/disconnect" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        recvReply conn >>= (@?= 220) . fst . snd
        send conn "QUIT\r\n"
        recvReply conn >>= (@?= 221) . fst . snd
    serverAct server = do
        log <- withMemLogger $ \logger ->
            runServer $ server { serverLogger = logger }
        log @?= B.concat
            [ "> 220 mx1.example.com Service ready\r\n"
            , "< QUIT\r\n"
            , "> 221 mx1.example.com Service closing transmission session\r\n"
            , "- [closing connection]\n"
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
        log <- withMemLogger $ \logger ->
            runServer $ server { serverLogger = logger }
        log @?= B.concat
            [ "> 220 mx1.example.com Service ready\r\n"
            , "< EHLO relay.example.com\r\n"
            , "> 250-mx1.example.com\r\n" `B.append` capabilities
            , "< MAIL FROM:<no-reply@example.com>\r\n"
            , "> 250 Mailbox <no-reply@example.com> OK\r\n"
            , "< RCPT TO:<jsmith@example.com>\r\n"
            , "> 250 Mailbox <jsmith@example.com> OK\r\n"
            , "< DATA\r\n"
            , "> 354 End data with <CR><LF>.<CR><LF>\r\n"
            , "< [226 bytes]\n"
            , "> 250 OK\r\n"
            , "< QUIT\r\n"
            , "> 221 mx1.example.com Service closing transmission session\r\n"
            , "- [closing connection]\n"
            ]

testPipelinedChat = testCase "pipelined chat" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        recvReply conn >>= (@?= 220) . fst . snd
        send conn "EHLO relay.example.com\r\n"
        recvReply conn >>= (@?= 250) . fst . snd
        send conn "MAIL FROM:<no-reply@example.com>\r\nRCPT TO:<jsmith@example.com>\r\nDATA\r\n"
        replicateM 3 (recvReply conn) >>= (@?= [250, 250, 354]) . map (fst . snd)
        send conn testMessage
        send conn "\r\n.\r\n"
        recvReply conn >>= (@?= 250) . fst . snd
        send conn "QUIT\r\n"
        recvReply conn >>= (@?= 221) . fst . snd
    serverAct server = do
        log <- withMemLogger $ \logger ->
            runServer $ server { serverLogger = logger }
        log @?= B.concat
            [ "> 220 mx1.example.com Service ready\r\n"
            , "< EHLO relay.example.com\r\n"
            , "> 250-mx1.example.com\r\n" `B.append` capabilities
            , "< MAIL FROM:<no-reply@example.com>\r\n"
            , "< RCPT TO:<jsmith@example.com>\r\n"
            , "< DATA\r\n"
            , "> 250 Mailbox <no-reply@example.com> OK\r\n"
            , "> 250 Mailbox <jsmith@example.com> OK\r\n"
            , "> 354 End data with <CR><LF>.<CR><LF>\r\n"
            , "< [226 bytes]\n"
            , "> 250 OK\r\n"
            , "< QUIT\r\n"
            , "> 221 mx1.example.com Service closing transmission session\r\n"
            , "- [closing connection]\n"
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
        log <- withMemLogger $ \logger ->
            runServer $ server { serverLogger = logger }
        log @?= B.concat
            [ "> 220 mx1.example.com Service ready\r\n"
            , "< EHLO relay.example.com\r\n"
            , "> 250-mx1.example.com\r\n" `B.append` capabilities
            , "< STARTTLS\r\n"
            , "> 220 Go ahead\r\n"
            , "- [tls handshake]\n"
            , "< EHLO relay.example.com\r\n"
            , "> 250-mx1.example.com\r\n" `B.append` tlsCapabilities
            , "< MAIL FROM:<no-reply@example.com>\r\n"
            , "> 250 Mailbox <no-reply@example.com> OK\r\n"
            , "< RCPT TO:<jsmith@example.com>\r\n"
            , "> 250 Mailbox <jsmith@example.com> OK\r\n"
            , "< DATA\r\n"
            , "> 354 End data with <CR><LF>.<CR><LF>\r\n"
            , "< [226 bytes]\n"
            , "> 250 OK\r\n"
            , "< QUIT\r\n"
            , "> 221 mx1.example.com Service closing transmission session\r\n"
            , "- [closing connection]\n"
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
        log <- withMemLogger $ \logger ->
            runServer $ server { serverLogger = logger }
        log @?= B.concat
            [ "> 220 mx1.example.com Service ready\r\n"
            , "< EHLO relay.example.com\r\n"
            , "> 250-mx1.example.com\r\n" `B.append` capabilities
            , "< STARTTLS\r\n"
            , "> 220 Go ahead\r\n"
            , "- [tls handshake]\n"
            , "< EHLO relay.example.com\r\n"
            , "> 250-mx1.example.com\r\n" `B.append` tlsCapabilities
            , "< AUTH PLAIN\r\n"
            , "> 334 Go ahead\r\n"
            , "< [...]\n"
            , "> 235 Authentication succeeded\r\n"
            , "< QUIT\r\n"
            , "> 221 mx1.example.com Service closing transmission session\r\n"
            , "- [closing connection]\n"
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
        log <- withMemLogger $ \logger ->
            runServer $ server { serverLogger = logger }
        log @?= B.concat
            [ "> 220 mx1.example.com Service ready\r\n"
            , "< EHLO relay.example.com\r\n"
            , "> 250-mx1.example.com\r\n" `B.append` capabilities
            , "< STARTTLS\r\n"
            , "> 220 Go ahead\r\n"
            , "- [tls handshake]\n"
            , "< EHLO relay.example.com\r\n"
            , "> 250-mx1.example.com\r\n" `B.append` tlsCapabilities
            , "< AUTH PLAIN Zm9vAGJhcgBiYXI=\r\n"
            , "> 235 Authentication succeeded\r\n"
            , "< QUIT\r\n"
            , "> 221 mx1.example.com Service closing transmission session\r\n"
            , "- [closing connection]\n"
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
        log <- withMemLogger $ \logger ->
            runServer $ server { serverLogger = logger }
        log @?= B.concat
            [ "> 220 mx1.example.com Service ready\r\n"
            , "< EHLO relay.example.com\r\n"
            , "> 250-mx1.example.com\r\n" `B.append` capabilities
            , "< STARTTLS\r\n"
            , "> 220 Go ahead\r\n"
            , "- [tls handshake]\n"
            , "< EHLO relay.example.com\r\n"
            , "> 250-mx1.example.com\r\n" `B.append` tlsCapabilities
            , "< AUTH LOGIN\r\n"
            , "> 334 VXNlcm5hbWU6\r\n"
            , "< [...]\n"
            , "> 334 UGFzc3dvcmQ6\r\n"
            , "< [...]\n"
            , "> 235 Authentication succeeded\r\n"
            , "< QUIT\r\n"
            , "> 221 mx1.example.com Service closing transmission session\r\n"
            , "- [closing connection]\n"
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
        log <- withMemLogger $ \logger ->
            runServer $ server { serverLogger = logger }
        log @?= B.concat
            [ "> 220 mx1.example.com Service ready\r\n"
            , "< EHLO relay.example.com\r\n"
            , "> 250-mx1.example.com\r\n" `B.append` capabilities
            , "< STARTTLS\r\n"
            , "> 220 Go ahead\r\n"
            , "- [tls handshake]\n"
            , "< EHLO relay.example.com\r\n"
            , "> 250-mx1.example.com\r\n" `B.append` tlsCapabilities
            , "< AUTH LOGIN Zm9v\r\n"
            , "> 334 UGFzc3dvcmQ6\r\n"
            , "< [...]\n"
            , "> 235 Authentication succeeded\r\n"
            , "< QUIT\r\n"
            , "> 221 mx1.example.com Service closing transmission session\r\n"
            , "- [closing connection]\n"
            ]

testPeerConnectionClosed = testCase "peer connection closed" $
    serverTest clientAct serverAct
  where
    clientAct conn =
        recvReply conn >>= (@?= 220) . fst . snd
    serverAct server = do
        log <- withMemLogger $ \logger -> do
            r <- try $ runServer $ server { serverLogger = logger }
            r @?= Left PeerConnectionClosed
        log @?= B.concat ["> 220 mx1.example.com Service ready\r\n"]

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
        log <- withMemLogger $ \logger ->
            runServer $ server { serverLogger = logger }
        log @?= B.concat
            [ "> 220 mx1.example.com Service ready\r\n"
            , "< FOO\r\n"
            , "> 500 Syntax error, command unrecognized\r\n"
            , "< QUIT\r\n"
            , "> 221 mx1.example.com Service closing transmission session\r\n"
            , "- [closing connection]\n"
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
        log <- withMemLogger $ \logger ->
            runServer $ server { serverLogger = logger }
        log @?= B.concat
            [ "> 220 mx1.example.com Service ready\r\n"
            , "< MAIL FROM:<no-reply@example.com>\r\n"
            , "> 503 Bad sequence of commands\r\n"
            , "< QUIT\r\n"
            , "> 221 mx1.example.com Service closing transmission session\r\n"
            , "- [closing connection]\n"
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
        log <- withMemLogger $ \logger ->
            runServer $ server { serverLogger = logger }
        log @?= B.concat
            [ "> 220 mx1.example.com Service ready\r\n"
            , "< EHLO relay.example.com\r\n"
            , "> 250-mx1.example.com\r\n" `B.append` capabilities
            , "< MAIL FROM:<no-reply@example.com> FOO=BAR\r\n"
            , "> 504 Command parameter or argument not implemented\r\n"
            , "< QUIT\r\n"
            , "> 221 mx1.example.com Service closing transmission session\r\n"
            , "- [closing connection]\n"
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
        log <- withMemLogger $ \logger ->
            runServer $ server { serverLogger = logger, serverVerifyReturnPath = \_ -> return PermFail }
        log @?= B.concat
            [ "> 220 mx1.example.com Service ready\r\n"
            , "< EHLO relay.example.com\r\n"
            , "> 250-mx1.example.com\r\n" `B.append` capabilities
            , "< MAIL FROM:<foo>\r\n"
            , "> 550 Syntax error, mailbox <foo> unrecognized\r\n"
            , "< QUIT\r\n"
            , "> 221 mx1.example.com Service closing transmission session\r\n"
            , "- [closing connection]\n"
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
        log <- withMemLogger $ \logger ->
            runServer $ server { serverLogger = logger, serverVerifyReturnPath = \_ -> return PermFail }
        log @?= B.concat
            [ "> 220 mx1.example.com Service ready\r\n"
            , "< EHLO relay.example.com\r\n"
            , "> 250-mx1.example.com\r\n" `B.append` capabilities
            , "< MAIL FROM:<no-reply@example.com>\r\n"
            , "> 550 Mailbox <no-reply@example.com> unavailable\r\n"
            , "< QUIT\r\n"
            , "> 221 mx1.example.com Service closing transmission session\r\n"
            , "- [closing connection]\n"
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
        log <- withMemLogger $ \logger ->
            runServer $ server { serverLogger = logger, serverVerifyRecipient = \_ -> return PermFail }
        log @?= B.concat
            [ "> 220 mx1.example.com Service ready\r\n"
            , "< EHLO relay.example.com\r\n"
            , "> 250-mx1.example.com\r\n" `B.append` capabilities
            , "< MAIL FROM:<no-reply@example.com>\r\n"
            , "> 250 Mailbox <no-reply@example.com> OK\r\n"
            , "< RCPT TO:<foo>\r\n"
            , "> 550 Syntax error, mailbox <foo> unrecognized\r\n"
            , "< QUIT\r\n"
            , "> 221 mx1.example.com Service closing transmission session\r\n"
            , "- [closing connection]\n"
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
        log <- withMemLogger $ \logger ->
            runServer $ server { serverLogger = logger, serverVerifyRecipient = \_ -> return PermFail }
        log @?= B.concat
            [ "> 220 mx1.example.com Service ready\r\n"
            , "< EHLO relay.example.com\r\n"
            , "> 250-mx1.example.com\r\n" `B.append` capabilities
            , "< MAIL FROM:<no-reply@example.com>\r\n"
            , "> 250 Mailbox <no-reply@example.com> OK\r\n"
            , "< RCPT TO:<jsmith@example.com>\r\n"
            , "> 550 Mailbox <jsmith@example.com> unavailable\r\n"
            , "< QUIT\r\n"
            , "> 221 mx1.example.com Service closing transmission session\r\n"
            , "- [closing connection]\n"
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
        log <- withMemLogger $ \logger ->
            runServer $ server { serverLogger = logger, serverMaxRecipients = 1 }
        log @?= B.concat
            [ "> 220 mx1.example.com Service ready\r\n"
            , "< EHLO relay.example.com\r\n"
            , "> 250-mx1.example.com\r\n" `B.append` capabilities
            , "< MAIL FROM:<no-reply@example.com>\r\n"
            , "> 250 Mailbox <no-reply@example.com> OK\r\n"
            , "< RCPT TO:<jsmith@example.com>\r\n"
            , "> 250 Mailbox <jsmith@example.com> OK\r\n"
            , "< RCPT TO:<msmith@example.com>\r\n"
            , "> 452 Too many recipients\r\n"
            , "< QUIT\r\n"
            , "> 221 mx1.example.com Service closing transmission session\r\n"
            , "- [closing connection]\n"
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
        log <- withMemLogger $ \logger ->
            runServer $ server { serverLogger = logger }
        log @?= B.concat
            [ "> 220 mx1.example.com Service ready\r\n"
            , "< EHLO relay.example.com\r\n"
            , "> 250-mx1.example.com\r\n" `B.append` capabilities
            , "< MAIL FROM:<no-reply@example.com>\r\n"
            , "> 250 Mailbox <no-reply@example.com> OK\r\n"
            , "< RCPT TO:<jsmith@example.com>\r\n"
            , "> 250 Mailbox <jsmith@example.com> OK\r\n"
            , "< DATA\r\n"
            , "> 354 End data with <CR><LF>.<CR><LF>\r\n"
            , "< [5 bytes]\n"
            , "> 550 Syntax error, message unrecognized\r\n"
            , "< QUIT\r\n"
            , "> 221 mx1.example.com Service closing transmission session\r\n"
            , "- [closing connection]\n"
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
        log <- withMemLogger $ \logger ->
            runServer $ server { serverLogger = logger, serverMaxMessageSize = 1 }
        log @?= B.concat
            [ "> 220 mx1.example.com Service ready\r\n"
            , "< EHLO relay.example.com\r\n"
            , "> 250-mx1.example.com\r\n" `B.append` capabilities'
            , "< MAIL FROM:<no-reply@example.com>\r\n"
            , "> 250 Mailbox <no-reply@example.com> OK\r\n"
            , "< RCPT TO:<jsmith@example.com>\r\n"
            , "> 250 Mailbox <jsmith@example.com> OK\r\n"
            , "< DATA\r\n"
            , "> 354 End data with <CR><LF>.<CR><LF>\r\n"
            , "< [226 bytes]\n"
            , "> 552 Message exceeds fixed maximum message size\r\n"
            , "< QUIT\r\n"
            , "> 221 mx1.example.com Service closing transmission session\r\n"
            , "- [closing connection]\n"
            ]
    capabilities' = B.concat
        [ "> 250-SIZE 1\r\n"
        , "> 250-8BITMIME\r\n"
        , "> 250-SMTPUTF8\r\n"
        , "> 250-PIPELINING\r\n"
        , "> 250 STARTTLS\r\n"
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
        log <- withMemLogger $ \logger ->
            runServer $ server { serverLogger = logger, serverAcceptMessage = \_ _ _ -> return PermFail }
        log @?= B.concat
            [ "> 220 mx1.example.com Service ready\r\n"
            , "< EHLO relay.example.com\r\n"
            , "> 250-mx1.example.com\r\n" `B.append` capabilities
            , "< MAIL FROM:<no-reply@example.com>\r\n"
            , "> 250 Mailbox <no-reply@example.com> OK\r\n"
            , "< RCPT TO:<jsmith@example.com>\r\n"
            , "> 250 Mailbox <jsmith@example.com> OK\r\n"
            , "< DATA\r\n"
            , "> 354 End data with <CR><LF>.<CR><LF>\r\n"
            , "< [226 bytes]\n"
            , "> 554 Transaction failed\r\n"
            , "< QUIT\r\n"
            , "> 221 mx1.example.com Service closing transmission session\r\n"
            , "- [closing connection]\n"
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
        log <- withMemLogger $ \logger ->
            runServer $ server { serverLogger = logger }
        log @?= B.concat
            [ "> 220 mx1.example.com Service ready\r\n"
            , "< EHLO relay.example.com\r\n"
            , "> 250-mx1.example.com\r\n" `B.append` capabilities
            , "< MAIL FROM:<no-reply@example.com>\r\n"
            , "> 250 Mailbox <no-reply@example.com> OK\r\n"
            , "< DATA\r\n"
            , "> 554 No valid recipients\r\n"
            , "< QUIT\r\n"
            , "> 221 mx1.example.com Service closing transmission session\r\n"
            , "- [closing connection]\n"
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
        log <- withMemLogger $ \logger ->
            runServer $ server { serverLogger = logger }
        log @?= B.concat
            [ "> 220 mx1.example.com Service ready\r\n"
            , "< EHLO relay.example.com\r\n"
            , "> 250-mx1.example.com\r\n" `B.append` capabilities
            , "< STARTTLS\r\n"
            , "> 220 Go ahead\r\n"
            , "- [tls handshake]\n"
            , "< EHLO relay.example.com\r\n"
            , "> 250-mx1.example.com\r\n" `B.append` tlsCapabilities
            , "< AUTH PLAIN Zm9v\r\n"
            , "> 501 Syntax error, credentials unrecognized\r\n"
            , "< QUIT\r\n"
            , "> 221 mx1.example.com Service closing transmission session\r\n"
            , "- [closing connection]\n"
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
        log <- withMemLogger $ \logger ->
            runServer $ server { serverLogger = logger, serverAuthenticate = \_ _ -> return PermFail }
        log @?= B.concat
            [ "> 220 mx1.example.com Service ready\r\n"
            , "< EHLO relay.example.com\r\n"
            , "> 250-mx1.example.com\r\n" `B.append` capabilities
            , "< STARTTLS\r\n"
            , "> 220 Go ahead\r\n"
            , "- [tls handshake]\n"
            , "< EHLO relay.example.com\r\n"
            , "> 250-mx1.example.com\r\n" `B.append` tlsCapabilities
            , "< AUTH PLAIN dXNlcm5hbWUAdXNlcm5hbWUAcGFzc3dvcmQ=\r\n"
            , "> 535 Authentication credentials invalid\r\n"
            , "< QUIT\r\n"
            , "> 221 mx1.example.com Service closing transmission session\r\n"
            , "- [closing connection]\n"
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
        log <- withMemLogger $ \logger ->
            runServer $ server { serverLogger = logger, serverReqAuth = True }
        log @?= B.concat
            [ "> 220 mx1.example.com Service ready\r\n"
            , "< EHLO relay.example.com\r\n"
            , "> 250-mx1.example.com\r\n" `B.append` capabilities
            , "< MAIL FROM:<no-reply@example.com>\r\n"
            , "> 530 Authentication required\r\n"
            , "< QUIT\r\n"
            , "> 221 mx1.example.com Service closing transmission session\r\n"
            , "- [closing connection]\n"
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
        log <- withMemLogger $ \logger ->
            runServer $ server { serverLogger = logger, serverReqTLS = True }
        log @?= B.concat
            [ "> 220 mx1.example.com Service ready\r\n"
            , "< EHLO relay.example.com\r\n"
            , "> 250-mx1.example.com\r\n" `B.append` capabilities
            , "< MAIL FROM:<no-reply@example.com>\r\n"
            , "> 530 Must issue a STARTTLS command first\r\n"
            , "< QUIT\r\n"
            , "> 221 mx1.example.com Service closing transmission session\r\n"
            , "- [closing connection]\n"
            ]
