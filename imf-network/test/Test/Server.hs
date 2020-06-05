{-# LANGUAGE OverloadedStrings #-}

{- HLINT ignore "Reduce duplication" -}

module Test.Server where

import           Control.Concurrent          (forkIO)
import           Control.Monad.IO.Unlift     (MonadIO)
import           Control.Monad.Reader        (runReaderT)
import           Data.Attoparsec.ByteString  (Result, count, eitherResult, parseWith)
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
import           UnliftIO.MVar               (newEmptyMVar, putMVar, takeMVar)

import           Data.IMF.Network.Connection
import           Data.IMF.Network.Errors
import           Data.IMF.Network.Parsers
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
        bracket (newServer <$> accept conn) (close . serverConnection) serverAct
    client sync = do
        takeMVar sync
        bracket (connect ("0.0.0.0", "0") ("127.0.0.1", "2525")) close clientAct
    newServer conn = Server
        { serverName             = "mx1.example.com"
        , serverConnection       = conn
        , serverTLSParams        = tlsServerParams testCertificate
        , serverLogger           = const $ return ()
        , serverAuthenticate     = \_ _ -> return Pass
        , serverVerifyReturnPath = \_ -> return Pass
        , serverVerifyRecipient  = \_ -> return Pass
        , serverAcceptMessage    = \_ _ _ -> return Pass
        , serverMaxRecipients    = 100
        , serverMaxMessageSize   = 10485760
        , serverReqTLS           = False
        , serverReqAuth          = False
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

recvReply :: Connection -> IO (Int, [Text])
recvReply conn = parseWith (recv conn) pReply "" >>= fromResult

recvReplies :: Connection -> Int -> IO [(Int, [Text])]
recvReplies conn n = parseWith (recv conn) (count n pReply) "" >>= fromResult

fromResult :: MonadIO m => Result a -> m a
fromResult = either throwString return . eitherResult

capabilities = B.concat
    [ "250-SIZE 10485760\r\n"
    , "250-8BITMIME\r\n"
    , "250-SMTPUTF8\r\n"
    , "250-PIPELINING\r\n"
    , "250 STARTTLS\r\n"
    ]

tlsCapabilities = B.concat
    [ "250-SIZE 10485760\r\n"
    , "250-8BITMIME\r\n"
    , "250-SMTPUTF8\r\n"
    , "250-PIPELINING\r\n"
    , "250 AUTH PLAIN LOGIN\r\n"
    ]

testConnect = testCase "connect/disconnect" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        fst <$> recvReply conn >>= (@?= 220)
        send conn "QUIT\r\n"
        fst <$> recvReply conn >>= (@?= 221)
    serverAct server = do
        log <- withMemLogger $ \logger ->
            runReaderT runServer $ server { serverLogger = logger }
        log @?= B.concat
            [ "> 220 mx1.example.com Service ready\r\n"
            , "< QUIT\r\n"
            , "> 221 mx1.example.com Service closing transmission session\r\n"
            ]

testSimpleChat = testCase "simple chat" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        fst <$> recvReply conn >>= (@?= 220)
        send conn "EHLO relay.example.com\r\n"
        fst <$> recvReply conn >>= (@?= 250)
        send conn "MAIL FROM:<no-reply@example.com>\r\n"
        fst <$> recvReply conn >>= (@?= 250)
        send conn "RCPT TO:<jsmith@example.com>\r\n"
        fst <$> recvReply conn >>= (@?= 250)
        send conn "DATA\r\n"
        recvReply conn >>= (@?= (354, ["End data with <CR><LF>.<CR><LF>"]))
        send conn $ LB.toStrict testMessage
        send conn "\r\n.\r\n"
        fst <$> recvReply conn >>= (@?= 250)
        send conn "QUIT\r\n"
        fst <$> recvReply conn >>= (@?= 221)
    serverAct server = do
        log <- withMemLogger $ \logger ->
            runReaderT runServer $ server { serverLogger = logger }
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
            , "< [...]\r\n"
            , "> 250 OK\r\n"
            , "< QUIT\r\n"
            , "> 221 mx1.example.com Service closing transmission session\r\n"
            ]

testPipelinedChat = testCase "pipelined chat" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        fst <$> recvReply conn >>= (@?= 220)
        send conn "EHLO relay.example.com\r\n"
        fst <$> recvReply conn >>= (@?= 250)
        send conn "MAIL FROM:<no-reply@example.com>\r\nRCPT TO:<jsmith@example.com>\r\nDATA\r\n"
        map fst <$> recvReplies conn 3 >>= (@?= [250, 250, 354])
        send conn $ LB.toStrict testMessage
        send conn "\r\n.\r\n"
        fst <$> recvReply conn >>= (@?= 250)
        send conn "QUIT\r\n"
        fst <$> recvReply conn >>= (@?= 221)
    serverAct server = do
        log <- withMemLogger $ \logger ->
            runReaderT runServer $ server { serverLogger = logger }
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
            , "< [...]\r\n"
            , "> 250 OK\r\n"
            , "< QUIT\r\n"
            , "> 221 mx1.example.com Service closing transmission session\r\n"
            ]

testSecuredChat = testCase "secured chat" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        fst <$> recvReply conn >>= (@?= 220)
        send conn "EHLO relay.example.com\r\n"
        fst <$> recvReply conn >>= (@?= 250)
        send conn "STARTTLS\r\n"
        fst <$> recvReply conn >>= (@?= 220)
        secure conn $ tlsClientParams "localhost" False
        send conn "EHLO relay.example.com\r\n"
        fst <$> recvReply conn >>= (@?= 250)
        send conn "MAIL FROM:<no-reply@example.com>\r\n"
        fst <$> recvReply conn >>= (@?= 250)
        send conn "RCPT TO:<jsmith@example.com>\r\n"
        fst <$> recvReply conn >>= (@?= 250)
        send conn "DATA\r\n"
        fst <$> recvReply conn >>= (@?= 354)
        send conn $ LB.toStrict testMessage
        send conn "\r\n.\r\n"
        fst <$> recvReply conn >>= (@?= 250)
        send conn "QUIT\r\n"
        fst <$> recvReply conn >>= (@?= 221)
    serverAct server = do
        log <- withMemLogger $ \logger ->
            runReaderT runServer $ server { serverLogger = logger }
        log @?= B.concat
            [ "> 220 mx1.example.com Service ready\r\n"
            , "< EHLO relay.example.com\r\n"
            , "> 250-mx1.example.com\r\n" `B.append` capabilities
            , "< STARTTLS\r\n"
            , "> 220 Go ahead\r\n"
            , ". [tls handshake]\r\n"
            , "< EHLO relay.example.com\r\n"
            , "> 250-mx1.example.com\r\n" `B.append` tlsCapabilities
            , "< MAIL FROM:<no-reply@example.com>\r\n"
            , "> 250 Mailbox <no-reply@example.com> OK\r\n"
            , "< RCPT TO:<jsmith@example.com>\r\n"
            , "> 250 Mailbox <jsmith@example.com> OK\r\n"
            , "< DATA\r\n"
            , "> 354 End data with <CR><LF>.<CR><LF>\r\n"
            , "< [...]\r\n"
            , "> 250 OK\r\n"
            , "< QUIT\r\n"
            , "> 221 mx1.example.com Service closing transmission session\r\n"
            ]

testAuthPlain = testCase "authenticate (plain)" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        fst <$> recvReply conn >>= (@?= 220)
        send conn "EHLO relay.example.com\r\n"
        fst <$> recvReply conn >>= (@?= 250)
        send conn "STARTTLS\r\n"
        fst <$> recvReply conn >>= (@?= 220)
        secure conn $ tlsClientParams "localhost" False
        send conn "EHLO relay.example.com\r\n"
        fst <$> recvReply conn >>= (@?= 250)
        send conn "AUTH PLAIN\r\n"
        fst <$> recvReply conn >>= (@?= 334)
        send conn "Zm9vAGJhcgBiYXI=\r\n"
        fst <$> recvReply conn >>= (@?= 235)
        send conn "QUIT\r\n"
        fst <$> recvReply conn >>= (@?= 221)
    serverAct server = do
        log <- withMemLogger $ \logger ->
            runReaderT runServer $ server { serverLogger = logger }
        log @?= B.concat
            [ "> 220 mx1.example.com Service ready\r\n"
            , "< EHLO relay.example.com\r\n"
            , "> 250-mx1.example.com\r\n" `B.append` capabilities
            , "< STARTTLS\r\n"
            , "> 220 Go ahead\r\n"
            , ". [tls handshake]\r\n"
            , "< EHLO relay.example.com\r\n"
            , "> 250-mx1.example.com\r\n" `B.append` tlsCapabilities
            , "< AUTH PLAIN\r\n"
            , "> 334 Go ahead\r\n"
            , "< [...]\r\n"
            , "> 235 Authentication succeeded\r\n"
            , "< QUIT\r\n"
            , "> 221 mx1.example.com Service closing transmission session\r\n"
            ]

testAuthPlainInit = testCase "authenticate (plain with initial)" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        fst <$> recvReply conn >>= (@?= 220)
        send conn "EHLO relay.example.com\r\n"
        fst <$> recvReply conn >>= (@?= 250)
        send conn "STARTTLS\r\n"
        fst <$> recvReply conn >>= (@?= 220)
        secure conn $ tlsClientParams "localhost" False
        send conn "EHLO relay.example.com\r\n"
        fst <$> recvReply conn >>= (@?= 250)
        send conn "AUTH PLAIN Zm9vAGJhcgBiYXI=\r\n"
        fst <$> recvReply conn >>= (@?= 235)
        send conn "QUIT\r\n"
        fst <$> recvReply conn >>= (@?= 221)
        return ()
    serverAct server = do
        log <- withMemLogger $ \logger ->
            runReaderT runServer $ server { serverLogger = logger }
        log @?= B.concat
            [ "> 220 mx1.example.com Service ready\r\n"
            , "< EHLO relay.example.com\r\n"
            , "> 250-mx1.example.com\r\n" `B.append` capabilities
            , "< STARTTLS\r\n"
            , "> 220 Go ahead\r\n"
            , ". [tls handshake]\r\n"
            , "< EHLO relay.example.com\r\n"
            , "> 250-mx1.example.com\r\n" `B.append` tlsCapabilities
            , "< AUTH PLAIN Zm9vAGJhcgBiYXI=\r\n"
            , "> 235 Authentication succeeded\r\n"
            , "< QUIT\r\n"
            , "> 221 mx1.example.com Service closing transmission session\r\n"
            ]

testAuthLogin = testCase "authenticate (login)" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        fst <$> recvReply conn >>= (@?= 220)
        send conn "EHLO relay.example.com\r\n"
        fst <$> recvReply conn >>= (@?= 250)
        send conn "STARTTLS\r\n"
        fst <$> recvReply conn >>= (@?= 220)
        secure conn $ tlsClientParams "localhost" False
        send conn "EHLO relay.example.com\r\n"
        fst <$> recvReply conn >>= (@?= 250)
        send conn "AUTH LOGIN\r\n"
        fst <$> recvReply conn >>= (@?= 334)
        send conn "Zm9v\r\n"
        fst <$> recvReply conn >>= (@?= 334)
        send conn "YmFy\r\n"
        fst <$> recvReply conn >>= (@?= 235)
        send conn "QUIT\r\n"
        fst <$> recvReply conn >>= (@?= 221)
    serverAct server = do
        log <- withMemLogger $ \logger ->
            runReaderT runServer $ server { serverLogger = logger }
        log @?= B.concat
            [ "> 220 mx1.example.com Service ready\r\n"
            , "< EHLO relay.example.com\r\n"
            , "> 250-mx1.example.com\r\n" `B.append` capabilities
            , "< STARTTLS\r\n"
            , "> 220 Go ahead\r\n"
            , ". [tls handshake]\r\n"
            , "< EHLO relay.example.com\r\n"
            , "> 250-mx1.example.com\r\n" `B.append` tlsCapabilities
            , "< AUTH LOGIN\r\n"
            , "> 334 VXNlcm5hbWU6\r\n"
            , "< [...]\r\n"
            , "> 334 UGFzc3dvcmQ6\r\n"
            , "< [...]\r\n"
            , "> 235 Authentication succeeded\r\n"
            , "< QUIT\r\n"
            , "> 221 mx1.example.com Service closing transmission session\r\n"
            ]

testAuthLoginInit = testCase "authenticate (login with initial)" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        fst <$> recvReply conn >>= (@?= 220)
        send conn "EHLO relay.example.com\r\n"
        fst <$> recvReply conn >>= (@?= 250)
        send conn "STARTTLS\r\n"
        fst <$> recvReply conn >>= (@?= 220)
        secure conn $ tlsClientParams "localhost" False
        send conn "EHLO relay.example.com\r\n"
        fst <$> recvReply conn >>= (@?= 250)
        send conn "AUTH LOGIN Zm9v\r\n"
        fst <$> recvReply conn >>= (@?= 334)
        send conn "YmFy\r\n"
        fst <$> recvReply conn >>= (@?= 235)
        send conn "QUIT\r\n"
        fst <$> recvReply conn >>= (@?= 221)
    serverAct server = do
        log <- withMemLogger $ \logger ->
            runReaderT runServer $ server { serverLogger = logger }
        log @?= B.concat
            [ "> 220 mx1.example.com Service ready\r\n"
            , "< EHLO relay.example.com\r\n"
            , "> 250-mx1.example.com\r\n" `B.append` capabilities
            , "< STARTTLS\r\n"
            , "> 220 Go ahead\r\n"
            , ". [tls handshake]\r\n"
            , "< EHLO relay.example.com\r\n"
            , "> 250-mx1.example.com\r\n" `B.append` tlsCapabilities
            , "< AUTH LOGIN Zm9v\r\n"
            , "> 334 UGFzc3dvcmQ6\r\n"
            , "< [...]\r\n"
            , "> 235 Authentication succeeded\r\n"
            , "< QUIT\r\n"
            , "> 221 mx1.example.com Service closing transmission session\r\n"
            ]

testPeerConnectionClosed = testCase "peer connection closed" $
    serverTest clientAct serverAct
  where
    clientAct conn =
        fst <$> recvReply conn >>= (@?= 220)
    serverAct server = do
        log <- withMemLogger $ \logger -> do
            r <- try $ runReaderT runServer $ server { serverLogger = logger }
            r @?= Left PeerConnectionClosed
        log @?= B.concat ["> 220 mx1.example.com Service ready\r\n"]

testCommandUnrecognized = testCase "command unrecognized" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        fst <$> recvReply conn >>= (@?= 220)
        send conn "FOO\r\n"
        fst <$> recvReply conn >>= (@?= 500)
        send conn "QUIT\r\n"
        fst <$> recvReply conn >>= (@?= 221)
    serverAct server = do
        log <- withMemLogger $ \logger ->
            runReaderT runServer $ server { serverLogger = logger }
        log @?= B.concat
            [ "> 220 mx1.example.com Service ready\r\n"
            , "< FOO\r\n"
            , "> 500 Syntax error, command unrecognized\r\n"
            , "< QUIT\r\n"
            , "> 221 mx1.example.com Service closing transmission session\r\n"
            ]

testCommandOutOfOrder = testCase "command out of order" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        fst <$> recvReply conn >>= (@?= 220)
        send conn "MAIL FROM:<no-reply@example.com>\r\n"
        fst <$> recvReply conn >>= (@?= 503)
        send conn "QUIT\r\n"
        fst <$> recvReply conn >>= (@?= 221)
    serverAct server = do
        log <- withMemLogger $ \logger ->
            runReaderT runServer $ server { serverLogger = logger }
        log @?= B.concat
            [ "> 220 mx1.example.com Service ready\r\n"
            , "< MAIL FROM:<no-reply@example.com>\r\n"
            , "> 503 Bad sequence of commands\r\n"
            , "< QUIT\r\n"
            , "> 221 mx1.example.com Service closing transmission session\r\n"
            ]

testParameterNotImplemented = testCase "parameter not implemented" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        fst <$> recvReply conn >>= (@?= 220)
        send conn "EHLO relay.example.com\r\n"
        fst <$> recvReply conn >>= (@?= 250)
        send conn "MAIL FROM:<no-reply@example.com> FOO=BAR\r\n"
        fst <$> recvReply conn >>= (@?= 504)
        send conn "QUIT\r\n"
        fst <$> recvReply conn >>= (@?= 221)
    serverAct server = do
        log <- withMemLogger $ \logger ->
            runReaderT runServer $ server { serverLogger = logger }
        log @?= B.concat
            [ "> 220 mx1.example.com Service ready\r\n"
            , "< EHLO relay.example.com\r\n"
            , "> 250-mx1.example.com\r\n" `B.append` capabilities
            , "< MAIL FROM:<no-reply@example.com> FOO=BAR\r\n"
            , "> 504 Command parameter or argument not implemented\r\n"
            , "< QUIT\r\n"
            , "> 221 mx1.example.com Service closing transmission session\r\n"
            ]

testReturnPathUnrecognized = testCase "return path mailbox unrecognized" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        fst <$> recvReply conn >>= (@?= 220)
        send conn "EHLO relay.example.com\r\n"
        fst <$> recvReply conn >>= (@?= 250)
        send conn "MAIL FROM:<foo>\r\n"
        fst <$> recvReply conn >>= (@?= 550)
        send conn "QUIT\r\n"
        fst <$> recvReply conn >>= (@?= 221)
    serverAct server = do
        log <- withMemLogger $ \logger ->
            runReaderT runServer $ server { serverLogger = logger, serverVerifyReturnPath = \_ -> return PermFail }
        log @?= B.concat
            [ "> 220 mx1.example.com Service ready\r\n"
            , "< EHLO relay.example.com\r\n"
            , "> 250-mx1.example.com\r\n" `B.append` capabilities
            , "< MAIL FROM:<foo>\r\n"
            , "> 550 Syntax error, mailbox <foo> unrecognized\r\n"
            , "< QUIT\r\n"
            , "> 221 mx1.example.com Service closing transmission session\r\n"
            ]

testReturnPathUnavailable = testCase "return path mailbox unavailable" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        fst <$> recvReply conn >>= (@?= 220)
        send conn "EHLO relay.example.com\r\n"
        fst <$> recvReply conn >>= (@?= 250)
        send conn "MAIL FROM:<no-reply@example.com>\r\n"
        fst <$> recvReply conn >>= (@?= 550)
        send conn "QUIT\r\n"
        fst <$> recvReply conn >>= (@?= 221)
    serverAct server = do
        log <- withMemLogger $ \logger ->
            runReaderT runServer $ server { serverLogger = logger, serverVerifyReturnPath = \_ -> return PermFail }
        log @?= B.concat
            [ "> 220 mx1.example.com Service ready\r\n"
            , "< EHLO relay.example.com\r\n"
            , "> 250-mx1.example.com\r\n" `B.append` capabilities
            , "< MAIL FROM:<no-reply@example.com>\r\n"
            , "> 550 Mailbox <no-reply@example.com> unavailable\r\n"
            , "< QUIT\r\n"
            , "> 221 mx1.example.com Service closing transmission session\r\n"
            ]

testRecipientUnrecognized = testCase "recipient mailbox unrecognized" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        fst <$> recvReply conn >>= (@?= 220)
        send conn "EHLO relay.example.com\r\n"
        fst <$> recvReply conn >>= (@?= 250)
        send conn "MAIL FROM:<no-reply@example.com>\r\n"
        fst <$> recvReply conn >>= (@?= 250)
        send conn "RCPT TO:<foo>\r\n"
        fst <$> recvReply conn >>= (@?= 550)
        send conn "QUIT\r\n"
        fst <$> recvReply conn >>= (@?= 221)
    serverAct server = do
        log <- withMemLogger $ \logger ->
            runReaderT runServer $ server { serverLogger = logger, serverVerifyRecipient = \_ -> return PermFail }
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
            ]

testRecipientUnavailable = testCase "recipient mailbox unavailable" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        fst <$> recvReply conn >>= (@?= 220)
        send conn "EHLO relay.example.com\r\n"
        fst <$> recvReply conn >>= (@?= 250)
        send conn "MAIL FROM:<no-reply@example.com>\r\n"
        fst <$> recvReply conn >>= (@?= 250)
        send conn "RCPT TO:<jsmith@example.com>\r\n"
        fst <$> recvReply conn >>= (@?= 550)
        send conn "QUIT\r\n"
        fst <$> recvReply conn >>= (@?= 221)
    serverAct server = do
        log <- withMemLogger $ \logger ->
            runReaderT runServer $ server { serverLogger = logger, serverVerifyRecipient = \_ -> return PermFail }
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
            ]

testTooManyRecipients = testCase "too many recipients" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        fst <$> recvReply conn >>= (@?= 220)
        send conn "EHLO relay.example.com\r\n"
        fst <$> recvReply conn >>= (@?= 250)
        send conn "MAIL FROM:<no-reply@example.com>\r\n"
        fst <$> recvReply conn >>= (@?= 250)
        send conn "RCPT TO:<jsmith@example.com>\r\n"
        fst <$> recvReply conn >>= (@?= 250)
        send conn "RCPT TO:<msmith@example.com>\r\n"
        fst <$> recvReply conn >>= (@?= 452)
        send conn "QUIT\r\n"
        fst <$> recvReply conn >>= (@?= 221)
    serverAct server = do
        log <- withMemLogger $ \logger ->
            runReaderT runServer $ server { serverLogger = logger, serverMaxRecipients = 1 }
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
            ]

testMessageUnrecognized = testCase "message unrecognized" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        fst <$> recvReply conn >>= (@?= 220)
        send conn "EHLO relay.example.com\r\n"
        fst <$> recvReply conn >>= (@?= 250)
        send conn "MAIL FROM:<no-reply@example.com>\r\n"
        fst <$> recvReply conn >>= (@?= 250)
        send conn "RCPT TO:<jsmith@example.com>\r\n"
        fst <$> recvReply conn >>= (@?= 250)
        send conn "DATA\r\n"
        fst <$> recvReply conn >>= (@?= 354)
        send conn "foo\r\n.\r\n"
        fst <$> recvReply conn >>= (@?= 550)
        send conn "QUIT\r\n"
        fst <$> recvReply conn >>= (@?= 221)
    serverAct server = do
        log <- withMemLogger $ \logger ->
            runReaderT runServer $ server { serverLogger = logger }
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
            , "< [...]\r\n"
            , "> 550 Syntax error, message unrecognized\r\n"
            , "< QUIT\r\n"
            , "> 221 mx1.example.com Service closing transmission session\r\n"
            ]

testMessageTooLarge = testCase "message too large" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        fst <$> recvReply conn >>= (@?= 220)
        send conn "EHLO relay.example.com\r\n"
        fst <$> recvReply conn >>= (@?= 250)
        send conn "MAIL FROM:<no-reply@example.com>\r\n"
        fst <$> recvReply conn >>= (@?= 250)
        send conn "RCPT TO:<jsmith@example.com>\r\n"
        fst <$> recvReply conn >>= (@?= 250)
        send conn "DATA\r\n"
        fst <$> recvReply conn >>= (@?= 354)
        send conn $ LB.toStrict testMessage
        send conn "\r\n.\r\n"
        fst <$> recvReply conn >>= (@?= 552)
        send conn "QUIT\r\n"
        fst <$> recvReply conn >>= (@?= 221)
    serverAct server = do
        log <- withMemLogger $ \logger ->
            runReaderT runServer $ server { serverLogger = logger, serverMaxMessageSize = 1 }
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
            , "< [...]\r\n"
            , "> 552 Message exceeds fixed maximum message size\r\n"
            , "< QUIT\r\n"
            , "> 221 mx1.example.com Service closing transmission session\r\n"
            ]
    capabilities' = B.concat
        [ "250-SIZE 1\r\n"
        , "250-8BITMIME\r\n"
        , "250-SMTPUTF8\r\n"
        , "250-PIPELINING\r\n"
        , "250 STARTTLS\r\n"
        ]

testMessageTansactionFailed = testCase "message transaction failed" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        fst <$> recvReply conn >>= (@?= 220)
        send conn "EHLO relay.example.com\r\n"
        fst <$> recvReply conn >>= (@?= 250)
        send conn "MAIL FROM:<no-reply@example.com>\r\n"
        fst <$> recvReply conn >>= (@?= 250)
        send conn "RCPT TO:<jsmith@example.com>\r\n"
        fst <$> recvReply conn >>= (@?= 250)
        send conn "DATA\r\n"
        fst <$> recvReply conn >>= (@?= 354)
        send conn $ LB.toStrict testMessage
        send conn "\r\n.\r\n"
        fst <$> recvReply conn >>= (@?= 554)
        send conn "QUIT\r\n"
        fst <$> recvReply conn >>= (@?= 221)
    serverAct server = do
        log <- withMemLogger $ \logger ->
            runReaderT runServer $ server { serverLogger = logger, serverAcceptMessage = \_ _ _ -> return PermFail }
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
            , "< [...]\r\n"
            , "> 554 Transaction failed\r\n"
            , "< QUIT\r\n"
            , "> 221 mx1.example.com Service closing transmission session\r\n"
            ]

testNoValidRecipients = testCase "no valid recipients" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        fst <$> recvReply conn >>= (@?= 220)
        send conn "EHLO relay.example.com\r\n"
        fst <$> recvReply conn >>= (@?= 250)
        send conn "MAIL FROM:<no-reply@example.com>\r\n"
        fst <$> recvReply conn >>= (@?= 250)
        send conn "DATA\r\n"
        fst <$> recvReply conn >>= (@?= 554)
        send conn "QUIT\r\n"
        fst <$> recvReply conn >>= (@?= 221)
    serverAct server = do
        log <- withMemLogger $ \logger ->
            runReaderT runServer $ server { serverLogger = logger }
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
            ]

testCredentialsUnrecognized = testCase "credentials unrecognized" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        fst <$> recvReply conn >>= (@?= 220)
        send conn "EHLO relay.example.com\r\n"
        fst <$> recvReply conn >>= (@?= 250)
        send conn "STARTTLS\r\n"
        fst <$> recvReply conn >>= (@?= 220)
        secure conn $ tlsClientParams "localhost" False
        send conn "EHLO relay.example.com\r\n"
        fst <$> recvReply conn >>= (@?= 250)
        send conn "AUTH PLAIN Zm9v\r\n"
        fst <$> recvReply conn >>= (@?= 501)
        send conn "QUIT\r\n"
        fst <$> recvReply conn >>= (@?= 221)
    serverAct server = do
        log <- withMemLogger $ \logger ->
            runReaderT runServer $ server { serverLogger = logger }
        log @?= B.concat
            [ "> 220 mx1.example.com Service ready\r\n"
            , "< EHLO relay.example.com\r\n"
            , "> 250-mx1.example.com\r\n" `B.append` capabilities
            , "< STARTTLS\r\n"
            , "> 220 Go ahead\r\n"
            , ". [tls handshake]\r\n"
            , "< EHLO relay.example.com\r\n"
            , "> 250-mx1.example.com\r\n" `B.append` tlsCapabilities
            , "< AUTH PLAIN Zm9v\r\n"
            , "> 501 Syntax error, credentials unrecognized\r\n"
            , "< QUIT\r\n"
            , "> 221 mx1.example.com Service closing transmission session\r\n"
            ]

testCredentialsInvalid = testCase "credentials invalid" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        fst <$> recvReply conn >>= (@?= 220)
        send conn "EHLO relay.example.com\r\n"
        fst <$> recvReply conn >>= (@?= 250)
        send conn "STARTTLS\r\n"
        fst <$> recvReply conn >>= (@?= 220)
        secure conn $ tlsClientParams "localhost" False
        send conn "EHLO relay.example.com\r\n"
        fst <$> recvReply conn >>= (@?= 250)
        send conn "AUTH PLAIN dXNlcm5hbWUAdXNlcm5hbWUAcGFzc3dvcmQ=\r\n"
        fst <$> recvReply conn >>= (@?= 535)
        send conn "QUIT\r\n"
        fst <$> recvReply conn >>= (@?= 221)
    serverAct server = do
        log <- withMemLogger $ \logger ->
            runReaderT runServer $ server { serverLogger = logger, serverAuthenticate = \_ _ -> return PermFail }
        log @?= B.concat
            [ "> 220 mx1.example.com Service ready\r\n"
            , "< EHLO relay.example.com\r\n"
            , "> 250-mx1.example.com\r\n" `B.append` capabilities
            , "< STARTTLS\r\n"
            , "> 220 Go ahead\r\n"
            , ". [tls handshake]\r\n"
            , "< EHLO relay.example.com\r\n"
            , "> 250-mx1.example.com\r\n" `B.append` tlsCapabilities
            , "< AUTH PLAIN dXNlcm5hbWUAdXNlcm5hbWUAcGFzc3dvcmQ=\r\n"
            , "> 535 Authentication credentials invalid\r\n"
            , "< QUIT\r\n"
            , "> 221 mx1.example.com Service closing transmission session\r\n"
            ]

testAuthenticationRequired = testCase "authentication required" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        fst <$> recvReply conn >>= (@?= 220)
        send conn "EHLO relay.example.com\r\n"
        fst <$> recvReply conn >>= (@?= 250)
        send conn "MAIL FROM:<no-reply@example.com>\r\n"
        fst <$> recvReply conn >>= (@?= 530)
        send conn "QUIT\r\n"
        fst <$> recvReply conn >>= (@?= 221)
    serverAct server = do
        log <- withMemLogger $ \logger ->
            runReaderT runServer $ server { serverLogger = logger, serverReqAuth = True }
        log @?= B.concat
            [ "> 220 mx1.example.com Service ready\r\n"
            , "< EHLO relay.example.com\r\n"
            , "> 250-mx1.example.com\r\n" `B.append` capabilities
            , "< MAIL FROM:<no-reply@example.com>\r\n"
            , "> 530 Authentication required\r\n"
            , "< QUIT\r\n"
            , "> 221 mx1.example.com Service closing transmission session\r\n"
            ]

testEncryptionRequired = testCase "encryption required" $
    serverTest clientAct serverAct
  where
    clientAct conn = do
        fst <$> recvReply conn >>= (@?= 220)
        send conn "EHLO relay.example.com\r\n"
        fst <$> recvReply conn >>= (@?= 250)
        send conn "MAIL FROM:<no-reply@example.com>\r\n"
        fst <$> recvReply conn >>= (@?= 530)
        send conn "QUIT\r\n"
        fst <$> recvReply conn >>= (@?= 221)
    serverAct server = do
        log <- withMemLogger $ \logger ->
            runReaderT runServer $ server { serverLogger = logger, serverReqTLS = True }
        log @?= B.concat
            [ "> 220 mx1.example.com Service ready\r\n"
            , "< EHLO relay.example.com\r\n"
            , "> 250-mx1.example.com\r\n" `B.append` capabilities
            , "< MAIL FROM:<no-reply@example.com>\r\n"
            , "> 530 Must issue a STARTTLS command first\r\n"
            , "< QUIT\r\n"
            , "> 221 mx1.example.com Service closing transmission session\r\n"
            ]
