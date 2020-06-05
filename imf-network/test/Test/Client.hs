{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{- HLINT ignore "Reduce duplication" -}

module Test.Client where

import           Control.Concurrent          (forkIO)
import           Control.Monad.IO.Unlift     (MonadIO)
import           Control.Monad.Reader        (runReaderT)
import           Data.Attoparsec.ByteString  (Result, eitherResult, parseWith)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as B
import qualified Data.ByteString.Lazy        as LB
import           Data.Text                   (Text)
import qualified Network.TLS                 as TLS
import           System.IO.Unsafe            (unsafePerformIO)
import           System.Log.FastLogger
import           Test.Tasty
import           Test.Tasty.HUnit
import           UnliftIO.Exception          (bracket, throwString)
import           UnliftIO.IORef              (atomicModifyIORef', newIORef, readIORef)
import           UnliftIO.MVar               (newEmptyMVar, putMVar, takeMVar)

import           Data.IMF
import           Data.IMF.Network.Client
import           Data.IMF.Network.Connection
import           Data.IMF.Network.Errors
import           Data.IMF.Network.Parsers

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
        bracket (accept conn) close serverAct
    client sync = do
        takeMVar sync
        bracket (newClient <$> connect ("0.0.0.0", "0") ("127.0.0.1", "2525")) (close . clientConnection) clientAct
    newClient conn = Client
        { clientName        = "relay.example.com"
        , clientConnection  = conn
        , clientTLSParams   = tlsClientParams "localhost" False
        , clientLogger      = const $ return ()
        , clientCredentials = Nothing
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

recvLine :: Connection -> IO ByteString
recvLine conn = parseWith (recv conn) pLine "" >>= fromResult

recvData :: Connection -> IO ByteString
recvData conn = parseWith (recv conn) pData "" >>= fromResult

fromResult :: MonadIO m => Result a -> m a
fromResult = either throwString return . eitherResult

testHello = testCase "init (ehlo)" $
    clientTest clientAct serverAct
  where
    serverAct conn = do
        send conn "220 smtp.example.com ESMTP Postfix\r\n"
        recvLine conn >>= (@?= "EHLO relay.example.com\r\n")
        send conn "250-smtp.example.com\r\n250-SIZE 14680064\r\n250 HELP\r\n"
    clientAct client = do
        log <- withMemLogger $ \logger -> do
            ClientSession{..} <- runReaderT setup $ client { clientLogger = logger }
            sessionExtentions @?= [("size",["14680064"]),("help",[])]
            isSecure (clientConnection client) >>= (@?= False)
            sessionUser @?= Nothing
        log @?= B.concat
            [ "< 220 smtp.example.com ESMTP Postfix\r\n"
            , "> EHLO relay.example.com\r\n"
            , "< 250-smtp.example.com\r\n250-SIZE 14680064\r\n250 HELP\r\n"
            ]

testHelloFallback = testCase "init (helo)" $
    clientTest clientAct serverAct
  where
    serverAct conn = do
        send conn "220 smtp.example.com ESMTP Postfix\r\n"
        recvLine conn >>= (@?= "EHLO relay.example.com\r\n")
        send conn "502 Command not implemented\r\n"
        recvLine conn >>= (@?= "HELO relay.example.com\r\n")
        send conn "250 smtp.example.com, I am glad to meet you\r\n"
    clientAct client = do
        log <- withMemLogger $ \logger -> do
            ClientSession{..} <- runReaderT setup $ client { clientLogger = logger }
            sessionExtentions @?= []
            isSecure (clientConnection client) >>= (@?= False)
            sessionUser @?= Nothing
        log @?= B.concat
            [ "< 220 smtp.example.com ESMTP Postfix\r\n"
            , "> EHLO relay.example.com\r\n"
            , "< 502 Command not implemented\r\n"
            , "> HELO relay.example.com\r\n"
            , "< 250 smtp.example.com, I am glad to meet you\r\n"
            ]

testStartTLS = testCase "init (ehlo + starttls)" $
    clientTest clientAct serverAct
  where
    serverAct conn = do
        send conn "220 smtp.example.com ESMTP Postfix\r\n"
        recvLine conn >>= (@?= "EHLO relay.example.com\r\n")
        send conn "250-smtp.example.com\r\n250 STARTTLS\r\n"
        recvLine conn >>= (@?= "STARTTLS\r\n")
        send conn "220 Go ahead\r\n"
        secure conn $ tlsServerParams testCertificate
        recvLine conn >>= (@?= "EHLO relay.example.com\r\n")
        send conn "250-smtp.example.com\r\n250 STARTTLS\r\n"
    clientAct client = do
        log <- withMemLogger $ \logger -> do
            ClientSession{..} <- runReaderT setup $ client { clientLogger = logger }
            sessionExtentions @?= [("starttls", [])]
            isSecure (clientConnection client) >>= (@?= True)
            sessionUser @?= Nothing
        log @?= B.concat
            [ "< 220 smtp.example.com ESMTP Postfix\r\n"
            , "> EHLO relay.example.com\r\n"
            , "< 250-smtp.example.com\r\n"
            ,   "250 STARTTLS\r\n"
            , "> STARTTLS\r\n"
            , "< 220 Go ahead\r\n"
            , ". [tls handshake]\r\n"
            , "> EHLO relay.example.com\r\n"
            , "< 250-smtp.example.com\r\n250 STARTTLS\r\n"
            ]

testAuthLogin = testCase "init (ehlo + starttls + login)" $
    clientTest clientAct serverAct
  where
    serverAct conn = do
        send conn "220 smtp.example.com ESMTP Postfix\r\n"
        recvLine conn >>= (@?= "EHLO relay.example.com\r\n")
        send conn "250-smtp.example.com\r\n250 STARTTLS\r\n"
        recvLine conn >>= (@?= "STARTTLS\r\n")
        send conn "220 Go ahead\r\n"
        secure conn $ tlsServerParams testCertificate
        recvLine conn >>= (@?= "EHLO relay.example.com\r\n")
        send conn "250-smtp.example.com\r\n250-STARTTLS\r\n250 AUTH LOGIN\r\n"
        recvLine conn >>= (@?= "AUTH LOGIN\r\n")
        send conn "334 VXNlcm5hbWU6\r\n"
        recvLine conn >>= (@?= "dXNlcm5hbWU=\r\n")
        send conn "334 UGFzc3dvcmQ6\r\n"
        recvLine conn >>= (@?= "cGFzc3dvcmQ=\r\n")
        send conn "235 Authentication successful.\r\n"
    clientAct client = do
        log <- withMemLogger $ \logger -> do
            ClientSession{..} <- runReaderT setup $ client { clientLogger = logger, clientCredentials = Just ("username", "password") }
            sessionExtentions @?= [("starttls", []), ("auth", ["login"])]
            isSecure (clientConnection client) >>= (@?= True)
            sessionUser @?= Just "username"
        log @?= B.concat
            [ "< 220 smtp.example.com ESMTP Postfix\r\n"
            , "> EHLO relay.example.com\r\n"
            , "< 250-smtp.example.com\r\n"
            ,   "250 STARTTLS\r\n"
            , "> STARTTLS\r\n"
            , "< 220 Go ahead\r\n"
            , ". [tls handshake]\r\n"
            , "> EHLO relay.example.com\r\n"
            , "< 250-smtp.example.com\r\n"
            ,   "250-STARTTLS\r\n"
            ,   "250 AUTH LOGIN\r\n"
            , "> AUTH LOGIN\r\n"
            , "< 334 VXNlcm5hbWU6\r\n"
            , "> dXNlcm5hbWU=\r\n"
            , "< 334 UGFzc3dvcmQ6\r\n"
            , "> cGFzc3dvcmQ=\r\n"
            , "< 235 Authentication successful.\r\n"
            ]

testAuthPlain = testCase "init (ehlo + starttls + plain)" $
    clientTest clientAct serverAct
  where
    serverAct conn = do
        send conn "220 smtp.example.com ESMTP Postfix\r\n"
        recvLine conn >>= (@?= "EHLO relay.example.com\r\n")
        send conn "250-smtp.example.com\r\n250 STARTTLS\r\n"
        recvLine conn >>= (@?= "STARTTLS\r\n")
        send conn "220 Go ahead\r\n"
        secure conn $ tlsServerParams testCertificate
        recvLine conn >>= (@?= "EHLO relay.example.com\r\n")
        send conn "250-smtp.example.com\r\n250-STARTTLS\r\n250 AUTH PLAIN\r\n"
        recvLine conn >>= (@?= "AUTH PLAIN dXNlcm5hbWUAdXNlcm5hbWUAcGFzc3dvcmQ=\r\n")
        send conn "235 Authentication successful.\r\n"
    clientAct client = do
        log <- withMemLogger $ \logger -> do
            ClientSession{..} <- runReaderT setup $ client { clientLogger = logger, clientCredentials = Just ("username", "password") }
            sessionExtentions @?= [("starttls", []), ("auth", ["plain"])]
            isSecure (clientConnection client) >>= (@?= True)
            sessionUser @?= Just "username"
        log @?= B.concat
            [ "< 220 smtp.example.com ESMTP Postfix\r\n"
            , "> EHLO relay.example.com\r\n"
            , "< 250-smtp.example.com\r\n"
            ,   "250 STARTTLS\r\n"
            , "> STARTTLS\r\n"
            , "< 220 Go ahead\r\n"
            , ". [tls handshake]\r\n"
            , "> EHLO relay.example.com\r\n"
            , "< 250-smtp.example.com\r\n"
            ,   "250-STARTTLS\r\n"
            ,   "250 AUTH PLAIN\r\n"
            , "> AUTH PLAIN dXNlcm5hbWUAdXNlcm5hbWUAcGFzc3dvcmQ=\r\n"
            , "< 235 Authentication successful.\r\n"
            ]

testDeliver = testCase "deliver" $
    clientTest clientAct serverAct
  where
    serverAct conn = do
        recvLine conn >>= (@?= "MAIL FROM:<matt@localhost>\r\n")
        send conn "250 Ok\r\n"
        recvLine conn >>= (@?= "RCPT TO:<mary@localhost>\r\n")
        send conn "250 Ok\r\n"
        recvLine conn >>= (@?= "DATA\r\n")
        send conn "354 End data with <CR><LF>.<CR><LF>\r\n"
        _ <- recvData conn
        send conn "250 Ok: queued as 12345\r\n"
    clientAct client = do
        log <- withMemLogger $ \logger ->
            runReaderT (deliver (Mailbox "" "matt" "localhost") [Mailbox "" "mary" "localhost"] testMessage) $ client { clientLogger = logger }
        log @?= B.concat
            [ "> MAIL FROM:<matt@localhost>\r\n"
            , "< 250 Ok\r\n"
            , "> RCPT TO:<mary@localhost>\r\n"
            , "< 250 Ok\r\n"
            , "> DATA\r\n"
            , "< 354 End data with <CR><LF>.<CR><LF>\r\n"
            , "> From: John Doe <jdoe@machine.example>\n"
            ,   "To: Mary Smith <mary@example.net>\n"
            ,   "Subject: Saying Hello\n"
            ,   "Date: Fri, 21 Nov 1997 09:55:06 -0600\n"
            ,   "Message-ID: <1234@local.machine.example>\n"
            ,   "\n"
            ,   "This is a message just to say hello.\n"
            ,   "So, \"Hello\".\n"
            , "> .\r\n"
            , "< 250 Ok: queued as 12345\r\n"
            ]

testQuit = testCase "quit" $
    clientTest clientAct serverAct
  where
    serverAct conn = do
        recvLine conn >>= (@?= "QUIT\r\n")
        send conn "221 Bye\r\n"
    clientAct client = do
        log <- withMemLogger $ \logger ->
            runReaderT quit $ client { clientLogger = logger }
        log @?= B.concat
            [ "> QUIT\r\n"
            , "< 221 Bye\r\n"
            ]
