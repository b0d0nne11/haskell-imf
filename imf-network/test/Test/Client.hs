{-# LANGUAGE OverloadedStrings #-}

{- HLINT ignore "Reduce duplication" -}

module Test.Client where

import           Control.Concurrent               (forkIO)
import           Control.Concurrent.MVar          (newEmptyMVar, putMVar, takeMVar)
import           Control.Monad.Reader             (runReaderT)
import           Data.Attoparsec.ByteString.Char8 (parse)
import qualified Data.ByteString.Lazy             as LB
import qualified Data.ByteString.Lazy.Char8       as LC
import           Data.Maybe                       (fromMaybe)
import           Network.Connection
import           System.IO.Unsafe                 (unsafePerformIO)
import           Test.Tasty
import           Test.Tasty.HUnit                 hiding (assert)
import           UnliftIO.Exception               (bracket)

import           Data.IMF
import           Data.IMF.Network.Client
import qualified Data.IMF.Network.Connection      as Conn
import           Data.IMF.Network.Errors
import           Data.IMF.Parsers.Network

connectionContext :: ConnectionContext
connectionContext = unsafePerformIO initConnectionContext
{-# NOINLINE connectionContext #-}

newClient :: IO Client
newClient = do
    conn <- connectTo connectionContext params
    return Client
        { clientName        = "relay.example.com"
        , clientConnection  = conn
        , clientLogger      = const $ return ()
        , clientUseStartTLS = Just $ TLSSettingsSimple True False False
        , clientUseAuth     = Just ("username", "password")
        }
  where
    params = ConnectionParams
        { connectionHostname  = "localhost"
        , connectionPort      = 2525
        , connectionUseSecure = Nothing
        , connectionUseSocks  = Nothing
        }

closeClient :: Client -> IO ()
closeClient = connectionClose . clientConnection

testMessage :: IO LB.ByteString
testMessage = LC.intercalate "\r\n" . LC.split '\n' <$> LB.readFile "./test/Fixtures/Messages/simple_addressing_1.txt"

chatTest :: (Client -> IO ()) -> (Conn.Connection -> IO ()) -> IO ()
chatTest clientAct serverAct = do
    sync <- newEmptyMVar
    _ <- forkIO $ server sync
    client sync
  where
    server sync = bracket (Conn.listen ("127.0.0.1", "2525")) Conn.close $ \conn -> do
        _ <- putMVar sync ()
        _ <- bracket (Conn.accept conn) Conn.close serverAct
        _ <- putMVar sync ()
        return ()
    client sync = do
        _ <- takeMVar sync
        _ <- bracket newClient closeClient clientAct
        _ <- takeMVar sync
        return ()

tests :: TestTree
tests = testGroup "client"
    [ testGreeting
    , testHelo
    , testEhlo
    , testHello
    , testStartTLS
    , testAuthLogin
    , testAuthPlain
    , testMailFrom
    , testRcptTo
    , testDataInit
    , testDataBlock
    , testDataTerm
    , testQuit
    , testDeliver
    ]

testGreeting = testCase "greeting" $
    chatTest clientAct serverAct
  where
    serverAct conn = do
        _ <- Conn.send conn "220 smtp.example.com ESMTP Postfix\r\n"
        return ()
    clientAct client = do
        sessionGreeting <- runReaderT greeting client
        sessionGreeting @?= ["smtp.example.com ESMTP Postfix"]

testHelo = testCase "hello (helo)" $
    chatTest clientAct serverAct
  where
    serverAct conn = do
        r <- cuttoff 10 $ parseWhileM (parse pLine) 4096 (Conn.recv conn)
        r @?= ["HELO relay.example.com"]
        _ <- Conn.send conn "250 smtp.example.com, I am glad to meet you\r\n"
        return ()
    clientAct client = do
        _ <- runReaderT helo client
        return ()

testEhlo = testCase "hello (ehlo)" $
    chatTest clientAct serverAct
  where
    serverAct conn = do
        r <- cuttoff 10 $ parseWhileM (parse pLine) 4096 (Conn.recv conn)
        r @?= ["EHLO relay.example.com"]
        _ <- Conn.send conn "250-smtp.example.com\r\n250-SIZE 14680064\r\n250-PIPELINING\r\n250 HELP\r\n"
        return ()
    clientAct client = do
        sessionExtentions <- runReaderT ehlo client
        sessionExtentions @?= [("size",["14680064"]),("pipelining",[]),("help",[])]

testHello = testCase "hello (ehlo + helo)" $
    chatTest clientAct serverAct
  where
    serverAct conn = do
        r <- cuttoff 10 $ parseWhileM (parse pLine) 4096 (Conn.recv conn)
        r @?= ["EHLO relay.example.com"]
        _ <- Conn.send conn "502 Command not implemented\r\n"
        r <- cuttoff 10 $ parseWhileM (parse pLine) 4096 (Conn.recv conn)
        r @?= ["HELO relay.example.com"]
        _ <- Conn.send conn "250 smtp.example.com, I am glad to meet you\r\n"
        return ()
    clientAct client = do
        sessionExtentions <- runReaderT hello client
        sessionExtentions @?= []

testStartTLS = testCase "startTLS" $
    chatTest clientAct serverAct
  where
    serverAct conn = do
        r <- cuttoff 10 $ parseWhileM (parse pLine) 4096 (Conn.recv conn)
        r @?= ["EHLO relay.example.com"]
        _ <- Conn.send conn "250-smtp.example.com\r\n250 STARTTLS\r\n"
        r <- cuttoff 10 $ parseWhileM (parse pLine) 4096 (Conn.recv conn)
        r @?= ["STARTTLS"]
        _ <- Conn.send conn "220 Go ahead\r\n"
        conn <- Conn.secureServer conn
        r <- cuttoff 10 $ parseWhileM (parse pLine) 4096 (Conn.recv conn)
        r @?= ["EHLO relay.example.com"]
        _ <- Conn.send conn "250-smtp.example.com\r\n250 STARTTLS\r\n"
        return ()
    clientAct client = do
        params <- ("starttls" `lookup`) <$> runReaderT hello client
        result <- runReaderT (startTLS params connectionContext) client
        result @?= (True, Just [("starttls", [])])

testAuthLogin = testCase "auth (login)" $
    chatTest clientAct serverAct
  where
    serverAct conn = do
        r <- cuttoff 10 $ parseWhileM (parse pLine) 4096 (Conn.recv conn)
        r @?= ["EHLO relay.example.com"]
        _ <- Conn.send conn "250-smtp.example.com\r\n250-STARTTLS\r\n250 AUTH LOGIN\r\n"
        r <- cuttoff 10 $ parseWhileM (parse pLine) 4096 (Conn.recv conn)
        r @?= ["STARTTLS"]
        _ <- Conn.send conn "220 Go ahead\r\n"
        conn <- Conn.secureServer conn
        r <- cuttoff 10 $ parseWhileM (parse pLine) 4096 (Conn.recv conn)
        r @?= ["EHLO relay.example.com"]
        _ <- Conn.send conn "250-smtp.example.com\r\n250-STARTTLS\r\n250 AUTH LOGIN\r\n"
        r <- cuttoff 10 $ parseWhileM (parse pLine) 4096 (Conn.recv conn)
        r @?= ["AUTH LOGIN"]
        _ <- Conn.send conn "334 VXNlcm5hbWU6\r\n"
        r <- cuttoff 10 $ parseWhileM (parse pLine) 4096 (Conn.recv conn)
        r @?= ["dXNlcm5hbWU="]
        _ <- Conn.send conn "334 UGFzc3dvcmQ6\r\n"
        r <- cuttoff 10 $ parseWhileM (parse pLine) 4096 (Conn.recv conn)
        r @?= ["cGFzc3dvcmQ="]
        _ <- Conn.send conn "235 Authentication successful.\r\n"
        return ()
    clientAct client = do
        params <- ("starttls" `lookup`) <$> runReaderT hello client
        params <- ("auth" `lookup`) . fromMaybe [] . snd <$> runReaderT (startTLS params connectionContext) client
        result <- runReaderT (auth params) client
        result @?= True

testAuthPlain = testCase "auth (plain)" $
    chatTest clientAct serverAct
  where
    serverAct conn = do
        r <- cuttoff 10 $ parseWhileM (parse pLine) 4096 (Conn.recv conn)
        r @?= ["EHLO relay.example.com"]
        _ <- Conn.send conn "250-smtp.example.com\r\n250-STARTTLS\r\n250 AUTH PLAIN\r\n"
        r <- cuttoff 10 $ parseWhileM (parse pLine) 4096 (Conn.recv conn)
        r @?= ["STARTTLS"]
        _ <- Conn.send conn "220 Go ahead\r\n"
        conn <- Conn.secureServer conn
        r <- cuttoff 10 $ parseWhileM (parse pLine) 4096 (Conn.recv conn)
        r @?= ["EHLO relay.example.com"]
        _ <- Conn.send conn "250-smtp.example.com\r\n250-STARTTLS\r\n250 AUTH PLAIN\r\n"
        r <- cuttoff 10 $ parseWhileM (parse pLine) 4096 (Conn.recv conn)
        r @?= ["AUTH PLAIN dXNlcm5hbWUAdXNlcm5hbWUAcGFzc3dvcmQ="]
        _ <- Conn.send conn "235 Authentication successful.\r\n"
        return ()
    clientAct client = do
        params <- ("starttls" `lookup`) <$> runReaderT hello client
        params <- ("auth" `lookup`) . fromMaybe [] . snd <$> runReaderT (startTLS params connectionContext) client
        result <- runReaderT (auth params) client
        result @?= True

testMailFrom = testCase "mailFrom" $
    chatTest clientAct serverAct
  where
    serverAct conn = do
        r <- cuttoff 10 $ parseWhileM (parse pLine) 4096 (Conn.recv conn)
        r @?= ["MAIL FROM: <matt@localhost>"]
        _ <- Conn.send conn "250 Ok\r\n"
        return ()
    clientAct client = do
        _ <- runReaderT (mailFrom $ Mailbox "" "matt" "localhost") client
        return ()

testRcptTo = testCase "rcptTo" $
    chatTest clientAct serverAct
  where
    serverAct conn = do
        r <- cuttoff 10 $ parseWhileM (parse pLine) 4096 (Conn.recv conn)
        r @?= ["RCPT TO: <mary@localhost>"]
        _ <- Conn.send conn "250 Ok\r\n"
        return ()
    clientAct client = do
        _ <- runReaderT (rcptTo $ Mailbox "" "mary" "localhost") client
        return ()

testDataInit = testCase "dataInit" $
    chatTest clientAct serverAct
  where
    serverAct conn = do
        r <- cuttoff 10 $ parseWhileM (parse pLine) 4096 (Conn.recv conn)
        r @?= ["DATA"]
        _ <- Conn.send conn "354 End data with <CR><LF>.<CR><LF>\r\n"
        return ()
    clientAct client = do
        _ <- runReaderT dataInit client
        return ()

testDataBlock = testCase "dataBlock" $
    chatTest clientAct serverAct
  where
    serverAct conn = do
        r <- cuttoff 10 $ parseWhileM (parse pLine) 4096 (Conn.recv conn)
        return ()
    clientAct client = do
        msg <- testMessage
        _ <- runReaderT (dataBlock msg) client
        return ()

testDataTerm = testCase "dataTerm" $
    chatTest clientAct serverAct
  where
    serverAct conn = do
        r <- cuttoff 10 $ parseWhileM (parse pLine) 4096 (Conn.recv conn)
        r @?= ["."]
        _ <- Conn.send conn "250 Ok: queued as 12345\r\n"
        return ()
    clientAct client = do
        _ <- runReaderT dataTerm client
        return ()

testQuit = testCase "quit" $
    chatTest clientAct serverAct
  where
    serverAct conn = do
        r <- cuttoff 10 $ parseWhileM (parse pLine) 4096 (Conn.recv conn)
        r @?= ["QUIT"]
        _ <- Conn.send conn "221 Bye\r\n"
        return ()
    clientAct client = do
        _ <- runReaderT quit client
        return ()

testDeliver = testCase "deliver" $
    chatTest clientAct serverAct
  where
    serverAct conn = do
        r <- cuttoff 10 $ parseWhileM (parse pLine) 4096 (Conn.recv conn)
        r @?= ["MAIL FROM: <matt@localhost>"]
        _ <- Conn.send conn "250 Ok\r\n"
        r <- cuttoff 10 $ parseWhileM (parse pLine) 4096 (Conn.recv conn)
        r @?= ["RCPT TO: <mary@localhost>"]
        _ <- Conn.send conn "250 Ok\r\n"
        r <- cuttoff 10 $ parseWhileM (parse pLine) 4096 (Conn.recv conn)
        r @?= ["DATA"]
        _ <- Conn.send conn "354 End data with <CR><LF>.<CR><LF>\r\n"
        _ <- cuttoff 10 $ parseWhileM (parse pLine) 4096 (Conn.recv conn)
        _ <- Conn.send conn "250 Ok: queued as 12345\r\n"
        return ()
    clientAct client = do
        msg <- testMessage
        _ <- deliver client (Envelope (Mailbox "" "matt" "localhost") (Mailbox "" "mary" "localhost")) msg
        return ()

