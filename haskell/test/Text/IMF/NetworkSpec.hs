{-# LANGUAGE OverloadedStrings #-}

module Text.IMF.NetworkSpec where

import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Char8         as C
import           Control.Concurrent             ( forkIO )
import           Control.Concurrent.MVar        ( newEmptyMVar
                                                , putMVar
                                                , takeMVar
                                                )
import           Control.Exception              ( bracket )
import           Network.Socket                 ( AddrInfo(..)
                                                , AddrInfoFlag(..)
                                                , SocketType(..)
                                                , Socket
                                                , defaultHints
                                                , getAddrInfo
                                                , socket
                                                , socketPort
                                                , connect
                                                , bind
                                                , listen
                                                , accept
                                                , close
                                                )
import qualified Network.Socket.ByteString     as Socket
import           Test.Hspec

import           Text.IMF.Mailbox
import           Text.IMF.Message
import           Text.IMF.Header
import           Text.IMF.Network

getSockets :: IO (Conn, Socket)
getSockets = do
    -- setup server socket
    let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
    addr:_ <- getAddrInfo (Just hints) (Just "127.0.0.1") Nothing
    serverSock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    bind serverSock $ addrAddress addr
    listen serverSock 1
    -- setup client connection
    serverPort <- C.pack . show <$> socketPort serverSock
    clientConn <- open "localdomain" [serverPort]
    -- return connected pair
    return (clientConn { connPort = "25" }, serverSock)

closeSockets :: (Conn, Socket) -> IO ()
closeSockets (clientConn, serverSock) = do
    close serverSock
    close $ connSocket clientConn

chatTest :: (Conn -> IO a) -> (Socket -> IO b) -> IO ()
chatTest clientAct serverAct =
    bracket getSockets closeSockets $ \(clientConn, serverSock) -> do
        sync <- newEmptyMVar
        _ <- forkIO $ server serverSock sync
        client clientConn sync
  where
    server sock sync = do
        putMVar sync ()
        _ <- bracket (fst <$> accept sock) close serverAct
        putMVar sync ()
    client conn sync = do
        takeMVar sync
        _ <- clientAct conn
        takeMVar sync

spec :: Spec
spec = do

    describe "greeting" $
        it "works" $ do
            let serverAct sock =
                    Socket.send sock "220 smtp.example.com ESMTP Postfix\r\n"
                clientAct conn = do
                    (s, log) <- runChatT greeting conn
                    s `shouldBe` SendState { mxHostname   = "localhost"
                                           , mxPort       = "25"
                                           , lastCommand  = Nothing
                                           , lastResponse = Just "220 smtp.example.com ESMTP Postfix\r\n"
                                           , lastRC       = Just 220
                                           , timestamps   = []
                                           }
                    log `shouldBe` ["220 smtp.example.com ESMTP Postfix"]
            chatTest clientAct serverAct

    describe "helo" $
        it "works" $ do
            let serverAct sock = do
                    _ <- Socket.recv sock 4096 `shouldReturn` "HELO relay.example.com\r\n"
                    Socket.send sock "250 smtp.example.com, I am glad to meet you\r\n"
                clientName = "relay.example.com"
                clientAct conn = do
                    (s, log) <- runChatT (helo clientName) conn
                    s `shouldBe` SendState { mxHostname   = "localhost"
                                           , mxPort       = "25"
                                           , lastCommand  = Just "HELO relay.example.com"
                                           , lastResponse = Just "250 smtp.example.com, I am glad to meet you\r\n"
                                           , lastRC       = Just 250
                                           , timestamps   = []
                                           }
                    log `shouldBe` [ "HELO relay.example.com"
                                   , "250 smtp.example.com, I am glad to meet you"
                                   ]
            chatTest clientAct serverAct

    describe "mailFrom" $
        it "works" $ do
            let serverAct sock = do
                    _ <- Socket.recv sock 4096 `shouldReturn` "MAIL FROM: <bob@example.com>\r\n"
                    Socket.send sock "250 Ok\r\n"
                sender = Mailbox "" "bob" "example.com"
                clientAct conn = do
                    (s, log) <- runChatT (mailFrom sender) conn
                    s `shouldBe` SendState { mxHostname   = "localhost"
                                           , mxPort       = "25"
                                           , lastCommand  = Just "MAIL FROM: <bob@example.com>"
                                           , lastResponse = Just "250 Ok\r\n"
                                           , lastRC       = Just 250
                                           , timestamps   = []
                                           }
                    log `shouldBe` [ "MAIL FROM: <bob@example.com>"
                                   , "250 Ok"
                                   ]
            chatTest clientAct serverAct

    describe "rcptTo" $
        it "works" $ do
            let serverAct sock = do
                    _ <- Socket.recv sock 4096 `shouldReturn` "RCPT TO: <alice@example.com>\r\n"
                    Socket.send sock "250 Ok\r\n"
                recipient = Mailbox "" "alice" "example.com"
                clientAct conn = do
                    (s, log) <- runChatT (rcptTo recipient) conn
                    s `shouldBe` SendState { mxHostname   = "localhost"
                                           , mxPort       = "25"
                                           , lastCommand  = Just "RCPT TO: <alice@example.com>"
                                           , lastResponse = Just "250 Ok\r\n"
                                           , lastRC       = Just 250
                                           , timestamps   = []
                                           }
                    log `shouldBe` [ "RCPT TO: <alice@example.com>"
                                   , "250 Ok"
                                   ]
            chatTest clientAct serverAct

    describe "dataInit" $
        it "works" $ do
            let serverAct sock = do
                    _ <- Socket.recv sock 4096 `shouldReturn` "DATA\r\n"
                    Socket.send sock "354 End data with <CR><LF>.<CR><LF>\r\n"
                clientAct conn = do
                    (s, log) <- runChatT dataInit conn
                    s `shouldBe` SendState { mxHostname   = "localhost"
                                           , mxPort       = "25"
                                           , lastCommand  = Just "DATA"
                                           , lastResponse = Just "354 End data with <CR><LF>.<CR><LF>\r\n"
                                           , lastRC       = Just 354
                                           , timestamps   = []
                                           }
                    log `shouldBe` [ "DATA"
                                   , "354 End data with <CR><LF>.<CR><LF>"
                                   ]
            chatTest clientAct serverAct

    describe "dataBlock" $
        it "works" $ do
            let serverAct sock =
                    Socket.recv sock 4096
                msg = Message (Header []) ""
                clientAct conn = do
                    (s, log) <- runChatT (dataBlock msg) conn
                    s `shouldBe` SendState { mxHostname   = "localhost"
                                           , mxPort       = "25"
                                           , lastCommand  = Nothing
                                           , lastResponse = Nothing
                                           , lastRC       = Nothing
                                           , timestamps   = []
                                           }
                    log `shouldBe` ["__REDACTED__"]
            chatTest clientAct serverAct

    describe "dataTerm" $
        it "works" $ do
            let serverAct sock = do
                    _ <- Socket.recv sock 4096 `shouldReturn` ".\r\n"
                    Socket.send sock "250 Ok: queued as 12345\r\n"
                clientAct conn = do
                    (s, log) <- runChatT dataTerm conn
                    s `shouldBe` SendState { mxHostname   = "localhost"
                                           , mxPort       = "25"
                                           , lastCommand  = Just "."
                                           , lastResponse = Just "250 Ok: queued as 12345\r\n"
                                           , lastRC       = Just 250
                                           , timestamps   = []
                                           }
                    log `shouldBe` [ "."
                                   , "250 Ok: queued as 12345"
                                   ]
            chatTest clientAct serverAct

    describe "chat" $
        it "works" $ do
            let serverAct sock = do
                    Socket.send sock "220 smtp.example.com ESMTP Postfix\r\n"
                    _ <- Socket.recv sock 4096 `shouldReturn` "HELO relay.example.com\r\n"
                    Socket.send sock "250 smtp.example.com, I am glad to meet you\r\n"
                    _ <- Socket.recv sock 4096 `shouldReturn` "MAIL FROM: <bob@example.com>\r\n"
                    Socket.send sock "250 Ok\r\n"
                    _ <- Socket.recv sock 4096 `shouldReturn` "RCPT TO: <alice@example.com>\r\n"
                    Socket.send sock "250 Ok\r\n"
                    _ <- Socket.recv sock 4096 `shouldReturn` "DATA\r\n"
                    Socket.send sock "354 End data with <CR><LF>.<CR><LF>\r\n"
                    Socket.recv sock 4096
                    Socket.send sock "250 Ok: queued as 12345\r\n"
                clientName = "relay.example.com"
                sender = Mailbox "" "bob" "example.com"
                recipient = Mailbox "" "alice" "example.com"
                msg = Message (Header []) ""
                clientAct conn = do
                    (s, log) <- runChatT (chat clientName sender recipient msg) conn
                    let s' = s { timestamps = [] }
                    s' `shouldBe` SendState { mxHostname   = "localhost"
                                            , mxPort       = "25"
                                            , lastCommand  = Just "."
                                            , lastResponse = Just "250 Ok: queued as 12345\r\n"
                                            , lastRC       = Just 250
                                            , timestamps   = []
                                            }
                    log `shouldBe` [ "220 smtp.example.com ESMTP Postfix"
                                   , "HELO relay.example.com"
                                   , "250 smtp.example.com, I am glad to meet you"
                                   , "MAIL FROM: <bob@example.com>"
                                   , "250 Ok"
                                   , "RCPT TO: <alice@example.com>"
                                   , "250 Ok"
                                   , "DATA"
                                   , "354 End data with <CR><LF>.<CR><LF>"
                                   , "__REDACTED__"
                                   , "."
                                   , "250 Ok: queued as 12345"
                                   ]
            chatTest clientAct serverAct

