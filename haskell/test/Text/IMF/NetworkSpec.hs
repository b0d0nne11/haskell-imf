{-# LANGUAGE OverloadedStrings #-}

module Text.IMF.NetworkSpec where

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
        _ <- putMVar sync ()
        _ <- bracket (fst <$> accept sock) close serverAct
        _ <- putMVar sync ()
        return ()
    client conn sync = do
        _ <- takeMVar sync
        _ <- clientAct conn
        _ <- takeMVar sync
        return ()

spec :: Spec
spec = do

    describe "greeting" $
        it "works" $ do
            let serverAct sock = do
                    _ <- Socket.send sock "220 smtp.example.com ESMTP Postfix\r\n"
                    return ()
                clientAct conn = do
                    (s, chatLog) <- runChatT greeting conn
                    s `shouldBe` SendState { mxHostname  = "localhost"
                                           , mxPort      = "25"
                                           , lastCommand = Nothing
                                           , lastReply   = Just (220, ["smtp.example.com ESMTP Postfix"])
                                           , timestamps  = []
                                           }
                    chatLog `shouldBe` ["220 smtp.example.com ESMTP Postfix"]
            chatTest clientAct serverAct

    describe "helo" $
        it "works" $ do
            let serverAct sock = do
                    _ <- Socket.recv sock 4096 `shouldReturn` "HELO relay.example.com\r\n"
                    _ <- Socket.send sock "250 smtp.example.com, I am glad to meet you\r\n"
                    return ()
                clientName = "relay.example.com"
                clientAct conn = do
                    (s, chatLog) <- runChatT (helo clientName) conn
                    s `shouldBe` SendState { mxHostname  = "localhost"
                                           , mxPort      = "25"
                                           , lastCommand = Just "HELO relay.example.com"
                                           , lastReply   = Just (250, ["smtp.example.com, I am glad to meet you"])
                                           , timestamps  = []
                                           }
                    chatLog `shouldBe` [ "HELO relay.example.com"
                                       , "250 smtp.example.com, I am glad to meet you"
                                       ]
            chatTest clientAct serverAct

    describe "mailFrom" $
        it "works" $ do
            let serverAct sock = do
                    _ <- Socket.recv sock 4096 `shouldReturn` "MAIL FROM: <bob@example.com>\r\n"
                    _ <- Socket.send sock "250 Ok\r\n"
                    return ()
                sender = Mailbox "" "bob" "example.com"
                clientAct conn = do
                    (s, chatLog) <- runChatT (mailFrom sender) conn
                    s `shouldBe` SendState { mxHostname  = "localhost"
                                           , mxPort      = "25"
                                           , lastCommand = Just "MAIL FROM: <bob@example.com>"
                                           , lastReply   = Just (250, ["Ok"])
                                           , timestamps  = []
                                           }
                    chatLog `shouldBe` [ "MAIL FROM: <bob@example.com>"
                                       , "250 Ok"
                                       ]
            chatTest clientAct serverAct

    describe "rcptTo" $
        it "works" $ do
            let serverAct sock = do
                    _ <- Socket.recv sock 4096 `shouldReturn` "RCPT TO: <alice@example.com>\r\n"
                    _ <- Socket.send sock "250 Ok\r\n"
                    return ()
                recipient = Mailbox "" "alice" "example.com"
                clientAct conn = do
                    (s, chatLog) <- runChatT (rcptTo recipient) conn
                    s `shouldBe` SendState { mxHostname  = "localhost"
                                           , mxPort      = "25"
                                           , lastCommand = Just "RCPT TO: <alice@example.com>"
                                           , lastReply   = Just (250, ["Ok"])
                                           , timestamps  = []
                                           }
                    chatLog `shouldBe` [ "RCPT TO: <alice@example.com>"
                                       , "250 Ok"
                                       ]
            chatTest clientAct serverAct

    describe "dataInit" $
        it "works" $ do
            let serverAct sock = do
                    _ <- Socket.recv sock 4096 `shouldReturn` "DATA\r\n"
                    _ <- Socket.send sock "354 End data with <CR><LF>.<CR><LF>\r\n"
                    return ()
                clientAct conn = do
                    (s, chatLog) <- runChatT dataInit conn
                    s `shouldBe` SendState { mxHostname  = "localhost"
                                           , mxPort      = "25"
                                           , lastCommand = Just "DATA"
                                           , lastReply   = Just (354, ["End data with <CR><LF>.<CR><LF>"])
                                           , timestamps  = []
                                           }
                    chatLog `shouldBe` [ "DATA"
                                       , "354 End data with <CR><LF>.<CR><LF>"
                                       ]
            chatTest clientAct serverAct

    describe "dataBlock" $
        it "works" $ do
            let serverAct sock = do
                    _ <- Socket.recv sock 4096
                    return ()
                msg = Message (Header []) ""
                clientAct conn = do
                    (s, chatLog) <- runChatT (dataBlock msg) conn
                    s `shouldBe` SendState { mxHostname  = "localhost"
                                           , mxPort      = "25"
                                           , lastCommand = Nothing
                                           , lastReply   = Nothing
                                           , timestamps  = []
                                           }
                    chatLog `shouldBe` ["__REDACTED__"]
            chatTest clientAct serverAct

    describe "dataTerm" $
        it "works" $ do
            let serverAct sock = do
                    _ <- Socket.recv sock 4096 `shouldReturn` ".\r\n"
                    _ <- Socket.send sock "250 Ok: queued as 12345\r\n"
                    return ()
                clientAct conn = do
                    (s, chatLog) <- runChatT dataTerm conn
                    s `shouldBe` SendState { mxHostname  = "localhost"
                                           , mxPort      = "25"
                                           , lastCommand = Just "."
                                           , lastReply   = Just (250, ["Ok: queued as 12345"])
                                           , timestamps  = []
                                           }
                    chatLog `shouldBe` [ "."
                                       , "250 Ok: queued as 12345"
                                       ]
            chatTest clientAct serverAct

    describe "multiline replies" $
        it "works" $ do
            let serverAct sock = do
                    _ <- Socket.recv sock 4096 `shouldReturn` "HELO relay.example.com\r\n"
                    _ <- Socket.send sock "250-this is\r\n250-a\r\n250 multiline reply\r\n"
                    return ()
                clientAct conn = do
                    (s, chatLog) <- runChatT (helo "relay.example.com") conn
                    s `shouldBe` SendState { mxHostname  = "localhost"
                                           , mxPort      = "25"
                                           , lastCommand = Just "HELO relay.example.com"
                                           , lastReply   = Just (250, ["this is", "a", "multiline reply"])
                                           , timestamps  = []
                                           }
                    chatLog `shouldBe` [ "HELO relay.example.com"
                                       , "250-this is\r\n250-a\r\n250 multiline reply"
                                       ]
            chatTest clientAct serverAct

    describe "chat" $
        it "works" $ do
            let serverAct sock = do
                    _ <- Socket.send sock "220 smtp.example.com ESMTP Postfix\r\n"
                    _ <- Socket.recv sock 4096 `shouldReturn` "HELO relay.example.com\r\n"
                    _ <- Socket.send sock "250 smtp.example.com, I am glad to meet you\r\n"
                    _ <- Socket.recv sock 4096 `shouldReturn` "MAIL FROM: <bob@example.com>\r\n"
                    _ <- Socket.send sock "250 Ok\r\n"
                    _ <- Socket.recv sock 4096 `shouldReturn` "RCPT TO: <alice@example.com>\r\n"
                    _ <- Socket.send sock "250 Ok\r\n"
                    _ <- Socket.recv sock 4096 `shouldReturn` "DATA\r\n"
                    _ <- Socket.send sock "354 End data with <CR><LF>.<CR><LF>\r\n"
                    _ <- Socket.recv sock 4096
                    _ <- Socket.send sock "250 Ok: queued as 12345\r\n"
                    return ()
                clientName = "relay.example.com"
                sender = Mailbox "" "bob" "example.com"
                recipient = Mailbox "" "alice" "example.com"
                msg = Message (Header []) ""
                clientAct conn = do
                    (s, chatLog) <- runChatT (chat clientName sender recipient msg) conn
                    let s' = s { timestamps = [] }
                    s' `shouldBe` SendState { mxHostname  = "localhost"
                                            , mxPort      = "25"
                                            , lastCommand = Just "."
                                            , lastReply   = Just (250, ["Ok: queued as 12345"])
                                            , timestamps  = []
                                            }
                    chatLog `shouldBe` [ "220 smtp.example.com ESMTP Postfix"
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

