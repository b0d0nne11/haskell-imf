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
                                           , extentions  = []
                                           }
                    chatLog `shouldBe` ["220 smtp.example.com ESMTP Postfix"]
            chatTest clientAct serverAct

    describe "hello (ehlo)" $
        it "works" $ do
            let serverAct sock = do
                    _ <- Socket.recv sock 4096 `shouldReturn` "EHLO relay.example.com\r\n"
                    _ <- Socket.send sock "250-smtp.example.com\r\n250-SIZE 14680064\r\n250-PIPELINING\r\n250 HELP\r\n"
                    return ()
                clientName = "relay.example.com"
                clientAct conn = do
                    (s, chatLog) <- runChatT (hello clientName) conn
                    s `shouldBe` SendState { mxHostname  = "localhost"
                                           , mxPort      = "25"
                                           , lastCommand = Just "EHLO relay.example.com"
                                           , lastReply   = Just (250, ["smtp.example.com", "SIZE 14680064", "PIPELINING", "HELP"])
                                           , timestamps  = []
                                           , extentions  = [("SIZE",["14680064"]),("PIPELINING",[]),("HELP",[])]
                                           }
                    chatLog `shouldBe` [ "EHLO relay.example.com"
                                       , "250-smtp.example.com\r\n250-SIZE 14680064\r\n250-PIPELINING\r\n250 HELP"
                                       ]
            chatTest clientAct serverAct

    describe "hello (helo)" $
        it "works" $ do
            let serverAct sock = do
                    _ <- Socket.recv sock 4096 `shouldReturn` "EHLO relay.example.com\r\n"
                    _ <- Socket.send sock "502 Command not implemented\r\n"
                    _ <- Socket.recv sock 4096 `shouldReturn` "HELO relay.example.com\r\n"
                    _ <- Socket.send sock "250 smtp.example.com, I am glad to meet you\r\n"
                    return ()
                clientName = "relay.example.com"
                clientAct conn = do
                    (s, chatLog) <- runChatT (hello clientName) conn
                    s `shouldBe` SendState { mxHostname  = "localhost"
                                           , mxPort      = "25"
                                           , lastCommand = Just "HELO relay.example.com"
                                           , lastReply   = Just (250, ["smtp.example.com, I am glad to meet you"])
                                           , timestamps  = []
                                           , extentions  = []
                                           }
                    chatLog `shouldBe` [ "EHLO relay.example.com"
                                       , "502 Command not implemented"
                                       , "HELO relay.example.com"
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
                                           , extentions  = []
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
                                           , extentions  = []
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
                                           , extentions  = []
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
                                           , extentions  = []
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
                                           , extentions  = []
                                           }
                    chatLog `shouldBe` [ "."
                                       , "250 Ok: queued as 12345"
                                       ]
            chatTest clientAct serverAct

    describe "chat" $
        it "works" $ do
            let serverAct sock = do
                    _ <- Socket.send sock "220 smtp.example.com ESMTP Postfix\r\n"
                    _ <- Socket.recv sock 4096 `shouldReturn` "EHLO relay.example.com\r\n"
                    _ <- Socket.send sock "250-smtp.example.com\r\n250-SIZE 14680064\r\n250-PIPELINING\r\n250 HELP\r\n"
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
                                            , extentions  = [("SIZE",["14680064"]),("PIPELINING",[]),("HELP",[])]
                                            }
                    chatLog `shouldBe` [ "220 smtp.example.com ESMTP Postfix"
                                       , "EHLO relay.example.com"
                                       , "250-smtp.example.com\r\n250-SIZE 14680064\r\n250-PIPELINING\r\n250 HELP"
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

