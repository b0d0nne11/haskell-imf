{-# LANGUAGE OverloadedStrings #-}

module Data.IMF.Network.ClientSpec where

import           Control.Concurrent             ( forkIO )
import           Control.Concurrent.MVar        ( newEmptyMVar
                                                , putMVar
                                                , takeMVar
                                                )
import           Control.Exception              ( bracket )
import           Control.Monad                  ( when )
import           Data.Attoparsec.ByteString.Char8
                                                ( Result
                                                , IResult(..)
                                                , parse
                                                )
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as LB
import qualified Data.ByteString.Lazy.Char8    as LC
import qualified Data.Text                     as T
import           Test.Hspec

import           Data.IMF.Types
import           Data.IMF.Network.Chat
import           Data.IMF.Network.Client
import           Data.IMF.Network.ClientPool
import qualified Data.IMF.Network.Connection   as Conn
import           Data.IMF.Network.Errors
import           Data.IMF.Parsers.Network

testClientParams :: ClientParams
testClientParams = ClientParams
    { clientName            = "relay.example.com"
    , clientSourceIP        = "127.0.0.1"
    , clientRecipientDomain = "localhost"
    , clientProxyHosts      = Nothing
    , clientAuthCredentials = Just ("username", "password")
    , clientTLSRequired     = False
    , clientTLSValidated    = False
    }

testClientLimits :: ClientLimits
testClientLimits = ClientLimits
    { maxClients        = 25
    , maxClientMessages = 10
    , maxClientIdleTime = 30
    , maxClientTime     = 300
    }

getTestBody :: IO LB.ByteString
getTestBody = do
    body <- LB.readFile "haskell/test/Fixtures/Messages/simple_addressing_1.txt"
    return $ LC.intercalate "\r\n" $ LC.split '\n' body

chatTest :: IO () -> (Conn.Connection -> IO ()) -> IO ()
chatTest clientAct serverAct =
    bracket (Conn.listen ("127.0.0.1", "2525")) Conn.close $ \serverConn -> do
        sync <- newEmptyMVar
        _ <- forkIO $ server serverConn sync
        client sync
  where
    server serverConn sync = do
        _ <- putMVar sync ()
        _ <- bracket (Conn.accept serverConn) Conn.close serverAct
        _ <- putMVar sync ()
        return ()
    client sync = do
        _ <- takeMVar sync
        _ <- clientAct
        _ <- takeMVar sync
        return ()

recvAll :: IO B.ByteString -> (B.ByteString -> Result a) -> IO a
recvAll recv parse = do
    chunk <- recv
    when (chunk == "") $ fail "peer has closed its side of the connection"
    case parse chunk of
        Done _ a       -> return a
        Fail{}         -> fail "bad response format"
        Partial parse' -> recvAll recv parse'

{-# ANN spec ("HLint: ignore Reduce duplication" :: String) #-}
spec :: Spec
spec = do

    describe "connect/disconnect" $
        it "works" $ do
            let serverAct _ = do
                    return ()
                clientAct = do
                    (c, chatLog) <- newClient >>= runChat (connect testClientParams >> disconnect)
                    clientLastCommand c `shouldBe` Nothing
                    clientLastReply c `shouldBe` Nothing
                    clientError c `shouldBe` OK
                    formatLogLines chatLog `shouldBe` ""
            chatTest clientAct serverAct

    describe "greeting" $
        it "works" $ do
            let serverAct conn = do
                    _ <- Conn.send conn "220 smtp.example.com ESMTP Postfix\r\n"
                    return ()
                clientAct = do
                    (c, _) <- newClient >>= runChat (connect testClientParams)
                    (c, chatLog) <- runChat greeting c
                    (c, _) <- runChat disconnect c
                    clientLastCommand c `shouldBe` Nothing
                    clientLastReply c `shouldBe` Just (220, ["smtp.example.com ESMTP Postfix"])
                    clientError c `shouldBe` OK
                    formatLogLines chatLog `shouldBe` "220 smtp.example.com ESMTP Postfix\r\n"
            chatTest clientAct serverAct

    describe "hello (ehlo)" $
        it "works" $ do
            let serverAct conn = do
                    _ <- recvAll (Conn.recv conn) (parse pLine) `shouldReturn` "EHLO relay.example.com"
                    _ <- Conn.send conn "250-smtp.example.com\r\n250-SIZE 14680064\r\n250-PIPELINING\r\n250 HELP\r\n"
                    return ()
                clientAct = do
                    (c, _) <- newClient >>= runChat (connect testClientParams)
                    (c, chatLog) <- runChat (hello testClientParams) c
                    (c, _) <- runChat disconnect c
                    clientExtensions c `shouldBe` [("size",["14680064"]),("pipelining",[]),("help",[])]
                    clientLastCommand c `shouldBe` Just "EHLO relay.example.com\r\n"
                    clientLastReply c `shouldBe` Just (250, ["smtp.example.com", "SIZE 14680064", "PIPELINING", "HELP"])
                    clientError c `shouldBe` OK
                    formatLogLines chatLog `shouldBe` T.concat [ "EHLO relay.example.com\r\n"
                                                               , "250-smtp.example.com\r\n250-SIZE 14680064\r\n250-PIPELINING\r\n250 HELP\r\n"
                                                               ]
            chatTest clientAct serverAct

    describe "hello (helo)" $
        it "works" $ do
            let serverAct conn = do
                    _ <- recvAll (Conn.recv conn) (parse pLine) `shouldReturn` "EHLO relay.example.com"
                    _ <- Conn.send conn "502 Command not implemented\r\n"
                    _ <- recvAll (Conn.recv conn) (parse pLine) `shouldReturn` "HELO relay.example.com"
                    _ <- Conn.send conn "250 smtp.example.com, I am glad to meet you\r\n"
                    return ()
                clientAct = do
                    (c, _) <- newClient >>= runChat (connect testClientParams)
                    (c, chatLog) <- runChat (hello testClientParams) c
                    (c, _) <- runChat disconnect c
                    clientExtensions c `shouldBe` []
                    clientLastCommand c `shouldBe` Just "HELO relay.example.com\r\n"
                    clientLastReply c `shouldBe` Just (250, ["smtp.example.com, I am glad to meet you"])
                    clientError c `shouldBe` OK
                    formatLogLines chatLog `shouldBe` T.concat [ "EHLO relay.example.com\r\n"
                                                               , "502 Command not implemented\r\n"
                                                               , "HELO relay.example.com\r\n"
                                                               , "250 smtp.example.com, I am glad to meet you\r\n"
                                                               ]
            chatTest clientAct serverAct

    describe "startTLS" $
        it "works" $ do
            let serverAct conn = do
                    _ <- recvAll (Conn.recv conn) (parse pLine) `shouldReturn` "EHLO relay.example.com"
                    _ <- Conn.send conn "250-smtp.example.com\r\n250 STARTTLS\r\n"
                    _ <- recvAll (Conn.recv conn) (parse pLine) `shouldReturn` "STARTTLS"
                    _ <- Conn.send conn "220 Go ahead\r\n"
                    conn <- Conn.secureServer conn
                    _ <- recvAll (Conn.recv conn) (parse pLine) `shouldReturn` "EHLO relay.example.com"
                    _ <- Conn.send conn "250-smtp.example.com\r\n250 STARTTLS\r\n"
                    return ()
                clientAct = do
                    (c, _) <- newClient >>= runChat (connect testClientParams)
                    (c, chatLog) <- runChat (hello testClientParams >> startTLS testClientParams) c
                    (c, _) <- runChat disconnect c
                    clientExtensions c `shouldBe` [("starttls",[])]
                    clientLastCommand c `shouldBe` Just "EHLO relay.example.com\r\n"
                    clientLastReply c `shouldBe` Just (250, ["smtp.example.com", "STARTTLS"])
                    clientError c `shouldBe` OK
                    formatLogLines chatLog `shouldBe` T.concat [ "EHLO relay.example.com\r\n"
                                                               , "250-smtp.example.com\r\n250 STARTTLS\r\n"
                                                               , "STARTTLS\r\n"
                                                               , "220 Go ahead\r\n"
                                                               , "EHLO relay.example.com\r\n"
                                                               , "250-smtp.example.com\r\n250 STARTTLS\r\n"
                                                               ]
            chatTest clientAct serverAct

    describe "auth (login)" $
        it "works" $ do
            let serverAct conn = do
                    _ <- recvAll (Conn.recv conn) (parse pLine) `shouldReturn` "EHLO relay.example.com"
                    _ <- Conn.send conn "250-smtp.example.com\r\n250-STARTTLS\r\n250 AUTH LOGIN\r\n"
                    _ <- recvAll (Conn.recv conn) (parse pLine) `shouldReturn` "STARTTLS"
                    _ <- Conn.send conn "220 Go ahead\r\n"
                    conn <- Conn.secureServer conn
                    _ <- recvAll (Conn.recv conn) (parse pLine) `shouldReturn` "EHLO relay.example.com"
                    _ <- Conn.send conn "250-smtp.example.com\r\n250-STARTTLS\r\n250 AUTH LOGIN\r\n"
                    _ <- recvAll (Conn.recv conn) (parse pLine) `shouldReturn` "AUTH LOGIN"
                    _ <- Conn.send conn "334 VXNlcm5hbWU6\r\n"
                    _ <- recvAll (Conn.recv conn) (parse pLine) `shouldReturn` "dXNlcm5hbWU="
                    _ <- Conn.send conn "334 UGFzc3dvcmQ6\r\n"
                    _ <- recvAll (Conn.recv conn) (parse pLine) `shouldReturn` "cGFzc3dvcmQ="
                    _ <- Conn.send conn "235 Authentication successful.\r\n"
                    return ()
                clientAct = do
                    (c, _) <- newClient >>= runChat (connect testClientParams)
                    (c, chatLog) <- runChat (hello testClientParams >> startTLS testClientParams >> auth testClientParams) c
                    (c, _) <- runChat disconnect c
                    clientExtensions c `shouldBe` [("starttls",[]),("auth",["login"])]
                    clientLastCommand c `shouldBe` Nothing
                    clientLastReply c `shouldBe` Just (235, ["Authentication successful."])
                    clientError c `shouldBe` OK
                    formatLogLines chatLog `shouldBe` T.concat [ "EHLO relay.example.com\r\n"
                                                               , "250-smtp.example.com\r\n250-STARTTLS\r\n250 AUTH LOGIN\r\n"
                                                               , "STARTTLS\r\n"
                                                               , "220 Go ahead\r\n"
                                                               , "EHLO relay.example.com\r\n"
                                                               , "250-smtp.example.com\r\n250-STARTTLS\r\n250 AUTH LOGIN\r\n"
                                                               , "AUTH LOGIN\r\n"
                                                               , "334 VXNlcm5hbWU6\r\n"
                                                               , "[...]\r\n"
                                                               , "334 UGFzc3dvcmQ6\r\n"
                                                               , "[...]\r\n"
                                                               , "235 Authentication successful.\r\n"
                                                               ]
            chatTest clientAct serverAct

    describe "auth (plain)" $
        it "works" $ do
            let serverAct conn = do
                    _ <- recvAll (Conn.recv conn) (parse pLine) `shouldReturn` "EHLO relay.example.com"
                    _ <- Conn.send conn "250-smtp.example.com\r\n250-STARTTLS\r\n250 AUTH PLAIN\r\n"
                    _ <- recvAll (Conn.recv conn) (parse pLine) `shouldReturn` "STARTTLS"
                    _ <- Conn.send conn "220 Go ahead\r\n"
                    conn <- Conn.secureServer conn
                    _ <- recvAll (Conn.recv conn) (parse pLine) `shouldReturn` "EHLO relay.example.com"
                    _ <- Conn.send conn "250-smtp.example.com\r\n250-STARTTLS\r\n250 AUTH PLAIN\r\n"
                    _ <- recvAll (Conn.recv conn) (parse pLine) `shouldReturn` "AUTH PLAIN dXNlcm5hbWUAdXNlcm5hbWUAcGFzc3dvcmQ="
                    _ <- Conn.send conn "235 Authentication successful.\r\n"
                    return ()
                clientAct = do
                    (c, _) <- newClient >>= runChat (connect testClientParams)
                    (c, chatLog) <- runChat (hello testClientParams >> startTLS testClientParams >> auth testClientParams) c
                    (c, _) <- runChat disconnect c
                    clientExtensions c `shouldBe` [("starttls",[]),("auth",["plain"])]
                    clientLastCommand c `shouldBe` Nothing
                    clientLastReply c `shouldBe` Just (235, ["Authentication successful."])
                    clientError c `shouldBe` OK
                    formatLogLines chatLog `shouldBe` T.concat [ "EHLO relay.example.com\r\n"
                                                               , "250-smtp.example.com\r\n250-STARTTLS\r\n250 AUTH PLAIN\r\n"
                                                               , "STARTTLS\r\n"
                                                               , "220 Go ahead\r\n"
                                                               , "EHLO relay.example.com\r\n"
                                                               , "250-smtp.example.com\r\n250-STARTTLS\r\n250 AUTH PLAIN\r\n"
                                                               , "[...]\r\n"
                                                               , "235 Authentication successful.\r\n"
                                                               ]
            chatTest clientAct serverAct

    describe "mailFrom" $
        it "works" $ do
            let serverAct conn = do
                    _ <- recvAll (Conn.recv conn) (parse pLine) `shouldReturn` "MAIL FROM: <jdoe@localhost>"
                    _ <- Conn.send conn "250 Ok\r\n"
                    return ()
                clientAct = do
                    (c, _) <- newClient >>= runChat (connect testClientParams)
                    (c, chatLog) <- runChat (mailFrom $ Mailbox "" "jdoe" "localhost") c
                    (c, _) <- runChat disconnect c
                    clientLastCommand c `shouldBe` Just "MAIL FROM: <jdoe@localhost>\r\n"
                    clientLastReply c `shouldBe` Just (250, ["Ok"])
                    clientError c `shouldBe` OK
                    formatLogLines chatLog `shouldBe` T.concat [ "MAIL FROM: <jdoe@localhost>\r\n"
                                                               , "250 Ok\r\n"
                                                               ]
            chatTest clientAct serverAct

    describe "rcptTo" $
        it "works" $ do
            let serverAct conn = do
                    _ <- recvAll (Conn.recv conn) (parse pLine) `shouldReturn` "RCPT TO: <mary@localhost>"
                    _ <- Conn.send conn "250 Ok\r\n"
                    return ()
                clientAct = do
                    (c, _) <- newClient >>= runChat (connect testClientParams)
                    (c, chatLog) <- runChat (rcptTo $ Mailbox "" "mary" "localhost") c
                    (c, _) <- runChat disconnect c
                    clientLastCommand c `shouldBe` Just "RCPT TO: <mary@localhost>\r\n"
                    clientLastReply c `shouldBe` Just (250, ["Ok"])
                    clientError c `shouldBe` OK
                    formatLogLines chatLog `shouldBe` T.concat [ "RCPT TO: <mary@localhost>\r\n"
                                                               , "250 Ok\r\n"
                                                               ]
            chatTest clientAct serverAct

    describe "dataInit" $
        it "works" $ do
            let serverAct conn = do
                    _ <- recvAll (Conn.recv conn) (parse pLine) `shouldReturn` "DATA"
                    _ <- Conn.send conn "354 End data with <CR><LF>.<CR><LF>\r\n"
                    return ()
                clientAct = do
                    (c, _) <- newClient >>= runChat (connect testClientParams)
                    (c, chatLog) <- runChat dataInit c
                    (c, _) <- runChat disconnect c
                    clientLastCommand c `shouldBe` Just "DATA\r\n"
                    clientLastReply c `shouldBe` Just (354, ["End data with <CR><LF>.<CR><LF>"])
                    clientError c `shouldBe` OK
                    formatLogLines chatLog `shouldBe` T.concat [ "DATA\r\n"
                                                               , "354 End data with <CR><LF>.<CR><LF>\r\n"
                                                               ]
            chatTest clientAct serverAct

    describe "dataBlock" $
        it "works" $ do
            let serverAct conn = do
                    _ <- recvAll (Conn.recv conn) (parse pLine)
                    return ()
                clientAct = do
                    body <- getTestBody
                    (c, _) <- newClient >>= runChat (connect testClientParams)
                    (c, chatLog) <- runChat (dataBlock body) c
                    (c, _) <- runChat disconnect c
                    clientLastCommand c `shouldBe` Nothing
                    clientLastReply c `shouldBe` Nothing
                    clientError c `shouldBe` OK
                    formatLogLines chatLog `shouldBe` "[...]\r\n"
            chatTest clientAct serverAct

    describe "dataTerm" $
        it "works" $ do
            let serverAct conn = do
                    _ <- recvAll (Conn.recv conn) (parse pLine) `shouldReturn` "."
                    _ <- Conn.send conn "250 Ok: queued as 12345\r\n"
                    return ()
                clientAct = do
                    (c, _) <- newClient >>= runChat (connect testClientParams)
                    (c, chatLog) <- runChat dataTerm c
                    (c, _) <- runChat disconnect c
                    clientLastCommand c `shouldBe` Just ".\r\n"
                    clientLastReply c `shouldBe` Just (250, ["Ok: queued as 12345"])
                    clientError c `shouldBe` OK
                    formatLogLines chatLog `shouldBe` T.concat [ ".\r\n"
                                                               , "250 Ok: queued as 12345\r\n"
                                                               ]
            chatTest clientAct serverAct

    describe "quit" $
        it "works" $ do
            let serverAct conn = do
                    _ <- recvAll (Conn.recv conn) (parse pLine) `shouldReturn` "QUIT"
                    _ <- Conn.send conn "221 Bye\r\n"
                    return ()
                clientAct = do
                    (c, _) <- newClient >>= runChat (connect testClientParams)
                    (c, chatLog) <- runChat quit c
                    (c, _) <- runChat disconnect c
                    clientLastCommand c `shouldBe` Just "QUIT\r\n"
                    clientLastReply c `shouldBe` Just (221, ["Bye"])
                    clientError c `shouldBe` OK
                    formatLogLines chatLog `shouldBe` T.concat [ "QUIT\r\n"
                                                               , "221 Bye\r\n"
                                                               ]
            chatTest clientAct serverAct

    describe "deliver" $
        it "works" $ do
            let serverAct conn = do
                    _ <- Conn.send conn "220 smtp.example.com ESMTP Postfix\r\n"
                    _ <- recvAll (Conn.recv conn) (parse pLine) `shouldReturn` "EHLO relay.example.com"
                    _ <- Conn.send conn "250-smtp.example.com\r\n250-SIZE 14680064\r\n250-PIPELINING\r\n250 HELP\r\n"
                    _ <- recvAll (Conn.recv conn) (parse pLine) `shouldReturn` "MAIL FROM: <jdoe@localhost>"
                    _ <- Conn.send conn "250 Ok\r\n"
                    _ <- recvAll (Conn.recv conn) (parse pLine) `shouldReturn` "RCPT TO: <mary@localhost>"
                    _ <- Conn.send conn "250 Ok\r\n"
                    _ <- recvAll (Conn.recv conn) (parse pLine) `shouldReturn` "DATA"
                    _ <- Conn.send conn "354 End data with <CR><LF>.<CR><LF>\r\n"
                    _ <- recvAll (Conn.recv conn) (parse pBody)
                    _ <- Conn.send conn "250 Ok: queued as 12345\r\n"
                    _ <- recvAll (Conn.recv conn) (parse pLine) `shouldReturn` "QUIT"
                    _ <- Conn.send conn "221 Bye\r\n"
                    return ()
                clientAct = do
                    body <- getTestBody
                    (c, _) <- getClient testClientParams
                    (c, chatLog) <- deliver (Envelope (Mailbox "" "jdoe" "localhost") (Mailbox "" "mary" "localhost")) body c
                    clientError c `shouldBe` OK
                    (c, _) <- closeClient c
                    clientExtensions c `shouldBe` [("size",["14680064"]),("pipelining",[]),("help",[])]
                    clientLastCommand c `shouldBe` Just "QUIT\r\n"
                    clientLastReply c `shouldBe` Just (221, ["Bye"])
                    clientError c `shouldBe` OK
                    clientMessagesSent c `shouldBe` 1
                    formatLogLines chatLog `shouldBe` T.concat [ "MAIL FROM: <jdoe@localhost>\r\n"
                                                               , "250 Ok\r\n"
                                                               , "RCPT TO: <mary@localhost>\r\n"
                                                               , "250 Ok\r\n"
                                                               , "DATA\r\n"
                                                               , "354 End data with <CR><LF>.<CR><LF>\r\n"
                                                               , "[...]\r\n"
                                                               , ".\r\n"
                                                               , "250 Ok: queued as 12345\r\n"
                                                               ]
            chatTest clientAct serverAct

    describe "deliverViaPool" $
        it "works" $ do
            let serverAct conn = do
                    _ <- Conn.send conn "220 smtp.example.com ESMTP Postfix\r\n"
                    _ <- recvAll (Conn.recv conn) (parse pLine) `shouldReturn` "EHLO relay.example.com"
                    _ <- Conn.send conn "250-smtp.example.com\r\n250-SIZE 14680064\r\n250-PIPELINING\r\n250 HELP\r\n"
                    _ <- recvAll (Conn.recv conn) (parse pLine) `shouldReturn` "MAIL FROM: <jdoe@localhost>"
                    _ <- Conn.send conn "250 Ok\r\n"
                    _ <- recvAll (Conn.recv conn) (parse pLine) `shouldReturn` "RCPT TO: <mary@localhost>"
                    _ <- Conn.send conn "250 Ok\r\n"
                    _ <- recvAll (Conn.recv conn) (parse pLine) `shouldReturn` "DATA"
                    _ <- Conn.send conn "354 End data with <CR><LF>.<CR><LF>\r\n"
                    _ <- recvAll (Conn.recv conn) (parse pBody)
                    _ <- Conn.send conn "250 Ok: queued as 12345\r\n"
                    _ <- recvAll (Conn.recv conn) (parse pLine) `shouldReturn` "QUIT"
                    _ <- Conn.send conn "221 Bye\r\n"
                    return ()
                clientAct = do
                    body <- getTestBody
                    pool <- getClientPool testClientLimits testClientParams
                    Just (c, chatLog) <- deliverViaPool (Envelope (Mailbox "" "jdoe" "localhost") (Mailbox "" "mary" "localhost")) body pool
                    closeClientPool pool
                    clientExtensions c `shouldBe` [("size",["14680064"]),("pipelining",[]),("help",[])]
                    clientLastCommand c `shouldBe` Just ".\r\n"
                    clientLastReply c `shouldBe` Just (250, ["Ok: queued as 12345"])
                    clientError c `shouldBe` OK
                    clientMessagesSent c `shouldBe` 1
                    formatLogLines chatLog `shouldBe` T.concat [ "MAIL FROM: <jdoe@localhost>\r\n"
                                                               , "250 Ok\r\n"
                                                               , "RCPT TO: <mary@localhost>\r\n"
                                                               , "250 Ok\r\n"
                                                               , "DATA\r\n"
                                                               , "354 End data with <CR><LF>.<CR><LF>\r\n"
                                                               , "[...]\r\n"
                                                               , ".\r\n"
                                                               , "250 Ok: queued as 12345\r\n"
                                                               ]
            chatTest clientAct serverAct
