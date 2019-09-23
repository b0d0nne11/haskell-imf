{-# LANGUAGE OverloadedStrings #-}

module Text.IMF.NetworkSpec where

import           Control.Concurrent          (forkIO)
import           Control.Concurrent.MVar     (newEmptyMVar, putMVar, takeMVar)
import           Control.Exception           (bracket)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as B
import qualified Data.ByteString.Char8       as C
import qualified Data.ByteString.Lazy        as LB
import           Data.Default.Class          (def)
import           Network.Socket              (AddrInfo (..), AddrInfoFlag (..),
                                              Socket, SocketOption (..),
                                              SocketType (..), accept, bind,
                                              close, defaultHints, getAddrInfo,
                                              listen, setSocketOption, socket)
import qualified Network.Socket              as Socket hiding (recv, send)
import qualified Network.Socket.ByteString   as Socket
import qualified Network.TLS                 as TLS
import qualified Network.TLS.Extra.Cipher    as TLS
import qualified Network.TLS.Extra.FFDHE     as TLS
import           Test.Hspec

import           Text.IMF.Format
import           Text.IMF.Header
import           Text.IMF.Mailbox
import           Text.IMF.Message
import           Text.IMF.Network.Client
import           Text.IMF.Network.Connection (Connection)
import qualified Text.IMF.Network.Connection as Connection

testClient :: Client
testClient = Client
    { connection  = Nothing
    , hostname    = ""
    , extensions  = []
    , lastCommand = Nothing
    , lastReply   = Nothing
    , lastErrors  = []
    }

testClientName :: ByteString
testClientName = "relay.example.com"

testConnParams :: ConnParams
testConnParams = ConnParams
    { connSourceIP = Just "127.0.0.1"
    , connRecipientDomain = "localhost"
    , connProxyHosts = Nothing
    }

testTLSParams :: Maybe TLSParams
testTLSParams = Just TLSParams
    { tlsRequired = False
    , tlsValidate = False
    }

testAuthParams :: Maybe AuthParams
testAuthParams = Just AuthParams
    { authRequired = False
    , authUsername = "username"
    , authPassword = "password"
    }

testClientParams :: ClientParams
testClientParams = ClientParams
    { clientName = testClientName
    , connParams = testConnParams
    , tlsParams  = testTLSParams
    , authParams = testAuthParams
    }

concatLog :: ClientLog -> ByteString
concatLog = B.concat . map (\(_, _, msg) -> msg)

getServerSocket :: IO Socket
getServerSocket = do
    -- setup server socket
    let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
    addr:_ <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just "2525")
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    setSocketOption sock ReuseAddr 1
    bind sock $ addrAddress addr
    listen sock 1
    return sock

closeServerSocket :: Socket -> IO ()
closeServerSocket = Socket.close

chatTest :: IO a -> (Socket -> IO b) -> IO ()
chatTest clientAct serverAct =
    bracket getServerSocket closeServerSocket $ \sock -> do
        sync <- newEmptyMVar
        _ <- forkIO $ server sock sync
        client sync
  where
    server sock sync = do
        _ <- putMVar sync ()
        _ <- bracket (fst <$> accept sock) Socket.close serverAct
        _ <- putMVar sync ()
        return ()
    client sync = do
        _ <- takeMVar sync
        _ <- clientAct
        _ <- takeMVar sync
        return ()

{-# ANN spec ("HLint: ignore Reduce duplication" :: String) #-}
spec :: Spec
spec = do

    describe "greeting" $
        it "works" $ do
            let serverAct sock = do
                    _ <- Socket.send sock "220 smtp.example.com ESMTP Postfix\r\n"
                    return ()
                clientAct = do
                    (s, chatLog) <- runChat (connect testConnParams >> greeting >> disconnect) testClient
                    lastCommand s `shouldBe` Nothing
                    lastReply s `shouldBe` Just (220, ["smtp.example.com ESMTP Postfix"])
                    lastErrors s `shouldBe` []
                    concatLog chatLog `shouldBe` B.concat ["220 smtp.example.com ESMTP Postfix\r\n"]
            chatTest clientAct serverAct

    describe "hello (ehlo)" $
        it "works" $ do
            let serverAct sock = do
                    _ <- Socket.recv sock 4096 `shouldReturn` "EHLO relay.example.com\r\n"
                    _ <- Socket.send sock "250-smtp.example.com\r\n250-SIZE 14680064\r\n250-PIPELINING\r\n250 HELP\r\n"
                    return ()
                clientAct = do
                    (s, chatLog) <- runChat (connect testConnParams >> hello testClientName >> disconnect) testClient
                    extensions s `shouldBe` [("size",["14680064"]),("pipelining",[]),("help",[])]
                    lastCommand s `shouldBe` Just "EHLO relay.example.com\r\n"
                    lastReply s `shouldBe` Just (250, ["smtp.example.com", "SIZE 14680064", "PIPELINING", "HELP"])
                    lastErrors s `shouldBe` []
                    concatLog chatLog `shouldBe` B.concat [ "EHLO relay.example.com\r\n"
                                                          , "250-smtp.example.com\r\n250-SIZE 14680064\r\n250-PIPELINING\r\n250 HELP\r\n"
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
                clientAct = do
                    (s, chatLog) <- runChat (connect testConnParams >> hello testClientName >> disconnect) testClient
                    extensions s `shouldBe` []
                    lastCommand s `shouldBe` Just "HELO relay.example.com\r\n"
                    lastReply s `shouldBe` Just (250, ["smtp.example.com, I am glad to meet you"])
                    lastErrors s `shouldBe` []
                    concatLog chatLog `shouldBe` B.concat [ "EHLO relay.example.com\r\n"
                                                          , "502 Command not implemented\r\n"
                                                          , "HELO relay.example.com\r\n"
                                                          , "250 smtp.example.com, I am glad to meet you\r\n"
                                                          ]
            chatTest clientAct serverAct

    describe "startTLS" $
        it "works" $ do
            let serverAct sock = do
                    _ <- Socket.recv sock 4096 `shouldReturn` "EHLO relay.example.com\r\n"
                    _ <- Socket.send sock "250-smtp.example.com\r\n250 STARTTLS\r\n"
                    _ <- Socket.recv sock 4096 `shouldReturn` "STARTTLS\r\n"
                    _ <- Socket.send sock "220 Go ahead\r\n"
                    creds <- TLS.credentialLoadX509 "haskell/test/Fixtures/localhost.crt" "haskell/test/Fixtures/localhost.key" >>= either fail return
                    let tlsParams = def { TLS.serverDHEParams = Just TLS.ffdhe4096
                                        , TLS.serverShared = def { TLS.sharedCredentials = TLS.Credentials [creds] }
                                        , TLS.serverSupported = def { TLS.supportedCiphers = TLS.ciphersuite_strong }
                                        }
                    ctx <- TLS.contextNew sock tlsParams
                    _ <- TLS.handshake ctx
                    _ <- TLS.recvData ctx `shouldReturn` "EHLO relay.example.com\r\n"
                    _ <- TLS.sendData ctx "250-smtp.example.com\r\n250 STARTTLS\r\n"
                    return ()
                clientAct = do
                    (s, chatLog) <- runChat (connect testConnParams >> hello testClientName >> startTLS testClientName testTLSParams >> disconnect) testClient
                    extensions s `shouldBe` [("starttls",[])]
                    lastCommand s `shouldBe` Just "EHLO relay.example.com\r\n"
                    lastReply s `shouldBe` Just (250, ["smtp.example.com", "STARTTLS"])
                    lastErrors s `shouldBe` []
                    concatLog chatLog `shouldBe` B.concat [ "EHLO relay.example.com\r\n"
                                                          , "250-smtp.example.com\r\n250 STARTTLS\r\n"
                                                          , "STARTTLS\r\n"
                                                          , "220 Go ahead\r\n"
                                                          , "[negotiate tls]r\n"
                                                          , "EHLO relay.example.com\r\n"
                                                          , "250-smtp.example.com\r\n250 STARTTLS\r\n"
                                                          ]
            chatTest clientAct serverAct

    describe "auth (login)" $
        it "works" $ do
            let serverAct sock = do
                    _ <- Socket.recv sock 4096 `shouldReturn` "EHLO relay.example.com\r\n"
                    _ <- Socket.send sock "250-smtp.example.com\r\n250-STARTTLS\r\n250 AUTH LOGIN\r\n"
                    _ <- Socket.recv sock 4096 `shouldReturn` "STARTTLS\r\n"
                    _ <- Socket.send sock "220 Go ahead\r\n"
                    creds <- TLS.credentialLoadX509 "haskell/test/Fixtures/localhost.crt" "haskell/test/Fixtures/localhost.key" >>= either fail return
                    let tlsParams = def { TLS.serverDHEParams = Just TLS.ffdhe4096
                                        , TLS.serverShared = def { TLS.sharedCredentials = TLS.Credentials [creds] }
                                        , TLS.serverSupported = def { TLS.supportedCiphers = TLS.ciphersuite_strong }
                                        }
                    ctx <- TLS.contextNew sock tlsParams
                    _ <- TLS.handshake ctx
                    _ <- TLS.recvData ctx `shouldReturn` "EHLO relay.example.com\r\n"
                    _ <- TLS.sendData ctx "250-smtp.example.com\r\n250-STARTTLS\r\n250 AUTH LOGIN\r\n"
                    _ <- TLS.recvData ctx `shouldReturn` "AUTH LOGIN\r\n"
                    _ <- TLS.sendData ctx "334 VXNlcm5hbWU6\r\n"
                    _ <- TLS.recvData ctx `shouldReturn` "dXNlcm5hbWU=\r\n"
                    _ <- TLS.sendData ctx "334 UGFzc3dvcmQ6\r\n"
                    _ <- TLS.recvData ctx `shouldReturn` "cGFzc3dvcmQ=\r\n"
                    _ <- TLS.sendData ctx "235 Authentication successful.\r\n"
                    return ()
                clientAct = do
                    (s, chatLog) <- runChat (connect testConnParams >> hello testClientName >> startTLS testClientName testTLSParams >> auth testAuthParams >> disconnect) testClient
                    extensions s `shouldBe` [("starttls",[]),("auth",["login"])]
                    lastCommand s `shouldBe` Nothing
                    lastReply s `shouldBe` Just (235, ["Authentication successful."])
                    lastErrors s `shouldBe` []
                    concatLog chatLog `shouldBe` B.concat [ "EHLO relay.example.com\r\n"
                                                          , "250-smtp.example.com\r\n250-STARTTLS\r\n250 AUTH LOGIN\r\n"
                                                          , "STARTTLS\r\n"
                                                          , "220 Go ahead\r\n"
                                                          , "[negotiate tls]r\n"
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
            let serverAct sock = do
                    _ <- Socket.recv sock 4096 `shouldReturn` "EHLO relay.example.com\r\n"
                    _ <- Socket.send sock "250-smtp.example.com\r\n250-STARTTLS\r\n250 AUTH PLAIN\r\n"
                    _ <- Socket.recv sock 4096 `shouldReturn` "STARTTLS\r\n"
                    _ <- Socket.send sock "220 Go ahead\r\n"
                    creds <- TLS.credentialLoadX509 "haskell/test/Fixtures/localhost.crt" "haskell/test/Fixtures/localhost.key" >>= either fail return
                    let tlsParams = def { TLS.serverDHEParams = Just TLS.ffdhe4096
                                        , TLS.serverShared = def { TLS.sharedCredentials = TLS.Credentials [creds] }
                                        , TLS.serverSupported = def { TLS.supportedCiphers = TLS.ciphersuite_strong }
                                        }
                    ctx <- TLS.contextNew sock tlsParams
                    _ <- TLS.handshake ctx
                    _ <- TLS.recvData ctx `shouldReturn` "EHLO relay.example.com\r\n"
                    _ <- TLS.sendData ctx "250-smtp.example.com\r\n250-STARTTLS\r\n250 AUTH PLAIN\r\n"
                    _ <- TLS.recvData ctx `shouldReturn` "AUTH PLAIN dXNlcm5hbWUAdXNlcm5hbWUAcGFzc3dvcmQ=\r\n"
                    _ <- TLS.sendData ctx "235 Authentication successful.\r\n"
                    return ()
                clientAct = do
                    (s, chatLog) <- runChat (connect testConnParams >> hello testClientName >> startTLS testClientName testTLSParams >> auth testAuthParams >> disconnect) testClient
                    extensions s `shouldBe` [("starttls",[]),("auth",["plain"])]
                    lastCommand s `shouldBe` Nothing
                    lastReply s `shouldBe` Just (235, ["Authentication successful."])
                    lastErrors s `shouldBe` []
                    concatLog chatLog `shouldBe` B.concat [ "EHLO relay.example.com\r\n"
                                                          , "250-smtp.example.com\r\n250-STARTTLS\r\n250 AUTH PLAIN\r\n"
                                                          , "STARTTLS\r\n"
                                                          , "220 Go ahead\r\n"
                                                          , "[negotiate tls]r\n"
                                                          , "EHLO relay.example.com\r\n"
                                                          , "250-smtp.example.com\r\n250-STARTTLS\r\n250 AUTH PLAIN\r\n"
                                                          , "[...]\r\n"
                                                          , "235 Authentication successful.\r\n"
                                                          ]
            chatTest clientAct serverAct

    describe "mailFrom" $
        it "works" $ do
            let serverAct sock = do
                    _ <- Socket.recv sock 4096 `shouldReturn` "MAIL FROM: <jdoe@localhost>\r\n"
                    _ <- Socket.send sock "250 Ok\r\n"
                    return ()
                clientAct = do
                    (s, chatLog) <- runChat (connect testConnParams >> mailFrom (Mailbox "" "jdoe" "localhost") >> disconnect) testClient
                    lastCommand s `shouldBe` Just "MAIL FROM: <jdoe@localhost>\r\n"
                    lastReply s `shouldBe` Just (250, ["Ok"])
                    lastErrors s `shouldBe` []
                    concatLog chatLog `shouldBe` B.concat [ "MAIL FROM: <jdoe@localhost>\r\n"
                                                          , "250 Ok\r\n"
                                                          ]
            chatTest clientAct serverAct

    describe "rcptTo" $
        it "works" $ do
            let serverAct sock = do
                    _ <- Socket.recv sock 4096 `shouldReturn` "RCPT TO: <mary@localhost>\r\n"
                    _ <- Socket.send sock "250 Ok\r\n"
                    return ()
                clientAct = do
                    (s, chatLog) <- runChat (connect testConnParams >> rcptTo (Mailbox "" "mary" "localhost") >> disconnect) testClient
                    lastCommand s `shouldBe` Just "RCPT TO: <mary@localhost>\r\n"
                    lastReply s `shouldBe` Just (250, ["Ok"])
                    lastErrors s `shouldBe` []
                    concatLog chatLog `shouldBe` B.concat [ "RCPT TO: <mary@localhost>\r\n"
                                                          , "250 Ok\r\n"
                                                          ]
            chatTest clientAct serverAct

    describe "dataInit" $
        it "works" $ do
            let serverAct sock = do
                    _ <- Socket.recv sock 4096 `shouldReturn` "DATA\r\n"
                    _ <- Socket.send sock "354 End data with <CR><LF>.<CR><LF>\r\n"
                    return ()
                clientAct = do
                    (s, chatLog) <- runChat (connect testConnParams >> dataInit >> disconnect) testClient
                    lastCommand s `shouldBe` Just "DATA\r\n"
                    lastReply s `shouldBe` Just (354, ["End data with <CR><LF>.<CR><LF>"])
                    lastErrors s `shouldBe` []
                    concatLog chatLog `shouldBe` B.concat [ "DATA\r\n"
                                                          , "354 End data with <CR><LF>.<CR><LF>\r\n"
                                                          ]
            chatTest clientAct serverAct

    describe "dataBlock" $
        it "works" $ do
            let serverAct sock = do
                    _ <- Socket.recv sock 4096
                    return ()
                clientAct = do
                    body <- LB.readFile "haskell/test/Fixtures/Messages/simple_addressing_1.txt"
                    (s, chatLog) <- runChat (connect testConnParams >> dataBlock body >> disconnect) testClient
                    lastCommand s `shouldBe` Nothing
                    lastReply s `shouldBe` Nothing
                    lastErrors s `shouldBe` []
                    concatLog chatLog `shouldBe` B.concat ["[...]\r\n"]
            chatTest clientAct serverAct

    describe "dataTerm" $
        it "works" $ do
            let serverAct sock = do
                    _ <- Socket.recv sock 4096 `shouldReturn` ".\r\n"
                    _ <- Socket.send sock "250 Ok: queued as 12345\r\n"
                    return ()
                clientAct = do
                    (s, chatLog) <- runChat (connect testConnParams >> dataTerm >> disconnect) testClient
                    lastCommand s `shouldBe` Just ".\r\n"
                    lastReply s `shouldBe` Just (250, ["Ok: queued as 12345"])
                    lastErrors s `shouldBe` []
                    concatLog chatLog `shouldBe` B.concat [ ".\r\n"
                                                          , "250 Ok: queued as 12345\r\n"
                                                          ]
            chatTest clientAct serverAct

    describe "quit" $
        it "works" $ do
            let serverAct sock = do
                    _ <- Socket.recv sock 4096 `shouldReturn` "QUIT\r\n"
                    _ <- Socket.send sock "221 Bye\r\n"
                    return ()
                clientAct = do
                    (s, chatLog) <- runChat (connect testConnParams >> quit >> disconnect) testClient
                    lastCommand s `shouldBe` Just "QUIT\r\n"
                    lastReply s `shouldBe` Just (221, ["Bye"])
                    lastErrors s `shouldBe` []
                    concatLog chatLog `shouldBe` B.concat [ "QUIT\r\n"
                                                          , "221 Bye\r\n"
                                                          ]
            chatTest clientAct serverAct

    describe "deliver" $
        it "works" $ do
            let serverAct sock = do
                    _ <- Socket.send sock "220 smtp.example.com ESMTP Postfix\r\n"
                    _ <- Socket.recv sock 4096 `shouldReturn` "EHLO relay.example.com\r\n"
                    _ <- Socket.send sock "250-smtp.example.com\r\n250-SIZE 14680064\r\n250-PIPELINING\r\n250 HELP\r\n"
                    _ <- Socket.recv sock 4096 `shouldReturn` "MAIL FROM: <jdoe@localhost>\r\n"
                    _ <- Socket.send sock "250 Ok\r\n"
                    _ <- Socket.recv sock 4096 `shouldReturn` "RCPT TO: <mary@localhost>\r\n"
                    _ <- Socket.send sock "250 Ok\r\n"
                    _ <- Socket.recv sock 4096 `shouldReturn` "DATA\r\n"
                    _ <- Socket.send sock "354 End data with <CR><LF>.<CR><LF>\r\n"
                    _ <- Socket.recv sock 4096
                    _ <- Socket.recv sock 4096 `shouldReturn` ".\r\n"
                    _ <- Socket.send sock "250 Ok: queued as 12345\r\n"
                    _ <- Socket.recv sock 4096 `shouldReturn` "QUIT\r\n"
                    _ <- Socket.send sock "221 Bye\r\n"
                    return ()
                clientAct = do
                    body <- LB.readFile "haskell/test/Fixtures/Messages/simple_addressing_1.txt"
                    (s, chatLog1) <- newClient testClientParams
                    lastErrors s `shouldBe` []
                    (s, chatLog2) <- deliver (Envelope (Mailbox "" "jdoe" "localhost") (Mailbox "" "mary" "localhost")) body s
                    lastErrors s `shouldBe` []
                    (s, chatLog3) <- termClient s
                    extensions s `shouldBe` [("size",["14680064"]),("pipelining",[]),("help",[])]
                    lastCommand s `shouldBe` Just "QUIT\r\n"
                    lastReply s `shouldBe` Just (221, ["Bye"])
                    lastErrors s `shouldBe` []
                    concatLog chatLog1 `shouldBe` B.concat [ "220 smtp.example.com ESMTP Postfix\r\n"
                                                           , "EHLO relay.example.com\r\n"
                                                           , "250-smtp.example.com\r\n250-SIZE 14680064\r\n250-PIPELINING\r\n250 HELP\r\n"
                                                           ]
                    concatLog chatLog2 `shouldBe` B.concat [ "MAIL FROM: <jdoe@localhost>\r\n"
                                                           , "250 Ok\r\n"
                                                           , "RCPT TO: <mary@localhost>\r\n"
                                                           , "250 Ok\r\n"
                                                           , "DATA\r\n"
                                                           , "354 End data with <CR><LF>.<CR><LF>\r\n"
                                                           , "[...]\r\n"
                                                           , ".\r\n"
                                                           , "250 Ok: queued as 12345\r\n"
                                                           ]
                    concatLog chatLog3 `shouldBe` B.concat [ "QUIT\r\n"
                                                           , "221 Bye\r\n"
                                                           ]
            chatTest clientAct serverAct

