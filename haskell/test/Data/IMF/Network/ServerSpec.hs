{-# LANGUAGE OverloadedStrings #-}

module Data.IMF.Network.ServerSpec where

import           Control.Concurrent             ( forkIO )
import           Control.Concurrent.MVar        ( newEmptyMVar
                                                , putMVar
                                                , takeMVar
                                                )
import           Control.Exception              ( bracket
                                                , throw
                                                )
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

import           Data.IMF.Network.Chat
import qualified Data.IMF.Network.Connection   as Conn
import           Data.IMF.Network.Errors
import           Data.IMF.Network.Server
import           Data.IMF.Parsers.Network

testServerParams :: ServerParams
testServerParams = ServerParams
    { serverName        = "mx1.example.com"
    , serverIP          = "127.0.0.1"
    , serverPort        = "2525"
    , serverMaxSessions = 10
    , serverMaxMsgSize  = 10485760
    , serverMaxRcpts    = 100
    }

testHooks :: Hooks
testHooks = Hooks
    { hookLogSession       = \_   -> do return ()
    , hookVerifyReturnPath = \_   -> do return ()
    , hookVerifyRcpt       = \_   -> do return ()
    , hookSaveMessage      = \_ _ -> do return ()
    }

getTestBody :: IO LB.ByteString
getTestBody = do
    body <- LB.readFile "haskell/test/Fixtures/Messages/simple_addressing_1.txt"
    return $ LC.intercalate "\r\n" $ LC.split '\n' body

chatTest :: (Conn.Connection -> IO ()) -> (Conn.Connection -> IO ()) -> IO ()
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
        _ <- bracket (Conn.connect ("127.0.0.1", "0") ("127.0.0.1", "2525")) Conn.close clientAct
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

spec :: Spec
spec = do

    describe "quit-only chat" $
        it "works" $ do
            let clientAct conn = do
                    _ <- fst <$> recvAll (Conn.recv conn) (parse pReply) `shouldReturn` 220
                    Conn.send conn "QUIT\r\n"
                    _ <- fst <$> recvAll (Conn.recv conn) (parse pReply) `shouldReturn` 221
                    return ()
            let serverAct conn = do
                    (s, log) <- newSession conn >>= runSession testServerParams testHooks (return True)
                    sessionError s `shouldBe` OK
                    sessionClientName s `shouldBe` ""
                    sessionInitialized s `shouldBe` False
                    sessionBuffer s `shouldBe` Nothing
                    formatLogLines log `shouldBe` T.concat [ "220 mx1.example.com Service ready\r\n"
                                                           , "QUIT\r\n"
                                                           , "221 mx1.example.com Service closing transmission session\r\n"
                                                           ]
            chatTest clientAct serverAct

    describe "simple chat" $
        it "works" $ do
            let clientAct conn = do
                    _ <- fst <$> recvAll (Conn.recv conn) (parse pReply) `shouldReturn` 220
                    Conn.send conn "EHLO relay.example.com\r\n"
                    _ <- fst <$> recvAll (Conn.recv conn) (parse pReply) `shouldReturn` 250
                    Conn.send conn "MAIL FROM:<no-reply@example.com>\r\n"
                    _ <- recvAll (Conn.recv conn) (parse pReply) `shouldReturn` (250, ["OK"])
                    Conn.send conn "RCPT TO:<jsmith@example.com>\r\n"
                    _ <- recvAll (Conn.recv conn) (parse pReply) `shouldReturn` (250, ["OK"])
                    Conn.send conn "DATA\r\n"
                    _ <- recvAll (Conn.recv conn) (parse pReply) `shouldReturn` (354, ["End data with <CR><LF>.<CR><LF>"])
                    body <- getTestBody
                    Conn.send conn $ body `LB.append` "\r\n.\r\n"
                    _ <- recvAll (Conn.recv conn) (parse pReply) `shouldReturn` (250, ["OK"])
                    Conn.send conn "QUIT\r\n"
                    _ <- fst <$> recvAll (Conn.recv conn) (parse pReply) `shouldReturn` 221
                    return ()
            let serverAct conn = do
                    (s, log) <- newSession conn >>= runSession testServerParams testHooks (return True)
                    sessionError s `shouldBe` OK
                    sessionClientName s `shouldBe` "relay.example.com"
                    sessionInitialized s `shouldBe` True
                    sessionBuffer s `shouldBe` Nothing
                    formatLogLines log `shouldBe` T.concat [ "220 mx1.example.com Service ready\r\n"
                                                           , "EHLO relay.example.com\r\n"
                                                           , "250-mx1.example.com\r\n"
                                                           , "250-SIZE 10485760\r\n"
                                                           , "250-8BITMIME\r\n"
                                                           , "250 SMTPUTF8\r\n"
                                                           , "MAIL FROM:<no-reply@example.com>\r\n"
                                                           , "250 OK\r\n"
                                                           , "RCPT TO:<jsmith@example.com>\r\n"
                                                           , "250 OK\r\n"
                                                           , "DATA\r\n"
                                                           , "354 End data with <CR><LF>.<CR><LF>\r\n"
                                                           , "[...]\r\n"
                                                           , "250 OK\r\n"
                                                           , "QUIT\r\n"
                                                           , "221 mx1.example.com Service closing transmission session\r\n"
                                                           ]
            chatTest clientAct serverAct

    describe "command unrecognized" $
        it "fails" $ do
            let clientAct conn = do
                    _ <- fst <$> recvAll (Conn.recv conn) (parse pReply) `shouldReturn` 220
                    Conn.send conn "FOO\r\n"
                    _ <- recvAll (Conn.recv conn) (parse pReply) `shouldReturn` (500, ["Syntax error, command unrecognized"])
                    Conn.send conn "QUIT\r\n"
                    _ <- fst <$> recvAll (Conn.recv conn) (parse pReply) `shouldReturn` 221
                    return ()
            let serverAct conn = do
                    (s, log) <- newSession conn >>= runSession testServerParams testHooks (return True)
                    sessionError s `shouldBe` OK
                    formatLogLines log `shouldBe` T.concat [ "220 mx1.example.com Service ready\r\n"
                                                           , "FOO\r\n"
                                                           , "500 Syntax error, command unrecognized\r\n"
                                                           , "QUIT\r\n"
                                                           , "221 mx1.example.com Service closing transmission session\r\n"
                                                           ]
            chatTest clientAct serverAct

    describe "parameter unrecognized" $
        it "fails" $ do
            let clientAct conn = do
                    _ <- fst <$> recvAll (Conn.recv conn) (parse pReply) `shouldReturn` 220
                    Conn.send conn "EHLO relay.example.com\r\n"
                    _ <- fst <$> recvAll (Conn.recv conn) (parse pReply) `shouldReturn` 250
                    Conn.send conn "MAIL FROM:<no-reply@example.com> FOO!=BAR\r\n"
                    _ <- recvAll (Conn.recv conn) (parse pReply) `shouldReturn` (501, ["Syntax error, command parameter or argument unrecognized"])
                    Conn.send conn "QUIT\r\n"
                    _ <- fst <$> recvAll (Conn.recv conn) (parse pReply) `shouldReturn` 221
                    return ()
            let serverAct conn = do
                    (s, log) <- newSession conn >>= runSession testServerParams testHooks (return True)
                    sessionError s `shouldBe` OK
                    formatLogLines log `shouldBe` T.concat [ "220 mx1.example.com Service ready\r\n"
                                                           , "EHLO relay.example.com\r\n"
                                                           , "250-mx1.example.com\r\n"
                                                           , "250-SIZE 10485760\r\n"
                                                           , "250-8BITMIME\r\n"
                                                           , "250 SMTPUTF8\r\n"
                                                           , "MAIL FROM:<no-reply@example.com> FOO!=BAR\r\n"
                                                           , "501 Syntax error, command parameter or argument unrecognized\r\n"
                                                           , "QUIT\r\n"
                                                           , "221 mx1.example.com Service closing transmission session\r\n"
                                                           ]
            chatTest clientAct serverAct

    describe "command out of order" $
        it "fails" $ do
            let clientAct conn = do
                    _ <- fst <$> recvAll (Conn.recv conn) (parse pReply) `shouldReturn` 220
                    Conn.send conn "MAIL FROM:<no-reply@example.com>\r\n"
                    _ <- recvAll (Conn.recv conn) (parse pReply) `shouldReturn` (503, ["Bad sequence of commands"])
                    Conn.send conn "QUIT\r\n"
                    _ <- fst <$> recvAll (Conn.recv conn) (parse pReply) `shouldReturn` 221
                    return ()
            let serverAct conn = do
                    (s, log) <- newSession conn >>= runSession testServerParams testHooks (return True)
                    sessionError s `shouldBe` OK
                    formatLogLines log `shouldBe` T.concat [ "220 mx1.example.com Service ready\r\n"
                                                           , "MAIL FROM:<no-reply@example.com>\r\n"
                                                           , "503 Bad sequence of commands\r\n"
                                                           , "QUIT\r\n"
                                                           , "221 mx1.example.com Service closing transmission session\r\n"
                                                           ]
            chatTest clientAct serverAct

    describe "parameter not implemented" $
        it "fails" $ do
            let clientAct conn = do
                    _ <- fst <$> recvAll (Conn.recv conn) (parse pReply) `shouldReturn` 220
                    Conn.send conn "EHLO relay.example.com\r\n"
                    _ <- fst <$> recvAll (Conn.recv conn) (parse pReply) `shouldReturn` 250
                    Conn.send conn "MAIL FROM:<no-reply@example.com> FOO=BAR\r\n"
                    _ <- recvAll (Conn.recv conn) (parse pReply) `shouldReturn` (504, ["Command parameter or argument not implemented"])
                    Conn.send conn "QUIT\r\n"
                    _ <- fst <$> recvAll (Conn.recv conn) (parse pReply) `shouldReturn` 221
                    return ()
            let serverAct conn = do
                    (s, log) <- newSession conn >>= runSession testServerParams testHooks (return True)
                    sessionError s `shouldBe` OK
                    formatLogLines log `shouldBe` T.concat [ "220 mx1.example.com Service ready\r\n"
                                                           , "EHLO relay.example.com\r\n"
                                                           , "250-mx1.example.com\r\n"
                                                           , "250-SIZE 10485760\r\n"
                                                           , "250-8BITMIME\r\n"
                                                           , "250 SMTPUTF8\r\n"
                                                           , "MAIL FROM:<no-reply@example.com> FOO=BAR\r\n"
                                                           , "504 Command parameter or argument not implemented\r\n"
                                                           , "QUIT\r\n"
                                                           , "221 mx1.example.com Service closing transmission session\r\n"
                                                           ]
            chatTest clientAct serverAct

    describe "return path mailbox unrecognized" $
        it "fails" $ do
            let clientAct conn = do
                    _ <- fst <$> recvAll (Conn.recv conn) (parse pReply) `shouldReturn` 220
                    Conn.send conn "EHLO relay.example.com\r\n"
                    _ <- fst <$> recvAll (Conn.recv conn) (parse pReply) `shouldReturn` 250
                    Conn.send conn "MAIL FROM:<foo>\r\n"
                    _ <- recvAll (Conn.recv conn) (parse pReply) `shouldReturn` (550, ["Syntax error, mailbox unrecognized"])
                    Conn.send conn "QUIT\r\n"
                    _ <- fst <$> recvAll (Conn.recv conn) (parse pReply) `shouldReturn` 221
                    return ()
            let serverAct conn = do
                    (s, log) <- newSession conn >>= runSession testServerParams testHooks (return True)
                    sessionError s `shouldBe` OK
                    sessionBuffer s `shouldBe` Nothing
                    formatLogLines log `shouldBe` T.concat [ "220 mx1.example.com Service ready\r\n"
                                                           , "EHLO relay.example.com\r\n"
                                                           , "250-mx1.example.com\r\n"
                                                           , "250-SIZE 10485760\r\n"
                                                           , "250-8BITMIME\r\n"
                                                           , "250 SMTPUTF8\r\n"
                                                           , "MAIL FROM:<foo>\r\n"
                                                           , "550 Syntax error, mailbox unrecognized\r\n"
                                                           , "QUIT\r\n"
                                                           , "221 mx1.example.com Service closing transmission session\r\n"
                                                           ]
            chatTest clientAct serverAct

    describe "return path mailbox unavailable" $
        it "fails" $ do
            let clientAct conn = do
                    _ <- fst <$> recvAll (Conn.recv conn) (parse pReply) `shouldReturn` 220
                    Conn.send conn "EHLO relay.example.com\r\n"
                    _ <- fst <$> recvAll (Conn.recv conn) (parse pReply) `shouldReturn` 250
                    Conn.send conn "MAIL FROM:<no-reply@example.com>\r\n"
                    _ <- recvAll (Conn.recv conn) (parse pReply) `shouldReturn` (550, ["Mailbox unavailable"])
                    Conn.send conn "QUIT\r\n"
                    _ <- fst <$> recvAll (Conn.recv conn) (parse pReply) `shouldReturn` 221
                    return ()
            let serverAct conn = do
                    let hooks = testHooks { hookVerifyReturnPath = \_ -> do throw PermanentFailure }
                    (s, log) <- newSession conn >>= runSession testServerParams hooks (return True)
                    sessionError s `shouldBe` OK
                    sessionBuffer s `shouldBe` Nothing
                    formatLogLines log `shouldBe` T.concat [ "220 mx1.example.com Service ready\r\n"
                                                           , "EHLO relay.example.com\r\n"
                                                           , "250-mx1.example.com\r\n"
                                                           , "250-SIZE 10485760\r\n"
                                                           , "250-8BITMIME\r\n"
                                                           , "250 SMTPUTF8\r\n"
                                                           , "MAIL FROM:<no-reply@example.com>\r\n"
                                                           , "550 Mailbox unavailable\r\n"
                                                           , "QUIT\r\n"
                                                           , "221 mx1.example.com Service closing transmission session\r\n"
                                                           ]
            chatTest clientAct serverAct

    describe "recipient mailbox unrecognized" $
        it "fails" $ do
            let clientAct conn = do
                    _ <- fst <$> recvAll (Conn.recv conn) (parse pReply) `shouldReturn` 220
                    Conn.send conn "EHLO relay.example.com\r\n"
                    _ <- fst <$> recvAll (Conn.recv conn) (parse pReply) `shouldReturn` 250
                    Conn.send conn "MAIL FROM:<no-reply@example.com>\r\n"
                    _ <- recvAll (Conn.recv conn) (parse pReply) `shouldReturn` (250, ["OK"])
                    Conn.send conn "RCPT TO:<foo>\r\n"
                    _ <- recvAll (Conn.recv conn) (parse pReply) `shouldReturn` (550, ["Syntax error, mailbox unrecognized"])
                    Conn.send conn "QUIT\r\n"
                    _ <- fst <$> recvAll (Conn.recv conn) (parse pReply) `shouldReturn` 221
                    return ()
            let serverAct conn = do
                    let hooks = testHooks { hookVerifyRcpt = \_ -> do throw PermanentFailure }
                    (s, log) <- newSession conn >>= runSession testServerParams hooks (return True)
                    sessionError s `shouldBe` OK
                    sessionBuffer s `shouldBe` (Just $ Buffer { returnPath = "no-reply@example.com", rcpts = [], failedRcpts = ["<foo>"] })
                    formatLogLines log `shouldBe` T.concat [ "220 mx1.example.com Service ready\r\n"
                                                           , "EHLO relay.example.com\r\n"
                                                           , "250-mx1.example.com\r\n"
                                                           , "250-SIZE 10485760\r\n"
                                                           , "250-8BITMIME\r\n"
                                                           , "250 SMTPUTF8\r\n"
                                                           , "MAIL FROM:<no-reply@example.com>\r\n"
                                                           , "250 OK\r\n"
                                                           , "RCPT TO:<foo>\r\n"
                                                           , "550 Syntax error, mailbox unrecognized\r\n"
                                                           , "QUIT\r\n"
                                                           , "221 mx1.example.com Service closing transmission session\r\n"
                                                           ]
            chatTest clientAct serverAct

    describe "recipient mailbox unavailable" $
        it "fails" $ do
            let clientAct conn = do
                    _ <- fst <$> recvAll (Conn.recv conn) (parse pReply) `shouldReturn` 220
                    Conn.send conn "EHLO relay.example.com\r\n"
                    _ <- fst <$> recvAll (Conn.recv conn) (parse pReply) `shouldReturn` 250
                    Conn.send conn "MAIL FROM:<no-reply@example.com>\r\n"
                    _ <- recvAll (Conn.recv conn) (parse pReply) `shouldReturn` (250, ["OK"])
                    Conn.send conn "RCPT TO:<jsmith@example.com>\r\n"
                    _ <- recvAll (Conn.recv conn) (parse pReply) `shouldReturn` (550, ["Mailbox unavailable"])
                    Conn.send conn "QUIT\r\n"
                    _ <- fst <$> recvAll (Conn.recv conn) (parse pReply) `shouldReturn` 221
                    return ()
            let serverAct conn = do
                    let hooks = testHooks { hookVerifyRcpt = \_ -> do throw PermanentFailure }
                    (s, log) <- newSession conn >>= runSession testServerParams hooks (return True)
                    sessionError s `shouldBe` OK
                    sessionBuffer s `shouldBe` (Just $ Buffer { returnPath = "no-reply@example.com", rcpts = [], failedRcpts = ["<jsmith@example.com>"] })
                    formatLogLines log `shouldBe` T.concat [ "220 mx1.example.com Service ready\r\n"
                                                           , "EHLO relay.example.com\r\n"
                                                           , "250-mx1.example.com\r\n"
                                                           , "250-SIZE 10485760\r\n"
                                                           , "250-8BITMIME\r\n"
                                                           , "250 SMTPUTF8\r\n"
                                                           , "MAIL FROM:<no-reply@example.com>\r\n"
                                                           , "250 OK\r\n"
                                                           , "RCPT TO:<jsmith@example.com>\r\n"
                                                           , "550 Mailbox unavailable\r\n"
                                                           , "QUIT\r\n"
                                                           , "221 mx1.example.com Service closing transmission session\r\n"
                                                           ]
            chatTest clientAct serverAct

    describe "message unrecognized" $
        it "fails" $ do
            let clientAct conn = do
                    _ <- fst <$> recvAll (Conn.recv conn) (parse pReply) `shouldReturn` 220
                    Conn.send conn "EHLO relay.example.com\r\n"
                    _ <- fst <$> recvAll (Conn.recv conn) (parse pReply) `shouldReturn` 250
                    Conn.send conn "MAIL FROM:<no-reply@example.com>\r\n"
                    _ <- recvAll (Conn.recv conn) (parse pReply) `shouldReturn` (250, ["OK"])
                    Conn.send conn "RCPT TO:<jsmith@example.com>\r\n"
                    _ <- recvAll (Conn.recv conn) (parse pReply) `shouldReturn` (250, ["OK"])
                    Conn.send conn "DATA\r\n"
                    _ <- recvAll (Conn.recv conn) (parse pReply) `shouldReturn` (354, ["End data with <CR><LF>.<CR><LF>"])
                    Conn.send conn "foo\r\n.\r\n"
                    _ <- recvAll (Conn.recv conn) (parse pReply) `shouldReturn` (550, ["Syntax error, message unrecognized"])
                    Conn.send conn "QUIT\r\n"
                    _ <- fst <$> recvAll (Conn.recv conn) (parse pReply) `shouldReturn` 221
                    return ()
            let serverAct conn = do
                    (s, log) <- newSession conn >>= runSession testServerParams testHooks (return True)
                    sessionError s `shouldBe` OK
                    sessionBuffer s `shouldBe` (Just $ Buffer { returnPath = "no-reply@example.com", rcpts = ["jsmith@example.com"], failedRcpts = [] })
                    formatLogLines log `shouldBe` T.concat [ "220 mx1.example.com Service ready\r\n"
                                                           , "EHLO relay.example.com\r\n"
                                                           , "250-mx1.example.com\r\n"
                                                           , "250-SIZE 10485760\r\n"
                                                           , "250-8BITMIME\r\n"
                                                           , "250 SMTPUTF8\r\n"
                                                           , "MAIL FROM:<no-reply@example.com>\r\n"
                                                           , "250 OK\r\n"
                                                           , "RCPT TO:<jsmith@example.com>\r\n"
                                                           , "250 OK\r\n"
                                                           , "DATA\r\n"
                                                           , "354 End data with <CR><LF>.<CR><LF>\r\n"
                                                           , "[...]\r\n"
                                                           , "550 Syntax error, message unrecognized\r\n"
                                                           , "QUIT\r\n"
                                                           , "221 mx1.example.com Service closing transmission session\r\n"
                                                           ]
            chatTest clientAct serverAct

    describe "message too large" $
        it "fails" $ do
            let clientAct conn = do
                    _ <- fst <$> recvAll (Conn.recv conn) (parse pReply) `shouldReturn` 220
                    Conn.send conn "EHLO relay.example.com\r\n"
                    _ <- fst <$> recvAll (Conn.recv conn) (parse pReply) `shouldReturn` 250
                    Conn.send conn "MAIL FROM:<no-reply@example.com>\r\n"
                    _ <- recvAll (Conn.recv conn) (parse pReply) `shouldReturn` (250, ["OK"])
                    Conn.send conn "RCPT TO:<jsmith@example.com>\r\n"
                    _ <- recvAll (Conn.recv conn) (parse pReply) `shouldReturn` (250, ["OK"])
                    Conn.send conn "DATA\r\n"
                    _ <- recvAll (Conn.recv conn) (parse pReply) `shouldReturn` (354, ["End data with <CR><LF>.<CR><LF>"])
                    body <- getTestBody
                    Conn.send conn $ body `LB.append` "\r\n.\r\n"
                    _ <- recvAll (Conn.recv conn) (parse pReply) `shouldReturn` (552, ["Message exceeds fixed maximum message size"])
                    Conn.send conn "QUIT\r\n"
                    _ <- fst <$> recvAll (Conn.recv conn) (parse pReply) `shouldReturn` 221
                    return ()
            let serverAct conn = do
                    let params = testServerParams { serverMaxMsgSize = 1 }
                    (s, log) <- newSession conn >>= runSession params testHooks (return True)
                    sessionError s `shouldBe` OK
                    sessionBuffer s `shouldBe` (Just $ Buffer { returnPath = "no-reply@example.com", rcpts = ["jsmith@example.com"], failedRcpts = [] })
                    formatLogLines log `shouldBe` T.concat [ "220 mx1.example.com Service ready\r\n"
                                                           , "EHLO relay.example.com\r\n"
                                                           , "250-mx1.example.com\r\n"
                                                           , "250-SIZE 1\r\n"
                                                           , "250-8BITMIME\r\n"
                                                           , "250 SMTPUTF8\r\n"
                                                           , "MAIL FROM:<no-reply@example.com>\r\n"
                                                           , "250 OK\r\n"
                                                           , "RCPT TO:<jsmith@example.com>\r\n"
                                                           , "250 OK\r\n"
                                                           , "DATA\r\n"
                                                           , "354 End data with <CR><LF>.<CR><LF>\r\n"
                                                           , "[...]\r\n"
                                                           , "552 Message exceeds fixed maximum message size\r\n"
                                                           , "QUIT\r\n"
                                                           , "221 mx1.example.com Service closing transmission session\r\n"
                                                           ]
            chatTest clientAct serverAct

    describe "message transaction failed" $
        it "fails" $ do
            let clientAct conn = do
                    _ <- fst <$> recvAll (Conn.recv conn) (parse pReply) `shouldReturn` 220
                    Conn.send conn "EHLO relay.example.com\r\n"
                    _ <- fst <$> recvAll (Conn.recv conn) (parse pReply) `shouldReturn` 250
                    Conn.send conn "MAIL FROM:<no-reply@example.com>\r\n"
                    _ <- recvAll (Conn.recv conn) (parse pReply) `shouldReturn` (250, ["OK"])
                    Conn.send conn "RCPT TO:<jsmith@example.com>\r\n"
                    _ <- recvAll (Conn.recv conn) (parse pReply) `shouldReturn` (250, ["OK"])
                    Conn.send conn "DATA\r\n"
                    _ <- recvAll (Conn.recv conn) (parse pReply) `shouldReturn` (354, ["End data with <CR><LF>.<CR><LF>"])
                    body <- getTestBody
                    Conn.send conn $ body `LB.append` "\r\n.\r\n"
                    _ <- recvAll (Conn.recv conn) (parse pReply) `shouldReturn` (554, ["Transaction failed"])
                    Conn.send conn "QUIT\r\n"
                    _ <- fst <$> recvAll (Conn.recv conn) (parse pReply) `shouldReturn` 221
                    return ()
            let serverAct conn = do
                    let hooks = testHooks { hookSaveMessage = \_ _ -> throw PermanentFailure }
                    (s, log) <- newSession conn >>= runSession testServerParams hooks (return True)
                    sessionError s `shouldBe` OK
                    sessionBuffer s `shouldBe` (Just $ Buffer { returnPath = "no-reply@example.com", rcpts = ["jsmith@example.com"], failedRcpts = [] })
                    formatLogLines log `shouldBe` T.concat [ "220 mx1.example.com Service ready\r\n"
                                                           , "EHLO relay.example.com\r\n"
                                                           , "250-mx1.example.com\r\n"
                                                           , "250-SIZE 10485760\r\n"
                                                           , "250-8BITMIME\r\n"
                                                           , "250 SMTPUTF8\r\n"
                                                           , "MAIL FROM:<no-reply@example.com>\r\n"
                                                           , "250 OK\r\n"
                                                           , "RCPT TO:<jsmith@example.com>\r\n"
                                                           , "250 OK\r\n"
                                                           , "DATA\r\n"
                                                           , "354 End data with <CR><LF>.<CR><LF>\r\n"
                                                           , "[...]\r\n"
                                                           , "554 Transaction failed\r\n"
                                                           , "QUIT\r\n"
                                                           , "221 mx1.example.com Service closing transmission session\r\n"
                                                           ]
            chatTest clientAct serverAct

    describe "early shutdown" $
        it "fails" $ do
            let clientAct conn = do
                    _ <- fst <$> recvAll (Conn.recv conn) (parse pReply) `shouldReturn` 220
                    Conn.send conn "EHLO relay.example.com\r\n"
                    _ <- fst <$> recvAll (Conn.recv conn) (parse pReply) `shouldReturn` 250
                    Conn.send conn "MAIL FROM:<no-reply@example.com>\r\n"
                    _ <- fst <$> recvAll (Conn.recv conn) (parse pReply) `shouldReturn` 421
                    Conn.send conn "QUIT\r\n"
                    _ <- fst <$> recvAll (Conn.recv conn) (parse pReply) `shouldReturn` 221
                    return ()
            let serverAct conn = do
                    (s, log) <- newSession conn >>= runSession testServerParams testHooks (return False)
                    sessionError s `shouldBe` OK
                    formatLogLines log `shouldBe` T.concat [ "220 mx1.example.com Service ready\r\n"
                                                           , "EHLO relay.example.com\r\n"
                                                           , "250-mx1.example.com\r\n"
                                                           , "250-SIZE 10485760\r\n"
                                                           , "250-8BITMIME\r\n"
                                                           , "250 SMTPUTF8\r\n"
                                                           , "MAIL FROM:<no-reply@example.com>\r\n"
                                                           , "421 mx1.example.com Service not available, shutting down\r\n"
                                                           , "QUIT\r\n"
                                                           , "221 mx1.example.com Service closing transmission session\r\n"
                                                           ]
            chatTest clientAct serverAct

    describe "too many recipients" $
        it "fails" $ do
            let clientAct conn = do
                    _ <- fst <$> recvAll (Conn.recv conn) (parse pReply) `shouldReturn` 220
                    Conn.send conn "EHLO relay.example.com\r\n"
                    _ <- fst <$> recvAll (Conn.recv conn) (parse pReply) `shouldReturn` 250
                    Conn.send conn "MAIL FROM:<no-reply@example.com>\r\n"
                    _ <- recvAll (Conn.recv conn) (parse pReply) `shouldReturn` (250, ["OK"])
                    Conn.send conn "RCPT TO:<jsmith@example.com>\r\n"
                    _ <- recvAll (Conn.recv conn) (parse pReply) `shouldReturn` (250, ["OK"])
                    Conn.send conn "RCPT TO:<msmith@example.com>\r\n"
                    _ <- recvAll (Conn.recv conn) (parse pReply) `shouldReturn` (452, ["Too many recipients"])
                    Conn.send conn "QUIT\r\n"
                    _ <- fst <$> recvAll (Conn.recv conn) (parse pReply) `shouldReturn` 221
                    return ()
            let serverAct conn = do
                    let params = testServerParams { serverMaxRcpts = 1 }
                    (s, log) <- newSession conn >>= runSession params testHooks (return True)
                    sessionError s `shouldBe` OK
                    formatLogLines log `shouldBe` T.concat [ "220 mx1.example.com Service ready\r\n"
                                                           , "EHLO relay.example.com\r\n"
                                                           , "250-mx1.example.com\r\n"
                                                           , "250-SIZE 10485760\r\n"
                                                           , "250-8BITMIME\r\n"
                                                           , "250 SMTPUTF8\r\n"
                                                           , "MAIL FROM:<no-reply@example.com>\r\n"
                                                           , "250 OK\r\n"
                                                           , "RCPT TO:<jsmith@example.com>\r\n"
                                                           , "250 OK\r\n"
                                                           , "RCPT TO:<msmith@example.com>\r\n"
                                                           , "452 Too many recipients\r\n"
                                                           , "QUIT\r\n"
                                                           , "221 mx1.example.com Service closing transmission session\r\n"
                                                           ]
            chatTest clientAct serverAct

    describe "peer connection closed" $
        it "fails" $ do
            let clientAct conn = do
                    _ <- fst <$> recvAll (Conn.recv conn) (parse pReply) `shouldReturn` 220
                    return ()
            let serverAct conn = do
                    (s, log) <- newSession conn >>= runSession testServerParams testHooks (return True)
                    sessionError s `shouldBe` PeerConnectionClosed
                    formatLogLines log `shouldBe` T.concat [ "220 mx1.example.com Service ready\r\n"
                                                           ]
            chatTest clientAct serverAct

