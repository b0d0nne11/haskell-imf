{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

{- HLINT ignore "Reduce duplication" -}

module Test.Server where

import           Control.Concurrent               (forkIO)
import           Control.Concurrent.MVar          (newEmptyMVar, putMVar, takeMVar)
import           Control.Exception                (bracket, throw)
import           Control.Monad                    (when)
import           Data.Attoparsec.ByteString.Char8 (IResult (..), Parser, Result, parse)
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Char8            as C
import qualified Data.ByteString.Lazy             as LB
import qualified Data.Text                        as T
import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.IMF.Network.Chat
import qualified Data.IMF.Network.Connection      as Conn
import           Data.IMF.Network.Errors
import           Data.IMF.Network.Server
import           Data.IMF.Parsers.Network

testServerParams :: ServerParams
testServerParams = ServerParams
    { serverName        = "mx1.example.com"
    , serverIP          = "127.0.0.1"
    , serverPort        = "2525"
    , serverMaxSessions = 10
    , serverMaxRcpts    = 100
    , serverMaxMsgSize  = 10485760
    , serverAuthReq     = False
    }

testHooks :: Hooks
testHooks = Hooks
    { hookLogSession       = \_   -> return ()
    , hookVerifyReturnPath = \_   -> return ()
    , hookVerifyRcpt       = \_   -> return ()
    , hookSaveMessage      = \_ _ -> return ()
    , hookAuthenticate     = \_ _ -> return ()
    }

testMessage :: IO LB.ByteString
testMessage = LB.fromStrict . (`C.append` "\r\n.\r\n") . C.intercalate "\r\n" . C.split '\n' <$> B.readFile "./test/Fixtures/Messages/simple_addressing_1.txt"

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

recvAll :: Parser a -> Conn.Connection -> IO [a]
recvAll parser = getAll (parse parser) []
  where
    getAll parser as conn = do
        bytes <- Conn.recv conn
        when (B.null bytes) $ fail "peer has closed its side of the connection"
        parseAll parser as bytes >>= \case
            (Just parser', as') -> getAll parser' as' conn
            (Nothing, as')      -> return as'
    parseAll parser as bytes =
        case parser bytes of
            Fail{}          -> fail "bad response format"
            Partial parser' -> return (Just parser', as)
            Done "" a       -> return (Nothing, as ++ [a])
            Done bytes' a   -> parseAll parser (as ++ [a]) bytes'

capabilities :: T.Text
capabilities = T.concat [ "250-SIZE 10485760\r\n"
                        , "250-8BITMIME\r\n"
                        , "250-SMTPUTF8\r\n"
                        , "250-PIPELINING\r\n"
                        , "250-STARTTLS\r\n"
                        , "250 AUTH PLAIN LOGIN\r\n"
                        ]

tests :: TestTree
tests = testGroup "client"
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
    , testParameterUnrecognized
    , testCommandOutOfOrder
    , testParameterNotImplemented
    , testEarlyShutdown
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

testConnect = testCase "connect/disconnect" $
    chatTest clientAct serverAct
  where
    clientAct conn = do
        r <- map fst <$> recvAll pReply conn
        r @?= [220]
        Conn.send conn "QUIT\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [221]
        return ()
    serverAct conn = do
        (s, log) <- newSession conn >>= runSession testServerParams testHooks (return True)
        sessionError s @?= Shutdown
        sessionClientName s @?= ""
        sessionInitialized s @?= False
        sessionAuthenticated s @?= False
        sessionBuffer s @?= Nothing
        formatLogLines log @?= T.concat [ "220 mx1.example.com Service ready\r\n"
                                        , "QUIT\r\n"
                                        , "221 mx1.example.com Service closing transmission session\r\n"
                                        ]

testSimpleChat = testCase "simple chat" $
    chatTest clientAct serverAct
  where
    clientAct conn = do
        r <- map fst <$> recvAll pReply conn
        r @?= [220]
        Conn.send conn "EHLO relay.example.com\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [250]
        Conn.send conn "MAIL FROM:<no-reply@example.com>\r\n"
        r <- recvAll pReply conn
        r @?= [(250, ["Mailbox <no-reply@example.com> OK"])]
        Conn.send conn "RCPT TO:<jsmith@example.com>\r\n"
        r <- recvAll pReply conn
        r @?= [(250, ["Mailbox <jsmith@example.com> OK"])]
        Conn.send conn "DATA\r\n"
        r <- recvAll pReply conn
        r @?= [(354, ["End data with <CR><LF>.<CR><LF>"])]
        testMessage >>= Conn.send conn
        r <- recvAll pReply conn
        r @?= [(250, ["OK"])]
        Conn.send conn "QUIT\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [221]
        return ()
    serverAct conn = do
        (s, log) <- newSession conn >>= runSession testServerParams testHooks (return True)
        sessionError s @?= Shutdown
        sessionClientName s @?= "relay.example.com"
        sessionInitialized s @?= True
        sessionAuthenticated s @?= False
        sessionBuffer s @?= Nothing
        formatLogLines log @?= T.concat [ "220 mx1.example.com Service ready\r\n"
                                        , "EHLO relay.example.com\r\n"
                                        , "250-mx1.example.com\r\n" `T.append` capabilities
                                        , "MAIL FROM:<no-reply@example.com>\r\n"
                                        , "250 Mailbox <no-reply@example.com> OK\r\n"
                                        , "RCPT TO:<jsmith@example.com>\r\n"
                                        , "250 Mailbox <jsmith@example.com> OK\r\n"
                                        , "DATA\r\n"
                                        , "354 End data with <CR><LF>.<CR><LF>\r\n"
                                        , "[...]\r\n"
                                        , "250 OK\r\n"
                                        , "QUIT\r\n"
                                        , "221 mx1.example.com Service closing transmission session\r\n"
                                        ]

testPipelinedChat = testCase "pipelined chat" $
    chatTest clientAct serverAct
  where
    clientAct conn = do
        r <- map fst <$> recvAll pReply conn
        r @?= [220]
        Conn.send conn "EHLO relay.example.com\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [250]
        Conn.send conn "MAIL FROM:<no-reply@example.com>\r\nRCPT TO:<jsmith@example.com>\r\nDATA\r\n"
        r <- recvAll pReply conn
        r @?= [(250, ["Mailbox <no-reply@example.com> OK"]), (250, ["Mailbox <jsmith@example.com> OK"]), (354, ["End data with <CR><LF>.<CR><LF>"])]
        testMessage >>= Conn.send conn
        r <- recvAll pReply conn
        r @?= [(250, ["OK"])]
        Conn.send conn "QUIT\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [221]
        return ()
    serverAct conn = do
        (s, log) <- newSession conn >>= runSession testServerParams testHooks (return True)
        sessionError s @?= Shutdown
        sessionClientName s @?= "relay.example.com"
        sessionInitialized s @?= True
        sessionAuthenticated s @?= False
        sessionBuffer s @?= Nothing
        formatLogLines log @?= T.concat [ "220 mx1.example.com Service ready\r\n"
                                        , "EHLO relay.example.com\r\n"
                                        , "250-mx1.example.com\r\n" `T.append` capabilities
                                        , "MAIL FROM:<no-reply@example.com>\r\n"
                                        , "RCPT TO:<jsmith@example.com>\r\n"
                                        , "DATA\r\n"
                                        , "250 Mailbox <no-reply@example.com> OK\r\n"
                                        , "250 Mailbox <jsmith@example.com> OK\r\n"
                                        , "354 End data with <CR><LF>.<CR><LF>\r\n"
                                        , "[...]\r\n"
                                        , "250 OK\r\n"
                                        , "QUIT\r\n"
                                        , "221 mx1.example.com Service closing transmission session\r\n"
                                        ]

testSecuredChat = testCase "secured chat" $
    chatTest clientAct serverAct
  where
    clientAct conn = do
        r <- map fst <$> recvAll pReply conn
        r @?= [220]
        Conn.send conn "EHLO relay.example.com\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [250]
        Conn.send conn "STARTTLS\r\n"
        r <- recvAll pReply conn
        r @?= [(220, ["Go ahead"])]
        conn <- Conn.secure conn "localhost" False
        Conn.send conn "EHLO relay.example.com\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [250]
        Conn.send conn "MAIL FROM:<no-reply@example.com>\r\n"
        r <- recvAll pReply conn
        r @?= [(250, ["Mailbox <no-reply@example.com> OK"])]
        Conn.send conn "RCPT TO:<jsmith@example.com>\r\n"
        r <- recvAll pReply conn
        r @?= [(250, ["Mailbox <jsmith@example.com> OK"])]
        Conn.send conn "DATA\r\n"
        r <- recvAll pReply conn
        r @?= [(354, ["End data with <CR><LF>.<CR><LF>"])]
        testMessage >>= Conn.send conn
        r <- recvAll pReply conn
        r @?= [(250, ["OK"])]
        Conn.send conn "QUIT\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [221]
        return ()
    serverAct conn = do
        (s, log) <- newSession conn >>= runSession testServerParams testHooks (return True)
        sessionError s @?= Shutdown
        sessionClientName s @?= "relay.example.com"
        sessionInitialized s @?= True
        sessionAuthenticated s @?= False
        sessionBuffer s @?= Nothing
        formatLogLines log @?= T.concat [ "220 mx1.example.com Service ready\r\n"
                                        , "EHLO relay.example.com\r\n"
                                        , "250-mx1.example.com\r\n" `T.append` capabilities
                                        , "STARTTLS\r\n"
                                        , "220 Go ahead\r\n"
                                        , "EHLO relay.example.com\r\n"
                                        , "250-mx1.example.com\r\n" `T.append` capabilities
                                        , "MAIL FROM:<no-reply@example.com>\r\n"
                                        , "250 Mailbox <no-reply@example.com> OK\r\n"
                                        , "RCPT TO:<jsmith@example.com>\r\n"
                                        , "250 Mailbox <jsmith@example.com> OK\r\n"
                                        , "DATA\r\n"
                                        , "354 End data with <CR><LF>.<CR><LF>\r\n"
                                        , "[...]\r\n"
                                        , "250 OK\r\n"
                                        , "QUIT\r\n"
                                        , "221 mx1.example.com Service closing transmission session\r\n"
                                        ]

testAuthPlain = testCase "authenticate (plain)" $
    chatTest clientAct serverAct
  where
    clientAct conn = do
        r <- map fst <$> recvAll pReply conn
        r @?= [220]
        Conn.send conn "EHLO relay.example.com\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [250]
        Conn.send conn "STARTTLS\r\n"
        r <- recvAll pReply conn
        r @?= [(220, ["Go ahead"])]
        conn <- Conn.secure conn "localhost" False
        Conn.send conn "EHLO relay.example.com\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [250]
        Conn.send conn "AUTH PLAIN\r\n"
        r <- recvAll pReply conn
        r @?= [(334, ["Go ahead"])]
        Conn.send conn "Zm9vAGJhcgBiYXI=\r\n"
        r <- recvAll pReply conn
        r @?= [(235, ["Authentication successful"])]
        Conn.send conn "QUIT\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [221]
        return ()
    serverAct conn = do
        (s, log) <- newSession conn >>= runSession testServerParams testHooks (return True)
        sessionError s @?= Shutdown
        sessionClientName s @?= "relay.example.com"
        sessionInitialized s @?= True
        sessionAuthenticated s @?= True
        sessionBuffer s @?= Nothing
        formatLogLines log @?= T.concat [ "220 mx1.example.com Service ready\r\n"
                                        , "EHLO relay.example.com\r\n"
                                        , "250-mx1.example.com\r\n" `T.append` capabilities
                                        , "STARTTLS\r\n"
                                        , "220 Go ahead\r\n"
                                        , "EHLO relay.example.com\r\n"
                                        , "250-mx1.example.com\r\n" `T.append` capabilities
                                        , "AUTH PLAIN\r\n"
                                        , "334 Go ahead\r\n"
                                        , "[...]\r\n"
                                        , "235 Authentication successful\r\n"
                                        , "QUIT\r\n"
                                        , "221 mx1.example.com Service closing transmission session\r\n"
                                        ]

testAuthPlainInit = testCase "authenticate (plain with initial)" $
    chatTest clientAct serverAct
  where
    clientAct conn = do
        r <- map fst <$> recvAll pReply conn
        r @?= [220]
        Conn.send conn "EHLO relay.example.com\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [250]
        Conn.send conn "STARTTLS\r\n"
        r <- recvAll pReply conn
        r @?= [(220, ["Go ahead"])]
        conn <- Conn.secure conn "localhost" False
        Conn.send conn "EHLO relay.example.com\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [250]
        Conn.send conn "AUTH PLAIN Zm9vAGJhcgBiYXI=\r\n"
        r <- recvAll pReply conn
        r @?= [(235, ["Authentication successful"])]
        Conn.send conn "QUIT\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [221]
        return ()
    serverAct conn = do
        (s, log) <- newSession conn >>= runSession testServerParams testHooks (return True)
        sessionError s @?= Shutdown
        sessionClientName s @?= "relay.example.com"
        sessionInitialized s @?= True
        sessionAuthenticated s @?= True
        sessionBuffer s @?= Nothing
        formatLogLines log @?= T.concat [ "220 mx1.example.com Service ready\r\n"
                                        , "EHLO relay.example.com\r\n"
                                        , "250-mx1.example.com\r\n" `T.append` capabilities
                                        , "STARTTLS\r\n"
                                        , "220 Go ahead\r\n"
                                        , "EHLO relay.example.com\r\n"
                                        , "250-mx1.example.com\r\n" `T.append` capabilities
                                        , "AUTH PLAIN Zm9vAGJhcgBiYXI=\r\n"
                                        , "235 Authentication successful\r\n"
                                        , "QUIT\r\n"
                                        , "221 mx1.example.com Service closing transmission session\r\n"
                                        ]

testAuthLogin = testCase "authenticate (login)" $
    chatTest clientAct serverAct
  where
    clientAct conn = do
        r <- map fst <$> recvAll pReply conn
        r @?= [220]
        Conn.send conn "EHLO relay.example.com\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [250]
        Conn.send conn "STARTTLS\r\n"
        r <- recvAll pReply conn
        r @?= [(220, ["Go ahead"])]
        conn <- Conn.secure conn "localhost" False
        Conn.send conn "EHLO relay.example.com\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [250]
        Conn.send conn "AUTH LOGIN\r\n"
        r <- recvAll pReply conn
        r @?= [(334, ["VXNlcm5hbWU6"])]
        Conn.send conn "Zm9v\r\n"
        r <- recvAll pReply conn
        r @?= [(334, ["UGFzc3dvcmQ6"])]
        Conn.send conn "YmFy\r\n"
        r <- recvAll pReply conn
        r @?= [(235, ["Authentication successful"])]
        Conn.send conn "QUIT\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [221]
        return ()
    serverAct conn = do
        (s, log) <- newSession conn >>= runSession testServerParams testHooks (return True)
        sessionError s @?= Shutdown
        sessionClientName s @?= "relay.example.com"
        sessionInitialized s @?= True
        sessionAuthenticated s @?= True
        sessionBuffer s @?= Nothing
        formatLogLines log @?= T.concat [ "220 mx1.example.com Service ready\r\n"
                                        , "EHLO relay.example.com\r\n"
                                        , "250-mx1.example.com\r\n" `T.append` capabilities
                                        , "STARTTLS\r\n"
                                        , "220 Go ahead\r\n"
                                        , "EHLO relay.example.com\r\n"
                                        , "250-mx1.example.com\r\n" `T.append` capabilities
                                        , "AUTH LOGIN\r\n"
                                        , "334 VXNlcm5hbWU6\r\n"
                                        , "[...]\r\n"
                                        , "334 UGFzc3dvcmQ6\r\n"
                                        , "[...]\r\n"
                                        , "235 Authentication successful\r\n"
                                        , "QUIT\r\n"
                                        , "221 mx1.example.com Service closing transmission session\r\n"
                                        ]

testAuthLoginInit = testCase "authenticate (login with initial)" $
    chatTest clientAct serverAct
  where
    clientAct conn = do
        r <- map fst <$> recvAll pReply conn
        r @?= [220]
        Conn.send conn "EHLO relay.example.com\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [250]
        Conn.send conn "STARTTLS\r\n"
        r <- recvAll pReply conn
        r @?= [(220, ["Go ahead"])]
        conn <- Conn.secure conn "localhost" False
        Conn.send conn "EHLO relay.example.com\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [250]
        Conn.send conn "AUTH LOGIN Zm9v\r\n"
        r <- recvAll pReply conn
        r @?= [(334, ["UGFzc3dvcmQ6"])]
        Conn.send conn "YmFy\r\n"
        r <- recvAll pReply conn
        r @?= [(235, ["Authentication successful"])]
        Conn.send conn "QUIT\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [221]
        return ()
    serverAct conn = do
        (s, log) <- newSession conn >>= runSession testServerParams testHooks (return True)
        sessionError s @?= Shutdown
        sessionClientName s @?= "relay.example.com"
        sessionInitialized s @?= True
        sessionAuthenticated s @?= True
        sessionBuffer s @?= Nothing
        formatLogLines log @?= T.concat [ "220 mx1.example.com Service ready\r\n"
                                        , "EHLO relay.example.com\r\n"
                                        , "250-mx1.example.com\r\n" `T.append` capabilities
                                        , "STARTTLS\r\n"
                                        , "220 Go ahead\r\n"
                                        , "EHLO relay.example.com\r\n"
                                        , "250-mx1.example.com\r\n" `T.append` capabilities
                                        , "AUTH LOGIN Zm9v\r\n"
                                        , "334 UGFzc3dvcmQ6\r\n"
                                        , "[...]\r\n"
                                        , "235 Authentication successful\r\n"
                                        , "QUIT\r\n"
                                        , "221 mx1.example.com Service closing transmission session\r\n"
                                        ]

testPeerConnectionClosed = testCase "peer connection closed" $
    chatTest clientAct serverAct
  where
    clientAct conn = do
        r <- map fst <$> recvAll pReply conn
        r @?= [220]
        return ()
    serverAct conn = do
        (s, log) <- newSession conn >>= runSession testServerParams testHooks (return True)
        sessionError s @?= PeerConnectionClosed
        formatLogLines log @?= T.concat [ "220 mx1.example.com Service ready\r\n"
                                        ]
testCommandUnrecognized = testCase "command unrecognized" $
    chatTest clientAct serverAct
  where
    clientAct conn = do
        r <- map fst <$> recvAll pReply conn
        r @?= [220]
        Conn.send conn "FOO\r\n"
        r <- recvAll pReply conn
        r @?= [(500, ["Syntax error, command unrecognized"])]
        Conn.send conn "QUIT\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [221]
        return ()
    serverAct conn = do
        (s, log) <- newSession conn >>= runSession testServerParams testHooks (return True)
        sessionError s @?= Shutdown
        formatLogLines log @?= T.concat [ "220 mx1.example.com Service ready\r\n"
                                        , "FOO\r\n"
                                        , "500 Syntax error, command unrecognized\r\n"
                                        , "QUIT\r\n"
                                        , "221 mx1.example.com Service closing transmission session\r\n"
                                        ]

testParameterUnrecognized = testCase "parameter unrecognized" $
    chatTest clientAct serverAct
  where
    clientAct conn = do
        r <- map fst <$> recvAll pReply conn
        r @?= [220]
        Conn.send conn "EHLO relay.example.com\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [250]
        Conn.send conn "MAIL FROM:<no-reply@example.com> FOO!=BAR\r\n"
        r <- recvAll pReply conn
        r @?= [(501, ["Syntax error, command parameter or argument unrecognized"])]
        Conn.send conn "QUIT\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [221]
        return ()
    serverAct conn = do
        (s, log) <- newSession conn >>= runSession testServerParams testHooks (return True)
        sessionError s @?= Shutdown
        formatLogLines log @?= T.concat [ "220 mx1.example.com Service ready\r\n"
                                        , "EHLO relay.example.com\r\n"
                                        , "250-mx1.example.com\r\n" `T.append` capabilities
                                        , "MAIL FROM:<no-reply@example.com> FOO!=BAR\r\n"
                                        , "501 Syntax error, command parameter or argument unrecognized\r\n"
                                        , "QUIT\r\n"
                                        , "221 mx1.example.com Service closing transmission session\r\n"
                                        ]

testCommandOutOfOrder = testCase "command out of order" $
    chatTest clientAct serverAct
  where
    clientAct conn = do
        r <- map fst <$> recvAll pReply conn
        r @?= [220]
        Conn.send conn "MAIL FROM:<no-reply@example.com>\r\n"
        r <- recvAll pReply conn
        r @?= [(503, ["Bad sequence of commands"])]
        Conn.send conn "QUIT\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [221]
        return ()
    serverAct conn = do
        (s, log) <- newSession conn >>= runSession testServerParams testHooks (return True)
        sessionError s @?= Shutdown
        formatLogLines log @?= T.concat [ "220 mx1.example.com Service ready\r\n"
                                        , "MAIL FROM:<no-reply@example.com>\r\n"
                                        , "503 Bad sequence of commands\r\n"
                                        , "QUIT\r\n"
                                        , "221 mx1.example.com Service closing transmission session\r\n"
                                        ]

testParameterNotImplemented = testCase "parameter not implemented" $
    chatTest clientAct serverAct
  where
    clientAct conn = do
        r <- map fst <$> recvAll pReply conn
        r @?= [220]
        Conn.send conn "EHLO relay.example.com\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [250]
        Conn.send conn "MAIL FROM:<no-reply@example.com> FOO=BAR\r\n"
        r <- recvAll pReply conn
        r @?= [(504, ["Command parameter or argument not implemented"])]
        Conn.send conn "QUIT\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [221]
        return ()
    serverAct conn = do
        (s, log) <- newSession conn >>= runSession testServerParams testHooks (return True)
        sessionError s @?= Shutdown
        formatLogLines log @?= T.concat [ "220 mx1.example.com Service ready\r\n"
                                        , "EHLO relay.example.com\r\n"
                                        , "250-mx1.example.com\r\n" `T.append` capabilities
                                        , "MAIL FROM:<no-reply@example.com> FOO=BAR\r\n"
                                        , "504 Command parameter or argument not implemented\r\n"
                                        , "QUIT\r\n"
                                        , "221 mx1.example.com Service closing transmission session\r\n"
                                        ]

testEarlyShutdown = testCase "early shutdown" $
    chatTest clientAct serverAct
  where
    clientAct conn = do
        r <- map fst <$> recvAll pReply conn
        r @?= [220]
        Conn.send conn "EHLO relay.example.com\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [250]
        Conn.send conn "MAIL FROM:<no-reply@example.com>\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [421]
        Conn.send conn "QUIT\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [221]
        return ()
    serverAct conn = do
        (s, log) <- newSession conn >>= runSession testServerParams testHooks (return False)
        sessionError s @?= Shutdown
        formatLogLines log @?= T.concat [ "220 mx1.example.com Service ready\r\n"
                                        , "EHLO relay.example.com\r\n"
                                        , "250-mx1.example.com\r\n" `T.append` capabilities
                                        , "MAIL FROM:<no-reply@example.com>\r\n"
                                        , "421 mx1.example.com Service not available, shutting down\r\n"
                                        , "QUIT\r\n"
                                        , "221 mx1.example.com Service closing transmission session\r\n"
                                        ]

testReturnPathUnrecognized = testCase "return path mailbox unrecognized" $
    chatTest clientAct serverAct
  where
    clientAct conn = do
        r <- map fst <$> recvAll pReply conn
        r @?= [220]
        Conn.send conn "EHLO relay.example.com\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [250]
        Conn.send conn "MAIL FROM:<foo>\r\n"
        r <- recvAll pReply conn
        r @?= [(550, ["Syntax error, mailbox <foo> unrecognized"])]
        Conn.send conn "QUIT\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [221]
        return ()
    serverAct conn = do
        (s, log) <- newSession conn >>= runSession testServerParams testHooks (return True)
        sessionError s @?= Shutdown
        sessionBuffer s @?= Nothing
        formatLogLines log @?= T.concat [ "220 mx1.example.com Service ready\r\n"
                                        , "EHLO relay.example.com\r\n"
                                        , "250-mx1.example.com\r\n" `T.append` capabilities
                                        , "MAIL FROM:<foo>\r\n"
                                        , "550 Syntax error, mailbox <foo> unrecognized\r\n"
                                        , "QUIT\r\n"
                                        , "221 mx1.example.com Service closing transmission session\r\n"
                                        ]

testReturnPathUnavailable = testCase "return path mailbox unavailable" $
    chatTest clientAct serverAct
  where
    clientAct conn = do
        r <- map fst <$> recvAll pReply conn
        r @?= [220]
        Conn.send conn "EHLO relay.example.com\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [250]
        Conn.send conn "MAIL FROM:<no-reply@example.com>\r\n"
        r <- recvAll pReply conn
        r @?= [(550, ["Mailbox <no-reply@example.com> unavailable"])]
        Conn.send conn "QUIT\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [221]
        return ()
    serverAct conn = do
        let hooks = testHooks { hookVerifyReturnPath = \_ -> throw PermanentFailure }
        (s, log) <- newSession conn >>= runSession testServerParams hooks (return True)
        sessionError s @?= Shutdown
        sessionBuffer s @?= Nothing
        formatLogLines log @?= T.concat [ "220 mx1.example.com Service ready\r\n"
                                        , "EHLO relay.example.com\r\n"
                                        , "250-mx1.example.com\r\n" `T.append` capabilities
                                        , "MAIL FROM:<no-reply@example.com>\r\n"
                                        , "550 Mailbox <no-reply@example.com> unavailable\r\n"
                                        , "QUIT\r\n"
                                        , "221 mx1.example.com Service closing transmission session\r\n"
                                        ]

testRecipientUnrecognized = testCase "recipient mailbox unrecognized" $
    chatTest clientAct serverAct
  where
    clientAct conn = do
        r <- map fst <$> recvAll pReply conn
        r @?= [220]
        Conn.send conn "EHLO relay.example.com\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [250]
        Conn.send conn "MAIL FROM:<no-reply@example.com>\r\n"
        r <- recvAll pReply conn
        r @?= [(250, ["Mailbox <no-reply@example.com> OK"])]
        Conn.send conn "RCPT TO:<foo>\r\n"
        r <- recvAll pReply conn
        r @?= [(550, ["Syntax error, mailbox <foo> unrecognized"])]
        Conn.send conn "QUIT\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [221]
        return ()
    serverAct conn = do
        let hooks = testHooks { hookVerifyRcpt = \_ -> throw PermanentFailure }
        (s, log) <- newSession conn >>= runSession testServerParams hooks (return True)
        sessionError s @?= Shutdown
        sessionBuffer s @?= (Just $ Buffer { returnPath = "no-reply@example.com", rcpts = [], failedRcpts = ["<foo>"] })
        formatLogLines log @?= T.concat [ "220 mx1.example.com Service ready\r\n"
                                        , "EHLO relay.example.com\r\n"
                                        , "250-mx1.example.com\r\n" `T.append` capabilities
                                        , "MAIL FROM:<no-reply@example.com>\r\n"
                                        , "250 Mailbox <no-reply@example.com> OK\r\n"
                                        , "RCPT TO:<foo>\r\n"
                                        , "550 Syntax error, mailbox <foo> unrecognized\r\n"
                                        , "QUIT\r\n"
                                        , "221 mx1.example.com Service closing transmission session\r\n"
                                        ]

testRecipientUnavailable = testCase "recipient mailbox unavailable" $
    chatTest clientAct serverAct
  where
    clientAct conn = do
        r <- map fst <$> recvAll pReply conn
        r @?= [220]
        Conn.send conn "EHLO relay.example.com\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [250]
        Conn.send conn "MAIL FROM:<no-reply@example.com>\r\n"
        r <- recvAll pReply conn
        r @?= [(250, ["Mailbox <no-reply@example.com> OK"])]
        Conn.send conn "RCPT TO:<jsmith@example.com>\r\n"
        r <- recvAll pReply conn
        r @?= [(550, ["Mailbox <jsmith@example.com> unavailable"])]
        Conn.send conn "QUIT\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [221]
        return ()
    serverAct conn = do
        let hooks = testHooks { hookVerifyRcpt = \_ -> throw PermanentFailure }
        (s, log) <- newSession conn >>= runSession testServerParams hooks (return True)
        sessionError s @?= Shutdown
        sessionBuffer s @?= (Just $ Buffer { returnPath = "no-reply@example.com", rcpts = [], failedRcpts = ["<jsmith@example.com>"] })
        formatLogLines log @?= T.concat [ "220 mx1.example.com Service ready\r\n"
                                        , "EHLO relay.example.com\r\n"
                                        , "250-mx1.example.com\r\n" `T.append` capabilities
                                        , "MAIL FROM:<no-reply@example.com>\r\n"
                                        , "250 Mailbox <no-reply@example.com> OK\r\n"
                                        , "RCPT TO:<jsmith@example.com>\r\n"
                                        , "550 Mailbox <jsmith@example.com> unavailable\r\n"
                                        , "QUIT\r\n"
                                        , "221 mx1.example.com Service closing transmission session\r\n"
                                        ]

testTooManyRecipients = testCase "too many recipients" $
    chatTest clientAct serverAct
  where
    clientAct conn = do
        r <- map fst <$> recvAll pReply conn
        r @?= [220]
        Conn.send conn "EHLO relay.example.com\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [250]
        Conn.send conn "MAIL FROM:<no-reply@example.com>\r\n"
        r <- recvAll pReply conn
        r @?= [(250, ["Mailbox <no-reply@example.com> OK"])]
        Conn.send conn "RCPT TO:<jsmith@example.com>\r\n"
        r <- recvAll pReply conn
        r @?= [(250, ["Mailbox <jsmith@example.com> OK"])]
        Conn.send conn "RCPT TO:<msmith@example.com>\r\n"
        r <- recvAll pReply conn
        r @?= [(452, ["Too many recipients"])]
        Conn.send conn "QUIT\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [221]
        return ()
    serverAct conn = do
        let params = testServerParams { serverMaxRcpts = 1 }
        (s, log) <- newSession conn >>= runSession params testHooks (return True)
        sessionError s @?= Shutdown
        sessionBuffer s @?= (Just $ Buffer { returnPath = "no-reply@example.com", rcpts = ["jsmith@example.com"], failedRcpts = [] })
        formatLogLines log @?= T.concat [ "220 mx1.example.com Service ready\r\n"
                                        , "EHLO relay.example.com\r\n"
                                        , "250-mx1.example.com\r\n" `T.append` capabilities
                                        , "MAIL FROM:<no-reply@example.com>\r\n"
                                        , "250 Mailbox <no-reply@example.com> OK\r\n"
                                        , "RCPT TO:<jsmith@example.com>\r\n"
                                        , "250 Mailbox <jsmith@example.com> OK\r\n"
                                        , "RCPT TO:<msmith@example.com>\r\n"
                                        , "452 Too many recipients\r\n"
                                        , "QUIT\r\n"
                                        , "221 mx1.example.com Service closing transmission session\r\n"
                                        ]

testMessageUnrecognized = testCase "message unrecognized" $
    chatTest clientAct serverAct
  where
    clientAct conn = do
        r <- map fst <$> recvAll pReply conn
        r @?= [220]
        Conn.send conn "EHLO relay.example.com\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [250]
        Conn.send conn "MAIL FROM:<no-reply@example.com>\r\n"
        r <- recvAll pReply conn
        r @?= [(250, ["Mailbox <no-reply@example.com> OK"])]
        Conn.send conn "RCPT TO:<jsmith@example.com>\r\n"
        r <- recvAll pReply conn
        r @?= [(250, ["Mailbox <jsmith@example.com> OK"])]
        Conn.send conn "DATA\r\n"
        r <- recvAll pReply conn
        r @?= [(354, ["End data with <CR><LF>.<CR><LF>"])]
        Conn.send conn "foo\r\n.\r\n"
        r <- recvAll pReply conn
        r @?= [(550, ["Syntax error, message unrecognized"])]
        Conn.send conn "QUIT\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [221]
        return ()
    serverAct conn = do
        (s, log) <- newSession conn >>= runSession testServerParams testHooks (return True)
        sessionError s @?= Shutdown
        sessionBuffer s @?= (Just $ Buffer { returnPath = "no-reply@example.com", rcpts = ["jsmith@example.com"], failedRcpts = [] })
        formatLogLines log @?= T.concat [ "220 mx1.example.com Service ready\r\n"
                                        , "EHLO relay.example.com\r\n"
                                        , "250-mx1.example.com\r\n" `T.append` capabilities
                                        , "MAIL FROM:<no-reply@example.com>\r\n"
                                        , "250 Mailbox <no-reply@example.com> OK\r\n"
                                        , "RCPT TO:<jsmith@example.com>\r\n"
                                        , "250 Mailbox <jsmith@example.com> OK\r\n"
                                        , "DATA\r\n"
                                        , "354 End data with <CR><LF>.<CR><LF>\r\n"
                                        , "[...]\r\n"
                                        , "550 Syntax error, message unrecognized\r\n"
                                        , "QUIT\r\n"
                                        , "221 mx1.example.com Service closing transmission session\r\n"
                                        ]

testMessageTooLarge = testCase "message too large" $
    chatTest clientAct serverAct
  where
    clientAct conn = do
        r <- map fst <$> recvAll pReply conn
        r @?= [220]
        Conn.send conn "EHLO relay.example.com\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [250]
        Conn.send conn "MAIL FROM:<no-reply@example.com>\r\n"
        r <- recvAll pReply conn
        r @?= [(250, ["Mailbox <no-reply@example.com> OK"])]
        Conn.send conn "RCPT TO:<jsmith@example.com>\r\n"
        r <- recvAll pReply conn
        r @?= [(250, ["Mailbox <jsmith@example.com> OK"])]
        Conn.send conn "DATA\r\n"
        r <- recvAll pReply conn
        r @?= [(354, ["End data with <CR><LF>.<CR><LF>"])]
        testMessage >>= Conn.send conn
        r <- recvAll pReply conn
        r @?= [(552, ["Message exceeds fixed maximum message size"])]
        Conn.send conn "QUIT\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [221]
        return ()
    serverAct conn = do
        let params = testServerParams { serverMaxMsgSize = 1 }
        (s, log) <- newSession conn >>= runSession params testHooks (return True)
        sessionError s @?= Shutdown
        sessionBuffer s @?= (Just $ Buffer { returnPath = "no-reply@example.com", rcpts = ["jsmith@example.com"], failedRcpts = [] })
        formatLogLines log @?= T.concat [ "220 mx1.example.com Service ready\r\n"
                                        , "EHLO relay.example.com\r\n"
                                        , "250-mx1.example.com\r\n" `T.append` T.replace "SIZE 10485760" "SIZE 1" capabilities
                                        , "MAIL FROM:<no-reply@example.com>\r\n"
                                        , "250 Mailbox <no-reply@example.com> OK\r\n"
                                        , "RCPT TO:<jsmith@example.com>\r\n"
                                        , "250 Mailbox <jsmith@example.com> OK\r\n"
                                        , "DATA\r\n"
                                        , "354 End data with <CR><LF>.<CR><LF>\r\n"
                                        , "[...]\r\n"
                                        , "552 Message exceeds fixed maximum message size\r\n"
                                        , "QUIT\r\n"
                                        , "221 mx1.example.com Service closing transmission session\r\n"
                                        ]

testMessageTansactionFailed = testCase "message transaction failed" $
    chatTest clientAct serverAct
  where
    clientAct conn = do
        r <- map fst <$> recvAll pReply conn
        r @?= [220]
        Conn.send conn "EHLO relay.example.com\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [250]
        Conn.send conn "MAIL FROM:<no-reply@example.com>\r\n"
        r <- recvAll pReply conn
        r @?= [(250, ["Mailbox <no-reply@example.com> OK"])]
        Conn.send conn "RCPT TO:<jsmith@example.com>\r\n"
        r <- recvAll pReply conn
        r @?= [(250, ["Mailbox <jsmith@example.com> OK"])]
        Conn.send conn "DATA\r\n"
        r <- recvAll pReply conn
        r @?= [(354, ["End data with <CR><LF>.<CR><LF>"])]
        testMessage >>= Conn.send conn
        r <- recvAll pReply conn
        r @?= [(554, ["Transaction failed"])]
        Conn.send conn "QUIT\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [221]
        return ()
    serverAct conn = do
        let hooks = testHooks { hookSaveMessage = \_ _ -> throw PermanentFailure }
        (s, log) <- newSession conn >>= runSession testServerParams hooks (return True)
        sessionError s @?= Shutdown
        sessionBuffer s @?= (Just $ Buffer { returnPath = "no-reply@example.com", rcpts = ["jsmith@example.com"], failedRcpts = [] })
        formatLogLines log @?= T.concat [ "220 mx1.example.com Service ready\r\n"
                                        , "EHLO relay.example.com\r\n"
                                        , "250-mx1.example.com\r\n" `T.append` capabilities
                                        , "MAIL FROM:<no-reply@example.com>\r\n"
                                        , "250 Mailbox <no-reply@example.com> OK\r\n"
                                        , "RCPT TO:<jsmith@example.com>\r\n"
                                        , "250 Mailbox <jsmith@example.com> OK\r\n"
                                        , "DATA\r\n"
                                        , "354 End data with <CR><LF>.<CR><LF>\r\n"
                                        , "[...]\r\n"
                                        , "554 Transaction failed\r\n"
                                        , "QUIT\r\n"
                                        , "221 mx1.example.com Service closing transmission session\r\n"
                                        ]

testNoValidRecipients = testCase "no valid recipients" $
    chatTest clientAct serverAct
  where
    clientAct conn = do
        r <- map fst <$> recvAll pReply conn
        r @?= [220]
        Conn.send conn "EHLO relay.example.com\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [250]
        Conn.send conn "MAIL FROM:<no-reply@example.com>\r\n"
        r <- recvAll pReply conn
        r @?= [(250, ["Mailbox <no-reply@example.com> OK"])]
        Conn.send conn "DATA\r\n"
        r <- recvAll pReply conn
        r @?= [(554, ["No valid recipients"])]
        Conn.send conn "QUIT\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [221]
        return ()
    serverAct conn = do
        (s, log) <- newSession conn >>= runSession testServerParams testHooks (return True)
        sessionError s @?= Shutdown
        formatLogLines log @?= T.concat [ "220 mx1.example.com Service ready\r\n"
                                        , "EHLO relay.example.com\r\n"
                                        , "250-mx1.example.com\r\n" `T.append` capabilities
                                        , "MAIL FROM:<no-reply@example.com>\r\n"
                                        , "250 Mailbox <no-reply@example.com> OK\r\n"
                                        , "DATA\r\n"
                                        , "554 No valid recipients\r\n"
                                        , "QUIT\r\n"
                                        , "221 mx1.example.com Service closing transmission session\r\n"
                                        ]

testCredentialsUnrecognized = testCase "credentials unrecognized" $
    chatTest clientAct serverAct
  where
    clientAct conn = do
        r <- map fst <$> recvAll pReply conn
        r @?= [220]
        Conn.send conn "EHLO relay.example.com\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [250]
        Conn.send conn "STARTTLS\r\n"
        r <- recvAll pReply conn
        r @?= [(220, ["Go ahead"])]
        conn <- Conn.secure conn "localhost" False
        Conn.send conn "EHLO relay.example.com\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [250]
        Conn.send conn "AUTH PLAIN Zm9v\r\n"
        r <- recvAll pReply conn
        r @?= [(501, ["Syntax error, credentials unrecognized"])]
        Conn.send conn "QUIT\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [221]
        return ()
    serverAct conn = do
        (s, log) <- newSession conn >>= runSession testServerParams testHooks (return True)
        sessionError s @?= Shutdown
        sessionClientName s @?= "relay.example.com"
        sessionInitialized s @?= True
        sessionAuthenticated s @?= False
        sessionBuffer s @?= Nothing
        formatLogLines log @?= T.concat [ "220 mx1.example.com Service ready\r\n"
                                        , "EHLO relay.example.com\r\n"
                                        , "250-mx1.example.com\r\n" `T.append` capabilities
                                        , "STARTTLS\r\n"
                                        , "220 Go ahead\r\n"
                                        , "EHLO relay.example.com\r\n"
                                        , "250-mx1.example.com\r\n" `T.append` capabilities
                                        , "AUTH PLAIN Zm9v\r\n"
                                        , "501 Syntax error, credentials unrecognized\r\n"
                                        , "QUIT\r\n"
                                        , "221 mx1.example.com Service closing transmission session\r\n"
                                        ]

testCredentialsInvalid = testCase "credentials invalid" $
    chatTest clientAct serverAct
  where
    clientAct conn = do
        r <- map fst <$> recvAll pReply conn
        r @?= [220]
        Conn.send conn "EHLO relay.example.com\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [250]
        Conn.send conn "STARTTLS\r\n"
        r <- recvAll pReply conn
        r @?= [(220, ["Go ahead"])]
        conn <- Conn.secure conn "localhost" False
        Conn.send conn "EHLO relay.example.com\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [250]
        Conn.send conn "AUTH PLAIN dXNlcm5hbWUAdXNlcm5hbWUAcGFzc3dvcmQ=\r\n"
        r <- recvAll pReply conn
        r @?= [(535, ["Authentication failed"])]
        Conn.send conn "QUIT\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [221]
        return ()
    serverAct conn = do
        let hooks = testHooks { hookAuthenticate = \_ _ -> throw PermanentFailure }
        (s, log) <- newSession conn >>= runSession testServerParams hooks (return True)
        sessionError s @?= Shutdown
        sessionClientName s @?= "relay.example.com"
        sessionInitialized s @?= True
        sessionAuthenticated s @?= False
        sessionBuffer s @?= Nothing
        formatLogLines log @?= T.concat [ "220 mx1.example.com Service ready\r\n"
                                        , "EHLO relay.example.com\r\n"
                                        , "250-mx1.example.com\r\n" `T.append` capabilities
                                        , "STARTTLS\r\n"
                                        , "220 Go ahead\r\n"
                                        , "EHLO relay.example.com\r\n"
                                        , "250-mx1.example.com\r\n" `T.append` capabilities
                                        , "AUTH PLAIN dXNlcm5hbWUAdXNlcm5hbWUAcGFzc3dvcmQ=\r\n"
                                        , "535 Authentication failed\r\n"
                                        , "QUIT\r\n"
                                        , "221 mx1.example.com Service closing transmission session\r\n"
                                        ]

testAuthenticationRequired = testCase "authentication required" $
    chatTest clientAct serverAct
  where
    clientAct conn = do
        r <- map fst <$> recvAll pReply conn
        r @?= [220]
        Conn.send conn "EHLO relay.example.com\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [250]
        Conn.send conn "MAIL FROM:<no-reply@example.com>\r\n"
        r <- recvAll pReply conn
        r @?= [(530, ["Authentication required"])]
        Conn.send conn "QUIT\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [221]
        return ()
    serverAct conn = do
        let params = testServerParams { serverAuthReq = True }
        (s, log) <- newSession conn >>= runSession params testHooks (return True)
        sessionError s @?= Shutdown
        sessionClientName s @?= "relay.example.com"
        sessionInitialized s @?= True
        sessionAuthenticated s @?= False
        sessionBuffer s @?= Nothing
        formatLogLines log @?= T.concat [ "220 mx1.example.com Service ready\r\n"
                                        , "EHLO relay.example.com\r\n"
                                        , "250-mx1.example.com\r\n" `T.append` capabilities
                                        , "MAIL FROM:<no-reply@example.com>\r\n"
                                        , "530 Authentication required\r\n"
                                        , "QUIT\r\n"
                                        , "221 mx1.example.com Service closing transmission session\r\n"
                                        ]

testEncryptionRequired = testCase "encryption required" $
    chatTest clientAct serverAct
  where
    clientAct conn = do
        r <- map fst <$> recvAll pReply conn
        r @?= [220]
        Conn.send conn "EHLO relay.example.com\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [250]
        Conn.send conn "AUTH PLAIN\r\n"
        r <- recvAll pReply conn
        r @?= [(538, ["Encryption required"])]
        Conn.send conn "QUIT\r\n"
        r <- map fst <$> recvAll pReply conn
        r @?= [221]
        return ()
    serverAct conn = do
        let params = testServerParams { serverAuthReq = True }
        (s, log) <- newSession conn >>= runSession params testHooks (return True)
        sessionError s @?= Shutdown
        sessionClientName s @?= "relay.example.com"
        sessionInitialized s @?= True
        sessionAuthenticated s @?= False
        sessionBuffer s @?= Nothing
        formatLogLines log @?= T.concat [ "220 mx1.example.com Service ready\r\n"
                                        , "EHLO relay.example.com\r\n"
                                        , "250-mx1.example.com\r\n" `T.append` capabilities
                                        , "AUTH PLAIN\r\n"
                                        , "538 Encryption required\r\n"
                                        , "QUIT\r\n"
                                        , "221 mx1.example.com Service closing transmission session\r\n"
                                        ]
