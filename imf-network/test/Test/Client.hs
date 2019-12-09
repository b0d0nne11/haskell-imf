{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

{- HLINT ignore "Reduce duplication" -}

module Test.Client where

import           Control.Concurrent               (forkIO)
import           Control.Concurrent.MVar          (newEmptyMVar, putMVar, takeMVar)
import           Control.Exception                (bracket)
import           Control.Monad                    (void, when)
import           Data.Attoparsec.ByteString.Char8 (IResult (..), Parser, Result, parse)
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Lazy             as LB
import qualified Data.ByteString.Lazy.Char8       as LC
import           Data.Pool                        (createPool, destroyAllResources)
import qualified Data.Text                        as T
import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.IMF
import           Data.IMF.Network.Chat
import           Data.IMF.Network.Client
import qualified Data.IMF.Network.Connection      as Conn
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
    { maxClientMessages = 10
    , maxClientTime     = 300
    }

testMessage :: IO LB.ByteString
testMessage = LC.intercalate "\r\n" . LC.split '\n' <$> LB.readFile "./test/Fixtures/Messages/simple_addressing_1.txt"

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

tests :: TestTree
tests = testGroup "client"
    [ testConnect
    , testGreeting
    , testExtendedHello
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
    , testDeliverViaPool
    ]

testConnect = testCase "connect/disconnect" $
    chatTest clientAct $ const $ return ()
  where
    clientAct = do
        (c, chatLog) <- newClient >>= runChat (connect testClientParams >> disconnect)
        clientLastCommand c @?= Nothing
        clientLastReply c @?= Nothing
        clientError c @?= OK
        formatLogLines chatLog @?= ""

testGreeting = testCase "greeting" $
    chatTest clientAct serverAct
  where
    serverAct conn = do
        _ <- Conn.send conn "220 smtp.example.com ESMTP Postfix\r\n"
        return ()
    clientAct = do
        (c, _) <- newClient >>= runChat (connect testClientParams)
        (c, chatLog) <- runChat greeting c
        (c, _) <- runChat disconnect c
        clientLastCommand c @?= Nothing
        clientLastReply c @?= Just (220, ["smtp.example.com ESMTP Postfix"])
        clientError c @?= OK
        formatLogLines chatLog @?= "220 smtp.example.com ESMTP Postfix\r\n"

testExtendedHello = testCase "hello (ehlo)" $
    chatTest clientAct serverAct
  where
    serverAct conn = do
        r <- recvAll pLine conn
        r @?= ["EHLO relay.example.com"]
        _ <- Conn.send conn "250-smtp.example.com\r\n250-SIZE 14680064\r\n250-PIPELINING\r\n250 HELP\r\n"
        return ()
    clientAct = do
        (c, _) <- newClient >>= runChat (connect testClientParams)
        (c, chatLog) <- runChat (hello testClientParams) c
        (c, _) <- runChat disconnect c
        clientExtensions c @?= [("size",["14680064"]),("pipelining",[]),("help",[])]
        clientLastCommand c @?= Just "EHLO relay.example.com\r\n"
        clientLastReply c @?= Just (250, ["smtp.example.com", "SIZE 14680064", "PIPELINING", "HELP"])
        clientError c @?= OK
        formatLogLines chatLog @?= T.concat [ "EHLO relay.example.com\r\n"
                                            , "250-smtp.example.com\r\n250-SIZE 14680064\r\n250-PIPELINING\r\n250 HELP\r\n"
                                            ]

testHello = testCase "hello (helo)" $
    chatTest clientAct serverAct
  where
    serverAct conn = do
        r <- recvAll pLine conn
        r @?= ["EHLO relay.example.com"]
        _ <- Conn.send conn "502 Command not implemented\r\n"
        r <- recvAll pLine conn
        r @?= ["HELO relay.example.com"]
        _ <- Conn.send conn "250 smtp.example.com, I am glad to meet you\r\n"
        return ()
    clientAct = do
        (c, _) <- newClient >>= runChat (connect testClientParams)
        (c, chatLog) <- runChat (hello testClientParams) c
        (c, _) <- runChat disconnect c
        clientExtensions c @?= []
        clientLastCommand c @?= Just "HELO relay.example.com\r\n"
        clientLastReply c @?= Just (250, ["smtp.example.com, I am glad to meet you"])
        clientError c @?= OK
        formatLogLines chatLog @?= T.concat [ "EHLO relay.example.com\r\n"
                                            , "502 Command not implemented\r\n"
                                            , "HELO relay.example.com\r\n"
                                            , "250 smtp.example.com, I am glad to meet you\r\n"
                                            ]

testStartTLS = testCase "startTLS" $
    chatTest clientAct serverAct
  where
    serverAct conn = do
        r <- recvAll pLine conn
        r @?= ["EHLO relay.example.com"]
        _ <- Conn.send conn "250-smtp.example.com\r\n250 STARTTLS\r\n"
        r <- recvAll pLine conn
        r @?= ["STARTTLS"]
        _ <- Conn.send conn "220 Go ahead\r\n"
        conn <- Conn.secureServer conn
        r <- recvAll pLine conn
        r @?= ["EHLO relay.example.com"]
        _ <- Conn.send conn "250-smtp.example.com\r\n250 STARTTLS\r\n"
        return ()
    clientAct = do
        (c, _) <- newClient >>= runChat (connect testClientParams)
        (c, chatLog) <- runChat (hello testClientParams >> startTLS testClientParams) c
        (c, _) <- runChat disconnect c
        clientExtensions c @?= [("starttls",[])]
        clientLastCommand c @?= Just "EHLO relay.example.com\r\n"
        clientLastReply c @?= Just (250, ["smtp.example.com", "STARTTLS"])
        clientError c @?= OK
        formatLogLines chatLog @?= T.concat [ "EHLO relay.example.com\r\n"
                                            , "250-smtp.example.com\r\n250 STARTTLS\r\n"
                                            , "STARTTLS\r\n"
                                            , "220 Go ahead\r\n"
                                            , "EHLO relay.example.com\r\n"
                                            , "250-smtp.example.com\r\n250 STARTTLS\r\n"
                                            ]

testAuthLogin = testCase "auth (login)" $
    chatTest clientAct serverAct
  where
    serverAct conn = do
        r <- recvAll pLine conn
        r @?= ["EHLO relay.example.com"]
        _ <- Conn.send conn "250-smtp.example.com\r\n250-STARTTLS\r\n250 AUTH LOGIN\r\n"
        r <- recvAll pLine conn
        r @?= ["STARTTLS"]
        _ <- Conn.send conn "220 Go ahead\r\n"
        conn <- Conn.secureServer conn
        r <- recvAll pLine conn
        r @?= ["EHLO relay.example.com"]
        _ <- Conn.send conn "250-smtp.example.com\r\n250-STARTTLS\r\n250 AUTH LOGIN\r\n"
        r <- recvAll pLine conn
        r @?= ["AUTH LOGIN"]
        _ <- Conn.send conn "334 VXNlcm5hbWU6\r\n"
        r <- recvAll pLine conn
        r @?= ["dXNlcm5hbWU="]
        _ <- Conn.send conn "334 UGFzc3dvcmQ6\r\n"
        r <- recvAll pLine conn
        r @?= ["cGFzc3dvcmQ="]
        _ <- Conn.send conn "235 Authentication successful.\r\n"
        return ()
    clientAct = do
        (c, _) <- newClient >>= runChat (connect testClientParams)
        (c, chatLog) <- runChat (hello testClientParams >> startTLS testClientParams >> auth testClientParams) c
        (c, _) <- runChat disconnect c
        clientExtensions c @?= [("starttls",[]),("auth",["login"])]
        clientLastCommand c @?= Nothing
        clientLastReply c @?= Just (235, ["Authentication successful."])
        clientError c @?= OK
        formatLogLines chatLog @?= T.concat [ "EHLO relay.example.com\r\n"
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

testAuthPlain = testCase "auth (plain)" $
    chatTest clientAct serverAct
  where
    serverAct conn = do
        r <- recvAll pLine conn
        r @?= ["EHLO relay.example.com"]
        _ <- Conn.send conn "250-smtp.example.com\r\n250-STARTTLS\r\n250 AUTH PLAIN\r\n"
        r <- recvAll pLine conn
        r @?= ["STARTTLS"]
        _ <- Conn.send conn "220 Go ahead\r\n"
        conn <- Conn.secureServer conn
        r <- recvAll pLine conn
        r @?= ["EHLO relay.example.com"]
        _ <- Conn.send conn "250-smtp.example.com\r\n250-STARTTLS\r\n250 AUTH PLAIN\r\n"
        r <- recvAll pLine conn
        r @?= ["AUTH PLAIN dXNlcm5hbWUAdXNlcm5hbWUAcGFzc3dvcmQ="]
        _ <- Conn.send conn "235 Authentication successful.\r\n"
        return ()
    clientAct = do
        (c, _) <- newClient >>= runChat (connect testClientParams)
        (c, chatLog) <- runChat (hello testClientParams >> startTLS testClientParams >> auth testClientParams) c
        (c, _) <- runChat disconnect c
        clientExtensions c @?= [("starttls",[]),("auth",["plain"])]
        clientLastCommand c @?= Nothing
        clientLastReply c @?= Just (235, ["Authentication successful."])
        clientError c @?= OK
        formatLogLines chatLog @?= T.concat [ "EHLO relay.example.com\r\n"
                                            , "250-smtp.example.com\r\n250-STARTTLS\r\n250 AUTH PLAIN\r\n"
                                            , "STARTTLS\r\n"
                                            , "220 Go ahead\r\n"
                                            , "EHLO relay.example.com\r\n"
                                            , "250-smtp.example.com\r\n250-STARTTLS\r\n250 AUTH PLAIN\r\n"
                                            , "[...]\r\n"
                                            , "235 Authentication successful.\r\n"
                                            ]

testMailFrom = testCase "mailFrom" $
    chatTest clientAct serverAct
  where
    serverAct conn = do
        r <- recvAll pLine conn
        r @?= ["MAIL FROM: <jdoe@localhost>"]
        _ <- Conn.send conn "250 Ok\r\n"
        return ()
    clientAct = do
        (c, _) <- newClient >>= runChat (connect testClientParams)
        (c, chatLog) <- runChat (mailFrom $ Mailbox "" "jdoe" "localhost") c
        (c, _) <- runChat disconnect c
        clientLastCommand c @?= Just "MAIL FROM: <jdoe@localhost>\r\n"
        clientLastReply c @?= Just (250, ["Ok"])
        clientError c @?= OK
        formatLogLines chatLog @?= T.concat [ "MAIL FROM: <jdoe@localhost>\r\n"
                                            , "250 Ok\r\n"
                                            ]

testRcptTo = testCase "rcptTo" $
    chatTest clientAct serverAct
  where
    serverAct conn = do
        r <- recvAll pLine conn
        r @?= ["RCPT TO: <mary@localhost>"]
        _ <- Conn.send conn "250 Ok\r\n"
        return ()
    clientAct = do
        (c, _) <- newClient >>= runChat (connect testClientParams)
        (c, chatLog) <- runChat (rcptTo $ Mailbox "" "mary" "localhost") c
        (c, _) <- runChat disconnect c
        clientLastCommand c @?= Just "RCPT TO: <mary@localhost>\r\n"
        clientLastReply c @?= Just (250, ["Ok"])
        clientError c @?= OK
        formatLogLines chatLog @?= T.concat [ "RCPT TO: <mary@localhost>\r\n"
                                            , "250 Ok\r\n"
                                            ]

testDataInit = testCase "dataInit" $
    chatTest clientAct serverAct
  where
    serverAct conn = do
        r <- recvAll pLine conn
        r @?= ["DATA"]
        _ <- Conn.send conn "354 End data with <CR><LF>.<CR><LF>\r\n"
        return ()
    clientAct = do
        (c, _) <- newClient >>= runChat (connect testClientParams)
        (c, chatLog) <- runChat dataInit c
        (c, _) <- runChat disconnect c
        clientLastCommand c @?= Just "DATA\r\n"
        clientLastReply c @?= Just (354, ["End data with <CR><LF>.<CR><LF>"])
        clientError c @?= OK
        formatLogLines chatLog @?= T.concat [ "DATA\r\n"
                                            , "354 End data with <CR><LF>.<CR><LF>\r\n"
                                            ]

testDataBlock = testCase "dataBlock" $
    chatTest clientAct serverAct
  where
    serverAct conn = do
        _ <- recvAll pLine conn
        return ()
    clientAct = do
        body <- testMessage
        (c, _) <- newClient >>= runChat (connect testClientParams)
        (c, chatLog) <- runChat (dataBlock body) c
        (c, _) <- runChat disconnect c
        clientLastCommand c @?= Nothing
        clientLastReply c @?= Nothing
        clientError c @?= OK
        formatLogLines chatLog @?= "[...]\r\n"

testDataTerm = testCase "dataTerm" $
    chatTest clientAct serverAct
  where
    serverAct conn = do
        r <- recvAll pLine conn
        r @?= ["."]
        _ <- Conn.send conn "250 Ok: queued as 12345\r\n"
        return ()
    clientAct = do
        (c, _) <- newClient >>= runChat (connect testClientParams)
        (c, chatLog) <- runChat dataTerm c
        (c, _) <- runChat disconnect c
        clientLastCommand c @?= Just ".\r\n"
        clientLastReply c @?= Just (250, ["Ok: queued as 12345"])
        clientError c @?= OK
        formatLogLines chatLog @?= T.concat [ ".\r\n"
                                            , "250 Ok: queued as 12345\r\n"
                                            ]

testQuit = testCase "quit" $
    chatTest clientAct serverAct
  where
    serverAct conn = do
        r <- recvAll pLine conn
        r @?= ["QUIT"]
        _ <- Conn.send conn "221 Bye\r\n"
        return ()
    clientAct = do
        (c, _) <- newClient >>= runChat (connect testClientParams)
        (c, chatLog) <- runChat quit c
        (c, _) <- runChat disconnect c
        clientLastCommand c @?= Just "QUIT\r\n"
        clientLastReply c @?= Just (221, ["Bye"])
        clientError c @?= OK
        formatLogLines chatLog @?= T.concat [ "QUIT\r\n"
                                            , "221 Bye\r\n"
                                            ]

testDeliver = testCase "deliver" $
    chatTest clientAct serverAct
  where
    serverAct conn = do
        _ <- Conn.send conn "220 smtp.example.com ESMTP Postfix\r\n"
        r <- recvAll pLine conn
        r @?= ["EHLO relay.example.com"]
        _ <- Conn.send conn "250-smtp.example.com\r\n250-SIZE 14680064\r\n250-PIPELINING\r\n250 HELP\r\n"
        r <- recvAll pLine conn
        r @?= ["MAIL FROM: <jdoe@localhost>"]
        _ <- Conn.send conn "250 Ok\r\n"
        r <- recvAll pLine conn
        r @?= ["RCPT TO: <mary@localhost>"]
        _ <- Conn.send conn "250 Ok\r\n"
        r <- recvAll pLine conn
        r @?= ["DATA"]
        _ <- Conn.send conn "354 End data with <CR><LF>.<CR><LF>\r\n"
        _ <- recvAll pBody conn
        _ <- Conn.send conn "250 Ok: queued as 12345\r\n"
        r <- recvAll pLine conn
        r @?= ["QUIT"]
        _ <- Conn.send conn "221 Bye\r\n"
        return ()
    clientAct = do
        body <- testMessage
        (c, _) <- getClient testClientParams
        (c, chatLog) <- deliver (Envelope (Mailbox "" "jdoe" "localhost") (Mailbox "" "mary" "localhost")) body c
        clientError c @?= OK
        (c, _) <- closeClient c
        clientExtensions c @?= [("size",["14680064"]),("pipelining",[]),("help",[])]
        clientLastCommand c @?= Just "QUIT\r\n"
        clientLastReply c @?= Just (221, ["Bye"])
        clientError c @?= OK
        clientMessagesSent c @?= 1
        formatLogLines chatLog @?= T.concat [ "MAIL FROM: <jdoe@localhost>\r\n"
                                            , "250 Ok\r\n"
                                            , "RCPT TO: <mary@localhost>\r\n"
                                            , "250 Ok\r\n"
                                            , "DATA\r\n"
                                            , "354 End data with <CR><LF>.<CR><LF>\r\n"
                                            , "[...]\r\n"
                                            , ".\r\n"
                                            , "250 Ok: queued as 12345\r\n"
                                            ]

testDeliverViaPool = testCase "deliverViaPool" $
    chatTest clientAct serverAct
  where
    serverAct conn = do
        _ <- Conn.send conn "220 smtp.example.com ESMTP Postfix\r\n"
        r <- recvAll pLine conn
        r @?= ["EHLO relay.example.com"]
        _ <- Conn.send conn "250-smtp.example.com\r\n250-SIZE 14680064\r\n250-PIPELINING\r\n250 HELP\r\n"
        r <- recvAll pLine conn
        r @?= ["MAIL FROM: <jdoe@localhost>"]
        _ <- Conn.send conn "250 Ok\r\n"
        r <- recvAll pLine conn
        r @?= ["RCPT TO: <mary@localhost>"]
        _ <- Conn.send conn "250 Ok\r\n"
        r <- recvAll pLine conn
        r @?= ["DATA"]
        _ <- Conn.send conn "354 End data with <CR><LF>.<CR><LF>\r\n"
        _ <- recvAll pBody conn
        _ <- Conn.send conn "250 Ok: queued as 12345\r\n"
        r <- recvAll pLine conn
        r @?= ["QUIT"]
        _ <- Conn.send conn "221 Bye\r\n"
        return ()
    clientAct = do
        body <- testMessage
        pool <- createPool (fst <$> getClient testClientParams) (void . closeClient) 1 30 10
        Just (c, chatLog) <- tryDeliverWithPool (Envelope (Mailbox "" "jdoe" "localhost") (Mailbox "" "mary" "localhost")) body testClientLimits pool
        destroyAllResources pool
        clientExtensions c @?= [("size",["14680064"]),("pipelining",[]),("help",[])]
        clientLastCommand c @?= Just ".\r\n"
        clientLastReply c @?= Just (250, ["Ok: queued as 12345"])
        clientError c @?= OK
        clientMessagesSent c @?= 1
        formatLogLines chatLog @?= T.concat [ "MAIL FROM: <jdoe@localhost>\r\n"
                                            , "250 Ok\r\n"
                                            , "RCPT TO: <mary@localhost>\r\n"
                                            , "250 Ok\r\n"
                                            , "DATA\r\n"
                                            , "354 End data with <CR><LF>.<CR><LF>\r\n"
                                            , "[...]\r\n"
                                            , ".\r\n"
                                            , "250 Ok: queued as 12345\r\n"
                                            ]
