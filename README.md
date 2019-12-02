# imf - Internet Message Format

This project is a collection of utilities for parsing, sending, receiving, and
manipulating email messages as defined by RFC 5323 and related specifications.

## Quickstart

### Parsing

Using `parse`:
```
$> stack ghci --ghc-options -XOverloadedStrings haskell/src/Data/IMF/Parsers.hs
...
*Data.IMF.Parsers> result = parse (pMailbox <* endOfInput) "John Smith <jsmith@example.com>"
*Data.IMF.Parsers> result
Partial _
*Data.IMF.Parsers> (Partial parser') = result
*Data.IMF.Parsers> parser' ""
Done "" (Mailbox {mboxDisplay = "John Smith", mboxLocal = "jsmith", mboxDomain = "example.com"})
```
Note that most of these parsers will return `Partial` results because there may still be trailing whitespace remaining.

Using `parseOnly`:
```
$> stack ghci --ghc-options -XOverloadedStrings haskell/src/Data/IMF/Parsers.hs
...
*Data.IMF.Parsers> parseOnly (pMailbox <* endOfInput) "John Smith <jsmith@example.com>"
Right (Mailbox {mboxDisplay = "John Smith", mboxLocal = "jsmith", mboxDomain = "example.com"})
```

### Sending

Using the standard client:
```
$> stack ghci --ghc-options -XOverloadedStrings haskell/src/Data/IMF/{Parsers.hs,Network.hs}
...
*Data.IMF.Parsers Data.IMF.Network> import qualified Data.ByteString.Lazy as LB
*Data.IMF.Parsers Data.IMF.Network LB> params = ClientParams { clientName = "relay.example.com", clientSourceIP = "0.0.0.0", clientRecipientDomain = "", clientProxyHosts = Just ["smtp.mailgun.org"], clientAuthCredentials = Just ("postmaster@example.com", "__redacted__"), clientTLSRequired = False, clientTLSValidated = False }
*Data.IMF.Parsers Data.IMF.Network LB> client <- fst <$> getClient params
*Data.IMF.Parsers Data.IMF.Network LB> envelope = Envelope { sender = "no-reply@example.com", recipient = "brendan@example.com" }
*Data.IMF.Parsers Data.IMF.Network LB> message <- LB.readFile "haskell/test/Fixtures/Messages/test_message.txt"
*Data.IMF.Parsers Data.IMF.Network LB> (client, log) <- deliver envelope message client
*Data.IMF.Parsers Data.IMF.Network LB> client
Client {clientConnection = 0.0.0.0:0<~>1.2.3.4:587, clientHostname = "smtp.mailgun.org", clientExtensions = [("auth",["plain","login"]),("size",["52428800"]),("8bitmime",[]),("enhancedstatuscodes",[]),("smtputf8",[])], clientLastCommand = Just ".\r\n", clientLastReply = Just (250,["Great success"]), clientError = OK, clientCreatedAt = 2019-11-25 05:10:51.090951629 UTC, clientMessagesSent = 1}
*Data.IMF.Parsers Data.IMF.Network LB> printLog log
  1 2019-11-25T05:11:24.3093    0.0000 -> MAIL FROM: <no-reply@example.com>
  2 2019-11-25T05:11:24.3694    0.0601 <- 250 Sender address accepted
  3 2019-11-25T05:11:24.3696    0.0603 -> RCPT TO: <brendan@example.com>
  4 2019-11-25T05:11:24.4288    0.1195 <- 250 Recipient address accepted
  5 2019-11-25T05:11:24.4293    0.1199 -> DATA
  6 2019-11-25T05:11:24.4870    0.1777 <- 354 Continue
  7 2019-11-25T05:11:24.4876    0.1782 -> [...]
  8 2019-11-25T05:11:24.4877    0.1783 -> .
  9 2019-11-25T05:11:25.2286    0.9192 <- 250 Great success
*Data.IMF.Parsers Data.IMF.Network LB> _ <- closeClient client
```

Using a client pool:
```
$> stack ghci --ghc-options -XOverloadedStrings haskell/src/Data/IMF/{Parsers.hs,Network.hs}
...
*Data.IMF.Parsers Data.IMF.Network> import qualified Data.ByteString.Lazy as LB
*Data.IMF.Parsers Data.IMF.Network LB> params = ClientParams { clientName = "relay.example.com", clientSourceIP = "0.0.0.0", clientRecipientDomain = "", clientProxyHosts = Just ["smtp.mailgun.org"], clientAuthCredentials = Just ("postmaster@example.com", "__redacted__"), clientTLSRequired = False, clientTLSValidated = False }
*Data.IMF.Parsers Data.IMF.Network LB> limits = ClientLimits { maxClients = 25, maxClientMessages = 10, maxClientIdleTime = 30, maxClientTime = 300 }
*Data.IMF.Parsers Data.IMF.Network LB> pool <- getClientPool limits params
*Data.IMF.Parsers Data.IMF.Network LB> envelope = Envelope { sender = "no-reply@example.com", recipient = "brendan@example.com" }
*Data.IMF.Parsers Data.IMF.Network LB> message <- LB.readFile "haskell/test/Fixtures/Messages/test_message.txt"
*Data.IMF.Parsers Data.IMF.Network LB> Just (client, log) <- deliverViaPool envelope message pool
*Data.IMF.Parsers Data.IMF.Network LB> client
Client {clientConnection = 0.0.0.0:0<~>1.2.3.4:587, clientHostname = "smtp.mailgun.org", clientExtensions = [("auth",["plain","login"]),("size",["52428800"]),("8bitmime",[]),("enhancedstatuscodes",[]),("smtputf8",[])], clientLastCommand = Just ".\r\n", clientLastReply = Just (250,["Great success"]), clientError = OK, clientCreatedAt = 2019-11-25 05:29:24.910494835 UTC, clientMessagesSent = 1}
*Data.IMF.Parsers Data.IMF.Network LB> printLog log
  1 2019-11-25T05:29:25.5327    0.0000 -> MAIL FROM: <no-reply@exmaple.com>
  2 2019-11-25T05:29:25.5760    0.0433 <- 250 Sender address accepted
  3 2019-11-25T05:29:25.5768    0.0441 -> RCPT TO: <brendan@example.com>
  4 2019-11-25T05:29:25.6220    0.0892 <- 250 Recipient address accepted
  5 2019-11-25T05:29:25.6223    0.0896 -> DATA
  6 2019-11-25T05:29:25.6656    0.1329 <- 354 Continue
  7 2019-11-25T05:29:25.6661    0.1334 -> [...]
  8 2019-11-25T05:29:25.6662    0.1335 -> .
  9 2019-11-25T05:29:25.8756    0.3429 <- 250 Great success
*Data.IMF.Parsers Data.IMF.Network LB> _ <- closeClientPool pool
```

### Receiving

```
$> stack ghci --ghc-options -XOverloadedStrings haskell/src/Data/IMF/Network.hs
...
*Data.IMF.Network> params = ServerParams { serverName = "mx1.dev.example.com", serverIP = "0.0.0.0", serverPort = "2525", serverMaxSessions = 10, serverMaxMsgSize = 10485760, serverMaxRcpts = 100 }
*Data.IMF.Network> hooks = Hooks { hookLogSession = \(session, log) -> do print session; printLog log, hookVerifyReturnPath = \_ -> return (), hookVerifyRcpt = \_ -> return (), hookSaveMessage = \buffer _ -> do print buffer }
*Data.IMF.Network> shutdownServer <- server params hooks
starting server with ServerParams {serverName = "mx1.dev.example.com", serverIP = "0.0.0.0", serverPort = "2525", serverMaxSessions = 10, serverMaxMsgSize = 10485760, serverMaxRcpts = 100}
up and running
*Data.IMF.Network>
Buffer {returnPath = Mailbox {mboxDisplay = "", mboxLocal = "brendan", mboxDomain = "example.com"}, rcpts = [Mailbox {mboxDisplay = "", mboxLocal = "brendan", mboxDomain = "dev.b0d0nne11.com"}], failedRcpts = []}
Session {sessionConnection = "closed", sessionError = OK, sessionCreatedAt = 2019-12-02 05:01:13.407413491 UTC, sessionClientName = "mail11.static.mailgun.info", sessionInitialized = True, sessionBuffer = Nothing}
  1 2019-12-02T05:01:13.4076    0.0000 -> 220 mx1.dev.example.com Service ready
  2 2019-12-02T05:01:13.4659    0.0583 <- EHLO mail11.static.mailgun.info
  3 2019-12-02T05:01:13.4664    0.0588 -> 250-mx1.dev.example.com
250-SIZE 10485760
250-8BITMIME
250 SMTPUTF8
  4 2019-12-02T05:01:13.5244    0.1168 <- MAIL FROM:<brendan@example.com> BODY=8BITMIME SMTPUTF8
  5 2019-12-02T05:01:13.5260    0.1184 -> 250 OK
  6 2019-12-02T05:01:13.5828    0.1752 <- RCPT TO:<brendan@dev.example.com>
  7 2019-12-02T05:01:13.5835    0.1759 -> 250 OK
  8 2019-12-02T05:01:13.6445    0.2369 <- DATA
  9 2019-12-02T05:01:13.6448    0.2372 -> 354 End data with <CR><LF>.<CR><LF>
 10 2019-12-02T05:01:13.9170    0.5094 <- [...]
 11 2019-12-02T05:01:13.9227    0.5151 -> 250 OK
 12 2019-12-02T05:03:35.1273  141.7197 <- QUIT
 13 2019-12-02T05:03:35.1276  141.7200 -> 221 mx1.dev.example.com Service closing transmission session
*Data.IMF.Network> shutdownServer
sending shutdown signal
waiting for main thread to finish
waiting for sessions to finish
```

## Development & Support

Feel free to open an issue or pull request on GitHub.

Copyright (c) 2017-2019, Brendan O'Donnell
