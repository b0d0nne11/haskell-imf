# imf-network

This package provides utilities for sending and receiving email messages.

## Quickstart

### Sending

Send an email:
```
$> cabal v2-repl imf-network
...
*Data.IMF.Network> :set -XOverloadedStrings
*Data.IMF.Network> import qualified Data.ByteString.Lazy as LB
*Data.IMF.Network LB> import System.Log.FastLogger
*Data.IMF.Network LB System.Log.FastLogger> import Control.Monad.Reader
*Data.IMF.Network LB System.Log.FastLogger Control.Monad.Reader> (logger, _) <- newFastLogger LogNone
*Data.IMF.Network LB System.Log.FastLogger Control.Monad.Reader> message <- LB.readFile "test/Fixtures/Messages/simple_addressing_1.txt"
*Data.IMF.Network LB System.Log.FastLogger Control.Monad.Reader> conn <- connectMX ("0.0.0.0", "0") "localhost"
*Data.IMF.Network LB System.Log.FastLogger Control.Monad.Reader> let client = Client "relay.example.com" conn (tlsClientParams "localhost" False) logger Nothing)
*Data.IMF.Network LB System.Log.FastLogger Control.Monad.Reader> runReaderT (setup >> deliver "no-reply@example.com" ["test@example.com"] message >> quit) client
*Data.IMF.Network LB System.Log.FastLogger Control.Monad.Reader> close conn
```

### Receiving

Receive an email sent via `swaks -s 127.0.0.1:2525 --to brendan@example.com`:
```
$> cabal v2-repl imf-network
...
*Data.IMF.Network> :set -XOverloadedStrings
*Data.IMF.Network> import qualified Network.TLS as TLS
*Data.IMF.Network TLS> import System.Log.FastLogger
*Data.IMF.Network TLS System.Log.FastLogger> import Control.Monad.Reader
*Data.IMF.Network TLS System.Log.FastLogger Control.Monad.Reader> (logger, _) <- newFastLogger $ LogStderr 0
*Data.IMF.Network TLS System.Log.FastLogger Control.Monad.Reader> Right cert <- TLS.credentialLoadX509 "test/Fixtures/localhost.crt" "test/Fixtures/localhost.key"
*Data.IMF.Network TLS System.Log.FastLogger Control.Monad.Reader> conn <- listen ("127.0.0.1", "2525")
*Data.IMF.Network TLS System.Log.FastLogger Control.Monad.Reader> accept conn >>= \conn' -> runReaderT runServer $ Server "mx1.example.com" conn' (tlsServerParams cert) logger (\_ _ -> return PermFail) (const $ return Pass) (const $ return Pass) (\_ _ _ -> return Pass) 100 10485760 False False
> 220 mx1.example.com Service ready
< EHLO localhost
> 250-mx1.example.com
250-SIZE 10485760
250-8BITMIME
250-SMTPUTF8
250-PIPELINING
250 STARTTLS
< MAIL FROM:<brendan@localhost>
> 250 Mailbox <brendan@localhost> OK
< RCPT TO:<brendan@example.com>
> 250 Mailbox <brendan@example.com> OK
< DATA
> 354 End data with <CR><LF>.<CR><LF>
< [...]
> 250 OK
< QUIT
> 221 mx1.example.com Service closing transmission session
*Data.IMF.Network TLS System.Log.FastLogger Control.Monad.Reader> close conn
```
