{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE QuasiQuotes       #-}

module MailBin.MTA
  ( runMTA
  )
where

import           Control.Concurrent          (forkIO)
import           Control.Monad               (forever)
import           Control.Monad.Reader        (runReaderT)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as B
import qualified Data.Configurator           as Config
import           Data.Configurator.Types     (Config)
import           Data.IMF                    (Mailbox (..), format, formatList)
import           Data.IMF.Network            (PassFail (..), Server (..), runServer)
import           Data.IMF.Network.Connection (Connection)
import qualified Data.IMF.Network.Connection as Connection
import           Data.Pool                   (Pool)
import           Data.String.QQ              (s)
import           Data.Text                   (Text)
import qualified Data.Text.Encoding          as T
import qualified Database.SQLite.Simple      as DB
import qualified Network.TLS                 as TLS

import           MailBin.DB

newServer :: Connection -> TLS.ServerParams -> Pool DB.Connection -> Server
newServer conn tlsParams dbPool = Server
    { serverName = "mailbin"
    , serverConnection = conn
    , serverTLSParams = tlsParams
    , serverLogger = \_ -> return ()
    , serverAuthenticate = \_ _ -> return PermFail
    , serverVerifyReturnPath = \_ -> return Pass
    , serverVerifyRecipient = \_ -> return Pass
    , serverAcceptMessage = accept dbPool
    , serverMaxRecipients = 10
    , serverMaxMessageSize = 4096
    , serverReqTLS = False
    , serverReqAuth = False
    }

accept :: Pool DB.Connection -> Maybe Mailbox -> [Mailbox] -> ByteString -> IO PassFail
accept dbPool rp rcpts msg =
    Pass <$ insertMail dbPool (maybe "" format rp) (formatList rcpts) (T.decodeUtf8 msg)

loadConnection :: Config -> IO Connection
loadConnection config = do
    host <- Config.lookupDefault "127.0.0.1" config "host"
    port <- Config.lookupDefault "2525" config "port"
    Connection.listen (host, port)

loadTLSParams :: Config -> IO TLS.ServerParams
loadTLSParams config = do
    crtFile <- Config.lookup config "certificate_file"
    keyFile <- Config.lookup config "private_key_file"
    (crt, key) <- case (crtFile, keyFile) of
        (Just crtFile, Just keyFile) -> do
            crt <- B.readFile crtFile
            key <- B.readFile keyFile
            return (crt, key)
        (Just _, Nothing)  -> fail "invalid configuration"
        (Nothing, Just _)  -> fail "invalid configuration"
        (Nothing, Nothing) -> return (localhostCrt, localhostKey)
    either fail (return . Connection.tlsServerParams) $
        TLS.credentialLoadX509FromMemory crt key

runMTA :: Config -> Pool DB.Connection -> IO ()
runMTA config dbPool = do
    conn <- loadConnection config
    tlsParams <- loadTLSParams config
    forever $ Connection.accept conn >>= \conn' ->
        forkIO $ runReaderT runServer $ newServer conn' tlsParams dbPool

localhostCrt :: ByteString
localhostCrt = [s|-----BEGIN CERTIFICATE-----
MIIDgjCCAmqgAwIBAgIJAJQZ2GrJ1yTTMA0GCSqGSIb3DQEBCwUAMFYxCzAJBgNV
BAYTAlhYMRUwEwYDVQQHDAxEZWZhdWx0IENpdHkxHDAaBgNVBAoME0RlZmF1bHQg
Q29tcGFueSBMdGQxEjAQBgNVBAMMCWxvY2FsaG9zdDAeFw0xOTAxMTgyMjM4NDNa
Fw0yOTAxMTUyMjM4NDNaMFYxCzAJBgNVBAYTAlhYMRUwEwYDVQQHDAxEZWZhdWx0
IENpdHkxHDAaBgNVBAoME0RlZmF1bHQgQ29tcGFueSBMdGQxEjAQBgNVBAMMCWxv
Y2FsaG9zdDCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAMLI/auGOiF+
LNP1vtlyQCKAv6mM0g8cClPkjW32m/KLaBosOwVankErou2wzxZWTpazttzP+pxG
BL1dZhpIWCp/27pFi87mWcAW6hyEBSHEG4nsTtFroumObdeJj8XMIwpV0XtJV3/k
Md+w56+nDlMpVRe4an9jMy0D7KI6NoW0KXH0VNl+lDhcHJb2RU5Q36t5BTeiOTr9
fwBJE7F3SuQJFuUWpqEa+ctY04U9yo55BYJxzjGa6FC6gIquW2vSWyXCgfiFdNGa
ttIU8eI8doQejZ5YLzhIgfczlGv8WNzhp9UhEP5RNsXlndN1XzUfG9GvRcPf/G2l
HnA+aGGlv2UCAwEAAaNTMFEwHQYDVR0OBBYEFOe6q/2EYTeQoSZqcen2xGeD/gI4
MB8GA1UdIwQYMBaAFOe6q/2EYTeQoSZqcen2xGeD/gI4MA8GA1UdEwEB/wQFMAMB
Af8wDQYJKoZIhvcNAQELBQADggEBAJzBseObOmKpdxbqfh4FJXTEJhiAufnFLGp9
fGECEXGdo7dFcXtX+u+0Cqbahoco6z58DG1CJ8DHQeZP/BgvUL6HxMong/Z4ZzUk
gGeki/NTi0f9ygabsWSYuJJJ96ySNQPDW5xy1yo28nT/aKCOfJ+fZjrEKixqlMbH
vUs4a08xOeU9jFS/E/IFwL0LrLHLqE2T3PrxXgLfiYYtn5svJkIfXIOAVYEihDlt
6MjHVWCo9peRNH8ptMfU+tAOwv8RfVf4i9m/LBpJpXwIbVGxK/83wG13pQY8AAaH
+JTZWYzcNAbC4BeQ5pFK/o3SLi/2udNAPJHqZW0BUZigy9oNlhM=
-----END CERTIFICATE-----|]

localhostKey :: ByteString
localhostKey = [s|-----BEGIN PRIVATE KEY-----
MIIEvwIBADANBgkqhkiG9w0BAQEFAASCBKkwggSlAgEAAoIBAQDCyP2rhjohfizT
9b7ZckAigL+pjNIPHApT5I1t9pvyi2gaLDsFWp5BK6LtsM8WVk6Ws7bcz/qcRgS9
XWYaSFgqf9u6RYvO5lnAFuochAUhxBuJ7E7Ra6Lpjm3XiY/FzCMKVdF7SVd/5DHf
sOevpw5TKVUXuGp/YzMtA+yiOjaFtClx9FTZfpQ4XByW9kVOUN+reQU3ojk6/X8A
SROxd0rkCRblFqahGvnLWNOFPcqOeQWCcc4xmuhQuoCKrltr0lslwoH4hXTRmrbS
FPHiPHaEHo2eWC84SIH3M5Rr/Fjc4afVIRD+UTbF5Z3TdV81HxvRr0XD3/xtpR5w
Pmhhpb9lAgMBAAECggEBAImBjBpfOoBUa18Cl8U3s3DoBOWzpMLH376TlQfw1Oxn
X4lCejPwYdHN9UnozHiPWxFmMww0wk053LS1z5N0SNFPy5T27ZiU4v0I8CJLTLgm
ke5O5OFQJATO6FoFIXrX/lCYfraS8vM068RnRVeqDiHFBu4k4QGmzQTVRWLMx5qg
dlEM/ptDcS7knEExVbBzlcC2AVcLOhlFEH2SH5Cnz8rKuP7hPz7/o18Gc2W96a3x
PzpByTgh9hTB+RPPuCPCLIRgYq6YOAswvMGkaqtpDtPcXbGXKfO6hfRL7EIQyYlN
o7ypeoVXDPIl/NSf6zOur1o0pzSvvh3tIR7Ts3txbMECgYEA5UclXWWuCFd8O/nZ
AJw9vySGxB0jow2UoMzP0g8ZOrILlnW1v73VUqutXTzhCR6adnTFBDImIt6y3glw
OVRzIO7frI+IjPVWMhp9OcyY54HGvejncDEDvjmwOlDMY2l4WtsXWIOaeTotSxPI
cquC1Jqt41iC2uDLMRpn156BNTUCgYEA2XyzPxi/JD4cyZ+wifJQtkY39/U/FdFW
6HepxDSpTZtCWq7sDFHI23tnCM9bhl6Pva+FAdoT0gehANgOcQs8UlHzPsVDL8k6
h/JPvsUO8V7JlyLYrnTl5xxC25pgX0fvVaa85Qu+vAi5XRZP+GikuMxgvmddDJSm
5JCxFiGvl3ECgYEA2GHz3phmzdKbGhNrfIr4L1vuoEmL54eanMOO4wWC3wVHsGE4
/VPQbyw8+oDJNQ0f4+nCQnp9yWz5zFmGA2UzHoKysC3qRnYIOlMLmjAAooS1J2gW
fKiVisLxEahNrQjOIJkjCFNzxzNjqbZq9+XsHHA66hQPszhyK85q8HDUKn0CgYEA
jCzTWteYWsOtykYigsarPtADQs3O20ERwsNdqBhJBrwLCGe1vDr6OnxO7F65nuFT
w+5cy9TC8Zv5KZIXvMSBHI21Ob/qjeIk13pV658XAnsVc1QCVG8DvvF4JhJaCw9S
MuJE3Vu0k30L+/Nll7XadUFFGXOFhZZJ+wdHG/yaMAECgYAXlIk3jdwXjJkpHV6F
0pLtk+kMDujjKehT/uE3EEcFskGa2N7PUYrOduJd6a174sseUuFoXLNa0YK4NifD
5/9uev3l0OkEoivGrTri3le24ckq1TuQFW8aov9dPjEQjzxd63zB8I4W05lh2KRq
N9uWbXtdxPA1rRkiwLa2oZ8SSQ==
-----END PRIVATE KEY-----|]
