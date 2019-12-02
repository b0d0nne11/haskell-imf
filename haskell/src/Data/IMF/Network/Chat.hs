{-# LANGUAGE OverloadedStrings #-}

module Data.IMF.Network.Chat
  ( Envelope(..)
  , Chat
  , runChat
  , liftChat
  , debug
  , send
  , recv
  , ChatLog
  , ChatLogEntryType(..)
  , formatLog
  , printLog
  , formatLogLines
  , printLogLines
  )
where

import           Control.Monad                  ( when )
import           Control.Monad.Except           ( ExceptT
                                                , runExceptT
                                                , throwError
                                                )
import           Control.Monad.Catch            ( catch )
import           Control.Monad.RWS              ( RWST
                                                , execRWST
                                                )
import           Control.Monad.Trans            ( liftIO )
import           Control.Monad.Writer           ( tell )
import           Data.Attoparsec.ByteString     ( Parser
                                                , IResult(..)
                                                , Result
                                                , parse
                                                )
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as LB
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT
import qualified Data.Text.Format              as TFormat
import           Data.Text.Encoding             ( decodeUtf8 )
import           Data.Time.Clock                ( NominalDiffTime
                                                , UTCTime
                                                , addUTCTime
                                                , diffUTCTime
                                                , getCurrentTime
                                                )
import           Data.Time.Format               ( formatTime
                                                , defaultTimeLocale
                                                )
import           System.Timeout                 ( timeout )

import           Data.IMF.Types
import qualified Data.IMF.Network.Connection   as Conn
import           Data.IMF.Network.Errors

-- | Message envelope
data Envelope = Envelope
    { sender    :: Mailbox -- ^ sender
    , recipient :: Mailbox -- ^ recipient
    }
  deriving (Show, Eq)

-- | SMTP chat monad
type Chat state = ExceptT ChatException (RWST () ChatLog state IO)

-- | Unwrap a SMTP chat monad into an IO action
runChat :: Chat state () -> state -> IO (state, ChatLog)
runChat f = execRWST (runExceptT f) ()

-- | Lift an IO action into the Chat monad
liftChat :: IO a -> Chat state a
liftChat f = liftIO f `catch` throwError

debug :: B.ByteString -> Chat state ()
debug msg = do
    now <- liftChat getCurrentTime
    tell [(now, Debug, "[" <> msg <> "]\r\n")]

recv :: Conn.Connection -- ^ connection
     -> Bool            -- ^ redact logs?
     -> Parser a        -- ^ parser
     -> NominalDiffTime -- ^ timeout
     -> Int             -- ^ maximum size
     -> Chat state a
recv conn redact parser ttl maxSize = do
    deadline <- liftChat $ addUTCTime ttl <$> getCurrentTime
    getAll deadline 0 (Conn.recv conn) (parse parser)
  where
    getAll :: UTCTime -> Int -> IO B.ByteString -> (B.ByteString -> Result a) -> Chat state a
    getAll deadline currentSize getter parser = do
        bytes <- until deadline getter
        when (bytes == "") $ throwError PeerConnectionClosed
        if redact then logRecv "[...]\r\n" else logRecv bytes
        when (currentSize + B.length bytes > maxSize) $ throwError SizeExceeded
        case parser bytes of
            Fail{}          -> throwError ParseFailure
            Partial parser' -> getAll deadline (currentSize + B.length bytes) getter parser'
            Done _ a        -> return a
    until :: UTCTime -> IO a -> Chat state a
    until deadline f = do
        secs <- liftChat $ diffUTCTime deadline <$> getCurrentTime
        a <- liftChat $ timeout (max 1 $ ceiling $ 1000000 * secs) f
        maybe (throwError Timeout) return a
    logRecv :: B.ByteString -> Chat state ()
    logRecv bytes = do
        now <- liftChat getCurrentTime
        tell [(now, Recv, bytes)]

send :: Conn.Connection -- ^ connection
     -> Bool            -- ^ redact logs?
     -> LB.ByteString
     -> Chat state ()
send conn redact bytes = do
    liftChat $ Conn.send conn bytes
    if redact then logSend "[...]\r\n" else logSend $ LB.toStrict bytes
    return ()
  where
    logSend :: B.ByteString -> Chat state ()
    logSend bytes = do
        now <- liftChat getCurrentTime
        tell [(now, Send, bytes)]

-- | SMTP chat log
type ChatLog = [ChatLogEntry]

type ChatLogEntry = (UTCTime, ChatLogEntryType, B.ByteString)

data ChatLogEntryType = Send | Recv | Debug
  deriving (Show, Eq)

-- | Format the log into a constant-width human-friendly format
formatLog :: ChatLog -> T.Text
formatLog [] = ""
formatLog log@((tzero, _, _):_) = T.concat $ zipWith (formatLogEntry tzero) [1..] log

-- | Print the formated log to stdout
printLog :: ChatLog -> IO ()
printLog = putStr . T.unpack . formatLog

-- | Format just the lines sent and received in the log
formatLogLines :: ChatLog -> T.Text
formatLogLines = T.concat . map (\(_, _, line) -> decodeUtf8 line) . filter (\(_, t, _) -> t /= Debug)

-- | Print the formated log lines to stdout
printLogLines :: ChatLog -> IO ()
printLogLines = putStr . T.unpack . formatLog

formatLogEntry :: UTCTime -> Int -> ChatLogEntry -> T.Text
formatLogEntry tzero seq (t, direction, bytes) =
    LT.toStrict $ TFormat.format "{} {} {} {} {}"
        ( TFormat.left 3 ' ' $ TFormat.shortest seq
        , formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S.%4q" t
        , TFormat.left 9 ' ' $ TFormat.fixed 4 tdelta
        , arrow :: T.Text
        , decodeUtf8 bytes
        )
  where
    tdelta = t `diffUTCTime` tzero
    arrow
      | direction == Send = "->"
      | direction == Recv = "<-"
      | otherwise         = "--"
