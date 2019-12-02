{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}

module Data.IMF.FFI
  ()
where

import           Control.Applicative            ( liftA2
                                                , liftA3
                                                )
import           Data.Attoparsec.Text           ( Parser
                                                , parseOnly
                                                , endOfInput
                                                , many1
                                                , sepBy1
                                                )
import           Data.Int                       ( Int64 )
import           Data.List                      ( intercalate )
import qualified Data.Text                     as T
import           Foreign.C.String               ( CString
                                                , newCString
                                                , peekCString
                                                )
import           Foreign.Marshal.Alloc          ( free )
import           Foreign.Ptr                    ( nullPtr )
import           Foreign.StablePtr              ( StablePtr
                                                , newStablePtr
                                                , deRefStablePtr
                                                , castPtrToStablePtr
                                                )
import           Safe                           ( atMay )

import           Data.IMF.Types
import           Data.IMF.Parsers.Mailbox
import           Data.IMF.Parsers.DateTime
import           Data.IMF.Parsers.MsgId
import           Data.IMF.Parsers.Header
import           Data.IMF.Parsers.Message

maybeStablePtr :: Maybe a -> IO (StablePtr a)
maybeStablePtr = maybe (return nullStablePtr) newStablePtr

eitherStablePtr :: Either a b -> IO (StablePtr b)
eitherStablePtr = either (const $ return nullStablePtr) newStablePtr

nullStablePtr :: StablePtr a
nullStablePtr = castPtrToStablePtr nullPtr

parseStablePtr :: Parser a -> CString -> IO (StablePtr a)
parseStablePtr parser cstr = peekCString cstr >>= eitherStablePtr . parseOnly (parser <* endOfInput) . T.pack

showStablePtr :: Show a => StablePtr a -> IO CString
showStablePtr ptr = deRefStablePtr ptr >>= newCString . show

showListStablePtr :: Show a => String -> StablePtr [a] -> IO CString
showListStablePtr sep ptr = deRefStablePtr ptr >>= newCString . intercalate sep . map show

equalsStablePtr :: Eq a => StablePtr a -> StablePtr a -> IO Bool
equalsStablePtr ptr1 ptr2 = liftA2 (==) (deRefStablePtr ptr1) (deRefStablePtr ptr2)

lengthStablePtr :: StablePtr [a] -> IO Int
lengthStablePtr ptr = length <$> deRefStablePtr ptr

atStablePtr :: StablePtr [a] -> Int -> IO (StablePtr a)
atStablePtr ptr key = liftA2 atMay (deRefStablePtr ptr) (return key) >>= maybeStablePtr

foreign export ccall hs_free_cstring :: CString -> IO ()

hs_free_cstring :: CString -> IO ()
hs_free_cstring = free

foreign export ccall mailbox_parse :: CString -> IO (StablePtr Mailbox)
foreign export ccall mailbox_show :: StablePtr Mailbox -> IO CString
foreign export ccall mailbox_equals :: StablePtr Mailbox -> StablePtr Mailbox -> IO Bool
foreign export ccall mailbox_from_parts :: CString -> CString -> CString -> IO (StablePtr Mailbox)
foreign export ccall mailbox_display :: StablePtr Mailbox -> IO CString
foreign export ccall mailbox_local :: StablePtr Mailbox -> IO CString
foreign export ccall mailbox_domain :: StablePtr Mailbox -> IO CString

mailbox_parse :: CString -> IO (StablePtr Mailbox)
mailbox_parse = parseStablePtr pMailbox

mailbox_show :: StablePtr Mailbox -> IO CString
mailbox_show = showStablePtr

mailbox_equals :: StablePtr Mailbox -> StablePtr Mailbox -> IO Bool
mailbox_equals = equalsStablePtr

mailbox_from_parts :: CString -> CString -> CString -> IO (StablePtr Mailbox)
mailbox_from_parts cstr1 cstr2 cstr3 = do
    mbox <- liftA3 Mailbox (T.pack <$> peekCString cstr1) (T.pack <$> peekCString cstr2) (T.pack <$> peekCString cstr3)
    eitherStablePtr $ parseOnly (pMailbox <* endOfInput) $ T.pack $ show mbox

mailbox_display :: StablePtr Mailbox -> IO CString
mailbox_display ptr = do
    Mailbox display _ _ <- deRefStablePtr ptr
    newCString $ T.unpack display

mailbox_local :: StablePtr Mailbox -> IO CString
mailbox_local ptr = do
    Mailbox _ local _ <- deRefStablePtr ptr
    newCString $ T.unpack local

mailbox_domain :: StablePtr Mailbox -> IO CString
mailbox_domain ptr = do
    Mailbox _ _ domain <- deRefStablePtr ptr
    newCString $ T.unpack domain

foreign export ccall mailbox_list_parse :: CString -> IO (StablePtr [Mailbox])
foreign export ccall mailbox_list_show :: StablePtr [Mailbox] -> IO CString
foreign export ccall mailbox_list_length :: StablePtr [Mailbox] -> IO Int
foreign export ccall mailbox_list_item :: StablePtr [Mailbox] -> Int -> IO (StablePtr Mailbox)

mailbox_list_parse :: CString -> IO (StablePtr [Mailbox])
mailbox_list_parse = parseStablePtr $ pMailbox `sepBy1` ","

mailbox_list_show :: StablePtr [Mailbox] -> IO CString
mailbox_list_show = showListStablePtr ", "

mailbox_list_length :: StablePtr [Mailbox] -> IO Int
mailbox_list_length = lengthStablePtr

mailbox_list_item :: StablePtr [Mailbox] -> Int -> IO (StablePtr Mailbox)
mailbox_list_item = atStablePtr

foreign export ccall datetime_parse :: CString -> IO (StablePtr ZonedTime)
foreign export ccall datetime_show :: StablePtr ZonedTime -> IO CString
foreign export ccall datetime_equals :: StablePtr ZonedTime -> StablePtr ZonedTime -> IO Bool
foreign export ccall datetime_from_nanoseconds :: Int64 -> Int -> IO (StablePtr ZonedTime)
foreign export ccall datetime_nanoseconds :: StablePtr ZonedTime -> IO Int64
foreign export ccall datetime_tzoffset :: StablePtr ZonedTime -> IO Int

datetime_parse :: CString -> IO (StablePtr ZonedTime)
datetime_parse = parseStablePtr pDateTime

datetime_show :: StablePtr ZonedTime -> IO CString
datetime_show = showStablePtr

datetime_equals :: StablePtr ZonedTime -> StablePtr ZonedTime -> IO Bool
datetime_equals = equalsStablePtr

datetime_from_nanoseconds :: Int64 -> Int -> IO (StablePtr ZonedTime)
datetime_from_nanoseconds ns tzoffset = newStablePtr $ zonedTimeFromNanoseconds ns tzoffset

datetime_nanoseconds :: StablePtr ZonedTime -> IO Int64
datetime_nanoseconds ptr = zonedTimeNanoseconds <$> deRefStablePtr ptr

datetime_tzoffset :: StablePtr ZonedTime -> IO Int
datetime_tzoffset ptr = zonedTimeTZOffset <$> deRefStablePtr ptr

foreign export ccall message_id_parse :: CString -> IO (StablePtr MsgId)
foreign export ccall message_id_show :: StablePtr MsgId -> IO CString
foreign export ccall message_id_equals :: StablePtr MsgId -> StablePtr MsgId -> IO Bool
foreign export ccall message_id_from_parts :: CString -> CString -> IO (StablePtr MsgId)
foreign export ccall message_id_left :: StablePtr MsgId -> IO CString
foreign export ccall message_id_right :: StablePtr MsgId -> IO CString

message_id_parse :: CString -> IO (StablePtr MsgId)
message_id_parse = parseStablePtr pMsgId

message_id_show :: StablePtr MsgId -> IO CString
message_id_show = showStablePtr

message_id_equals :: StablePtr MsgId -> StablePtr MsgId -> IO Bool
message_id_equals = equalsStablePtr

message_id_from_parts :: CString -> CString -> IO (StablePtr MsgId)
message_id_from_parts cstr1 cstr2 = do
    msgid <- liftA2 MsgId (T.pack <$> peekCString cstr1) (T.pack <$> peekCString cstr2)
    eitherStablePtr $ parseOnly (pMsgId <* endOfInput) $ T.pack $ show msgid

message_id_left :: StablePtr MsgId -> IO CString
message_id_left ptr = do
    MsgId left _ <- deRefStablePtr ptr
    newCString $ T.unpack left

message_id_right :: StablePtr MsgId -> IO CString
message_id_right ptr = do
    MsgId _ right <- deRefStablePtr ptr
    newCString $ T.unpack right

foreign export ccall header_parse :: CString -> IO (StablePtr Header)
foreign export ccall header_show :: StablePtr Header -> IO CString
foreign export ccall header_equals :: StablePtr Header -> StablePtr Header -> IO Bool

header_parse :: CString -> IO (StablePtr Header)
header_parse = parseStablePtr pHeader

header_show :: StablePtr Header -> IO CString
header_show = showStablePtr

header_equals :: StablePtr Header -> StablePtr Header -> IO Bool
header_equals = equalsStablePtr

foreign export ccall header_list_parse :: CString -> IO (StablePtr [Header])
foreign export ccall header_list_show :: StablePtr [Header] -> IO CString
foreign export ccall header_list_length :: StablePtr [Header] -> IO Int
foreign export ccall header_list_item :: StablePtr [Header] -> Int -> IO (StablePtr Header)

header_list_parse :: CString -> IO (StablePtr [Header])
header_list_parse = parseStablePtr $ many1 pHeader

header_list_show :: StablePtr [Header] -> IO CString
header_list_show = showListStablePtr ""

header_list_length :: StablePtr [Header] -> IO Int
header_list_length = lengthStablePtr

header_list_item :: StablePtr [Header] -> Int -> IO (StablePtr Header)
header_list_item = atStablePtr

foreign export ccall message_parse :: CString -> IO (StablePtr Message)
foreign export ccall message_show :: StablePtr Message -> IO CString
foreign export ccall message_equals :: StablePtr Message -> StablePtr Message -> IO Bool
foreign export ccall message_headers :: StablePtr Message -> IO (StablePtr [Header])
foreign export ccall message_body :: StablePtr Message -> IO CString

message_parse :: CString -> IO (StablePtr Message)
message_parse = parseStablePtr pMessage

message_show :: StablePtr Message -> IO CString
message_show = showStablePtr

message_equals :: StablePtr Message -> StablePtr Message -> IO Bool
message_equals = equalsStablePtr

message_headers :: StablePtr Message -> IO (StablePtr [Header])
message_headers ptr = do
    Message headers _ <- deRefStablePtr ptr
    newStablePtr headers

message_body :: StablePtr Message -> IO CString
message_body ptr = do
    Message _ body <- deRefStablePtr ptr
    newCString $ T.unpack body
