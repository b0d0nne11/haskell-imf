{-# LANGUAGE ForeignFunctionInterface #-}

module Text.IMF.FFI
  ()
where

import           Control.Applicative            ( liftA2
                                                , liftA3
                                                )
import           Data.Int                       ( Int64 )
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

import           Text.IMF.Format
import           Text.IMF.Mailbox
import           Text.IMF.DateTime
import           Text.IMF.MsgId
import           Text.IMF.Header
import           Text.IMF.Message

maybeStablePtr :: Maybe a -> IO (StablePtr a)
maybeStablePtr = maybe (return nullStablePtr) newStablePtr

eitherStablePtr :: Either a b -> IO (StablePtr b)
eitherStablePtr = either (const $ return nullStablePtr) newStablePtr

nullStablePtr :: StablePtr a
nullStablePtr = castPtrToStablePtr nullPtr

parseStablePtr :: ParseMessage a => CString -> IO (StablePtr a)
parseStablePtr cstr = parseMessage <$> peekCString cstr >>= eitherStablePtr

formatStablePtr :: FormatMessage a => StablePtr a -> IO CString
formatStablePtr ptr = formatMessage <$> deRefStablePtr ptr >>= newCString

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
foreign export ccall mailbox_format :: StablePtr Mailbox -> IO CString
foreign export ccall mailbox_equals :: StablePtr Mailbox -> StablePtr Mailbox -> IO Bool
foreign export ccall mailbox_from_parts :: CString -> CString -> CString -> IO (StablePtr Mailbox)
foreign export ccall mailbox_display :: StablePtr Mailbox -> IO CString
foreign export ccall mailbox_local :: StablePtr Mailbox -> IO CString
foreign export ccall mailbox_domain :: StablePtr Mailbox -> IO CString

mailbox_parse :: CString -> IO (StablePtr Mailbox)
mailbox_parse = parseStablePtr

mailbox_format :: StablePtr Mailbox -> IO CString
mailbox_format = formatStablePtr

mailbox_equals :: StablePtr Mailbox -> StablePtr Mailbox -> IO Bool
mailbox_equals = equalsStablePtr

mailbox_from_parts :: CString -> CString -> CString -> IO (StablePtr Mailbox)
mailbox_from_parts cstr1 cstr2 cstr3 = do
    mbox <- liftA3 Mailbox (peekCString cstr1) (peekCString cstr2) (peekCString cstr3)
    eitherStablePtr $ validateMessage mbox

mailbox_display :: StablePtr Mailbox -> IO CString
mailbox_display ptr = do
    Mailbox display _ _ <- deRefStablePtr ptr
    newCString display

mailbox_local :: StablePtr Mailbox -> IO CString
mailbox_local ptr = do
    Mailbox _ local _ <- deRefStablePtr ptr
    newCString local

mailbox_domain :: StablePtr Mailbox -> IO CString
mailbox_domain ptr = do
    Mailbox _ _ domain <- deRefStablePtr ptr
    newCString domain

foreign export ccall mailbox_list_parse :: CString -> IO (StablePtr [Mailbox])
foreign export ccall mailbox_list_format :: StablePtr [Mailbox] -> IO CString
foreign export ccall mailbox_list_length :: StablePtr [Mailbox] -> IO Int
foreign export ccall mailbox_list_item :: StablePtr [Mailbox] -> Int -> IO (StablePtr Mailbox)

mailbox_list_parse :: CString -> IO (StablePtr [Mailbox])
mailbox_list_parse = parseStablePtr

mailbox_list_format :: StablePtr [Mailbox] -> IO CString
mailbox_list_format = formatStablePtr

mailbox_list_length :: StablePtr [Mailbox] -> IO Int
mailbox_list_length = lengthStablePtr

mailbox_list_item :: StablePtr [Mailbox] -> Int -> IO (StablePtr Mailbox)
mailbox_list_item = atStablePtr

foreign export ccall datetime_parse :: CString -> IO (StablePtr DateTime)
foreign export ccall datetime_format :: StablePtr DateTime -> IO CString
foreign export ccall datetime_equals :: StablePtr DateTime -> StablePtr DateTime -> IO Bool
foreign export ccall datetime_from_nanoseconds :: Int64 -> Int -> IO (StablePtr DateTime)
foreign export ccall datetime_nanoseconds :: StablePtr DateTime -> IO Int64
foreign export ccall datetime_tzoffset :: StablePtr DateTime -> IO Int

datetime_parse :: CString -> IO (StablePtr DateTime)
datetime_parse = parseStablePtr

datetime_format :: StablePtr DateTime -> IO CString
datetime_format = formatStablePtr

datetime_equals :: StablePtr DateTime -> StablePtr DateTime -> IO Bool
datetime_equals = equalsStablePtr

datetime_from_nanoseconds :: Int64 -> Int -> IO (StablePtr DateTime)
datetime_from_nanoseconds ns tzoffset =
    newStablePtr $ datetimeFromNanoseconds ns tzoffset

datetime_nanoseconds :: StablePtr DateTime -> IO Int64
datetime_nanoseconds ptr = datetimeNanoseconds <$> deRefStablePtr ptr

datetime_tzoffset :: StablePtr DateTime -> IO Int
datetime_tzoffset ptr = datetimeTZOffset <$> deRefStablePtr ptr

foreign export ccall message_id_parse :: CString -> IO (StablePtr MsgId)
foreign export ccall message_id_format :: StablePtr MsgId -> IO CString
foreign export ccall message_id_equals :: StablePtr MsgId -> StablePtr MsgId -> IO Bool
foreign export ccall message_id_from_parts :: CString -> CString -> IO (StablePtr MsgId)
foreign export ccall message_id_left :: StablePtr MsgId -> IO CString
foreign export ccall message_id_right :: StablePtr MsgId -> IO CString

message_id_parse :: CString -> IO (StablePtr MsgId)
message_id_parse = parseStablePtr

message_id_format :: StablePtr MsgId -> IO CString
message_id_format = formatStablePtr

message_id_equals :: StablePtr MsgId -> StablePtr MsgId -> IO Bool
message_id_equals = equalsStablePtr

message_id_from_parts :: CString -> CString -> IO (StablePtr MsgId)
message_id_from_parts cstr1 cstr2 = do
    msgid <- liftA2 MsgId (peekCString cstr1) (peekCString cstr2)
    eitherStablePtr $ validateMessage msgid

message_id_left :: StablePtr MsgId -> IO CString
message_id_left ptr = do
    MsgId left _ <- deRefStablePtr ptr
    newCString left

message_id_right :: StablePtr MsgId -> IO CString
message_id_right ptr = do
    MsgId _ right <- deRefStablePtr ptr
    newCString right

foreign export ccall header_field_parse :: CString -> IO (StablePtr HeaderField)
foreign export ccall header_field_format :: StablePtr HeaderField -> IO CString
foreign export ccall header_field_equals :: StablePtr HeaderField -> StablePtr HeaderField -> IO Bool
foreign export ccall header_field_name :: StablePtr HeaderField -> IO CString
foreign export ccall header_field_value :: StablePtr HeaderField -> IO CString

header_field_parse :: CString -> IO (StablePtr HeaderField)
header_field_parse = parseStablePtr

header_field_format :: StablePtr HeaderField -> IO CString
header_field_format = formatStablePtr

header_field_equals :: StablePtr HeaderField -> StablePtr HeaderField -> IO Bool
header_field_equals = equalsStablePtr

header_field_name :: StablePtr HeaderField -> IO CString
header_field_name ptr = do
    f <- deRefStablePtr ptr
    newCString $ fieldName f

header_field_value :: StablePtr HeaderField -> IO CString
header_field_value ptr = do
    f <- deRefStablePtr ptr
    newCString $ fieldValue f

foreign export ccall header_parse :: CString -> IO (StablePtr Header)
foreign export ccall header_format :: StablePtr Header -> IO CString
foreign export ccall header_length :: StablePtr Header -> IO Int
foreign export ccall header_item :: StablePtr Header -> Int -> IO (StablePtr HeaderField)

header_parse :: CString -> IO (StablePtr Header)
header_parse = parseStablePtr

header_format :: StablePtr Header -> IO CString
header_format = formatStablePtr

header_length :: StablePtr Header -> IO Int
header_length ptr = do
    Header fs <- deRefStablePtr ptr
    return $ length fs

header_item :: StablePtr Header -> Int -> IO (StablePtr HeaderField)
header_item ptr key = do
    Header fs <- deRefStablePtr ptr
    maybeStablePtr $ atMay fs key

foreign export ccall message_parse :: CString -> IO (StablePtr Message)
foreign export ccall message_format :: StablePtr Message -> IO CString
foreign export ccall message_equals :: StablePtr Message -> StablePtr Message -> IO Bool
foreign export ccall message_header :: StablePtr Message -> IO (StablePtr Header)
foreign export ccall message_body :: StablePtr Message -> IO CString

message_parse :: CString -> IO (StablePtr Message)
message_parse = parseStablePtr

message_format :: StablePtr Message -> IO CString
message_format = formatStablePtr

message_equals :: StablePtr Message -> StablePtr Message -> IO Bool
message_equals = equalsStablePtr

message_header :: StablePtr Message -> IO (StablePtr Header)
message_header ptr = do
    Message header _ <- deRefStablePtr ptr
    newStablePtr header

message_body :: StablePtr Message -> IO CString
message_body ptr = do
    Message _ body <- deRefStablePtr ptr
    newCString body
