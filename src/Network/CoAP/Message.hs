module Network.CoAP.Message
( Header (Header)
, Message (Message)
, MessageId
, Type (CON, NON, ACK, RST)
, decode
, getMessage
, encode
, RequestMethod
, ResponseCode
, Code (Empty, Request, Response)
, messageCode
, messageHeader
, messageId
, messageToken
, messagePayload
, messageVersion
, messageType
, messageOptions
) where

import Debug.Trace
import Network.CoAP.Options
import Network.CoAP.Payload
import Network.CoAP.Request hiding (Request)
import Network.CoAP.Response hiding (Response)
import Data.ByteString.Lazy
import qualified Data.ByteString as BS
import Data.Word
import Data.Maybe
import Data.Binary hiding (encode, decode)
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Prelude hiding (null, length, fromStrict, toStrict)

type RequestMethod = Method


data Code = Request RequestMethod
          | Response ResponseCode
          | Empty
          deriving (Show)

data Type = CON | NON | ACK | RST deriving (Show)

data Header = Header
  { messageVersion     :: Version
  , messageType        :: Type
  , messageCode        :: Code
  , messageId          :: MessageId
  }

type Version     = Int
type MessageId   = Word16
type Token       = BS.ByteString

data Message = Message
  { messageHeader  :: Header
  , messageToken   :: Maybe Token
  , messageOptions :: [(Option, OptionValue)]
  , messagePayload :: Maybe Payload
  }

getType :: Word8 -> Get Type
getType 0 = return (CON)
getType 1 = return (NON)
getType 2 = return (ACK)
getType 3 = return (RST)
getType _ = fail "Unknown type"

getRequestMethod :: Word8 -> Get RequestMethod
getRequestMethod 1 = return (GET)
getRequestMethod 2 = return (POST)
getRequestMethod 3 = return (PUT)
getRequestMethod 4 = return (DELETE)
getRequestMethod _ = fail "Unknown request method"

getResponseCode :: Word8 -> Get ResponseCode
getResponseCode _ = return (Created)

getCode :: Word8 -> Word8 -> Get Code
getCode 0 0 = return (Empty)
getCode 0 detail = do
  method <- getRequestMethod detail
  return (Request method)

getCode code detail =
  if code == 2 || code == 4 || code == 5
     then do
       responseCode <- getResponseCode detail
       return (Response responseCode)
     else fail "Unknown class"


getTokenLength :: Word8 -> Get Word8
getTokenLength len =
  if len > 8
     then fail "Invalid token length"
     else return (len)

getHeader :: Get (Header, Word8)
getHeader = do
  tmp <- getWord8
  let version = fromIntegral (shiftR ((.&.) tmp 0xC0) 6)
  msgType <- getType (shiftR ((.&.) tmp 0x30) 4)
  tokenLength <- getTokenLength ((.&.) tmp 0x0F)

  c <- getWord8
  let clazz  = fromIntegral (shiftR ((.&.) c 0x70) 5)
  let detail = fromIntegral ((.&.) c 0x1F)
  code <- getCode clazz detail

  id <- getWord16be
  return (trace ("TKL: " ++ (show tokenLength) ++ ", clazz: " ++ (show clazz) ++ ", detail: " ++ (show detail) ++ ", code: " ++ (show code) ++ ", id: " ++ (show id)) ((Header version msgType code (fromIntegral id)), tokenLength))

getToken :: Word8 -> Get (Maybe Token)
getToken 0 = return (Nothing)
getToken n = do
  str <- getByteString (fromIntegral n)
  return (trace ("Token length: " ++ (show str)) (Just str))

getOption :: Get (Maybe (Option, OptionValue))
getOption = do
  e <- isEmpty
  if e
  then return Nothing
  else do
    tmp <- getWord8
    if tmp == 0xFF
    then return Nothing
    else do
      let delta  = fromIntegral ((.&.) (shiftR tmp 4) 0x0F)
      let valueLength = fromIntegral ((.&.) tmp 0x0F)
      rem <- remaining
      let v = trace ("Got delta " ++ (show delta) ++ ", and len " ++ (show valueLength) ++ " remaining " ++ (show rem)) delta
      if v == 0
      then do
        value <- getByteString valueLength
        return (Just (ContentFormat, trace ("Value is " ++ (show value)) value))
      else do
        value <- getByteString valueLength
        return (Just (ContentFormat, trace ("Value is " ++ (show value)) value))

getOptions :: Get ([(Option, OptionValue)])
getOptions = do
  opt <- getOption
  case opt of
    Nothing -> do
      return []
    Just o  -> do
      opts <- getOptions
      return (o:opts)

getPayload :: Get (Maybe Payload)
getPayload = do
  str <- getRemainingLazyByteString
  return (if null str
         then Nothing
         else Just (BS.pack (unpack str)))


getMessage :: Get Message
getMessage = do
  (header, tokenLength) <- getHeader
  token <- getToken tokenLength
  options <- getOptions
  payload <- getPayload
  return (Message { messageHeader  = header
                  , messageToken   = token
                  , messageOptions = options
                  , messagePayload = payload })

decode :: BS.ByteString -> Message
decode msg = runGet getMessage (fromStrict msg)

encodeType :: Type -> Word8
encodeType CON = 0
encodeType NON = 1
encodeType ACK = 2
encodeType RST = 3

encodeRequestMethod :: RequestMethod -> Word8
encodeRequestMethod GET    = 1
encodeRequestMethod POST   = 2
encodeRequestMethod PUT    = 3
encodeRequestMethod DELETE = 4

encodeResponseCode :: ResponseCode -> Word8
encodeResponseCode _ = 0

encodeCode :: Code -> Word8
encodeCode Empty = 0
encodeCode (Request detail) = encodeRequestMethod detail
encodeCode (Response detail) = (.|.) (shiftL 2 5) (encodeResponseCode detail)

putHeader :: Header -> Word8 -> Put
putHeader header tokenLength = do
  let version = fromIntegral (messageVersion header) :: Word8
  let eType   = encodeType (messageType header)
  let code    = messageCode    header
  let id      = messageId      header
  putWord8 ((.|.) ((.|.) (shiftL version 6) (shiftL eType 4)) ((.&.) tokenLength 0x0F))
  putWord8 (encodeCode code)
  putWord16be id


putToken :: Maybe Token -> Put
putToken Nothing = return ()
putToken (Just token) = putByteString token

putOptions :: [(Option, OptionValue)] -> Put
putOptions options = return ()

putPayload :: Maybe Payload -> Put
putPayload Nothing = return ()
putPayload (Just payload) = do
  putWord8 0xFF
  putByteString payload

putMessage :: Message -> Put
putMessage msg = do
  let token = messageToken msg
  let tokenLength = case token of Just t -> BS.length t
                                  _ -> 0
  putHeader  (messageHeader  msg) (fromIntegral tokenLength)
  putToken   token
  putOptions (messageOptions msg)
  putPayload (messagePayload msg)

encode :: Message -> BS.ByteString
encode msg = toStrict (runPut (putMessage msg))
