module Network.CoAP.MessageCodec
( decode
, encode
) where

import Debug.Trace
import Network.CoAP.Options
import Network.CoAP.Payload
import Network.CoAP.Message
import Network.CoAP.Request hiding (Request)
import Network.CoAP.Response hiding (Response)
import Data.ByteString.Lazy
import qualified Data.ByteString as BS
import Data.Word
import Data.Maybe
import Data.List (sortOn)
import Data.Binary hiding (encode, decode)
import qualified Data.Binary as DB
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Prelude hiding (null, length, fromStrict, toStrict)

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
  -- return (trace ("TKL: " ++ (show tokenLength) ++ ", clazz: " ++ (show clazz) ++ ", detail: " ++ (show detail) ++ ", code: " ++ (show code) ++ ", id: " ++ (show id)) ((Header version msgType code (fromIntegral id)), tokenLength))
  return ((Header version msgType code (fromIntegral id)), tokenLength)

getToken :: Word8 -> Get (Maybe Token)
getToken 0 = return (Nothing)
getToken n = do
  str <- getByteString (fromIntegral n)
  --return (trace ("Token length: " ++ (show str)) (Just str))
  return (Just str)

intToMediaType 0  = TextPlain
intToMediaType 40 = ApplicationLinkFormat
intToMediaType 41 = ApplicationXml
intToMediaType 42 = ApplicationOctetStream
intToMediaType 47 = ApplicationExi
intToMediaType 50 = ApplicationJson

decodeOption :: Int -> BS.ByteString -> Int -> Option
decodeOption 1 value _ = IfMatch value
decodeOption 3 value _ = UriHost value
decodeOption 4 value _ = ETag value
decodeOption 5 _ _ = IfNoneMatch
decodeOption 7 value valueLen = UriPort (decodeOptionInt value valueLen)
decodeOption 8 value _ = LocationPath value
decodeOption 11 value _ = UriPath value
decodeOption 12 value valueLen = ContentFormat (intToMediaType (decodeOptionInt value valueLen))
decodeOption 14 value valueLen = MaxAge (decodeOptionInt value valueLen)
decodeOption 15 value _ = UriQuery value
decodeOption 17 value valueLen = Accept (decodeOptionInt value valueLen)
decodeOption 20 value _ = LocationQuery value
decodeOption 35 value _ = ProxyUri value
decodeOption 39 value _ = ProxyScheme value
decodeOption 60 value valueLen = Size1 (decodeOptionInt value valueLen)

getOptionInt :: Int -> Get Int
getOptionInt valueLen = do
  if valueLen < 256
  then do
    v <- getWord8
    return (fromIntegral v)
  else if valueLen < 65536
       then do
         v <- getWord16be
         return (fromIntegral v)
       else if valueLen < 16777216
            then do
              a <- getWord8
              b <- getWord8
              c <- getWord8
              return ((.|.) ((.|.) (shiftL (fromIntegral a :: Int) 16) (shiftL (fromIntegral b :: Int) 8)) (fromIntegral c :: Int))
            else do
              v <- getWord32be
              return (fromIntegral v)

decodeOptionInt :: BS.ByteString -> Int -> Int
decodeOptionInt value valueLen = runGet (getOptionInt valueLen) (fromStrict value)

getOptionPart :: Int -> Get (Int)
getOptionPart part = do
  if part == 14
  then do
    v <- getWord16be
    return ((fromIntegral v) + 269)
  else if part == 13
       then do
         v <- getWord8
         return ((fromIntegral v) + 13)
       else return part

getOption :: Int -> Get (Maybe Option)
getOption lastCode = do
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
      fullDelta <- getOptionPart delta
      let optCode = lastCode + fullDelta

      fullLength <- getOptionPart valueLength
      value <- getByteString fullLength
      let option = decodeOption optCode value fullLength

      return (Just option)

getOptionsLoop :: Int -> Get ([Option])
getOptionsLoop lastCode = do
  opt <- getOption lastCode
  case opt of
    Nothing -> do
      return []
    Just o  -> do
      let optCode = fst (encodeOption o)
      opts <- getOptionsLoop optCode
      return (o:opts)

getOptions :: Get ([Option])
getOptions = getOptionsLoop 0

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

encodeResponseCode :: ResponseCode -> (Word8, Word8)
encodeResponseCode Created               = (2, 1)
encodeResponseCode Deleted               = (2, 2)
encodeResponseCode Valid                 = (2, 3)
encodeResponseCode Changed               = (2, 4)
encodeResponseCode Content               = (2, 5)
encodeResponseCode BadRequest            = (4, 0)
encodeResponseCode Unauthorized          = (4, 1)
encodeResponseCode BadOption             = (4, 2)
encodeResponseCode Forbidden             = (4, 3)
encodeResponseCode NotFound              = (4, 4)
encodeResponseCode MethodNotAllowed      = (4, 5)
encodeResponseCode NotAcceptable         = (4, 6)
encodeResponseCode PreconditionFailed    = (4, 12)
encodeResponseCode RequestEntityTooLarge = (4, 13)
encodeResponseCode UnsupportedFormat     = (4, 15)
encodeResponseCode InternalServerError   = (5, 0)
encodeResponseCode NotImplemented        = (5, 1)
encodeResponseCode BadGateway            = (5, 2)
encodeResponseCode ServiceUnavailable    = (5, 3)
encodeResponseCode GatewayTimeout        = (5, 4)
encodeResponseCode ProxyingNotSupported  = (5, 5)

encodeCode :: Code -> Word8
encodeCode Empty = 0
encodeCode (Request detail) = encodeRequestMethod detail
encodeCode (Response detail) =
  let (responseClass, responseDetail) = encodeResponseCode detail
   in (.|.) (shiftL responseClass 5) responseDetail

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

putOptionInt :: Int -> Put
putOptionInt 0 = return ()
putOptionInt n = do
  if n < 256
  then putWord8 (fromIntegral n)
  else if n < 65536
       then putWord16be (fromIntegral n)
       else putWord32be (fromIntegral n)

encodeOptionInt :: Int -> BS.ByteString
encodeOptionInt n = BS.pack (unpack (runPut (putOptionInt n)))

mediaTypeToInt :: MediaType -> Int
mediaTypeToInt TextPlain              = 0
mediaTypeToInt ApplicationLinkFormat  = 40
mediaTypeToInt ApplicationXml         = 41
mediaTypeToInt ApplicationOctetStream = 42
mediaTypeToInt ApplicationExi         = 47
mediaTypeToInt ApplicationJson        = 50

encodeOption :: Option -> (Int, BS.ByteString)
encodeOption (IfMatch value)            = (1, value)
encodeOption (UriHost value)            = (3, value)
encodeOption (ETag value)               = (4, value)
encodeOption IfNoneMatch                = (5, BS.empty)
encodeOption (UriPort value)            = (7, encodeOptionInt value)
encodeOption (LocationPath value)       = (8, value)
encodeOption (UriPath value)            = (11, value)
encodeOption (ContentFormat mediaType)  = (12, encodeOptionInt (mediaTypeToInt mediaType))
encodeOption (MaxAge value)             = (14, encodeOptionInt value)
encodeOption (UriQuery value)           = (15, value)
encodeOption (Accept value)             = (17, encodeOptionInt value)
encodeOption (LocationQuery value)      = (20, value)
encodeOption (ProxyUri value)           = (35, value)
encodeOption (ProxyScheme value)        = (39, value)
encodeOption (Size1 value)              = (60, encodeOptionInt value)

encodeOptionPart :: Int -> Word8
encodeOptionPart value =
  if value > 268
  then 14
  else if value > 12
       then 13
       else fromIntegral value

putOptionPart :: Word8 -> Int -> Put
putOptionPart valueLen value = do
  if valueLen == 13
  then putWord8 (fromIntegral (value - 13))
  else if valueLen == 14
       then putWord16be (fromIntegral (value - 269))
       else return ()

putOption :: Int -> BS.ByteString -> Put
putOption nextDelta optValue = do
  let optValueLen = BS.length optValue
  let valueLen = encodeOptionPart optValueLen
  let deltaValue = encodeOptionPart nextDelta
  let tmp = (.|.) (shiftL deltaValue 4) ((.&.) valueLen 0x0F)

  putWord8 tmp
  putOptionPart deltaValue nextDelta
  putOptionPart valueLen optValueLen
  putByteString optValue

putOptionsLoop :: Int -> [Option] -> Put
putOptionsLoop lastNumber [] = return ()
putOptionsLoop lastNumber (opt:options) = do
  let (optNumber, optValue) = encodeOption opt
  let nextDelta = optNumber - lastNumber
  putOption nextDelta optValue
  putOptionsLoop optNumber options

putOptions :: [Option] -> Put
putOptions options = do
  let sortedOptions = sortOn (\o -> fst (encodeOption o)) options
  putOptionsLoop 0 sortedOptions

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
