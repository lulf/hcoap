module Network.CoAP.Message
( Header
, Message
, Type
, decode
, getMessage
, encode
) where

import Data.ByteString.Lazy
import qualified Data.ByteString as BS
import Data.Word
import Data.Maybe
import Data.Binary hiding (encode, decode)
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits

data Type = CON | NON | ACK | RST

data RequestMethod = GET | POST | PUT | DELETE

data ResponseCode = Created
                  | Deleted
                  | Valid
                  | Changed
                  | Content
                  | BadRequest
                  | Unauthorized
                  | BadOption
                  | Forbidden
                  | NotFound
                  | MethodNotAllowed
                  | NotAcceptable
                  | PreconditionFailed
                  | RequestEntityTooLarge
                  | UnsupportedFormat
                  | InternalServerError
                  | NotImplemented
                  | BadGateway
                  | ServiceUnavailable
                  | GatewayTimeout
                  | ProxyingNotSupported

data Code = Request RequestMethod
          | Response ResponseCode
          | Empty

data Header = Header
  { messageVersion     :: Version
  , messageType        :: Type
  , messageCode        :: Code
  , messageId          :: Id
  }

type Version     = Int
type Id          = Word16
type Token       = BS.ByteString
type OptionValue = ByteString

data Option = ContentFormat
            | ETag
            | LocationPath
            | LocationQuery
            | MaxAge
            | ProxyUri
            | ProxyScheme
            | UriHost
            | UriPath
            | UriPort
            | UriQuery
            | Accept
            | IfMatch
            | IfNoneMatch
            | Size1

data Message = Message
  { messageHeader  :: Header
  , messageToken   :: Maybe Token
  , messageOptions :: [(Option, OptionValue)]
  , messagePayload :: Maybe ByteString
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
  return ((Header version msgType code (fromIntegral id)), tokenLength)

getToken :: Word8 -> Get (Maybe Token)
getToken 0 = return (Nothing)
getToken n = do
  str <- getByteString (fromIntegral n)
  return (Just str)

getOption :: Get (Option, OptionValue)
getOption = fail "No supported options"

getOptions :: Get ([(Option, OptionValue)])
getOptions = do
  marker <- getWord8
  if marker == 0xFF
     then return ([])
     else do
       opt <- getOption
       opts <- getOptions
       return (opt:opts)

getPayload :: Get (Maybe ByteString)
getPayload = do
  str <- getRemainingLazyByteString
  return (if (Data.ByteString.Lazy.null str)
         then Nothing
         else Just str)


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

decode :: ByteString -> Message
decode msg = runGet getMessage msg

--  let version = fromIntegral (shiftR ((.&.) tmp 0xC0) 6)
--  msgType <- getType (shiftR ((.&.) tmp 0x30) 4)
--  tokenLength <- getTokenLength (fromIntegral ((.&.) tmp 0x0F) :: Int)
--
--  c <- getWord8
--  let clazz  = fromIntegral (shiftR ((.&.) c 0x70) 5)
--  let detail = fromIntegral ((.&.) c 0x1F)
--  code <- getCode clazz detail
--
--  id <- getWord16be
--  return ((Header version msgType code (fromIntegral id)), tokenLength)

encodeType :: Type -> Word8
encodeType CON = 0
encodeType NON = 1
encodeType ACK = 2
encodeType RST = 3

putHeader :: Header -> Word8 -> Put
putHeader header tokenLength = do
  let version = fromIntegral (messageVersion header) :: Word8
  let eType   = encodeType (messageType header)

  let code    = messageCode    header
  let id      = messageId      header

  putWord8 ((.|.) ((.|.) (shiftL version 6) (shiftL eType 4)) ((.&.) tokenLength 0x0F))

  return ()


putToken :: Maybe Token -> Put
putToken token = return ()

putOptions :: [(Option, OptionValue)] -> Put
putOptions options = return ()

putPayload :: Maybe ByteString -> Put
putPayload payload = return ()

putMessage :: Message -> Put
putMessage msg = do
  let token = messageToken msg
  let tokenLength = case token of Just t -> BS.length t
                                  _ -> 0
  putHeader  (messageHeader  msg) (fromIntegral tokenLength)
  putToken   token
  putOptions (messageOptions msg)
  putPayload (messagePayload msg)
  return ()

encode :: Message -> ByteString
encode msg = runPut (putMessage msg)
