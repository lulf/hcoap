module Network.CoAP.Message
( Header
, Message
, Type
, decode
, getMessage
, encode
) where

import Data.ByteString.Lazy
import Data.Word
import Data.Binary hiding (encode, decode)
import Data.Binary.Get
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
type Token       = Int
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
  , messageOptions :: Maybe [(Option, OptionValue)]
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


getTokenLength :: Int -> Get Int
getTokenLength len =
  if len > 8
     then fail "Invalid token length"
     else return (len)

getHeader :: Get (Header, Int)
getHeader = do
  tmp <- getWord8
  let version = fromIntegral (shiftR ((.&.) tmp 0xC0) 6)
  msgType <- getType (shiftR ((.&.) tmp 0x30) 4)
  tokenLength <- getTokenLength (fromIntegral ((.&.) tmp 0x0F) :: Int)

  c <- getWord8
  let clazz  = fromIntegral (shiftR ((.&.) c 0x70) 5)
  let detail = fromIntegral ((.&.) c 0x1F)
  code <- getCode clazz detail

  id <- getWord16be
  return ((Header version msgType code (fromIntegral id)), tokenLength)

getMessage :: Get Message
getMessage = do
  (header, tokenLength) <- getHeader
  return (Message { messageHeader  = header
             , messageToken   = Nothing
             , messageOptions = Nothing
             , messagePayload = Nothing })



decode :: ByteString -> Message
decode msg = runGet getMessage msg

encode :: Message -> ByteString
encode _ = empty
