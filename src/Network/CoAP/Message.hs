module Network.CoAP.Message
( Header
, Message
, Type
, decode
, decodeMessage
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

parseType :: Word8 -> Maybe Type
parseType 0 = Just CON
parseType 1 = Just NON
parseType 2 = Just ACK
parseType 3 = Just RST
parseType _ = Nothing

parseHeader :: Word8 -> Maybe Header
parseHeader tmp = do
  let version = fromIntegral (shiftR ((.&.) tmp 0xC0) 6)
  msgType <- parseType (shiftR ((.&.) tmp 0x30) 4)
  let tokenLength = fromIntegral ((.&.) tmp 0x0F) :: Int
  return (Header version msgType Empty (fromIntegral 1))

decodeMessage :: Get (Maybe Message)
decodeMessage = do
  tmp <- getWord8
  code <- getWord8
  id <- getWord16be
  let header = parseHeader tmp
  return (maybe Nothing (\hdr -> Just (Message
         { messageHeader  = hdr
         , messageToken   = Nothing
         , messageOptions = Nothing
         , messagePayload = Nothing })) header)



decode :: ByteString -> Maybe Message
decode msg = runGet decodeMessage msg

encode :: Message -> ByteString
encode _ = empty
