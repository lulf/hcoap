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

decodeMessage :: Get (Message)
decodeMessage = do
  tmp <- getWord8
  let version = shiftR ((.&.) tmp 0xC0) 6
  let msgType = parseType (shiftR (.&.) tmp 0x30) 4
  let tokenLength = ((.&.) tmp 0x0F) :: Int

  code <- getWord8
  id <- getWord16be
  let header = Header 1 ACK Empty (fromIntegral 1)
  return (Message
         { messageHeader  = header
         , messageToken   = Nothing
         , messageOptions = Nothing
         , messagePayload = Nothing })



decode :: ByteString -> Maybe Message
decode msg =
  let parsedMsg = runGet decodeMessage msg
   in Nothing

encode :: Message -> ByteString
encode _ = empty
