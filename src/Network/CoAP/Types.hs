module Network.CoAP.Types where

import Data.ByteString.Lazy
import qualified Data.ByteString as BS
import Data.Word
import Data.Maybe
import Network.Socket

type Endpoint = SockAddr

-- Supported media types in CoAP RFC
data MediaType = TextPlain
               | ApplicationLinkFormat
               | ApplicationXml
               | ApplicationOctetStream
               | ApplicationExi
               | ApplicationJson
               deriving (Show)

type OptionString = BS.ByteString

data Option = ContentFormat MediaType
            | ETag OptionString
            | LocationPath OptionString
            | LocationQuery OptionString
            | MaxAge Int
            | ProxyUri OptionString
            | ProxyScheme OptionString
            | UriHost OptionString
            | UriPath OptionString
            | UriPort Int
            | UriQuery OptionString
            | Accept Int
            | IfMatch OptionString
            | IfNoneMatch
            | Size1 Int
            deriving (Show)

type Payload = BS.ByteString
type RequestId = Word16
data Method = GET | POST | PUT | DELETE deriving (Show)

data CoAPRequest = CoAPRequest
    { requestId      :: RequestId
    , requestMethod  :: Method
    , requestOptions :: [Option]
    , requestPayload :: Maybe Payload
    , requestOrigin  :: SockAddr } deriving (Show)

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
                  deriving (Show)

data CoAPResponse = CoAPResponse
  { request          :: CoAPRequest
  , responseCode     :: ResponseCode
  , responseOptions  :: [Option]
  , responsePayload  :: Maybe Payload } deriving (Show)


data MessageCode = CodeRequest Method
                 | CodeResponse ResponseCode
                 | CodeEmpty
                 deriving (Show)

data MessageType = CON | NON | ACK | RST deriving (Show)

data MessageHeader = MessageHeader
  { messageVersion     :: MessageVersion 
  , messageType        :: MessageType
  , messageCode        :: MessageCode
  , messageId          :: MessageId
  } deriving (Show)

type MessageVersion = Int
type MessageId      = Word16
type MessageToken   = BS.ByteString

data Message = Message
  { messageHeader  :: MessageHeader
  , messageToken   :: Maybe MessageToken
  , messageOptions :: [Option]
  , messagePayload :: Maybe Payload
  } deriving (Show)


