module Network.CoAP.Types where

import Data.ByteString.Lazy
import qualified Data.ByteString as BS
import Data.Word
import Data.Maybe
import Network.Socket

type Endpoint = SockAddr
data Transport =
  Transport { sendTo :: BS.ByteString -> Endpoint -> IO Int
            , recvFrom :: IO (BS.ByteString, Endpoint)
            , localEndpoint :: IO Endpoint }

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
data Method = GET | POST | PUT | DELETE deriving (Show, Eq)

data MessageContext = MessageContext { message :: Message
                                     , srcEndpoint :: Endpoint
                                     , dstEndpoint :: Endpoint } deriving (Show)

data Response =
  Response { responseCode    :: ResponseCode
           , responseOptions :: [Option]
           , responsePayload :: Maybe Payload  } deriving (Show)

data Request =
  Request { requestMethod   :: Method
          , requestOptions  :: [Option]
          , requestPayload  :: Maybe Payload
          , requestReliable :: Bool } deriving (Show)

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
                  deriving (Show, Eq)

data MessageCode = CodeRequest Method
                 | CodeResponse ResponseCode
                 | CodeEmpty
                 deriving (Show, Eq)

data MessageType = CON | NON | ACK | RST deriving (Show, Eq)

data MessageHeader = MessageHeader
  { messageVersion     :: MessageVersion 
  , messageType        :: MessageType
  , messageCode        :: MessageCode
  , messageId          :: MessageId
  } deriving (Show)

type MessageVersion = Int
type MessageId      = Word16
type Token          = BS.ByteString

data Message = Message
  { messageHeader  :: MessageHeader
  , messageToken   :: Token
  , messageOptions :: [Option]
  , messagePayload :: Maybe Payload
  } deriving (Show)


