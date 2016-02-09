{-|
Module:  Network.CoAP.Types
Description: CoAP types
Maintainer: ulf.lilleengen@gmail.com
License: BSD3
-}
module Network.CoAP.Types where

import Data.ByteString.Lazy
import qualified Data.ByteString as BS
import Data.Word
import Data.Maybe
import Network.Socket

-- | A CoAP endpoint.
type Endpoint = SockAddr

-- | A CoAP transport instance capable of sending and receiving data
data Transport =
  Transport { sendTo :: BS.ByteString -> Endpoint -> IO Int
            , recvFrom :: IO (BS.ByteString, Endpoint)
            , localEndpoint :: IO Endpoint }

-- | Supported media types in CoAP RFC for ContentFormat
data MediaType = TextPlain
               | ApplicationLinkFormat
               | ApplicationXml
               | ApplicationOctetStream
               | ApplicationExi
               | ApplicationJson
               deriving (Show)

-- | CoAP Option ByteString
type OptionString = BS.ByteString

-- | CoAP Option types.
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

-- | Request/response payload ByteString
type Payload = BS.ByteString

-- | Request Method
data Method = GET | POST | PUT | DELETE deriving (Show, Eq)

-- | A message with additional context on source and destination
data MessageContext = MessageContext { message :: Message
                                     , srcEndpoint :: Endpoint
                                     , dstEndpoint :: Endpoint } deriving (Show)
-- | CoAP response.
data Response =
  Response { responseCode    :: ResponseCode
           , responseOptions :: [Option]
           , responsePayload :: Maybe Payload  } deriving (Show)

-- | CoAP request.
data Request =
  Request { requestMethod   :: Method
          , requestOptions  :: [Option]
          , requestPayload  :: Maybe Payload
          , requestReliable :: Bool } deriving (Show)

-- | CoAP response codes.
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

-- | Valid CoAP message codes.
data MessageCode = CodeRequest Method
                 | CodeResponse ResponseCode
                 | CodeEmpty
                 deriving (Show, Eq)

-- | Valid CoAP message types.
data MessageType = CON | NON | ACK | RST deriving (Show, Eq)

-- | CoAP version. Only a value of 1 is valid.
type MessageVersion = Int
-- | CoAP message id. This is unique for each message from a given endpoint.
type MessageId      = Word16
-- | CoAP request token. This is unique for a request from a given endpoint.
type Token          = BS.ByteString

-- | CoAP message record. Can be encoded and decoded to ByteString.
data Message = Message
  { messageVersion :: MessageVersion 
  , messageType    :: MessageType
  , messageCode    :: MessageCode
  , messageId      :: MessageId
  , messageToken   :: Token
  , messageOptions :: [Option]
  , messagePayload :: Maybe Payload
  } deriving (Show)


