module Network.CoAP.Message
( Message
, MessageType
, parseMessage
, encodeMessage
) where

import Data.ByteString
import Data.Word

data MessageType = CON | NCON | ACK | RESET

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

data MessageCode = Request RequestMethod
                 | Response ResponseCode
                 | Empty



data Message = Message
  { messageVersion     :: Int
  , messageType        :: MessageType
  , messageTokenLength :: Int
  , messageCode        :: MessageCode
  , messageId          :: Word16
  }

parseMessage :: ByteString -> Maybe Message
parseMessage _ = Nothing

encodeMessage :: Message -> ByteString
encodeMessage _ = empty
