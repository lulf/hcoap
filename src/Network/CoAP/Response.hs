module Network.CoAP.Response where

import Network.CoAP.Options
import Network.CoAP.Payload
import Network.Socket

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

data Response = Response
  { responseCode    :: ResponseCode
  , responseOptions :: [(Option, OptionValue)]
  , responsePayload :: Payload
  , requestOrigin   :: SockAddr }
