module Network.CoAP.Response where

import Network.CoAP.Options
import Network.CoAP.Payload
import Network.CoAP.Request
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
                  deriving (Show)

data Response = Response
  { request          :: Request
  , responseCode     :: ResponseCode
  , responseOptions  :: [(Option, OptionValue)]
  , responsePayload  :: Maybe Payload } deriving (Show)
