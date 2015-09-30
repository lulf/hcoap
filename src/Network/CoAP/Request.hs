module Network.CoAP.Request where

import Network.CoAP.Options
import Network.CoAP.Payload
import Network.Socket

data Method = GET | POST | PUT | DELETE

data Request = Request
  { requestMethod  :: Method
  , requestOptions :: [(Option, OptionValue)]
  , requestPayload :: Maybe Payload
  , requestOrigin  :: SockAddr }
