module Network.CoAP.Request where

import Network.CoAP.Options
import Network.CoAP.Payload
import Network.Socket
import Data.Word

type RequestId = Word16
data Method = GET | POST | PUT | DELETE

data Request = Request
  { requestId      :: RequestId
  , requestMethod  :: Method
  , requestOptions :: [(Option, OptionValue)]
  , requestPayload :: Maybe Payload
  , requestOrigin  :: SockAddr }
