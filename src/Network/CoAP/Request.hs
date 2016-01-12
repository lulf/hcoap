module Network.CoAP.Request where

import Network.CoAP.Options
import Network.CoAP.Payload
import Network.Socket
import Data.Word

type RequestId = Word16
data Method = GET | POST | PUT | DELETE deriving (Show)

data Request = Request
  { requestId      :: RequestId
  , requestMethod  :: Method
  , requestOptions :: [Option]
  , requestPayload :: Maybe Payload
  , requestOrigin  :: SockAddr } deriving (Show)
