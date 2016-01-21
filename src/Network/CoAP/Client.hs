module Network.CoAP.Client
( Request(..)
, requestMethod
, requestOptions
, requestPayload
, requestOrigin
, Method(..)
, Response(..)
, createResponse
, ResponseCode(..)
, Option(..)
, MediaType(..)
, runServer
) where

import Network.CoAP.Messaging
import Network.CoAP.Types
import Control.Monad.State
import Network.Socket

type Response = CoAPResponse

data Request =
  Request { requestMethod :: Method
          , requestOptions :: [Option]
          , requestPayload :: Maybe Payload }
                    

doRequest :: Socket -> SockAddr -> Request -> IO Response
doRequest sock addr req = do
  let state = createMessagingState
  (response, _) <- runStateT (runClient sock addr req) state
  return response

runClient :: Socket -> SockAddr -> Request -> MessagingState Response
runClient sock addr (Request method options payload) = do
  sendRequest sock addr method options payload

