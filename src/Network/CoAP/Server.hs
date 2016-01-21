module Network.CoAP.Server
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

type Request = CoAPRequest
type Response = CoAPResponse

createResponse :: Request -> ResponseCode -> [Option] -> Maybe Payload -> Response
createResponse req code options payload =
    CoAPResponse { request = req
                 , responseCode = code
                 , responseOptions = options
                 , responsePayload = payload }

runServer :: Socket -> (Request -> IO Response) -> IO ()
runServer sock requestHandler = do
  let state = createMessagingState
  runStateT (runServerOnce sock requestHandler) state
  return ()

runServerOnce :: Socket -> (Request -> IO Response) -> MessagingState ()
runServerOnce sock requestHandler = do
  request <- recvRequest sock
  response <- liftIO (requestHandler request)
  sendResponse sock response
