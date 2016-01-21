module Network.CoAP.Server
( Request
, requestMethod
, requestOptions
, requestPayload
, requestOrigin
, Method(..)
, Response(..)
, ResponseCode(..)
, Option(..)
, MediaType(..)
, runServer
) where

import Network.CoAP.Messaging
import Network.CoAP.Types
import Control.Monad.State
import Network.Socket

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
