module Network.CoAP
( module Network.CoAP.Request
, module Network.CoAP.Response
, module Network.CoAP.Options
, runServer
) where

import Network.CoAP.Request
import Network.CoAP.Response
import Network.CoAP.Message
import Network.CoAP.Messaging
import Network.CoAP.Options
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
