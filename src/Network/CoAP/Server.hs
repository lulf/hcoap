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
import Control.Concurrent
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
  state <- createMessagingState sock
  msgLoop <- forkIO (messagingLoop state) 
  requestLoop state requestHandler

requestLoop :: MessagingState -> (Request -> IO Response) -> IO ()
requestLoop state requestHandler = do
  --message <- recvMessageWithType CodeRequest state
  putStrLn "Waiting for incoming message"
  message <- recvMessageWithCode (CodeRequest GET) state
  putStrLn ("Received message: " ++ (show message))
--  response <- requestHandler request
--  sendResponse response state
  requestLoop state requestHandler
