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

requestMethod :: CoAPRequest -> Method
requestMethod request =
  let (CodeRequest method) = messageCode (messageHeader (requestMessage request))
   in method

requestOptions :: CoAPRequest -> [Option]
requestOptions = messageOptions . requestMessage

requestPayload :: CoAPRequest -> Maybe Payload
requestPayload = messagePayload . requestMessage

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
  putStrLn "Waiting for incoming message"
  request <- recvRequest state
  putStrLn ("Received request: " ++ (show request))
  response <- requestHandler request
  putStrLn ("Produced response: " ++ (show response))
  sendResponse response state
  requestLoop state requestHandler
