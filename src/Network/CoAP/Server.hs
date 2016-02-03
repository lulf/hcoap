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

runServer :: Transport -> (Request -> IO Response) -> IO ()
runServer transport requestHandler = do
  state <- createMessagingState transport
  msgLoop <- forkIO (messagingLoop state) 
  requestLoop state requestHandler

createRequest :: MessageContext -> CoAPRequest
createRequest reqCtx =
    CoAPRequest { requestMessage = message reqCtx
                                   , requestOrigin = srcEndpoint reqCtx
                                   , requestDestination = dstEndpoint reqCtx }

requestLoop :: MessagingState -> (Request -> IO Response) -> IO ()
requestLoop state requestHandler = do
  {-putStrLn "Waiting for incoming message"-}
  requestCtx <- recvRequest state
  let request = createRequest requestCtx
  {-putStrLn ("Received request: " ++ (show request))-}
  response <- requestHandler request
  {-putStrLn ("Produced response: " ++ (show response))-}
  let responseMsg = createResponseMessage response
  sendResponse requestCtx responseMsg state
  requestLoop state requestHandler

createResponseMessage :: CoAPResponse -> Message
createResponseMessage response =
  let origMsg = requestMessage (request response)
      origHeader = messageHeader origMsg
      header = MessageHeader { messageVersion = messageVersion origHeader
                             , messageType = messageType origHeader
                             , messageCode = CodeResponse (responseCode response)
                             , messageId = messageId origHeader }
   in Message { messageHeader  = header
              , messageToken   = messageToken origMsg
              , messageOptions = responseOptions response
              , messagePayload = responsePayload response }

