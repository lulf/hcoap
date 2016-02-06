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
, createServer
, shutdownServer
, Server(..)
, RequestHandler
) where

import Network.CoAP.Messaging
import Network.CoAP.Types
import Control.Concurrent.Async
import Control.Concurrent.STM
import Network.Socket

type RequestHandler = (Request -> IO Response)
type Request = CoAPRequest
type Response = CoAPResponse

data Server = Server { runServer :: IO ()
                     , msgThreadId :: Async () }
                       

createServer :: Transport -> RequestHandler -> IO Server
createServer transport handler = do
  state <- createMessagingState transport
  msgThread <- async (messagingLoop state)
  return Server { runServer = requestLoop state handler
                , msgThreadId = msgThread }

shutdownServer :: Server -> IO ()
shutdownServer server = wait (msgThreadId server)

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

createRequest :: MessageContext -> CoAPRequest
createRequest reqCtx =
    CoAPRequest { requestMessage = message reqCtx
                                   , requestOrigin = srcEndpoint reqCtx
                                   , requestDestination = dstEndpoint reqCtx }

handleRequest :: MessageContext -> RequestHandler -> MessagingState -> IO ()
handleRequest requestCtx requestHandler state = do
  -- TODO: Add timeout
  let request = createRequest requestCtx
  {-putStrLn ("Received request: " ++ (show request))-}
  response <- requestHandler request
  {-putStrLn ("Produced response: " ++ (show response))-}
  let responseMsg = createResponseMessage response
  sendResponse requestCtx responseMsg state

requestLoop :: MessagingState -> RequestHandler -> IO ()
requestLoop state requestHandler = do
  {-putStrLn "Waiting for incoming message"-}
  requestCtx <- recvRequest state
  _ <- async (handleRequest requestCtx requestHandler state)
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

