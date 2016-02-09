{-|
Module:  Network.CoAP.Server
Description: CoAP server library
Maintainer: ulf.lilleengen@gmail.com
License: BSD3

The CoAP server API is intended to provide the minimal building block needed for creating CoAP servers. The API exposes CoAP requests and response types and handles all internal messaging details of the CoAP protocol.

Example:
@
  server <- createServer (createUDPTransport socket) (\(req, endpoint) = do
    let payload = Just (B.pack ("{\"msg\":\"Hello, Client\"}"))
    return (Response Content [ContentFormat ApplicationJson] payload))
  runServer server
@
-}
module Network.CoAP.Server
( Request(..)
, Method(..)
, Response(..)
, ResponseCode(..)
, Option(..)
, MediaType(..)
, createServer
, Server(..)
, RequestHandler
) where

import Network.CoAP.Messaging
import Network.CoAP.Types
import Control.Concurrent.Async
import Control.Concurrent.STM
import Network.Socket

-- | A request handler for a CoAP request. The request may be called by multiple threads
-- concurrently.
type RequestHandler = (Request, Endpoint) -> IO Response

-- | A CoAP server instance.
data Server = Server { runServer :: IO ()
                     , stopServer :: IO () }
                       

-- | Create a CoAP server with a given transport and request handler
createServer :: Transport -> RequestHandler -> IO Server
createServer transport handler = do
  state <- createMessagingState transport
  msgThreads <- startMessaging state
  return Server { runServer = requestLoop state handler
                , stopServer = shutdownServer state msgThreads }

-- | Shutdown a CoAP server.
shutdownServer :: MessagingState -> [Async ()] -> IO ()
shutdownServer state threads = do
  stopMessaging state threads
  mapM_ wait threads

createRequest :: MessageContext -> Request
createRequest reqCtx =
  let msg = message reqCtx
      (CodeRequest method) = messageCode (messageHeader msg)
   in Request { requestMethod = method
              , requestOptions = messageOptions msg 
              , requestPayload = messagePayload msg
              , requestReliable = messageType (messageHeader msg) == CON }

handleRequest :: MessageContext -> RequestHandler -> MessagingState -> IO ()
handleRequest requestCtx requestHandler state = do
  -- TODO: Add timeout
  let request = createRequest requestCtx
  {-putStrLn ("Received request: " ++ (show request))-}
  response <- requestHandler (request, srcEndpoint requestCtx)
  {-putStrLn ("Produced response: " ++ (show response))-}
  let responseMsg = createResponseMessage (message requestCtx) response
  sendResponse requestCtx responseMsg state

requestLoop :: MessagingState -> RequestHandler -> IO ()
requestLoop state requestHandler = do
  {-putStrLn "Waiting for incoming message"-}
  requestCtx <- recvRequest state
  _ <- async (handleRequest requestCtx requestHandler state)
  requestLoop state requestHandler

createResponseMessage :: Message -> Response -> Message
createResponseMessage origMsg response =
  let origHeader = messageHeader origMsg
      header = MessageHeader { messageVersion = messageVersion origHeader
                             , messageType = messageType origHeader
                             , messageCode = CodeResponse (responseCode response)
                             , messageId = messageId origHeader }
   in Message { messageHeader  = header
              , messageToken   = messageToken origMsg
              , messageOptions = responseOptions response
              , messagePayload = responsePayload response }

