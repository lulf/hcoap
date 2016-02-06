{-|
Module:  Network.CoAP.Server
Description: CoAP server library
Maintainer: ulf.lilleengen@gmail.com
License: BSD3

The CoAP server API is intended to provide the minimal building block needed for creating CoAP servers. The API exposes CoAP requests and response types and handles all internal messaging details of the CoAP protocol.
-}
module Network.CoAP.Server
( Request(..)
, Method(..)
, Response(..)
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

type RequestHandler = (Request, Endpoint) -> IO Response

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

