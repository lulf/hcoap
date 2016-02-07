{-|
Module:  Network.CoAP.Client
Description: CoAP client library
Maintainer: ulf.lilleengen@gmail.com
License: BSD3

The CoAP client API is intended to provide the minimal building block needed for sending CoAP requests. The API exposes CoAP request and response types and handles all internal messaging details of the CoAP protocol.

Example:
@
  client <- createClient (createUDPTransport socket)
  doRequest client (SockAddrInet 5683 0) (Request GET [UriPath path] Nothing True)
@
-}
module Network.CoAP.Client
( Request(..)
, Client
, Method(..)
, Response(..)
, ResponseCode(..)
, Option(..)
, OptionString
, MediaType(..)
, doRequest
, createClient
, shutdownClient
) where

import Network.CoAP.Messaging
import Network.CoAP.Types
import Control.Monad.State
import Control.Concurrent.Async
import Control.Concurrent.STM
import Network.Socket
import Data.ByteString
import Data.Word
import System.Random
                    
-- | A client that can perform CoAP requests.
data Client = Client { -- | Send a CoAP request to a given endpoint. Returns a CoAP response.
                       doRequest :: Endpoint -> Request -> IO Response
                     , msgThreadId :: Async ()}

-- | Create a client using a given transport. This will spawn internal messaging threads making the
-- client ready to send requests.
createClient :: Transport -> IO Client
createClient transport = do
  state <- createMessagingState transport
  msgThread <- async (messagingLoop state)
  return Client { doRequest = doRequestInternal state
                , msgThreadId = msgThread }

-- | Shuts down the internal messaging threads and stops the client
shutdownClient :: Client -> IO ()
shutdownClient client = wait (msgThreadId client) -- TODO: Actually shut down the threads...

generateToken :: Int -> IO [Word8]
generateToken 0 = return []
generateToken len = do
  tkn <- randomIO
  next <- generateToken (len - 1)
  return (tkn:next)

doRequestInternal :: MessagingState -> Endpoint -> Request -> IO Response
doRequestInternal state dest (Request method options payload reliable) = do
  let header = MessageHeader { messageVersion = 1
                             , messageType = if reliable then CON else NON
                             , messageCode = CodeRequest method
                             , messageId = 0 }
  tokenLen <- randomRIO (0, 8)
  token <- generateToken tokenLen
  let msg = Message { messageHeader = header
                      , messageToken = pack token
                      , messageOptions = options
                      , messagePayload = payload }
  sendRequest msg dest state
  responseCtx <- recvResponse msg dest state
  let (Message (MessageHeader _ _ (CodeResponse rCode) _) _ opts pload) = message responseCtx
  return Response { responseCode = rCode
                  , responseOptions = opts
                  , responsePayload = pload }
