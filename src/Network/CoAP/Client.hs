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
, Client(..)
, Method(..)
, Response(..)
, ResponseCode(..)
, Option(..)
, OptionString
, MediaType(..)
, createClient
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
import Network.URI
                    
-- | A client that can perform CoAP requests.
data Client = Client { -- | Send a CoAP request to a given endpoint. Returns a CoAP response.
                       doRequest :: URI -> Request -> IO Response
                       -- | Stop a client. Ensures that no threads are running and that messaging
                       -- layer is shut down.
                     , shutdownClient :: IO () }

-- | Create a client using a given transport. This will spawn internal messaging threads making the
-- client ready to send requests.
createClient :: Transport -> IO Client
createClient transport = do
  state <- createMessagingState transport
  msgThreads <- startMessaging state
  return Client { doRequest = doRequestInternal state
                , shutdownClient = stopClient state msgThreads }

-- | Shuts down the internal messaging threads and stops the client
stopClient :: MessagingState -> [Async ()] -> IO ()
stopClient state threads = do
  stopMessaging state threads
  mapM_ wait threads

generateToken :: Int -> IO [Word8]
generateToken 0 = return []
generateToken len = do
  tkn <- randomIO
  next <- generateToken (len - 1)
  return (tkn:next)

addURIOptions  :: URI -> [Option] -> [Option]
addURIOptions _ o = o

createEndpoint :: URI -> Endpoint
createEndpoint uri =
  let (Just auth) = uriAuthority uri
      hname = uriRegName auth
      port = fromIntegral (read (uriPort auth) :: Int)
      addr = read (hname
   in if isIPv6address hname
      then SockAddrInet6 port 0 addr 0
      else SockAddrInet port ((read hname) :: Net4Addr)

doRequestInternal :: MessagingState -> URI -> Request -> IO Response
doRequestInternal state uri (Request method options payload reliable) = do
  let dest = createEndpoint uri
  tokenLen <- randomRIO (0, 8)
  token <- generateToken tokenLen
  let msg = Message { messageVersion = 1
                    , messageType = if reliable then CON else NON
                    , messageCode = CodeRequest method
                    , messageId = 0
                    , messageToken = pack token
                    , messageOptions = addURIOptions uri options
                    , messagePayload = payload }
  sendRequest msg dest state
  responseCtx <- recvResponse msg dest state
  let (Message _ _ (CodeResponse rCode) _ _ opts pload) = message responseCtx
  return Response { responseCode = rCode
                  , responseOptions = opts
                  , responsePayload = pload }
