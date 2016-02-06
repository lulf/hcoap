module Network.CoAP.Client
( Request(..)
, Method(..)
, Response(..)
, ResponseCode(..)
, Option(..)
, MediaType(..)
, Client(..)
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
                    
data Client = Client { doRequest :: Endpoint -> Request -> IO Response
                     , msgThreadId :: Async ()}

createClient :: Transport -> IO Client
createClient transport = do
  state <- createMessagingState transport
  msgThread <- async (messagingLoop state)
  return Client { doRequest = doRequestInternal state
                , msgThreadId = msgThread }

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
