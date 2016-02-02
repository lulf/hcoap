module Network.CoAP.Client
( Request(..)
, T.Method(..)
, Response(..)
, T.ResponseCode(..)
, T.Option(..)
, T.MediaType(..)
, doRequest
) where

import Network.CoAP.Messaging
import qualified Network.CoAP.Types as T
import Control.Monad.State
import Control.Concurrent
import Network.Socket
import Data.ByteString
import Data.Word
import System.Random

data Response =
  Response { responseCode :: T.ResponseCode
           , responseOptions :: [T.Option]
           , responsePayload :: Maybe T.Payload  } deriving (Show)

data Request =
  Request { requestMethod :: T.Method
          , requestOptions :: [T.Option]
          , requestPayload :: Maybe T.Payload
          , requestReliable :: Bool } deriving (Show)
                    


generateToken :: Int -> IO [Word8]
generateToken 0 = return []
generateToken len = do
  tkn <- randomIO
  next <- generateToken (len - 1)
  return (tkn:next)

doRequest :: T.Transport -> T.Endpoint -> Request -> IO Response
doRequest transport dest (Request method options payload reliable) = do
  state <- createMessagingState transport
  msgThread <- forkIO (messagingLoop state)
  let header = T.MessageHeader { T.messageVersion = 1
                               , T.messageType = if reliable then T.CON else T.NON
                               , T.messageCode = T.CodeRequest method
                               , T.messageId = 0 }
  tokenLen <- randomRIO (0, 8)
  token <- generateToken tokenLen
  let msg = T.Message { T.messageHeader = header
                      , T.messageToken = pack token
                      , T.messageOptions = options
                      , T.messagePayload = payload }
  sendRequest msg dest state
  responseCtx <- recvResponse msg dest state
  let (T.Message (T.MessageHeader _ _ (T.CodeResponse rCode) _) _ opts pload) = T.message responseCtx
  return Response { responseCode = rCode
                  , responseOptions = opts
                  , responsePayload = pload }
