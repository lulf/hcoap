
module Network.CoAP.Messaging
( recvRequest
, sendResponse
, sendRequest
, MessageStore
) where

import Network.CoAP.Message
import qualified Network.CoAP.Request as Req
import qualified Network.CoAP.Response as Res
import Control.Monad
import Control.Monad.State.Lazy
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import qualified Network.Socket.ByteString as N

type MessageList = [Message]
type MessageStore = State MessageList Message

queueMessage :: Message -> MessageStore
queueMessage message = do
  msgList <- get
  let newList = message:msgList
  put newList

createRequest :: SockAddr -> Message -> Req.Method -> Req.Request
createRequest clientHost message method =
   Req.Request { Req.requestMethod  = method
               , Req.requestOptions = messageOptions message
               , Req.requestPayload = messagePayload message
               , Req.requestOrigin  = clientHost }

handleRequest :: SockAddr -> Message -> Req.Method -> MessageStore -> Req.Request
handleRequest hostAddr message method store =
  createRequest hostAddr message method

handleResponse :: Message -> Res.ResponseCode -> MessageStore
handleResponse _ _ _ = error "Hey"

handleEmpty :: Message -> MessageStore
handleEmpty message store = error "hey"

recvRequest :: Socket -> MessageStore -> IO (Req.Request)
recvRequest sock store = do
  (msgData, hostAddr) <- N.recvFrom sock 65535
  let message = decode msgData
  let header  = messageHeader message
  let code    = messageCode header
  case code of
    Request method -> handleRequest hostAddr message method store
    Response responseCode -> do
      --let x = handleResponse message responseCode store
      recvRequest sock store
    Empty -> do
      -- handleEmpty message store
      recvRequest sock store

sendResponse :: Socket -> MessageStore -> Res.Response -> IO ()
sendResponse sock store response = return ()

sendRequest :: Socket -> MessageStore -> Req.Request -> IO Res.Response
sendRequest _ _ _ = error "Not defined"
