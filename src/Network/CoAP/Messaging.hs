
module Network.CoAP.Messaging
( recvRequest
, sendResponse
, sendRequest
, createMessageStore
, MessageStore
) where

import Network.CoAP.Message
import qualified Network.CoAP.Request as Req
import qualified Network.CoAP.Response as Res
import Control.Monad
import Control.Monad.State.Lazy
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import qualified Network.Socket.ByteString as N

-- A message store contains an inbound and outbound list of messages that needs to be ACKed
type MessageStore = ([Message], [Message])

createMessageStore :: MessageStore
createMessageStore = ([], [])

queueInboundMessage :: Message -> State MessageStore ()
queueInboundMessage message = do
  (inbound, outbound) <- get
  let newInbound = message:inbound
  put (newInbound, outbound)
  return ()

createRequest :: SockAddr -> Message -> Req.Method -> Req.Request
createRequest clientHost message method =
   Req.Request { Req.requestMethod  = method
               , Req.requestOptions = messageOptions message
               , Req.requestPayload = messagePayload message
               , Req.requestOrigin  = clientHost }

handleRequest :: SockAddr -> Message -> Req.Method -> MessageStore -> (Req.Request, MessageStore)
handleRequest hostAddr message method store = do
  let (_, newStore) = runState (queueInboundMessage message) store
  (createRequest hostAddr message method, newStore)

handleResponse :: Message -> Res.ResponseCode -> MessageStore -> IO MessageStore
handleResponse _ _ store = return (store)

handleEmpty :: Message -> MessageStore -> IO MessageStore
handleEmpty _ store = return (store)

recvRequest :: Socket -> MessageStore -> IO (Req.Request, MessageStore)
recvRequest sock store = do
  (msgData, hostAddr) <- N.recvFrom sock 65535
  let message = decode msgData
  let header  = messageHeader message
  let code    = messageCode header
  case code of
    Request method -> do
      return (handleRequest hostAddr message method store)
    Response responseCode -> do
      newStore <- handleResponse message responseCode store
      recvRequest sock newStore
    Empty -> do
      newStore <- handleEmpty message store
      recvRequest sock newStore

sendResponse :: Socket -> MessageStore -> Res.Response -> IO ()
sendResponse sock store response = return ()

sendRequest :: Socket -> MessageStore -> Req.Request -> IO Res.Response
sendRequest _ _ _ = error "Not defined"
