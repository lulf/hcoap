
module Network.CoAP.Messaging
( recvRequest
, sendResponse
, sendRequest
, createMessagingState
, MessagingState
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
type MessagingState a = StateT MessageStore IO a

createMessagingState :: MessageStore
createMessagingState = ([], [])

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

recvRequest :: Socket -> MessagingState Req.Request
recvRequest sock = do
  (msgData, hostAddr) <- liftIO (N.recvFrom sock 65535)
  let store = createMessagingState
  let message = decode msgData
  let header  = messageHeader message
  let code    = messageCode header
  case code of
    Request method -> do
      let (req, store) = handleRequest hostAddr message method store
      return req
    Response responseCode -> do
      _ <- liftIO (handleResponse message responseCode store)
      recvRequest sock
    Empty -> do
      _ <- liftIO (handleEmpty message store)
      recvRequest sock

sendResponse :: Socket -> Res.Response -> MessagingState ()
sendResponse sock response = return ()

sendRequest :: Socket -> MessageStore -> Req.Request -> IO Res.Response
sendRequest _ _ _ = error "Not defined"
