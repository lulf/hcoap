
module Network.CoAP.Messaging
( recvRequest
, sendResponse
, sendRequest
, createMessagingState
, MessagingState
) where

import Network.CoAP.Message
import Data.List (deleteBy)
import qualified Network.CoAP.Request as Req
import qualified Network.CoAP.Response as Res
import Control.Monad
import Data.Maybe
import Control.Monad.State.Lazy
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import qualified Network.Socket.ByteString as N

-- A message store contains an inbound and outbound list of messages that needs to be ACKed
type MessageList = [(MessageId, Message)]
type MessageStore = (MessageList, MessageList)
type MessagingState a = StateT MessageStore IO a

createMessagingState :: MessageStore
createMessagingState = ([], [])

queueInboundMessage :: Message -> MessagingState ()
queueInboundMessage message = do
  (inbound, outbound) <- get
  let newInbound = (messageId (messageHeader message), message):inbound
  put (newInbound, outbound)
  return ()

takeMessage :: MessageId -> MessageList -> (Maybe Message, MessageList)
takeMessage messageId messageList = do
  let message = lookup messageId messageList
  let filteredList = filter (\(id, _) -> id == messageId) messageList
  (message, filteredList)
    

takeInboundMessage :: MessageId -> MessagingState (Maybe Message)
takeInboundMessage messageId = do
  (inbound, outbound) <- get
  let (message, newInbound) = takeMessage messageId inbound
  put (newInbound, outbound)
  return message


createRequest :: SockAddr -> Message -> Req.Method -> Req.Request
createRequest clientHost message method =
   Req.Request { Req.requestMethod  = method
               , Req.requestOptions = messageOptions message
               , Req.requestPayload = messagePayload message
               , Req.requestOrigin  = clientHost }

handleRequest :: SockAddr -> Message -> Req.Method -> MessagingState Req.Request
handleRequest hostAddr message method = do
  queueInboundMessage message
  return (createRequest hostAddr message method)

handleResponse :: Message -> Res.ResponseCode -> MessagingState ()
handleResponse _ _ = error "Unexpected message response"

handleEmpty :: Message -> MessagingState ()
handleEmpty message = do
  (inbound, outbound) <- get
  let header = messageHeader message
  let mid = messageId header
  let mtype = messageType header
  let (origMessage, newOutbound) = takeMessage mid outbound

  case mtype of
    ACK -> put (inbound, newOutbound)
    _ -> error "Unable to handle empty message type"

recvRequest :: Socket -> MessagingState Req.Request
recvRequest sock = do
  (msgData, hostAddr) <- liftIO (N.recvFrom sock 65535)
  let message = decode msgData
  let header  = messageHeader message
  let code    = messageCode header
  case code of
    Request method -> do
      request <- handleRequest hostAddr message method
      return request
    Response responseCode -> do
      handleResponse message responseCode
      recvRequest sock
    Empty -> do
      handleEmpty message
      recvRequest sock

sendResponse :: Socket -> Res.Response -> MessagingState ()
sendResponse sock response = return ()

sendRequest :: Socket -> MessageStore -> Req.Request -> IO Res.Response
sendRequest _ _ _ = error "Not defined"
