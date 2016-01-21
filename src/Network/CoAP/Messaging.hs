
module Network.CoAP.Messaging
( recvRequest
, sendResponse
, sendRequest
, createMessagingState
, MessagingState
) where

import Network.CoAP.Types
import Network.CoAP.MessageCodec
import Data.List (deleteBy)
import Control.Monad
import Control.Monad.State.Lazy
import Data.Maybe
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


createRequest :: Endpoint -> Message -> Method -> Request
createRequest clientHost message method =
   Request { requestMethod  = method
           , requestOptions = messageOptions message
           , requestPayload = messagePayload message
           , requestOrigin  = clientHost
           , requestId      = messageId (messageHeader message) }

handleRequest :: Endpoint -> Message -> Method -> MessagingState Request
handleRequest endpoint message method = do
  queueInboundMessage message
  return (createRequest endpoint message method)

handleResponse :: Message -> ResponseCode -> MessagingState ()
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

recvRequest :: Socket -> MessagingState Request
recvRequest sock = do
  (msgData, endpoint) <- liftIO (N.recvFrom sock 65535)
  let message = decode msgData
  let header  = messageHeader message
  let code    = messageCode header
  case code of
    CodeRequest method -> do
      request <- handleRequest endpoint message method
      return request
    CodeResponse responseCode -> do
      handleResponse message responseCode
      recvRequest sock
    CodeEmpty -> do
      handleEmpty message
      recvRequest sock

responseType :: MessageType -> MessageType
responseType CON = ACK
responseType NON = NON
responseType _   = error "Unexpected request code type"

responseHeader :: MessageHeader -> Response -> MessageHeader
responseHeader origHeader response =
  MessageHeader { messageVersion = messageVersion origHeader 
                , messageType = responseType (messageType origHeader)
                , messageCode = CodeResponse (responseCode response)
                , messageId = messageId origHeader }
    

sendResponse :: Socket -> Response -> MessagingState ()
sendResponse sock response = do
  let req = request response
  let origin = requestOrigin req
  let msgId = requestId req
  (Just origMsg) <- takeInboundMessage msgId
  let outgoingMessage = Message { messageHeader  = responseHeader (messageHeader origMsg) response
                                , messageToken   = messageToken origMsg
                                , messageOptions = responseOptions response
                                , messagePayload = responsePayload response }


  let encoded = encode outgoingMessage
  _ <- liftIO (N.sendTo sock encoded origin)
  return ()

sendRequest :: Socket -> MessageStore -> Request -> IO Response
sendRequest sock _ _ = error "Not defined"
