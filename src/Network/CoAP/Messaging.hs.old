
module Network.CoAP.Messaging
( recvRequest
, recvResponse
, sendResponse
, sendRequest
, createMessagingState
, MessagingState
) where

-- TODO: This module should be split in two
-- * One part which only handles queueing outgoing/incoming messages and performs timeout checks to
--   ack messages
-- * One part working on request/responses that will simply ask the first part in a (non)blocking
--   fashion about messages matching a desired pattern (I.e. all messages, requests only, or
--   responses matching a token).
--
-- This parts may run in multiple threads

import Network.CoAP.Types
import Network.CoAP.MessageCodec
import Data.List (deleteBy, find, partition, filter)
import Control.Monad
import Control.Monad.State.Lazy
import Data.Maybe
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import qualified Network.Socket.ByteString as N

-- A message store contains an inbound and outbound list of messages that needs to be ACKed
type MessageList = [Message]
type MessageStore = (MessageList, MessageList)
type MessagingState a = StateT MessageStore IO a

createMessagingState :: MessageStore
createMessagingState = ([], [])

queueInboundMessage :: Message -> MessagingState ()
queueInboundMessage message = do
  (inbound, outbound) <- get
  let newInbound = message:inbound
  put (newInbound, outbound)
  return ()

takeMessageById :: MessageId -> MessageList -> (Maybe Message, MessageList)
takeMessageById msgId messageList = do
  let message = find (\x -> msgId == messageId (messageHeader x)) messageList
  let filteredList = filter (\x -> msgId /= messageId (messageHeader x)) messageList
  (message, filteredList)

takeMessageByToken :: Token -> MessageList -> ([Message], MessageList)
takeMessageByToken token = partition (\x -> token == messageToken x)

takeOutboundMessageByToken :: Token -> MessagingState (Maybe Message)
takeOutboundMessageByToken token = do
  (inbound, outbound) <- get
  let (messages, newOutbound) = takeMessageByToken token outbound
  -- In case we have multiple message, return only the first
  put (inbound, newOutbound)
  return (if null messages
          then Nothing
          else Just (head messages))

takeInboundMessageByToken :: Token -> MessagingState (Maybe Message)
takeInboundMessageByToken token = do
  (inbound, outbound) <- get
  let (messages, newInbound) = takeMessageByToken token inbound
  -- In case we have multiple message, return only the first
  put (newInbound, outbound)
  return (if null messages
          then Nothing
          else Just (head messages))


createRequest :: (Endpoint, Endpoint) -> Message -> Method -> CoAPRequest
createRequest (clientHost, srvHost) message method =
   CoAPRequest { requestMethod      = method
               , requestOptions     = messageOptions message
               , requestPayload     = messagePayload message
               , requestOrigin      = clientHost
               , requestDestination = srvHost
               , requestToken       = messageToken message }

handleRequest :: (Endpoint, Endpoint) -> Message -> Method -> MessagingState CoAPRequest
handleRequest endpoints message method = do
  queueInboundMessage message
  return (createRequest endpoints message method)


handleEmpty :: Message -> MessagingState ()
handleEmpty message = do
  (inbound, outbound) <- get
  let header = messageHeader message
  let mid = messageId header
  let mtype = messageType header
  let (origMessage, newOutbound) = takeMessageById mid outbound

  case mtype of
    ACK -> put (inbound, newOutbound)
    _ -> error "Unable to handle empty message type"

recvRequest :: Socket -> MessagingState CoAPRequest
recvRequest sock = do
  (msgData, srcEndpoint) <- liftIO (N.recvFrom sock 65535)
  destEndpoint <- liftIO (getSocketName sock)
  let message = decode msgData
  let header  = messageHeader message
  let code    = messageCode header
  case code of
    CodeRequest method -> do
      request <- handleRequest (srcEndpoint, destEndpoint) message method
      return request
    CodeResponse _ -> error "Unexpected message response"
    CodeEmpty -> do
      handleEmpty message
      recvRequest sock

responseType :: MessageType -> MessageType
responseType CON = ACK
responseType NON = NON
responseType _   = error "Unexpected request code type"

responseHeader :: MessageHeader -> CoAPResponse -> MessageHeader
responseHeader origHeader response =
  MessageHeader { messageVersion = messageVersion origHeader 
                , messageType = responseType (messageType origHeader)
                , messageCode = CodeResponse (responseCode response)
                , messageId = messageId origHeader }
    

sendResponse :: Socket -> CoAPResponse -> MessagingState ()
sendResponse sock response = do
  let req = request response
  let origin = requestOrigin req
  let reqId = requestToken req
  (Just origMsg) <- takeInboundMessageByToken reqId
  let outgoingMessage = Message { messageHeader  = responseHeader (messageHeader origMsg) response
                                , messageToken   = messageToken origMsg
                                , messageOptions = responseOptions response
                                , messagePayload = responsePayload response }


  let encoded = encode outgoingMessage
  _ <- liftIO (N.sendTo sock encoded origin)
  return ()

allocateMessageId :: IO MessageId
allocateMessageId = return 0

sendRequest :: Socket -> CoAPRequest -> MessagingState ()
sendRequest sock request = do
  msgId <- liftIO allocateMessageId
  let header = MessageHeader { messageVersion = 1
                             , messageType = CON -- Todo support unreliable
                             , messageCode = CodeRequest (requestMethod request)
                             , messageId = msgId }
  let token = requestToken request
  let msg = Message { messageHeader = header
                    , messageToken = token
                    , messageOptions = requestOptions request
                    , messagePayload = requestPayload request }
  let encoded = encode msg
  _ <- liftIO (N.sendTo sock encoded (requestDestination request))
  return ()

createResponse :: CoAPRequest -> Message -> ResponseCode -> CoAPResponse
createResponse request message responseCode =
  CoAPResponse { request = request
               , responseCode = responseCode
               , responseOptions = messageOptions message
               , responsePayload = messagePayload message }

handleResponse :: CoAPRequest -> Message -> ResponseCode -> MessagingState CoAPResponse
handleResponse request msg responseCode = do
  _ <- takeOutboundMessageByToken (requestToken request)
  queueInboundMessage msg -- For timeouts
  return (createResponse request msg responseCode)
  

recvResponse :: Socket -> CoAPRequest -> MessagingState CoAPResponse
recvResponse sock request = do
  (msgData, srcEndpoint) <- liftIO (N.recvFrom sock 65535)
  destEndpoint <- liftIO (getSocketName sock)
  let message = decode msgData
  let header  = messageHeader message
  let code    = messageCode header
  case code of
    CodeRequest method -> error "Request was unexpected"
    CodeResponse responseCode -> do
      handleResponse request message responseCode
    CodeEmpty -> do
      handleEmpty message
      recvResponse sock request
