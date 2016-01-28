module Network.CoAP.Messaging
( createMessagingState
, MessagingState
, messagingLoop
, recvRequest
, sendResponse
) where
import Network.CoAP.Types
import Network.CoAP.MessageCodec
import Data.List (deleteBy, find, partition, filter, minimumBy, delete, sortBy)
import Control.Monad
import Control.Monad.State.Lazy
import Data.Maybe
import Data.Ord
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import qualified Network.Socket.ByteString as N
import Control.Concurrent.STM
import Control.Concurrent

-- TODO: This module should be split in two
-- * One part which only handles queueing outgoing/incoming messages and performs timeout checks to
--   ack messages
-- * One part working on request/responses that will simply ask the first part in a (non)blocking
--   fashion about messages matching a desired pattern (I.e. all messages, requests only, or
--   responses matching a token).
--
-- This parts may run in multiple threads

type TimeStamp = Int

data MessageState = MessageState { messageContext :: MessageContext
                                 , insertionTime  :: TimeStamp } deriving(Show)

cmpMessageState a b =
  ((messageId (messageHeader (message (messageContext a)))) == (messageId (messageHeader (message (messageContext b))))) &&
  ((srcEndpoint (messageContext a)) == (srcEndpoint (messageContext b)))

instance Eq MessageState where
  (==) = cmpMessageState

-- A message store contains an inbound and outbound list of messages that needs to be ACKed
type MessageList = [MessageState]
data MessagingStore = MessagingStore { incomingMessages :: TVar MessageList
                                     , outgoingMessages :: TVar MessageList
                                     , unackedMessages  :: TVar MessageList }
data MessagingState = MessagingState Socket MessagingStore

createMessagingState :: Socket -> IO MessagingState
createMessagingState sock = do
  incoming <- newTVarIO []
  outgoing <- newTVarIO []
  unacked <- newTVarIO []
  return (MessagingState sock (MessagingStore incoming outgoing unacked))

queueMessages :: [MessageState] -> TVar MessageList -> STM ()
queueMessages messages msgListVar = do
  msgList <- readTVar msgListVar
  writeTVar msgListVar (messages ++ msgList)

queueMessage :: MessageState -> TVar MessageList -> STM ()
queueMessage message = queueMessages [message]

takeMessage :: TVar MessageList -> STM MessageState
takeMessage msgListVar = do
  msgList <- readTVar msgListVar
  if null msgList
  then retry
  else do
    let message = minimumBy (comparing insertionTime) msgList
    let newMsgList = delete message msgList
    writeTVar msgListVar newMsgList
    return message

takeMessagesOlderThan :: TimeStamp -> TVar MessageList -> STM [MessageState]
takeMessagesOlderThan timeStamp msgListVar = do
  msgList <- readTVar msgListVar
  let (oldMessages, remainingMessages) = partition (\x -> timeStamp > (insertionTime x)) msgList
  writeTVar msgListVar remainingMessages
  return oldMessages

cmpMessageCode :: MessageCode -> MessageCode -> Bool
cmpMessageCode (CodeRequest _) (CodeRequest _) = True
cmpMessageCode (CodeResponse _) (CodeResponse _) = True
cmpMessageCode CodeEmpty CodeEmpty = True
cmpMessageCode _ _ = False

takeMessageByCode :: MessageCode -> TVar MessageList -> STM MessageState
takeMessageByCode msgCode msgListVar = do
  msgList <- readTVar msgListVar
  if null msgList
  then retry
  else do
    let sortedMsgList = sortBy (comparing insertionTime) msgList
    let msg = find (\x -> cmpMessageCode msgCode (messageCode (messageHeader (message (messageContext x))))) sortedMsgList
    case msg of
      Nothing -> retry
      Just m -> do
        let newMsgList = delete m msgList
        writeTVar msgListVar newMsgList
        return m

takeMessageByToken :: Token -> TVar MessageList -> STM (Maybe MessageState)
takeMessageByToken token msgListVar = do
  msgList <- readTVar msgListVar
  if null msgList
  then return Nothing
  else do
    let sortedMsgList = sortBy (comparing insertionTime) msgList
    let msg = find (\x -> token == messageToken (message (messageContext x))) sortedMsgList
    return msg
  

recvLoop :: MessagingState -> IO ()
recvLoop state@(MessagingState sock store) = do
  putStrLn "Waiting for UDP packet"
  (msgData, srcEndpoint) <- N.recvFrom sock 65535
  dstEndpoint <- getSocketName sock
  let now = 1234
  let message = decode msgData
  let messageCtx = MessageContext { message = message
                                  , srcEndpoint = srcEndpoint
                                  , dstEndpoint = dstEndpoint }
  let messageState = MessageState { messageContext = messageCtx 
                                  , insertionTime = now }
  atomically (queueMessage messageState (incomingMessages store))
  recvLoop state
  
sendLoop :: MessagingState -> IO ()
sendLoop state@(MessagingState sock store) = do
  putStrLn "Sending queued packets"
  msgState <- atomically (takeMessage (outgoingMessages store))
  let msgCtx = messageContext msgState
  let msgData = encode (message msgCtx)
  let origin = dstEndpoint msgCtx
  _ <- N.sendTo sock msgData origin
  sendLoop state


createAckMessage :: MessageState -> MessageState
createAckMessage origMessageState =
  let origCtx = messageContext origMessageState
      origMessage = message origCtx
      origHeader = messageHeader origMessage
      newHeader = MessageHeader { messageVersion = messageVersion origHeader
                                , messageType = ACK
                                , messageCode = CodeEmpty
                                , messageId = messageId origHeader }
      newMessage = Message { messageHeader = newHeader
                           , messageToken = messageToken origMessage
                           , messageOptions = []
                           , messagePayload = Nothing }
      newCtx = MessageContext { message = newMessage
                              , srcEndpoint = dstEndpoint origCtx
                              , dstEndpoint = srcEndpoint origCtx}
   in MessageState { messageContext = newCtx
                   , insertionTime = 1234 }-- FIX

    

timerLoop :: MessagingState -> IO ()
timerLoop state@(MessagingState sock store) = do
  atomically (do
    oldMessages <- takeMessagesOlderThan 12345 (unackedMessages store)
    let ackMessages = map createAckMessage oldMessages
    queueMessages ackMessages (unackedMessages store))

messagingLoop :: MessagingState -> IO ()
messagingLoop state = do
  recvThread <- forkIO (recvLoop state)
  sendThread <- forkIO (sendLoop state)
  timerLoop state

sendMessage :: Message -> Endpoint -> MessagingState -> IO ()
sendMessage message dstEndpoint (MessagingState sock store) = do
  srcEndpoint <- getSocketName sock
  let now = 12345
  let ctx = MessageContext { message = message
                           , srcEndpoint = srcEndpoint
                           , dstEndpoint = dstEndpoint }
  let messageState = MessageState { messageContext = ctx
                                  , insertionTime = now }
  atomically (queueMessage messageState (outgoingMessages store))

recvMessageWithCode :: MessageCode -> MessagingState -> IO MessageContext
recvMessageWithCode msgCode (MessagingState sock store) = do
  atomically (do
    msgState <- takeMessageByCode msgCode (incomingMessages store)
    let msgCtx = messageContext msgState
    let msgType = messageType (messageHeader (message msgCtx))
    when (msgType == CON) (queueMessage msgState (unackedMessages store))
    return msgCtx)

recvRequest :: MessagingState -> IO CoAPRequest
recvRequest state = do
  messageCtx <- recvMessageWithCode (CodeRequest GET) state
  return (CoAPRequest { requestMessage = message messageCtx
                      , requestOrigin = srcEndpoint messageCtx
                      , requestDestination = dstEndpoint messageCtx })

createResponse :: MessageId -> CoAPResponse -> Message
createResponse msgId response =
  let origMsg = requestMessage (request response)
      origHeader = messageHeader origMsg
      header = MessageHeader { messageVersion = messageVersion origHeader
                             , messageType = messageType origHeader
                             , messageCode = CodeResponse (responseCode response)
                             , messageId = msgId }
   in Message { messageHeader  = header
              , messageToken   = messageToken origMsg
              , messageOptions = responseOptions response
              , messagePayload = responsePayload response }

createAckResponse :: CoAPResponse -> Message
createAckResponse response =
  let origMsg = requestMessage (request response)
      origHeader = messageHeader origMsg
      header = MessageHeader { messageVersion = messageVersion origHeader
                             , messageType = ACK
                             , messageCode = CodeResponse (responseCode response)
                             , messageId = messageId origHeader }
   in Message { messageHeader  = header
              , messageToken   = messageToken origMsg
              , messageOptions = responseOptions response
              , messagePayload = responsePayload response }

allocateMessageId :: IO MessageId
allocateMessageId = return 0

sendResponse :: CoAPResponse -> MessagingState -> IO ()
sendResponse response state@(MessagingState _ store) = do
  let req = request response
  let origin = requestOrigin req
  let origMsg = requestMessage req
  let reqToken = messageToken origMsg
  unackedMsg <- atomically (takeMessageByToken reqToken (unackedMessages store))  

  msgId <- case unackedMsg of
             Nothing -> allocateMessageId
             _       -> return (messageId (messageHeader origMsg))
    
  let outgoingMessage = case unackedMsg of
                          Nothing -> createResponse msgId response
                          _       -> createAckResponse response

  sendMessage outgoingMessage origin state
--
--
--  let encoded = encode outgoingMessage
--  _ <- liftIO (N.sendTo sock encoded origin)
--  return ()
--      request <- handleRequest (srcEndpoint, destEndpoint) message method
--      return request
--    CodeResponse _ -> error "Unexpected message response"
--    CodeEmpty -> do
--      handleEmpty message
--      recvRequest sock
-- sendMessage :: Message -> MessagingState ()
-- recvMessageWithToken :: Token -> MessagingState Message

--handleEmpty message = do
--  (inbound, outbound) <- get
--  let header = messageHeader message
--  let mid = messageId header
--  let mtype = messageType header
--  let (origMessage, newOutbound) = takeMessageById mid outbound
--
--  case mtype of
--    ACK -> put (inbound, newOutbound)
--    _ -> error "Unable to handle empty message type"
--

--
--
--sendRequest :: Socket -> CoAPRequest -> MessagingState ()
--sendRequest sock request = do
--  msgId <- liftIO allocateMessageId
--  let header = MessageHeader { messageVersion = 1
--                             , messageType = CON -- Todo support unreliable
--                             , messageCode = CodeRequest (requestMethod request)
--                             , messageId = msgId }
--  let token = requestToken request
--  let msg = Message { messageHeader = header
--                    , messageToken = token
--                    , messageOptions = requestOptions request
--                    , messagePayload = requestPayload request }
--  let encoded = encode msg
--  _ <- liftIO (N.sendTo sock encoded (requestDestination request))
--  return ()
--
--createResponse :: CoAPRequest -> Message -> ResponseCode -> CoAPResponse
--createResponse request message responseCode =
--  CoAPResponse { request = request
--               , responseCode = responseCode
--               , responseOptions = messageOptions message
--               , responsePayload = messagePayload message }
--
--handleResponse :: CoAPRequest -> Message -> ResponseCode -> MessagingState CoAPResponse
--handleResponse request msg responseCode = do
--  _ <- takeOutboundMessageByToken (requestToken request)
--  queueInboundMessage msg -- For timeouts
--  return (createResponse request msg responseCode)
--  
--
--recvResponse :: Socket -> CoAPRequest -> MessagingState CoAPResponse
--recvResponse sock request = do
--  (msgData, srcEndpoint) <- liftIO (N.recvFrom sock 65535)
--  destEndpoint <- liftIO (getSocketName sock)
--  let message = decode msgData
--  let header  = messageHeader message
--  let code    = messageCode header
--  case code of
--    CodeRequest method -> error "Request was unexpected"
--    CodeResponse responseCode -> do
--      handleResponse request message responseCode
--    CodeEmpty -> do
--      handleEmpty message
--      recvResponse sock request
