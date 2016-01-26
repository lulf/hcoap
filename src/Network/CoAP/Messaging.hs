module Network.CoAP.Messaging
( createMessagingState
, MessagingState
, sendMessage
, recvMessageWithCode
, messagingLoop
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

data MessageState = MessageState { message        :: Message
                                 , insertionTime  :: TimeStamp
                                 , srcEndpoint    :: Endpoint
                                 , dstEndpoint    :: Endpoint } deriving(Show)

cmpMessageState a b =
  ((messageId (messageHeader (message a))) == (messageId (messageHeader (message b)))) &&
  ((srcEndpoint a) == (srcEndpoint b))

instance Eq MessageState where
  (==) = cmpMessageState

-- A message store contains an inbound and outbound list of messages that needs to be ACKed
type MessageList = [MessageState]
data MessagingStore = MessagingStore { incomingMessages :: TVar MessageList
                                     , outgoingMessages :: TVar MessageList }
data MessagingState = MessagingState Socket MessagingStore

createMessagingState :: Socket -> IO MessagingState
createMessagingState sock = do
  incoming <- newTVarIO []
  outgoing <- newTVarIO []
  return (MessagingState sock (MessagingStore incoming outgoing))

queueMessage :: MessageState -> TVar MessageList -> STM ()
queueMessage message msgListVar = do
  msgList <- readTVar msgListVar
  writeTVar msgListVar (message:msgList)

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
    let msg = find (\x -> cmpMessageCode msgCode (messageCode (messageHeader (message x)))) msgList
    case msg of
      Nothing -> retry
      Just m -> do
        let newMsgList = delete m msgList
        writeTVar msgListVar newMsgList
        return m


recvLoop :: MessagingState -> IO ()
recvLoop state@(MessagingState sock store) = do
  putStrLn "Waiting for UDP packet"
  (msgData, srcEndpoint) <- N.recvFrom sock 65535
  dstEndpoint <- getSocketName sock
  let now = 1234
  let message = decode msgData
  let messageState = MessageState { message = message
                                  , insertionTime = now
                                  , srcEndpoint = srcEndpoint
                                  , dstEndpoint = dstEndpoint }
  atomically (queueMessage messageState (incomingMessages store))
  recvLoop state
  
sendLoop :: MessagingState -> IO ()
sendLoop state@(MessagingState sock store) = do
  putStrLn "Sending queued packets"
  msgState <- atomically (takeMessage (outgoingMessages store))
  let msgData = encode (message msgState)
  let origin = dstEndpoint msgState
  _ <- N.sendTo sock msgData origin
  sendLoop state

--timerLoop :: IO ()
--
messagingLoop :: MessagingState -> IO ()
messagingLoop state = do
  recvThread <- forkIO (recvLoop state)
  sendLoop state
--  timerThread <- forkIO timerLoop

sendMessage :: Message -> Endpoint -> MessagingState -> IO ()
sendMessage message dstEndpoint (MessagingState sock store) = do
  srcEndpoint <- getSocketName sock
  let now = 12345
  let messageState = MessageState { message = message
                                  , insertionTime = now
                                  , srcEndpoint = srcEndpoint
                                  , dstEndpoint = dstEndpoint }
  atomically (queueMessage messageState (outgoingMessages store))

recvMessageWithCode :: MessageCode -> MessagingState -> IO Message
recvMessageWithCode msgCode (MessagingState sock store) = do
  msgState <- atomically (takeMessageByCode msgCode (incomingMessages store))
  return (message msgState)

-- sendMessage :: Message -> MessagingState ()
-- recvMessageWithToken :: Token -> MessagingState Message


--createRequest :: (Endpoint, Endpoint) -> Message -> Method -> CoAPRequest
--createRequest (clientHost, srvHost) message method =
--   CoAPRequest { requestMethod      = method
--               , requestOptions     = messageOptions message
--               , requestPayload     = messagePayload message
--               , requestOrigin      = clientHost
--               , requestDestination = srvHost
--               , requestToken       = messageToken message }
--
--handleRequest :: (Endpoint, Endpoint) -> Message -> Method -> MessagingState CoAPRequest
--handleRequest endpoints message method = do
--  queueInboundMessage message
--  return (createRequest endpoints message method)
--
--
--handleEmpty :: Message -> MessagingState ()
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
--recvRequest :: Socket -> MessagingState CoAPRequest
--recvRequest sock = do
--  (msgData, srcEndpoint) <- liftIO (N.recvFrom sock 65535)
--  destEndpoint <- liftIO (getSocketName sock)
--  let message = decode msgData
--  let header  = messageHeader message
--  let code    = messageCode header
--  case code of
--    CodeRequest method -> do
--      request <- handleRequest (srcEndpoint, destEndpoint) message method
--      return request
--    CodeResponse _ -> error "Unexpected message response"
--    CodeEmpty -> do
--      handleEmpty message
--      recvRequest sock
--
--responseType :: MessageType -> MessageType
--responseType CON = ACK
--responseType NON = NON
--responseType _   = error "Unexpected request code type"
--
--responseHeader :: MessageHeader -> CoAPResponse -> MessageHeader
--responseHeader origHeader response =
--  MessageHeader { messageVersion = messageVersion origHeader 
--                , messageType = responseType (messageType origHeader)
--                , messageCode = CodeResponse (responseCode response)
--                , messageId = messageId origHeader }
--    
--
--sendResponse :: Socket -> CoAPResponse -> MessagingState ()
--sendResponse sock response = do
--  let req = request response
--  let origin = requestOrigin req
--  let reqId = requestToken req
--  (Just origMsg) <- takeInboundMessageByToken reqId
--  let outgoingMessage = Message { messageHeader  = responseHeader (messageHeader origMsg) response
--                                , messageToken   = messageToken origMsg
--                                , messageOptions = responseOptions response
--                                , messagePayload = responsePayload response }
--
--
--  let encoded = encode outgoingMessage
--  _ <- liftIO (N.sendTo sock encoded origin)
--  return ()
--
--allocateMessageId :: IO MessageId
--allocateMessageId = return 0
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
