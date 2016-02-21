module Network.CoAP.Messaging
( createMessagingState
, MessagingState
, startMessaging
, stopMessaging
, recvRequest
, sendResponse
, sendRequest
, recvResponse
) where
import Network.CoAP.Types
import Network.CoAP.MessageCodec
import Data.List (deleteBy, find, partition, filter, minimumBy, delete, sortBy)
import Control.Monad
import Control.Monad.State.Lazy
import Data.Ord
import Data.ByteString (empty)
import Control.Concurrent.STM
import Control.Concurrent
import Control.Exception
import Control.Concurrent.Async
import Data.Maybe
import Data.Time
import System.Random

type TimeStamp = UTCTime

data MessageState = MessageState { messageContext  :: MessageContext
                                 , insertionTime   :: TimeStamp
                                 , replyTimeout    :: Double 
                                 , retransmitCount :: Integer } deriving (Show)


instance Eq MessageState where
  (==) a b = messageId (message (messageContext a)) == messageId (message (messageContext b)) &&
             srcEndpoint (messageContext a) == srcEndpoint (messageContext b)

-- A message store contains an inbound and outbound list of messages that needs to be ACKed
type MessageList = [MessageState]
data MessagingStore = MessagingStore { incomingMessages    :: TVar MessageList
                                     , outgoingMessages    :: TVar MessageList
                                     , unackedMessages     :: TVar MessageList 
                                     , unconfirmedMessages :: TVar MessageList }
data MessagingState = MessagingState Transport MessagingStore

createMessagingState :: Transport -> IO MessagingState
createMessagingState transport = do
  incoming <- newTVarIO []
  outgoing <- newTVarIO []
  unacked <- newTVarIO []
  unconfirmed <- newTVarIO []
  return (MessagingState transport (MessagingStore incoming outgoing unacked unconfirmed))

queueMessages :: [MessageState] -> TVar MessageList -> STM ()
queueMessages messages msgListVar = do
  msgList <- readTVar msgListVar
  writeTVar msgListVar (messages ++ msgList)

queueMessage :: MessageState -> TVar MessageList -> STM ()
queueMessage message = queueMessages [message]

takeMessageRetry :: TVar MessageList -> STM MessageState
takeMessageRetry msgListVar = do
  msgList <- readTVar msgListVar
  if null msgList
  then retry
  else do
    let message = minimumBy (comparing insertionTime) msgList
    let newMsgList = delete message msgList
    writeTVar msgListVar newMsgList
    return message

takeMessagesRetryMatching :: (MessageState -> Bool) -> TVar MessageList -> STM [MessageState]
takeMessagesRetryMatching msgFilter msgListVar = do
  msgList <- readTVar msgListVar
  if null msgList
  then retry
  else do
    let (matchedMessages, remainingMessages) = partition msgFilter msgList
    writeTVar msgListVar remainingMessages
    return matchedMessages

takeMessagesOlderThan :: TimeStamp -> TVar MessageList -> STM [MessageState]
takeMessagesOlderThan timeStamp = takeMessagesRetryMatching (\x -> timeStamp > insertionTime x)

checkRetransmit :: TimeStamp -> MessageState -> Bool
checkRetransmit now msgState =
  let timeout = replyTimeout msgState
      startTime = insertionTime msgState
      endTime = addUTCTime (realToFrac timeout) startTime
   in now > endTime

takeMessagesToRetransmit :: TimeStamp -> TVar MessageList -> STM [MessageState]
takeMessagesToRetransmit now = takeMessagesRetryMatching (checkRetransmit now)

takeMessageRetryMatching :: (MessageState -> Bool) -> TVar MessageList -> STM MessageState
takeMessageRetryMatching matchFilter msgListVar = do
  msgList <- readTVar msgListVar
  if null msgList
  then retry
  else do
    let sortedMsgList = sortBy (comparing insertionTime) msgList
    let msg = find matchFilter sortedMsgList
    case msg of
      Nothing -> retry
      Just m -> do
        let newMsgList = delete m msgList
        writeTVar msgListVar newMsgList
        return m

takeMessageMatching :: (MessageState -> Bool) -> TVar MessageList -> STM (Maybe MessageState)
takeMessageMatching matchFilter msgListVar = do
  msgList <- readTVar msgListVar
  if null msgList
  then return Nothing
  else do
    let sortedMsgList = sortBy (comparing insertionTime) msgList
    let (foundMessages, remainingMessages) = partition matchFilter sortedMsgList
    writeTVar msgListVar remainingMessages
    return (listToMaybe foundMessages)

takeMessageByTokenAndOrigin :: Token -> Endpoint -> TVar MessageList -> STM (Maybe MessageState)
takeMessageByTokenAndOrigin token origin = takeMessageMatching (\x -> (origin == dstEndpoint (messageContext x)) && token == messageToken (message (messageContext x)))

takeMessageByIdAndOrigin :: MessageId -> Endpoint -> TVar MessageList -> STM (Maybe MessageState)
takeMessageByIdAndOrigin msgId origin = takeMessageMatching (\x -> (origin == dstEndpoint (messageContext x)) && (msgId == messageId (message (messageContext x))))

recvLoopSuccess :: MessagingState -> Endpoint -> Message -> IO ()
recvLoopSuccess state@(MessagingState transport store) srcEndpoint message = do
  dstEndpoint <- localEndpoint transport
  now <- getCurrentTime

  let messageCtx = MessageContext { message = message
                                  , srcEndpoint = srcEndpoint
                                  , dstEndpoint = dstEndpoint }
  let messageState = MessageState { messageContext = messageCtx 
                                  , insertionTime = now
                                  , replyTimeout = 0
                                  , retransmitCount = 0 }
  let msgId = messageId message
  let msgType = messageType message
  let msgCode = messageCode message
  atomically (when (msgType == ACK) (do _ <- takeMessageByIdAndOrigin msgId srcEndpoint (unconfirmedMessages store)
                                        return ()))
  atomically (when (msgCode /= CodeEmpty) (queueMessage messageState (incomingMessages store)))

recvLoopError :: String -> IO ()
recvLoopError err = putStrLn ("Error parsing message: " ++ show err ++ ", skipping")

recvLoop :: MessagingState -> IO ()
recvLoop state@(MessagingState transport _) = do
  {-putStrLn "Waiting for UDP packet"-}
  (msgData, srcEndpoint) <- recvFrom transport
  either recvLoopError (recvLoopSuccess state srcEndpoint) (decode msgData)
  recvLoop state
  
sendLoop :: MessagingState -> IO ()
sendLoop state@(MessagingState transport store) = do
  msgState <- atomically (do
    msgState <- takeMessageRetry (outgoingMessages store)
    let msgType = messageType (message (messageContext msgState))
    when (msgType == CON) (queueMessage msgState (unconfirmedMessages store))
    return msgState)
  let msgCtx = messageContext msgState
  let msgData = encode (message msgCtx)
  let origin = dstEndpoint msgCtx
  _ <- sendTo transport msgData origin
  sendLoop state


createAckMessage :: UTCTime -> MessageState -> MessageState
createAckMessage now origMessageState =
  let origCtx = messageContext origMessageState
      origMessage = message origCtx

      newMessage = Message { messageVersion = messageVersion origMessage
                           , messageType = ACK
                           , messageCode = CodeEmpty
                           , messageId = messageId origMessage
                           , messageToken = empty
                           , messageOptions = []
                           , messagePayload = Nothing }
      newCtx = MessageContext { message = newMessage
                              , srcEndpoint = dstEndpoint origCtx
                              , dstEndpoint = srcEndpoint origCtx}
   in MessageState { messageContext = newCtx
                   , replyTimeout = 0
                   , retransmitCount = 0
                   , insertionTime = now }

    

ackTimeout :: Double
ackTimeout = 2

ackLoop :: MessagingState -> IO ()
ackLoop state@(MessagingState _ store) = do
  takeStamp <- getCurrentTime
  let nomTime = realToFrac (-ackTimeout)
  let oldestTime = addUTCTime nomTime takeStamp
  oldMessages <- atomically (takeMessagesOlderThan oldestTime (unackedMessages store))
  if null oldMessages
  then do
    threadDelay 100000
    ackLoop state
  else do
    putStrLn "Timeout! Queueing ack messages"
    now <- getCurrentTime
    let ackMessages = map (createAckMessage now) oldMessages
    atomically (queueMessages ackMessages (outgoingMessages store))
    ackLoop state

ackRandomFactor :: Double
ackRandomFactor = 1.5

maxRetransmitCount :: Integer
maxRetransmitCount = 4

adjustRetransmissionState :: TimeStamp -> MessageState -> MessageState
adjustRetransmissionState now msgState =
  MessageState { messageContext = messageContext msgState
               , insertionTime = now
               , replyTimeout = replyTimeout msgState * 2
               , retransmitCount = retransmitCount msgState + 1 }

retransmitLoop :: MessagingState -> IO ()
retransmitLoop state@(MessagingState _ store) = do
  now <- getCurrentTime
  toRetransmit <- atomically (takeMessagesToRetransmit now (unconfirmedMessages store))
  if null toRetransmit
  then threadDelay 100000
  else (do
    putStrLn ("Attempting to retransmit messages " ++ show toRetransmit)
    let adjustedMessages = filter (\s -> retransmitCount s <= maxRetransmitCount) (map (adjustRetransmissionState now) toRetransmit)
    atomically (queueMessages adjustedMessages (outgoingMessages store)))
  retransmitLoop state
  

runLoop :: MessagingState -> (MessagingState -> IO ()) -> IO ()
runLoop state fn = do
  err <- try (fn state) :: IO (Either AsyncException ())
  return ()

startMessaging :: MessagingState -> IO [Async ()]
startMessaging state = mapM (async . runLoop state) [recvLoop, sendLoop, ackLoop, retransmitLoop]

stopMessaging :: MessagingState -> [Async ()] -> IO ()
stopMessaging state = mapM_ cancel 

sendMessage :: Message -> Endpoint -> MessagingState -> IO ()
sendMessage message dstEndpoint (MessagingState transport store) = do
  srcEndpoint <- localEndpoint transport
  now <- getCurrentTime
  initialTimeout <- randomRIO (ackTimeout, ackTimeout * ackRandomFactor)
  {-putStrLn ("Queueing message " ++ (show message) ++ " for sending")-}
  let ctx = MessageContext { message = message
                           , srcEndpoint = srcEndpoint
                           , dstEndpoint = dstEndpoint }
  let messageState = MessageState { messageContext = ctx
                                  , insertionTime = now
                                  , replyTimeout = initialTimeout
                                  , retransmitCount = 0}
  atomically (queueMessage messageState (outgoingMessages store))

recvMessageMatching :: (MessageState -> Bool) -> MessagingState -> IO MessageContext
recvMessageMatching matchFilter (MessagingState _ store) = do
  msgState <- atomically (takeMessageRetryMatching matchFilter (incomingMessages store))
  let msgCtx = messageContext msgState
  let msgType = messageType (message msgCtx)
  when (msgType == CON) (atomically (queueMessage msgState (unackedMessages store)))
  return msgCtx


cmpMessageCode :: MessageCode -> MessageCode -> Bool
cmpMessageCode (CodeRequest _) (CodeRequest _) = True
cmpMessageCode (CodeResponse _) (CodeResponse _) = True
cmpMessageCode CodeEmpty CodeEmpty = True
cmpMessageCode _ _ = False

recvRequest :: MessagingState -> IO MessageContext
recvRequest = recvMessageMatching (cmpMessageCode (CodeRequest GET) . messageCode . message . messageContext)

createAckResponse :: Message -> Message
createAckResponse response =
   Message { messageVersion = messageVersion response
           , messageType = ACK
           , messageCode = messageCode response
           , messageId = messageId response
           , messageToken   = messageToken response
           , messageOptions = messageOptions response
           , messagePayload = messagePayload response }

setMessageId :: MessageId -> Message -> Message
setMessageId msgId response =
   Message { messageVersion = messageVersion response
           , messageType = messageType response
           , messageCode = messageCode response
           , messageId = msgId
           , messageToken   = messageToken response
           , messageOptions = messageOptions response
           , messagePayload = messagePayload response }

allocateMessageId :: IO MessageId
allocateMessageId = randomIO
  

sendResponse :: MessageContext -> Message -> MessagingState -> IO ()
sendResponse requestCtx response state@(MessagingState _ store) = do
  let origin = srcEndpoint requestCtx
  let reqToken = messageToken (message requestCtx)
  unackedMsg <- atomically (takeMessageByTokenAndOrigin reqToken origin (unackedMessages store))  

  msgId <- case unackedMsg of
             Nothing -> allocateMessageId
             _       -> return (messageId (message requestCtx))
    
  let outgoingMessage = case unackedMsg of
                          Nothing -> setMessageId msgId response
                          _       -> createAckResponse response

  sendMessage outgoingMessage origin state

sendRequest :: Message -> Endpoint -> MessagingState -> IO ()
sendRequest (Message msgVersion msgType msgCode _ tkn opts payload) msgdest state = do
  msgId <- allocateMessageId
  let msg = Message { messageVersion = msgVersion
                    , messageType = msgType
                    , messageCode = msgCode
                    , messageId = msgId
                    , messageToken = tkn
                    , messageOptions = opts
                    , messagePayload = payload }
  sendMessage msg msgdest state

recvResponse :: Message -> Endpoint -> MessagingState -> IO MessageContext
recvResponse reqMessage endpoint = recvMessageMatching matchFilter
  where matchFilter x = cmpMessageCode (CodeResponse Created) (messageCode (message (messageContext x))) && messageToken reqMessage == messageToken (message (messageContext x))

