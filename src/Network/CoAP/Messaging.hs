module Network.CoAP.Messaging
( createMessagingState
, MessagingState
, recvMessage
, sendMessage
, ackMessage
, checkAcks
, checkRetransmits
, checkExpired
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
                                     , unackedMessages     :: TVar MessageList 
                                     , ackedMessages       :: TVar MessageList
                                     , unconfirmedMessages :: TVar MessageList }
data MessagingState = MessagingState Transport MessagingStore

createMessagingState :: Transport -> IO MessagingState
createMessagingState transport = do
  incoming <- newTVarIO []
  unacked <- newTVarIO []
  acked <- newTVarIO []
  unconfirmed <- newTVarIO []
  return (MessagingState transport (MessagingStore incoming unacked acked unconfirmed))

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

takeMessagesMatching :: (MessageState -> Bool) -> TVar MessageList -> STM [MessageState]
takeMessagesMatching matchFilter msgListVar = do
  msgList <- readTVar msgListVar
  if null msgList
  then return []
  else do
    let sortedMsgList = sortBy (comparing insertionTime) msgList
    let (foundMessages, remainingMessages) = partition matchFilter sortedMsgList
    writeTVar msgListVar remainingMessages
    return foundMessages

findMessagesMatching :: (MessageState -> Bool) -> TVar MessageList -> STM [MessageState]
findMessagesMatching matchFilter msgListVar = do
  msgList <- readTVar msgListVar
  if null msgList
  then return []
  else do
    let sortedMsgList = sortBy (comparing insertionTime) msgList
    let (foundMessages, _) = partition matchFilter sortedMsgList
    return foundMessages

takeMessagesByTokenAndSender :: Token -> Endpoint -> TVar MessageList -> STM [MessageState]
takeMessagesByTokenAndSender token sender = takeMessagesMatching (\x -> (sender == srcEndpoint (messageContext x)) && token == messageToken (message (messageContext x)))

takeMessagesByIdAndReceiver :: MessageId -> Endpoint -> TVar MessageList -> STM [MessageState]
takeMessagesByIdAndReceiver msgId receiver = takeMessagesMatching (\x -> (receiver == dstEndpoint (messageContext x)) && (msgId == messageId (message (messageContext x))))

findMessagesByIdAndSender msgId sender = findMessagesMatching (\x -> (sender == srcEndpoint (messageContext x)) && (msgId == messageId (message (messageContext x))))

enqueueForAck :: MessagingStore -> MessageContext -> UTCTime -> STM (Maybe MessageContext)
enqueueForAck store messageCtx@(MessageContext message srcEndpoint _) now = do
  let messageState = MessageState { messageContext = messageCtx 
                                  , insertionTime = now
                                  , replyTimeout = 0
                                  , retransmitCount = 0 }
  let msgId = messageId message
  let msgType = messageType message

  let msgFind = findMessagesByIdAndSender msgId srcEndpoint
  acked <- msgFind (ackedMessages store)
  unacked <- msgFind (unackedMessages store)
  if msgType == CON && (not (null acked) || not (null unacked))
  then do
    queueMessage messageState (unackedMessages store)
    return Nothing
  else return $ Just messageCtx


processMessage :: MessagingState -> Endpoint -> Message -> IO (Maybe MessageContext)
processMessage state@(MessagingState transport store) srcEndpoint message = do
  dstEndpoint <- localEndpoint transport
  now <- getCurrentTime

  let messageCtx = MessageContext { message = message
                                  , srcEndpoint = srcEndpoint
                                  , dstEndpoint = dstEndpoint }

  let msgId = messageId message
  let msgType = messageType message
  let msgCode = messageCode message
  if (msgType == ACK)
  then atomically $ do
    _ <- takeMessagesByIdAndReceiver msgId srcEndpoint (unconfirmedMessages store)
    return Nothing
  else if (msgCode /= CodeEmpty)
       then atomically $ enqueueForAck store messageCtx now
       else return Nothing

recvLoopError :: String -> IO ()
recvLoopError err = putStrLn ("Error parsing message: " ++ show err ++ ", skipping")

recvMessage :: MessagingState -> IO (Either String MessageContext)
recvMessage state@(MessagingState transport _) = do
  {-putStrLn "Waiting for UDP packet"-}
  (msgData, srcEndpoint) <- recvFrom transport
  let m = decode msgData
  case m of
    Left e -> return $ Left e
    Right msg -> (do
      maybeCtx <- processMessage state srcEndpoint msg
      case maybeCtx of
        Nothing -> recvMessage state
        Just ctx -> return $ Right ctx)
--    
--  return $ either recvLoopError (recvLoopSuccess state srcEndpoint) (decode msgData)
  
sendMessageInternal :: MessagingState -> MessageState -> IO ()
sendMessageInternal state@(MessagingState transport store) msgState = do
  let msgCtx  = messageContext msgState
      msg     = message msgCtx
      msgData = encode msg
      origin  = dstEndpoint msgCtx
      msgType = messageType msg

  atomically (do
    when (msgType == CON) (queueMessage msgState (unconfirmedMessages store))
    when (msgType == ACK) (queueMessage msgState (ackedMessages store)))

  _ <- sendTo transport msgData origin
  return ()

createAckMessage :: UTCTime -> MessageState -> MessageState
createAckMessage now origMessageState =
  let origCtx = messageContext origMessageState
      origMessage = message origCtx

      newMessage = Message { messageVersion = messageVersion origMessage
                           , messageType = ACK
                           , messageCode = CodeEmpty
                           , messageId = messageId origMessage
                           , messageToken = messageToken origMessage
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

maxAckWait :: Double
maxAckWait = 0.5

ackMessage :: Message -> Endpoint -> MessagingState -> IO ()
ackMessage msg dstEndpoint state@(MessagingState transport store) = do
  let reqToken = messageToken msg
  unackedMsgs <- atomically (takeMessagesByTokenAndSender reqToken dstEndpoint (unackedMessages store))  
  srcEndpoint <- localEndpoint transport
  now <- getCurrentTime
  case unackedMsgs of
    [] -> return () -- Already acked
    _  -> do
      let ack = createAckResponse msg
      let ctx = MessageContext { message = msg
                               , srcEndpoint = srcEndpoint
                               , dstEndpoint = dstEndpoint }
      let messageState = MessageState { messageContext = ctx
                                      , insertionTime = now
                                      , replyTimeout = 0
                                      , retransmitCount = 0}
      sendMessageInternal state messageState


checkAcks :: MessagingState -> IO Bool
checkAcks state@(MessagingState _ store) = do
  now <- getCurrentTime
  let nomTime = realToFrac (-ackTimeout)
  let oldestTime = addUTCTime nomTime now
  oldMessages <- atomically (takeMessagesOlderThan oldestTime (unackedMessages store))
  if null oldMessages
  then return False
  else (do
    nowInsert <- getCurrentTime
    let ackMessages = map (createAckMessage nowInsert) oldMessages
    putStrLn ("Timeout! Sending ack messages" ++ show ackMessages)
    mapM_ (sendMessageInternal state) ackMessages
    return True)

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

checkRetransmits :: MessagingState -> IO Bool
checkRetransmits state@(MessagingState _ store) = do
  now <- getCurrentTime
  toRetransmit <- atomically (takeMessagesToRetransmit now (unconfirmedMessages store))
  if null toRetransmit
  then return False
  else (do
    retransmitNow <- getCurrentTime
    putStrLn ("Attempting to retransmit messages " ++ show toRetransmit)
    let adjustedMessages = filter (\s -> retransmitCount s <= maxRetransmitCount) (map (adjustRetransmissionState retransmitNow) toRetransmit)
    mapM_ (sendMessageInternal state) adjustedMessages
    return True)

exchangeLifetime :: Double
exchangeLifetime = 247

checkExpired :: MessagingState -> IO Bool
checkExpired state@(MessagingState _ store) = do
  now <- getCurrentTime
  let nomTime = realToFrac (-exchangeLifetime)
  let oldestTime = addUTCTime nomTime now 
  oldMessages <- atomically (takeMessagesOlderThan oldestTime (ackedMessages store))
  if null oldMessages
  then return False
  else (do
    putStrLn "Timeout! removed messages from duplicate detection filter"
    return True)

sendMessage :: Message -> Endpoint -> MessagingState -> IO ()
sendMessage msg dstEndpoint state@(MessagingState transport store) = do
  -- Check if this message has not been acked yet and use the message id corresponded to the
  -- requested ack
  let reqToken = messageToken msg
  unackedMsgs <- atomically (takeMessagesByTokenAndSender reqToken dstEndpoint (unackedMessages store))  

  -- unacked <- atomically (readTVar (unackedMessages store))
  -- putStrLn ("Unacked messages at time of sending response: " ++ show unacked)
  msgId <- case unackedMsgs of
             [] -> allocateMessageId
             _  -> return (messageId msg)
    
  let outgoingMessage = case unackedMsgs of
                          [] -> setMessageId msgId msg
                          _  -> createAckResponse msg

  srcEndpoint <- localEndpoint transport
  now <- getCurrentTime
  initialTimeout <- randomRIO (ackTimeout, ackTimeout * ackRandomFactor)

  {-putStrLn ("Queueing message " ++ (show message) ++ " for sending")-}
  let ctx = MessageContext { message = msg
                           , srcEndpoint = srcEndpoint
                           , dstEndpoint = dstEndpoint }
  let messageState = MessageState { messageContext = ctx
                                  , insertionTime = now
                                  , replyTimeout = initialTimeout
                                  , retransmitCount = 0}
  sendMessageInternal state messageState

cmpMessageCode :: MessageCode -> MessageCode -> Bool
cmpMessageCode (CodeRequest _) (CodeRequest _) = True
cmpMessageCode (CodeResponse _) (CodeResponse _) = True
cmpMessageCode CodeEmpty CodeEmpty = True
cmpMessageCode _ _ = False

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
