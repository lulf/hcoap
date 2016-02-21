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

takeMessagesByTokenAndOrigin :: Token -> Endpoint -> TVar MessageList -> STM [MessageState]
takeMessagesByTokenAndOrigin token origin = takeMessagesMatching (\x -> (origin == dstEndpoint (messageContext x)) && token == messageToken (message (messageContext x)))

takeMessagesByIdAndOrigin :: MessageId -> Endpoint -> TVar MessageList -> STM [MessageState]
takeMessagesByIdAndOrigin msgId origin = takeMessagesMatching (\x -> (origin == dstEndpoint (messageContext x)) && (msgId == messageId (message (messageContext x))))

findMessagesByIdAndOrigin msgId origin = findMessagesMatching (\x -> (origin == dstEndpoint (messageContext x)) && (msgId == messageId (message (messageContext x))))

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
  atomically (when (msgType == ACK) (do _ <- takeMessagesByIdAndOrigin msgId srcEndpoint (unconfirmedMessages store)
                                        return ()))
  atomically (when (msgCode /= CodeEmpty) (do
    let msgFind = findMessagesByIdAndOrigin msgId srcEndpoint
    acked <- msgFind (ackedMessages store)
    incoming <- msgFind (incomingMessages store)
    unacked <- msgFind (unackedMessages store)
    if msgType == CON && (not (null acked) || not (null incoming) || not (null unacked))
    then queueMessage messageState (unackedMessages store)
    else queueMessage messageState (incomingMessages store)))

recvLoopError :: String -> IO ()
recvLoopError err = putStrLn ("Error parsing message: " ++ show err ++ ", skipping")

recvLoop :: MessagingState -> IO ()
recvLoop state@(MessagingState transport _) = do
  {-putStrLn "Waiting for UDP packet"-}
  (msgData, srcEndpoint) <- recvFrom transport
  either recvLoopError (recvLoopSuccess state srcEndpoint) (decode msgData)
  recvLoop state
  
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
  then threadDelay 100000
  else (do
    putStrLn "Timeout! Queueing ack messages"
    now <- getCurrentTime
    let ackMessages = map (createAckMessage now) oldMessages
    mapM_ (sendMessageInternal state) ackMessages)
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
    mapM_ (sendMessageInternal state) adjustedMessages)
  retransmitLoop state

exchangeLifetime :: Double
exchangeLifetime = 247

ackedExpirerLoop :: MessagingState -> IO ()
ackedExpirerLoop state@(MessagingState _ store) = do
  takeStamp <- getCurrentTime
  let nomTime = realToFrac (-exchangeLifetime)
  let oldestTime = addUTCTime nomTime takeStamp
  oldMessages <- atomically (takeMessagesOlderThan oldestTime (ackedMessages store))
  if null oldMessages
  then threadDelay 100000
  else putStrLn "Timeout! removed messages from duplicate detection filter"
  ackedExpirerLoop state

runLoop :: MessagingState -> (MessagingState -> IO ()) -> IO ()
runLoop state fn = do
  err <- try (fn state) :: IO (Either AsyncException ())
  return ()

startMessaging :: MessagingState -> IO [Async ()]
startMessaging state = mapM (async . runLoop state) [recvLoop, ackLoop, retransmitLoop, ackedExpirerLoop]

stopMessaging :: MessagingState -> [Async ()] -> IO ()
stopMessaging state = mapM_ cancel 

sendMessage :: Message -> Endpoint -> MessagingState -> IO ()
sendMessage message dstEndpoint state@(MessagingState transport _) = do
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
  sendMessageInternal state messageState

recvMessageMatching :: (MessageState -> Bool) -> MessagingState -> IO MessageContext
recvMessageMatching matchFilter (MessagingState _ store) =
  atomically (do
    msgState <- takeMessageRetryMatching matchFilter (incomingMessages store)
    let msgCtx = messageContext msgState
    let msgId = messageId (message msgCtx)
    let msgType = messageType (message msgCtx)
    when (msgType == CON) (do
      -- If a message with the same id and origin is already queued for ACK, consider this message a
      -- duplicate. Enqueue it as unacked, but don't process it.
      unacked <- takeMessagesByIdAndOrigin msgId (dstEndpoint msgCtx) (unackedMessages store)
      mapM_ (\x -> queueMessage x (unackedMessages store)) (msgState:unacked)
      unless (null unacked) retry)
    return msgCtx)


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
  unackedMsgs <- atomically (takeMessagesByTokenAndOrigin reqToken origin (unackedMessages store))  

  msgId <- case unackedMsgs of
             [] -> allocateMessageId
             _  -> return (messageId (message requestCtx))
    
  let outgoingMessage = case unackedMsgs of
                          [] -> setMessageId msgId response
                          _  -> createAckResponse response

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

