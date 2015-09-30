
module Network.CoAP.Messaging
( recvRequest
, sendResponse
, sendRequest
, MessagingStore
) where

import Network.CoAP.Message
import Control.Monad
import Control.Monad.ST.Lazy
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import qualified Network.Socket.ByteString as N

createResponse :: ResponseCode -> Message -> Message
createResponse code req =
  let reqHeader = messageHeader req
      responseHeader = Header { messageVersion = messageVersion reqHeader
                              , messageType    = ACK
                              , messageCode    = Response code
                              , messageId      = messageId reqHeader }
      in Message { messageHeader = responseHeader
                 , messageToken  = Nothing
                 , messageOptions = []
                 , messagePayload = Nothing }

sendResponse :: Socket -> SockAddr -> Message -> IO ()
sendResponse sock hostAddr response = do
  N.sendTo sock (encode response) hostAddr
  return ()

type MessageList = [Message]
type MessageStore = State MessageList

queueMessage :: Message -> MessagingStore
queueMessage message = do
  msgList <- get
  let newList = message:msgList
  put newList

recvRequest :: Socket -> MessagingStore -> IO (Request)
recvRequest sock store = do
  (msgData, hostAddr) <- N.recvFrom sock 65535
  let message = decode msgData
  evalState (queueMessage message) store
  createRequest hostAddr message
  let msgType = messageType (messageHeader message)

sendResponse :: Socket -> MessagingStore -> Response -> IO ()
sendResponse sock store response = return ()

sendRequest :: Socket -> MessagingStore -> Request -> IO Response
