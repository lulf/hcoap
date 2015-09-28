
module Network.CoAP.Messaging
(
  recvMessage,
  sendMessage
) where

import Network.CoAP.Message
import Control.Monad
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

recvMessage :: Socket -> (RequestMethod -> ResponseCode) -> IO ()
recvMessage sock handler = do
  (msgData, hostAddr) <- N.recvFrom sock 65535
  let message = decode msgData
  let request = messageCode (messageHeader message)
  let response =
        case request of
          Request method -> Just (createResponse (handler method) message)
          _              -> Nothing
  forM_ response (sendResponse sock hostAddr)

sendMessage :: Message -> Socket -> IO ()
sendMessage message sock = return ()
