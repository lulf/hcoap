
module Network.CoAP.Messaging
(
  recvMessage,
  sendMessage
) where

import Network.CoAP.Message
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


recvMessage :: Socket -> (RequestMethod -> ResponseCode) -> IO ()
recvMessage sock handler = do
  (msgData, hostAddr) <- N.recvFrom sock 65535
  let message = decode msgData
  case (messageCode (messageHeader message)) of
    Request method -> let response = createResponse (handler method) message
                       in do
                         N.sendTo sock (encode response) hostAddr
                         return ()
    _              -> return ()

sendMessage :: Message -> Socket -> IO ()
sendMessage message sock = return ()
