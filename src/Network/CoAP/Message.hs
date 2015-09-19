module Network.CoAP.Message
( Message
, MessageType
, parseMessage
, encodeMessage
) where

import Data.ByteString
import Data.Word

data MessageType = Confirmable | NonConfirmable | Acknowledgement | Reset

data Message = Message
  { messageVersion     :: Int
  , messageType        :: MessageType
  , messageTokenLength :: Int
  , messageCode        :: Word8
  , messageId          :: Word16
  }

parseMessage :: ByteString -> Maybe Message
parseMessage _ = Nothing

encodeMessage :: Message -> ByteString
encodeMessage _ = empty
