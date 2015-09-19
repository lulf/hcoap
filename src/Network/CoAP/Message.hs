module Network.CoAP.Message
(
  Message
) where

data Message = Message
  { messageVersion     :: Int
  , messageType        :: Int
  , messageTokenLength :: Int
  , messageCode        :: Int
  , messageId          :: Int
  }
