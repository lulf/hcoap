module Network.CoAP.Message
( Message(..)
, MessageHeader(..)
, MessageVersion(..)
, MessageType(..)
, MessageId(..)
, MessageCode(..)
, MessageToken(..)
, Payload(..)
, Option(..)
, MediaType(..)
, Method(..)
, encode
, decode
) where

import Network.CoAP.Types
import Network.CoAP.MessageCodec (encode, decode)
