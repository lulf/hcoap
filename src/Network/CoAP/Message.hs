module Network.CoAP.Message
( Header (Header)
, Message (Message)
, MessageId
, Type (CON, NON, ACK, RST)
, RequestMethod
, Token
, Code (Empty, Request, Response)
, messageCode
, messageHeader
, messageId
, messageToken
, messagePayload
, messageVersion
, messageType
, messageOptions
) where

import Network.CoAP.Options
import Network.CoAP.Payload
import Network.CoAP.Request hiding (Request)
import Network.CoAP.Response hiding (Response)
import Data.ByteString.Lazy
import qualified Data.ByteString as BS
import Data.Word
import Data.Maybe
import Data.List (sortOn)
import Data.Binary hiding (encode, decode)
import Data.Bits
import Prelude hiding (null, length, fromStrict, toStrict)

type RequestMethod = Method

data Code = Request RequestMethod
          | Response ResponseCode
          | Empty
          deriving (Show)

data Type = CON | NON | ACK | RST deriving (Show)

data Header = Header
  { messageVersion     :: Version
  , messageType        :: Type
  , messageCode        :: Code
  , messageId          :: MessageId
  }

type Version     = Int
type MessageId   = Word16
type Token       = BS.ByteString

data Message = Message
  { messageHeader  :: Header
  , messageToken   :: Maybe Token
  , messageOptions :: [(Option, OptionValue)]
  , messagePayload :: Maybe Payload
  }
