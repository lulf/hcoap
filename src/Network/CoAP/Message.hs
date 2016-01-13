module Network.CoAP.Message
( Header (Header)
, Message (Message)
, MessageId
, Type (CON, NON, ACK, RST)
, Token
, Code (CodeEmpty, CodeRequest, CodeResponse)
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
import Network.CoAP.Request
import Network.CoAP.Response
import Data.ByteString.Lazy
import qualified Data.ByteString as BS
import Data.Word
import Data.Maybe
import Data.List (sortOn)
import Data.Binary hiding (encode, decode)
import Data.Bits
import Prelude hiding (null, length, fromStrict, toStrict)

data Code = CodeRequest Method
          | CodeResponse ResponseCode
          | CodeEmpty
          deriving (Show)

data Type = CON | NON | ACK | RST deriving (Show)

data Header = Header
  { messageVersion     :: Version
  , messageType        :: Type
  , messageCode        :: Code
  , messageId          :: MessageId
  } deriving (Show)

type Version     = Int
type MessageId   = Word16
type Token       = BS.ByteString

data Message = Message
  { messageHeader  :: Header
  , messageToken   :: Maybe Token
  , messageOptions :: [Option]
  , messagePayload :: Maybe Payload
  } deriving (Show)
