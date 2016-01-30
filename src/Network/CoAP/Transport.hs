module Network.CoAP.Transport
( createUDPTransport
) where

import Network.CoAP.Types
import qualified Network.Socket.ByteString as N
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Data.ByteString

createUDPTransport :: Socket -> Transport
createUDPTransport sock =
  Transport { sendTo = N.sendTo sock
            , recvFrom = N.recvFrom sock 65535
            , localEndpoint = getSocketName sock }
