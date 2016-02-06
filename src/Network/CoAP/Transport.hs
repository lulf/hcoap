{-|
Module:  Network.CoAP.Transport
Description: CoAP transport library
Maintainer: ulf.lilleengen@gmail.com
License: BSD3

The CoAP transport API is intended to provide different transports available for CoAP. Currently only UDP transport is supported.
-}
module Network.CoAP.Transport
( createUDPTransport
, Endpoint(..)
, Transport(..)
) where

import Network.CoAP.Types
import qualified Network.Socket.ByteString as N
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Data.ByteString

-- | Create UDP transport instance for a given socket.
createUDPTransport :: Socket -> Transport
createUDPTransport sock =
  Transport { sendTo = N.sendTo sock
            , recvFrom = N.recvFrom sock 65535
            , localEndpoint = getSocketName sock }
