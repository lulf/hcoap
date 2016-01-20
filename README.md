Hcoap is a haskell CoAP library. See http://coap.technology/ for more information about the CoAP
protocol. The library aims to support RFC 7252 specification, though currently this library only
provides APIs for writing CoAP servers.

The library is split into a high-level API in Network.CoAP (currently only for servers), and a lower layer API in Network.CoAP.Messaging and Network.CoAP.Message for working directly with CoAP messages.

A high-level client API is in the works.
