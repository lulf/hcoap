Hcoap is a haskell CoAP library. See http://coap.technology/ for more information about the CoAP
protocol. The library aims to support RFC 7252 specification, though currently this library only
provides APIs for writing CoAP servers.

The library is split into a high-level API in Network.CoAP.Server (currently only for servers), and a lower layer API in Network.CoAP.Message for working directly with CoAP messages.

A high-level client API is in the works.

[![Build Status](https://travis-ci.org/lulf/hcoap.svg?branch=master)](https://travis-ci.org/lulf/hcoap)
