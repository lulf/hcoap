Hcoap is a haskell CoAP library. See http://coap.technology/ for more information about the CoAP
protocol. The library aims to support RFC 7252 specification, and currently only support non-secure
CoAP transport.

The library is split into a high-level API in Network.CoAP.Server and Network.CoAP.Client, and a lower layer API in Network.CoAP.Message for working directly with CoAP messages.

[![Build Status](https://travis-ci.org/lulf/hcoap.svg?branch=master)](https://travis-ci.org/lulf/hcoap)

## Example server

## Example client
    main = do
      let request = Request { requestMethod = GET
                            , requestOptions = [UriPath (B.pack "hello")]
                            , requestPayload = Nothing
                            , requestReliable = True }
    withSocketsDo $ do
      sock <- socket AF_INET6 Datagram defaultProtocol
      bindSocket sock (SockAddrInet6 12345 0 iN6ADDR_ANY 0)
      let transport = createUDPTransport sock
      let dest = SockAddrInet6 5683 0 (0, 0, 0, 0) 0
      client <- createClient transport
      response <- doRequest client dest request
      putStrLn ("Got response: " ++ show response)
      return ()
