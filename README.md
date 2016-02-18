Hcoap is a haskell CoAP library. See http://coap.technology/ for more information about the CoAP
protocol. The library aims to support RFC 7252 specification, and currently only support non-secure
CoAP transport.

The library is split into a high-level API in Network.CoAP.Server and Network.CoAP.Client, and a lower layer API in Network.CoAP.Message for working directly with CoAP messages.

[![Build Status](https://travis-ci.org/lulf/hcoap.svg?branch=master)](https://travis-ci.org/lulf/hcoap)

## Example server
    main =
      withSocketsDo $ do
        sock <- socket AF_INET6 Datagram defaultProtocol
        bindSocket sock (SockAddrInet6 5683 0 iN6ADDR_ANY 0)
        server <- createServer (createUDPTransport sock) (\(request, _) -> do
          let payload = Just (B.pack "{\"value\":\"foo\"}")
          return (Response Content [ContentFormat ApplicationJson] payload))
        runServer server

## Example client
    main = do
      let request = Request { requestMethod = GET
                            , requestOptions = []
                            , requestPayload = Nothing
                            , requestReliable = True }
    withSocketsDo $ do
      sock <- socket AF_INET6 Datagram defaultProtocol
      bindSocket sock (SockAddrInet6 0 0 iN6ADDR_ANY 0)
      let transport = createUDPTransport sock
      client <- createClient transport
      uri <- parseURI "coap://[::1]:5683/hello"
      response <- doRequest client uri request
      putStrLn ("Got response: " ++ show response)
      return ()
