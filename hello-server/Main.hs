import Network.CoAP
import Network.Socket

requestHandler :: Request -> IO Response
requestHandler req = do
  let response = Response { request = req
                          , responseCode = NotFound
                          , responseOptions = []
                          , responsePayload = Nothing }
  return response
              

main :: IO ()
main = do
  withSocketsDo $ do
    sock <- socket AF_INET6 Datagram defaultProtocol
    bindSocket sock (SockAddrInet6 12345 0 iN6ADDR_ANY 0)
    runServer sock requestHandler

