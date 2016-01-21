import Network.CoAP.Client
import Network.Socket

main :: IO ()
main = do
  let request = Request { requestMethod = GET
                        , requestOptions = []
                        , requestPayload = Nothing }
  withSocketsDo $ do
    sock <- socket AF_INET6 Datagram defaultProtocol
    bindSocket sock (SockAddrInet6 12345 0 iN6ADDR_ANY 0)
    response <- doRequest sock (SockAddrInet6 12346 0 iN6ADDR_ANY 0) request
    return ()

