import Network.CoAP.Client
import Network.CoAP.Transport
import Network.Socket

import qualified Data.ByteString.Char8 as B

main :: IO ()
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
