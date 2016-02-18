import Network.CoAP.Client
import Network.CoAP.Transport
import Network.Socket
import Network.URI

import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
  let request = Request { requestMethod = GET
                        , requestOptions = []
                        , requestPayload = Nothing
                        , requestReliable = True }
  withSocketsDo $ do
    sock <- socket AF_INET6 Datagram defaultProtocol
    bindSocket sock (SockAddrInet6 12345 0 iN6ADDR_ANY 0)
    let transport = createUDPTransport sock
    client <- createClient transport
    let (Just uri) = parseURI "coap://[::1]:5683/hello"
    response <- doRequest client uri request
    putStrLn ("Got response: " ++ show response)
    return ()
