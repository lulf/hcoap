import Network.CoAP.Server
import Network.Socket
import qualified Data.ByteString.Char8 as B

findPath :: [Option] -> B.ByteString
findPath [] = B.empty
findPath (option:options) =
  case option of
    UriPath value -> value
    _             -> findPath options

requestHandler :: Request -> IO Response
requestHandler request = do
  let options = requestOptions request
  let path = findPath options
  let payload = Just (B.pack ("{\"path\":\"" ++ (B.unpack path) ++ "\"}"))
  return (createResponse request Content [(ContentFormat ApplicationJson)] payload)
              

main :: IO ()
main = do
  withSocketsDo $ do
    sock <- socket AF_INET6 Datagram defaultProtocol
    bindSocket sock (SockAddrInet6 12345 0 iN6ADDR_ANY 0)
    runServer sock requestHandler

