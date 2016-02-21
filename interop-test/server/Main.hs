import Network.CoAP.Server
import Network.CoAP.Transport
import Network.Socket
import qualified Data.ByteString.Char8 as B

findPath :: [Option] -> B.ByteString
findPath [] = B.empty
findPath (option:options) =
  case option of
    UriPath value -> value
    _             -> findPath options

requestHandler :: RequestHandler
requestHandler (request, _) = do
  let options = requestOptions request
  let path = B.unpack (findPath options)
  if path == ".well-known/core"
  then return (Response Content [ContentFormat ApplicationLinkFormat] Nothing)
  else return (Response Content [ContentFormat ApplicationJson] (Just (B.pack ("{\"path\":\"hello\"}"))))
              

main :: IO ()
main =
  withSocketsDo $ do
    sock <- socket AF_INET Datagram defaultProtocol
    bindSocket sock (SockAddrInet 5683 iNADDR_ANY)
    server <- createServer (createUDPTransport sock) requestHandler
    runServer server

