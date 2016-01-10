import Network.CoAP
import Network.Socket
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Binary

findPath :: [Option] -> B.ByteString
findPath [] = B.empty
findPath (option:options) =
  case option of
    UriPath value -> value
    _             -> findPath options

requestHandler :: Request -> IO Response
requestHandler req@(Request id method options payload origin) = do
  let path = findPath options
  let response = Response { request = req
                          , responseCode = Content
                          , responseOptions = [ContentFormat ApplicationJson]
                          , responsePayload = Just (B.pack ("{\"path\":\"" ++ (B.unpack path) ++ "\"}")) }
  return response
              

main :: IO ()
main = do
  withSocketsDo $ do
    sock <- socket AF_INET6 Datagram defaultProtocol
    bindSocket sock (SockAddrInet6 12345 0 iN6ADDR_ANY 0)
    runServer sock requestHandler

