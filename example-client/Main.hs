import Network.CoAP.Client
import Network.CoAP.Transport
import Network.Socket
import Network.URI
import System.Environment

import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
  args <- getArgs
  if length args < 2
  then printUsage
  else runClient args

runClient :: [String] -> IO ()
runClient (method:uriStr:_) = do
  let request = Request { requestMethod = read method :: Method
                        , requestOptions = []
                        , requestPayload = Nothing
                        , requestReliable = True }
  withSocketsDo $ do
    sock <- socket AF_INET6 Datagram defaultProtocol
    bindSocket sock (SockAddrInet6 0 0 iN6ADDR_ANY 0)
    let transport = createUDPTransport sock
    client <- createClient transport
    let (Just uri) = parseURI uriStr
    response <- doRequest client uri request
    putStrLn ("Got response: " ++ show response)
    return ()

printUsage :: IO ()
printUsage = do
  pname <- getProgName
  putStrLn ("Usage: " ++ pname ++ " <method> <uri>")
