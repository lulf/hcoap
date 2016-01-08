import Network.CoAP
import Network.Socket
import Data.ByteString.Char8

requestHandler :: Request -> IO Response
requestHandler req = do
  let response = Response { request = req
                          , responseCode = Content
                          , responseOptions = [(ContentFormat, pack "application/json")]
                          , responsePayload = Just (pack "{\"foo\":42}") }
  return response
              

main :: IO ()
main = do
  withSocketsDo $ do
    sock <- socket AF_INET6 Datagram defaultProtocol
    bindSocket sock (SockAddrInet6 12345 0 iN6ADDR_ANY 0)
    runServer sock requestHandler

