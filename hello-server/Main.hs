import Network.CoAP
import Network.Socket
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Binary

requestHandler :: Request -> IO Response
requestHandler req = do
  let response = Response { request = req
                          , responseCode = Content
                          , responseOptions = [(ContentFormat, B.pack (L.unpack (encode (50 :: Word8))))]
                          , responsePayload = Just (B.pack "{\"foo\":42}") }
  return response
              

main :: IO ()
main = do
  withSocketsDo $ do
    sock <- socket AF_INET6 Datagram defaultProtocol
    bindSocket sock (SockAddrInet6 12345 0 iN6ADDR_ANY 0)
    runServer sock requestHandler

