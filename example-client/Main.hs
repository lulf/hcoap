import Network.CoAP
import Network.Socket
import qualified Data.ByteString.Char8 as B

Request = Request { method :: Method
                  , options :: [Options]
                  , payload :: Maybe Payload }

main :: IO ()
main = do
  withSocketsDo $ do
    sock <- socket AF_INET6 Datagram defaultProtocol
    bindSocket sock (SockAddrInet6 12345 0 iN6ADDR_ANY 0)
    runClient sock destaddr request

