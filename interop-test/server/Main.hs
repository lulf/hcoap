import Network.CoAP.Server
import Network.CoAP.Transport
import Network.Socket
import qualified Data.ByteString.Char8 as B
import Control.Concurrent

findPath :: [Option] -> B.ByteString
findPath [] = B.empty
findPath (option:options) =
  let restPath = findPath options
      sep      = if B.null restPath then B.empty else B.pack "/"
   in case option of
        UriPath value -> B.append value (B.append sep restPath)
        _             -> findPath options


requestHandler :: RequestHandler
requestHandler req@(request, _) = do
  let options = requestOptions request
  let path = B.unpack (findPath options)
  case path of
    ".well-known/core" -> handleCore req
    "test"             -> handleTest req
    "separate"         -> handleSeparate req
    "seg1/seg2/seg3"   -> handleSeg req
    "query"            -> handleQuery req
    "location-query"   -> handleLocationQuery req
    "multi-format"     -> handleMultiFormat req
    "validate"         -> handleValidate req
    "create1"          -> handleCreate1 req
    _                  -> error ("Unknown path " ++ show path)

handleCore :: RequestHandler
handleCore req = return (Response Content [ContentFormat ApplicationLinkFormat] Nothing)

handleTest :: RequestHandler
handleTest req = return (Response Content [ContentFormat ApplicationJson] (Just (B.pack ("{\"test\":\"hello\"}"))))

handleSeparate ::RequestHandler
handleSeparate req = do
  threadDelay 1000000
  return (Response Content [ContentFormat ApplicationJson] (Just (B.pack ("{\"separate\":\"hello\"}"))))
              
handleSeg :: RequestHandler
handleSeg req = return (Response Content [ContentFormat ApplicationJson] (Just (B.pack ("{\"Seg\":\"hello\"}"))))

handleQuery :: RequestHandler
handleQuery req = return (Response Content [ContentFormat ApplicationJson] (Just (B.pack ("{\"Query\":\"hello\"}"))))

handleLocationQuery :: RequestHandler
handleLocationQuery req = return (Response Content [ContentFormat ApplicationJson] (Just (B.pack ("{\"LocationQuery\":\"hello\"}"))))

handleMultiFormat :: RequestHandler
handleMultiFormat req = return (Response Content [ContentFormat ApplicationJson] (Just (B.pack ("{\"MultiFormat\":\"hello\"}"))))

handleValidate :: RequestHandler
handleValidate req = return (Response Content [ContentFormat ApplicationJson] (Just (B.pack ("{\"Validate\":\"hello\"}"))))

handleCreate1 :: RequestHandler
handleCreate1 req = return (Response Content [ContentFormat ApplicationJson] (Just (B.pack ("{\"Create1\":\"hello\"}"))))

main :: IO ()
main =
  withSocketsDo $ do
    sock <- socket AF_INET Datagram defaultProtocol
    bindSocket sock (SockAddrInet 5683 iNADDR_ANY)
    server <- createServer (createUDPTransport sock) requestHandler
    runServer server

