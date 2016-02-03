import Network.CoAP.Message
import qualified Network.CoAP.Server as S
import qualified Network.CoAP.Client as C
import Network.CoAP.Transport
import Network.Socket hiding (sendTo, recvFrom)
import Test.HUnit
import Data.ByteString.Char8 hiding (putStrLn)
import Test.QuickCheck
import Prelude hiding (null, length)
import Control.Concurrent.Chan
import Control.Concurrent
import Control.Monad
import System.Random
import System.Timeout
import Data.Maybe

data Cluster = Cluster Transport Transport

tests = TestList [TestLabel "testReliability" testReliability]

sendToChan prob chan msg endpoint = do
  putStrLn ("Attempting to send message to " ++ (show endpoint) ++ " with prob " ++ show prob)
  v <- randomRIO(0, 1)
  when (v < prob) (writeChan chan msg)
  return (length msg)

createUnstableTransport :: Double -> (Endpoint, Chan ByteString) -> (Endpoint, Chan ByteString) -> Transport
createUnstableTransport prob (localE, localC) (remoteE, remoteC) =
  Transport { sendTo = sendToChan prob remoteC
            , recvFrom = do
                d <- readChan localC
                return (d, remoteE)
            , localEndpoint = return localE }

testHandler :: S.Request -> IO S.Response
testHandler req = return (S.createResponse req S.Created [] (Just (pack "Hello, Client")))

instance Arbitrary C.Request where
  arbitrary = do
    pload <- arbitrary
    return (C.Request { C.requestMethod = GET
                      , C.requestOptions = []
                      , C.requestPayload = pload
                      , C.requestReliable = True })

testReliability =
  TestCase (do
    linkAProb <- randomRIO(1, 1)
    linkBProb <- randomRIO(1, 1)
    chanA <- newChan
    chanB <- newChan
    let endpointA = SockAddrUnix "A"
    let endpointB = SockAddrUnix "B"
    let transportA = createUnstableTransport linkAProb (endpointA, chanA) (endpointB, chanB)
    let transportB = createUnstableTransport linkBProb (endpointB, chanB) (endpointA, chanA)
    serverThread <- forkIO (S.runServer transportA testHandler)
    
    reqs <- generate (vector 2)
    mapM_ (\req -> do
      response <- timeout 200000000 (C.doRequest transportB endpointA req)
      assertBool ("Timed out waiting for response on reliable request " ++ show req) (isJust response)
      let res = fromJust response
      assertEqual "Bad response code" C.Created (C.responseCode res)
      assertEqual "Bad payload" "Hello, Client" (unpack (fromJust (C.responsePayload res)))
      ) reqs)

-- TODO, negative tests:
-- * Invalid token length
-- * Invalid code
-- * Unknown options
-- * Bad payload marker

instance Arbitrary MessageType where
  arbitrary = elements [CON, NON, ACK, RST]

instance Arbitrary Method where
  arbitrary = elements [PUT, GET, POST, DELETE]

instance Arbitrary MessageHeader where
  arbitrary = do
    msgType <- arbitrary
    msgMethod <- arbitrary
    msgId <- arbitrary
    return (MessageHeader { messageVersion = 1
                          , messageType    = msgType
                          , messageCode    = CodeRequest msgMethod
                          , messageId      = msgId})


instance Arbitrary ByteString where
  arbitrary = suchThat (fmap pack arbitrary) (\s -> ((length s > 0) && (length s <= 8)))

instance Arbitrary MediaType where
  arbitrary = elements [TextPlain, ApplicationLinkFormat, ApplicationXml, ApplicationOctetStream, ApplicationExi, ApplicationJson]

instance Arbitrary Option where
  arbitrary = do
    mediaType <- arbitrary
    etag <- arbitrary
    loc <- arbitrary
    locquery <- arbitrary
    puri <- arbitrary
    pscheme <- arbitrary
    uriHost <- arbitrary
    uriPath <- arbitrary
    uriQuery <- arbitrary
    ifMatch <- arbitrary
    elements [ContentFormat mediaType, ETag etag, LocationPath loc, LocationQuery locquery, ProxyUri puri, ProxyScheme pscheme, UriHost uriHost, UriPath uriPath, UriQuery uriQuery, IfMatch ifMatch]

instance Arbitrary Message where
  arbitrary = do
    hdr <- arbitrary
    tkn <- arbitrary
    options <- arbitrary
    payload <- arbitrary
    return (Message { messageHeader = hdr
                    , messageToken = tkn
                    , messageOptions = [options]
                    , messagePayload = payload })


checkCodec msg =
  let encoded = encode msg
      decoded = decode encoded
   in (show msg) == (show decoded)

main :: IO ()
main = do
  putStrLn "Running HUnit tests"
  runTestTT tests
  putStrLn "Running QuickCheck tests"
  quickCheck checkCodec
  return ()

