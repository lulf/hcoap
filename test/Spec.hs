import Network.CoAP.Message
import Test.HUnit
import Data.ByteString hiding (putStrLn)
import Test.QuickCheck
import Prelude hiding (null, length)

tests = TestList [TestLabel "testEncodeDecode" testEncodeDecode]

testEncodeDecode =
  TestCase (do
    let hdr = MessageHeader { messageVersion = 1
                            , messageType = RST 
                            , messageCode = CodeRequest PUT
                            , messageId = 1 }
    let msg = Message { messageHeader = hdr
                      , messageToken = empty
                      , messageOptions = [ContentFormat TextPlain]
                      , messagePayload = Nothing }
      
    let encoded = encode msg
    let decoded = decode encoded
    
    assertEqual "Decoded message not same as original" (show msg) (show decoded)
    )

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

