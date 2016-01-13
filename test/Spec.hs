import Network.CoAP.MessageCodec
import Network.CoAP.Message
import Network.CoAP.Request
import Test.HUnit
import Data.ByteString
import Test.QuickCheck

tests = TestList [TestLabel "testEncodeDecode" testEncodeDecode]

testEncodeDecode =
  TestCase (do
    let hdr = Header { messageVersion = 1
                     , messageType = CON
                     , messageCode = CodeRequest GET
                     , messageId = 3 }
    let msg = Message { messageHeader = hdr
                      , messageToken = Nothing
                      , messageOptions = []
                      , messagePayload = Nothing }
      
    let encoded = encode msg
    let decoded = decode encoded
    
    assertEqual "Decoded message not same as original" (show msg) (show decoded)
    )
  
instance Arbitrary Type where
  arbitrary = elements [CON, NON, ACK, RST]

instance Arbitrary Method where
  arbitrary = elements [PUT, GET, POST, DELETE]

instance Arbitrary Header where
  arbitrary = do
    msgType <- arbitrary
    msgMethod <- arbitrary
    msgId <- arbitrary
    return (Header { messageVersion = 1
                   , messageType    = msgType
                   , messageCode    = CodeRequest msgMethod
                   , messageId      = msgId})

genSafeChar :: Gen Char
genSafeChar = elements ['a'..'z']

genSafeString :: Gen String
genSafeString = vectorOf 10 genSafeChar

instance Arbitrary ByteString where
  arbitrary = do
    str <- arbitrary
    return (pack str)


instance Arbitrary Message where
  arbitrary = do
    hdr <- arbitrary
    return (Message { messageHeader = hdr
                    , messageToken = Nothing
                    , messageOptions = []
                    , messagePayload = Nothing })


checkCodec msg =
  let encoded = encode msg
      decoded = decode encoded
   in (show msg) == (show decoded)

main :: IO ()
main = do
  runTestTT tests
  quickCheck checkCodec
  return ()

