import Network.CoAP.MessageCodec
import Network.CoAP.Message
import qualified Network.CoAP.Request as Req
import Test.HUnit

tests = TestList [TestLabel "testEncodeDecode" testEncodeDecode]

main :: IO ()
main = do
  runTestTT tests
  return ()

testEncodeDecode =
  TestCase (do
    let hdr = Header { messageVersion = 1
                     , messageType = CON
                     , messageCode = Request Req.GET
                     , messageId = 3 }
    let msg = Message { messageHeader = hdr
                      , messageToken = Nothing
                      , messageOptions = []
                      , messagePayload = Nothing }
      
    let encoded = encode msg
    let decoded = decode encoded
    
    assertEqual "Decoded message not same as original" (show msg) (show decoded)
    )
  
