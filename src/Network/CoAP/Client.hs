module Network.CoAP.Client
( Request(..)
, T.Method(..)
, Response(..)
, T.ResponseCode(..)
, T.Option(..)
, T.MediaType(..)
, doRequest
) where

import Network.CoAP.Messaging
import qualified Network.CoAP.Types as T
import Control.Monad.State
import Network.Socket
import Data.ByteString.Char8

type Response = T.CoAPResponse

data Request =
  Request { requestMethod :: T.Method
          , requestOptions :: [T.Option]
          , requestPayload :: Maybe T.Payload }
                    

doRequest :: Socket -> SockAddr -> Request -> IO Response
doRequest sock addr req = do
  let state = createMessagingState
  (response, _) <- runStateT (runClient sock addr req) state
  return response

runClient :: Socket -> SockAddr -> Request -> MessagingState Response
runClient sock addr (Request method options payload) = do
  myAddr <- liftIO (getSocketName sock)
  let tkn = (pack "Hello")
  let request = T.CoAPRequest { T.requestToken = tkn
                                  , T.requestMethod = method
                                  , T.requestOptions = options
                                  , T.requestPayload = payload
                                  , T.requestDestination = addr
                                  , T.requestOrigin = myAddr }
  sendRequest sock request
  recvResponse sock request
