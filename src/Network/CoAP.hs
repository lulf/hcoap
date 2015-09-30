module Network.CoAP
( module Network.CoAP.Request
, module Network.CoAP.Response
) where

import Network.CoAP.Request
import Network.CoAP.Response
import Network.CoAP.Message
import Network.CoAP.Messaging

runServer sock requestHandler = do
  let store = MessagingStore
  request <- recvRequest sock store
  response <- requestHandler request
  sendResponse sock store response
