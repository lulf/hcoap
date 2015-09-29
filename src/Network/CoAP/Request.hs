module Network.CoAP.Request where

import Network.CoAP.Options
import Network.CoAP.Payload

data Method = GET | POST | PUT | DELETE

data Request = Request Method [(Option, OptionValue)] Payload
