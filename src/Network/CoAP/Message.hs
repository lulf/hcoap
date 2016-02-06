{-|
Module:  Network.CoAP.Message
Description: CoAP message library
Maintainer: ulf.lilleengen@gmail.com
License: BSD3

The CoAP message API is intended to provide the minimal building block needed for creating, encoding and decoding CoAP messages. The module can be used to build alternative CoAP messaging layers.
-}
module Network.CoAP.Message
( Message(..)
, MessageHeader(..)
, MessageVersion(..)
, MessageType(..)
, MessageId(..)
, MessageCode(..)
, Token(..)
, Payload(..)
, Option(..)
, MediaType(..)
, Method(..)
, encode
, decode
) where

import Network.CoAP.Types
import Network.CoAP.MessageCodec (encode, decode)
