module Network.CoAP.Options where

import Data.ByteString
import Data.Binary hiding (encode, decode)

data MediaType = TextPlain
               | ApplicationLinkFormat
               | ApplicationXml
               | ApplicationOctetStream
               | ApplicationExi
               | ApplicationJson
               deriving (Show)

data Option = ContentFormat MediaType
            | ETag ByteString
            | LocationPath ByteString
            | LocationQuery ByteString
            | MaxAge Int
            | ProxyUri ByteString
            | ProxyScheme ByteString
            | UriHost ByteString
            | UriPath ByteString
            | UriPort Int
            | UriQuery ByteString
            | Accept Int
            | IfMatch ByteString
            | IfNoneMatch
            | Size1 Int
            deriving (Show)
