module Network.CoAP.Options where

import Data.ByteString

data Option = ContentFormat
            | ETag
            | LocationPath
            | LocationQuery
            | MaxAge
            | ProxyUri
            | ProxyScheme
            | UriHost
            | UriPath
            | UriPort
            | UriQuery
            | Accept
            | IfMatch
            | IfNoneMatch
            | Size1
            deriving (Show)

type OptionValue = ByteString

decodeOption :: Int -> Option
decodeOption 0  = ContentFormat
decodeOption 1  = ETag
decodeOption 2  = LocationPath
decodeOption 3  = LocationQuery
decodeOption 4  = MaxAge
decodeOption 5  = ProxyUri
decodeOption 6  = ProxyScheme
decodeOption 7  = UriHost
decodeOption 8  = UriPath
decodeOption 9  = UriPort
decodeOption 10 = UriQuery
decodeOption 11 = Accept
decodeOption 12 = IfMatch
decodeOption 13 = IfNoneMatch
decodeOption 14 = Size1

encodeOption :: Option -> Int
encodeOption ContentFormat = 0
encodeOption ETag          = 1
encodeOption LocationPath  = 2
encodeOption LocationQuery = 3
encodeOption MaxAge        = 4
encodeOption ProxyUri      = 5
encodeOption ProxyScheme   = 6
encodeOption UriHost       = 7
encodeOption UriPath       = 8
encodeOption UriPort       = 9
encodeOption UriQuery      = 10
encodeOption Accept        = 11
encodeOption IfMatch       = 12
encodeOption IfNoneMatch   = 13
encodeOption Size1         = 14
