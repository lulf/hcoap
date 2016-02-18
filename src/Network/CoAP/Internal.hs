module Network.CoAP.Internal
( createEndpoint
, createOpts
) where

import Network.DNS.Lookup
import Network.DNS.Resolver
import Data.ByteString.Char8
import Network.URI
import Data.IP
import Network.Socket
import Network.CoAP.Types

createIPv4Endpoint :: [IPv4] -> PortNumber -> Endpoint
createIPv4Endpoint (addr:_) port = SockAddrInet port (toHostAddress addr)

createIPv6Endpoint :: [IPv6] -> PortNumber -> Endpoint
createIPv6Endpoint (addr:_) port = SockAddrInet6 port 0 (toHostAddress6 addr) 0

stripBrackets :: String -> String
stripBrackets [] = []
stripBrackets [']'] = []
stripBrackets ('[':rest) = stripBrackets rest
stripBrackets (h:t) = h:stripBrackets t

parsePort :: String -> Int
parsePort [] = 5683
parsePort (':':rest) = parsePort rest
parsePort s = read s

createEndpoint :: URI -> IO Endpoint
createEndpoint uri = do
  let (Just auth) = uriAuthority uri
      hname = stripBrackets (uriRegName auth)
      port = fromIntegral (parsePort (uriPort auth))
  if isIPv6address hname
  then return (createIPv6Endpoint [read hname :: IPv6] port)
  else if isIPv4address hname
       then return (createIPv4Endpoint [read hname :: IPv4] port)
       else resolveEndpoint hname port

resolveEndpoint :: String -> PortNumber -> IO Endpoint
resolveEndpoint hostName port = do
  let hname = pack hostName
  rs <- makeResolvSeed defaultResolvConf
  ipv6Addr <- withResolver rs (`lookupAAAA` hname)
  case ipv6Addr of
    Left _ -> do
      ipv4Addr <- withResolver rs (`lookupA` hname)
      case ipv4Addr of
        Left _ -> error "Unable to resolve hostname"
        Right addr -> return (createIPv4Endpoint addr port)
    Right addr -> return (createIPv6Endpoint addr port)

parsePath :: String -> String
parsePath [] = []
parsePath ('/':rest) = rest
parsePath any = any

createOpts :: URI -> [Option]
createOpts uri =
  let (Just auth) = uriAuthority uri
      path = parsePath (uriPath uri)
      port = parsePort (uriPort auth)
   in [UriPath (pack path), UriPort port]
