-- | Guards against SSRF when the server fetches a user-supplied URL on the
-- user's behalf (e.g. title lookups): resolves the host and rejects it if
-- any resolved address falls in a private, loopback, link-local, or other
-- non-public range.
module Network.PrivateAddress
  ( isDisallowedFetchHost,
    isDisallowedFetchUrl,
  )
where

import ClassyPrelude
import Data.IP (AddrRange, IP (..), IPv4, IPv6, fromSockAddr, isMatchedTo)
import Network.HTTP.Client qualified as NH
import Network.Socket (AddrInfo (addrAddress), HostName, defaultHints, getAddrInfo)

isDisallowedFetchHost :: HostName -> IO Bool
isDisallowedFetchHost host = do
  addrs <- getAddrInfo (Just defaultHints) (Just host) Nothing `catch` \(_ :: SomeException) -> pure []
  pure $ null addrs || any (maybe True (isPrivateOrReservedIP . fst) . fromSockAddr . addrAddress) addrs

isDisallowedFetchUrl :: Text -> IO Bool
isDisallowedFetchUrl rawUrl =
  ( do
      req <- NH.parseRequest (unpack rawUrl)
      isDisallowedFetchHost (unpack (decodeUtf8 (NH.host req)))
  )
    `catch` \(_ :: SomeException) -> pure True

isPrivateOrReservedIP :: IP -> Bool
isPrivateOrReservedIP (IPv4 ip) = any (ip `isMatchedTo`) privateIPv4Ranges
isPrivateOrReservedIP (IPv6 ip) = any (ip `isMatchedTo`) privateIPv6Ranges

privateIPv4Ranges :: [AddrRange IPv4]
privateIPv4Ranges =
  [ "0.0.0.0/8", -- "this" network
    "10.0.0.0/8", -- private
    "100.64.0.0/10", -- carrier-grade NAT
    "127.0.0.0/8", -- loopback
    "169.254.0.0/16", -- link-local (incl. cloud metadata endpoints)
    "172.16.0.0/12", -- private
    "192.0.0.0/24", -- IETF protocol assignments
    "192.0.2.0/24", -- documentation (TEST-NET-1)
    "192.168.0.0/16", -- private
    "198.18.0.0/15", -- benchmarking
    "198.51.100.0/24", -- documentation (TEST-NET-2)
    "203.0.113.0/24", -- documentation (TEST-NET-3)
    "224.0.0.0/4", -- multicast
    "240.0.0.0/4", -- reserved
    "255.255.255.255/32" -- broadcast
  ]

privateIPv6Ranges :: [AddrRange IPv6]
privateIPv6Ranges =
  [ "::1/128", -- loopback
    "::/128", -- unspecified
    "::ffff:0:0/96", -- IPv4-mapped (denylisted wholesale rather than unwrapped)
    "64:ff9b::/96", -- NAT64
    "100::/64", -- discard-only
    "2001:db8::/32", -- documentation
    "fc00::/7", -- unique local (private)
    "fe80::/10", -- link-local
    "ff00::/8" -- multicast
  ]
