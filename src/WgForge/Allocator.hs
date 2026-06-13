module WgForge.Allocator (
  allocate,
) where

import Data.IP (AddrRange, IPv4, mlen)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import WgForge.Cidr
import WgForge.Spec

-- | Allocate IP addresses to peers based on the provided CIDR range and peer specifications.
-- Static addresses specified in the peer specifications are preserved
-- while dynamic addresses are assigned in the ascending order based on the peer names.
allocate :: AddrRange IPv4 -> Map.Map PeerName PeerSpec -> Map.Map PeerName IPv4
allocate range p =
  Map.union static dynamic
 where
  static = Map.mapMaybe address p
  dynamicPeers = Map.keys $ Map.difference p static
  used = Set.fromList (Map.elems static)
  dynamic = Map.fromList $ zip dynamicPeers (availableAddresses range used)

availableAddresses :: AddrRange IPv4 -> Set.Set IPv4 -> [IPv4]
availableAddresses range used =
  filter keep $ enumFromTo (rangeBase range) (broadcastAddress range)
 where
  reserved
    | mlen range <= 30 = Set.fromList [networkAddress range, broadcastAddress range]
    | otherwise = Set.empty
  usedReserved = Set.union used reserved
  keep ip = ip `Set.notMember` usedReserved
