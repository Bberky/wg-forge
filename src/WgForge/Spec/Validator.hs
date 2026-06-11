module WgForge.Spec.Validator (
  validateNetwork,
  validateSegmentSpec,
  validatePeerRoles,
  validateEndpoints,
  validateNatPairs,
  validateReachability,
  validateAddressing,
  validateAddressesInCidr,
  validateReservedAddresses,
  validateAddressCollisions,
  validateCidrCapacity,
) where

import Data.Bits (complement, shiftL, (.&.), (.|.))
import Data.Foldable (traverse_)
import Data.IP (AddrRange, IPv4, addrRangePair, fromIPv4w, isMatchedTo, mlen, toIPv4w)
import Data.List (intersect, tails)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import qualified Data.Set as Set
import Data.Word (Word32)
import Validation (Validation (Failure), failureIf)

import WgForge.Error (
  ValidationError (
    AddressCollision,
    AddressIsReserved,
    AddressOutOfCidr,
    CidrOverflow,
    InsufficientPeers,
    IslandPeer,
    MissingEndpoint,
    NatPairInMesh,
    PeerBothRoles
  ),
 )
import WgForge.Spec (
  Network (..),
  NetworkSpec (..),
  PeerName,
  PeerSpec (..),
  SegmentName,
  SegmentSpec (..),
 )

validateNetwork :: Network -> Validation (NonEmpty ValidationError) Network
validateNetwork net@(Network _ _ segMap) =
  net
    <$ traverse_ (uncurry validateSegmentSpec) (Map.toList segMap)
    <* validateEndpoints net
    <* validateNatPairs net
    <* validateReachability net
    <* validateAddressing net

-- | Validate that each segment has enough peers and has no role conflicts.
validateSegmentSpec ::
  SegmentName -> SegmentSpec -> Validation (NonEmpty ValidationError) SegmentSpec
validateSegmentSpec sn seg@(FullMesh fmPeers) =
  seg
    <$ failureIf (length fmPeers < 2) (InsufficientPeers sn "requires at least 2 peers")
    <* validatePeerRoles sn seg
validateSegmentSpec sn seg@(HubSpoke hubs spokes _) =
  seg
    <$ failureIf (null hubs) (InsufficientPeers sn "requires at least 1 hub")
    <* failureIf (null spokes) (InsufficientPeers sn "requires at least 1 spoke")
    <* validatePeerRoles sn seg
validateSegmentSpec sn seg@(Relay relays clients _) =
  seg
    <$ failureIf (null relays) (InsufficientPeers sn "requires at least 1 relay")
    <* failureIf (null clients) (InsufficientPeers sn "requires at least 1 client")
    <* validatePeerRoles sn seg

-- | Validate that no peer holds conflicting roles within the same segment.
validatePeerRoles ::
  SegmentName -> SegmentSpec -> Validation (NonEmpty ValidationError) SegmentSpec
validatePeerRoles sn seg@(HubSpoke hubs spokes _) =
  seg
    <$ traverse_
      (failureIf True . PeerBothRoles sn)
      (hubs `intersect` spokes)
validatePeerRoles sn seg@(Relay relays clients _) =
  seg
    <$ traverse_
      (failureIf True . PeerBothRoles sn)
      (relays `intersect` clients)
validatePeerRoles _ seg = pure seg

-- | Validate that every hub and relay peer has an endpoint declared.
validateEndpoints :: Network -> Validation (NonEmpty ValidationError) Network
validateEndpoints net@(Network _ peerMap segMap) =
  net
    <$ traverse_
      (\p -> Failure $ MissingEndpoint p :| [])
      missingPeers
 where
  hubsAndRelays = Set.toAscList $ foldMap centralPeers (Map.elems segMap)
  centralPeers (HubSpoke hubs _ _) = Set.fromList hubs
  centralPeers (Relay relays _ _) = Set.fromList relays
  centralPeers (FullMesh _) = Set.empty
  missingPeers = filter lacksEndpoint hubsAndRelays
  lacksEndpoint pn = case Map.lookup pn peerMap of
    Just ps -> isNothing (endpoint ps)
    Nothing -> False

-- | Validate that for every tunnel (A, B) in any segment, at least one peer has an endpoint.
validateNatPairs :: Network -> Validation (NonEmpty ValidationError) Network
validateNatPairs net@(Network _ peerMap segMap) =
  net
    <$ traverse_
      (\(sn, p1, p2) -> Failure $ NatPairInMesh sn p1 p2 :| [])
      badPairs
 where
  badPairs = do
    (sn, seg) <- Map.toAscList segMap
    let edges = Set.toAscList $ Set.fromList [canonicPair e | e <- segmentEdges seg]
    (p1, p2) <- edges
    case (lookupEndpoint p1, lookupEndpoint p2) of
      (Nothing, Nothing) -> [(sn, p1, p2)]
      _ -> []
  lookupEndpoint pn = Map.lookup pn peerMap >>= endpoint
  canonicPair (a, b) = (min a b, max a b)

-- | Enumerate the tunnel edges produced by a segment.
segmentEdges :: SegmentSpec -> [(PeerName, PeerName)]
segmentEdges (FullMesh fmPeers) =
  [(a, b) | (a : rest) <- tails fmPeers, b <- rest]
segmentEdges (HubSpoke hubs spokes _) =
  [(h, s) | h <- hubs, s <- spokes]
    ++ [(h1, h2) | (h1 : rest) <- tails hubs, h2 <- rest]
segmentEdges (Relay relays clients _) =
  [(r, c) | r <- relays, c <- clients]
    ++ [(r1, r2) | (r1 : rest) <- tails relays, r2 <- rest]

-- | Validate that every peer in 'specPeers' appears in at least one segment.
validateReachability :: Network -> Validation (NonEmpty ValidationError) Network
validateReachability net@(Network _ peerMap segMap) =
  net
    <$ traverse_
      (\p -> Failure $ IslandPeer p :| [])
      islands
 where
  assigned = foldMap segmentPeers (Map.elems segMap)
  segmentPeers (FullMesh ps) = Set.fromList ps
  segmentPeers (HubSpoke hs ss _) = Set.fromList (hs ++ ss)
  segmentPeers (Relay rs cs _) = Set.fromList (rs ++ cs)
  islands = filter (`Set.notMember` assigned) (Map.keys peerMap)

-- | Validate explicit peer addresses and CIDR capacity.
validateAddressing :: Network -> Validation (NonEmpty ValidationError) Network
validateAddressing net =
  net
    <$ validateAddressesInCidr net
    <* validateReservedAddresses net
    <* validateAddressCollisions net
    <* validateCidrCapacity net

-- | Validate that every explicit address lies inside the network CIDR.
validateAddressesInCidr :: Network -> Validation (NonEmpty ValidationError) Network
validateAddressesInCidr net@(Network ns peerMap _) =
  net
    <$ traverse_
      (\(pn, a) -> Failure $ AddressOutOfCidr pn a :| [])
      (filter (\(_, a) -> not (a `isMatchedTo` cidr ns)) (explicitAddresses peerMap))

-- | Validate that no explicit address is the network or broadcast address.
--   For \/31 and \/32 networks there are no reserved addresses.
validateReservedAddresses :: Network -> Validation (NonEmpty ValidationError) Network
validateReservedAddresses net@(Network ns peerMap _) =
  net
    <$ traverse_
      (\(pn, a) -> Failure $ AddressIsReserved pn a :| [])
      (filter (isReserved . snd) (explicitAddresses peerMap))
 where
  range = cidr ns
  isReserved a =
    mlen range <= 30 && (a == networkAddress range || a == broadcastAddress range)

-- | Validate that no two peers claim the same explicit address.
validateAddressCollisions :: Network -> Validation (NonEmpty ValidationError) Network
validateAddressCollisions net@(Network _ peerMap _) =
  net
    <$ traverse_
      (\(a, p1, p2) -> Failure $ AddressCollision p1 p2 a :| [])
      collisions
 where
  claims =
    Map.fromListWith (flip (++)) [(a, [pn]) | (pn, a) <- explicitAddresses peerMap]
  collisions = do
    (a, claimants) <- Map.toAscList claims
    (p1 : rest) <- tails claimants
    p2 <- rest
    pure (a, p1, p2)

-- | Validate that the network CIDR can accommodate all peers.
validateCidrCapacity :: Network -> Validation (NonEmpty ValidationError) Network
validateCidrCapacity net@(Network ns peerMap _) =
  net <$ failureIf (peerCount > hostCount) (CidrOverflow peerCount hostCount)
 where
  peerCount = Map.size peerMap
  prefixLen = mlen (cidr ns)
  total = 1 `shiftL` (32 - prefixLen) :: Int
  hostCount = if prefixLen >= 31 then total else total - 2

explicitAddresses :: Map.Map PeerName PeerSpec -> [(PeerName, IPv4)]
explicitAddresses peerMap =
  [(pn, a) | (pn, ps) <- Map.toAscList peerMap, Just a <- [address ps]]

networkAddress :: AddrRange IPv4 -> IPv4
networkAddress range = toIPv4w (fromIPv4w (rangeBase range) .&. rangeMask range)

broadcastAddress :: AddrRange IPv4 -> IPv4
broadcastAddress range =
  toIPv4w ((fromIPv4w (rangeBase range) .&. rangeMask range) .|. complement (rangeMask range))

rangeBase :: AddrRange IPv4 -> IPv4
rangeBase = fst . addrRangePair

rangeMask :: AddrRange IPv4 -> Word32
rangeMask range = complement (1 `shiftL` (32 - mlen range) - 1)
