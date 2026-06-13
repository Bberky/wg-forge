{-# LANGUAGE OverloadedStrings #-}

-- | Shared fixtures and builders for the test suite.
module Spec.Fixtures (
  -- * IP literals
  ipAddr1,
  ipAddr2,
  ipRange1,
  ipRangeStr1,
  ipRange2,
  ipRangeStr2,

  -- * Names
  sn,
  sn2,
  alice,
  bob,
  carol,
  dave,

  -- * Builders
  sampleEndpoint,
  sampleCidr,
  sampleNetSpec,
  mkPeer,
  mkPeerAddr,
  mkNetwork,
  mkAddrNetwork,

  -- * Helpers
  failureErrors,

  -- * Allocator generator
  AllocCase (..),
  genAllocCase,
) where

import Data.Bits (complement, shiftL, (.&.))
import Data.IP (AddrRange, IPv4, makeAddrRange, toIPv4, toIPv4w)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Word (Word32)
import Test.QuickCheck (Arbitrary (..), Gen, choose, elements, shuffle, vectorOf)
import Validation (Validation (..))

import WgForge.Spec

ipAddr1 :: IPv4
ipAddr1 = toIPv4 [10, 0, 0, 0]

ipAddr2 :: IPv4
ipAddr2 = toIPv4 [192, 168, 1, 0]

ipRange1 :: AddrRange IPv4
ipRange1 = makeAddrRange ipAddr1 24

ipRangeStr1 :: String
ipRangeStr1 = "10.0.0.0/24"

ipRange2 :: AddrRange IPv4
ipRange2 = makeAddrRange ipAddr2 16

ipRangeStr2 :: String
ipRangeStr2 = "192.168.1.0/16"

sn, sn2 :: SegmentName
sn = SegmentName "seg"
sn2 = SegmentName "seg2"

alice, bob, carol, dave :: PeerName
alice = PeerName "alice"
bob = PeerName "bob"
carol = PeerName "carol"
dave = PeerName "dave"

sampleEndpoint :: Endpoint
sampleEndpoint = Endpoint (HostName "vpn.example.com") (Port 51820)

sampleCidr :: AddrRange IPv4
sampleCidr = ipRange1

sampleNetSpec :: NetworkSpec
sampleNetSpec = NetworkSpec Nothing sampleCidr

-- | Peer with an optional endpoint; all other fields are defaults.
mkPeer :: Maybe Endpoint -> PeerSpec
mkPeer ep = PeerSpec ep Nothing Nothing Nothing []

-- | Peer with an optional explicit address; all other fields are defaults.
mkPeerAddr :: Maybe IPv4 -> PeerSpec
mkPeerAddr addr = PeerSpec Nothing Nothing Nothing addr []

-- | Build a Network from peer and segment lists.
mkNetwork ::
  [(PeerName, PeerSpec)] ->
  [(SegmentName, SegmentSpec)] ->
  Network
mkNetwork ps segs =
  Network sampleNetSpec (Map.fromList ps) (Map.fromList segs)

-- | Build a segmentless Network with the given CIDR and peers.
mkAddrNetwork :: AddrRange IPv4 -> [(PeerName, PeerSpec)] -> Network
mkAddrNetwork netCidr ps =
  Network (NetworkSpec Nothing netCidr) (Map.fromList ps) Map.empty

-- | Extract the accumulated errors of a failed validation (empty on success).
failureErrors :: Validation (NonEmpty e) a -> [e]
failureErrors (Failure es) = NE.toList es
failureErrors (Success _) = []

-- | A valid allocator input: a CIDR with a peer map whose size never exceeds
-- the addressable capacity, and whose explicit addresses are distinct, in
-- range, and non-reserved. Constructed directly (no generate-and-filter).
data AllocCase = AllocCase
  { acRange :: AddrRange IPv4,
    acPeers :: Map PeerName PeerSpec
  }
  deriving (Eq, Show)

instance Arbitrary AllocCase where
  arbitrary = genAllocCase

genAllocCase :: Gen AllocCase
genAllocCase = do
  prefix <- choose (20, 30)
  baseW <- arbitrary
  let mask = complement (1 `shiftL` (32 - prefix) - 1) :: Word32
      networkW = baseW .&. mask
      range = makeAddrRange (toIPv4w networkW) prefix
      capacity = (2 ^ (32 - prefix)) - 2 :: Int -- usable hosts (prefix <= 30)
  n <- choose (0, min capacity 64)
  names <- genDistinct n genName
  shuffledNames <- shuffle names
  numExplicit <- choose (0, n)
  offsets <- genDistinct numExplicit (choose (1, capacity))
  let (expNames, dynNames) = splitAt numExplicit shuffledNames
      explicitPeers =
        zipWith
          (\nm off -> (nm, mkPeerAddr (Just (toIPv4w (networkW + fromIntegral off)))))
          expNames
          offsets
      dynPeers = [(nm, mkPeer Nothing) | nm <- dynNames]
  pure (AllocCase range (Map.fromList (explicitPeers ++ dynPeers)))

-- | A valid peer name from @[a-z0-9]+@ (a subset of the spec grammar; no @-@).
genName :: Gen PeerName
genName = do
  len <- choose (1, 8)
  cs <- vectorOf len (elements (['a' .. 'z'] ++ ['0' .. '9']))
  pure (PeerName (T.pack cs))

-- | Draw @k@ distinct values from a generator (rejection on collision).
genDistinct :: (Ord a) => Int -> Gen a -> Gen [a]
genDistinct k gen = go Set.empty
 where
  go acc
    | Set.size acc >= k = pure (Set.toList acc)
    | otherwise = do
        x <- gen
        go (Set.insert x acc)
