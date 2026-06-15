module WgForge.Compiler (
  CompiledPeer (..),
  CompiledPeerEntry (..),
  compile,
  segmentContribution,
) where

import Data.IP (AddrRange, IPv4, makeAddrRange, toIPv4w)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word16)

import WgForge.Key
import WgForge.Spec

data CompiledPeer = CompiledPeer
  { ifacePrivKey :: PrivateKey,
    ifaceAddress :: IPv4,
    ifaceListenPort :: Maybe Port,
    peerEntries :: Map PeerName CompiledPeerEntry
  }
  deriving (Eq, Show)

data CompiledPeerEntry = CompiledPeerEntry
  { pubKey :: PublicKey,
    endpoint :: Maybe Endpoint,
    allowedIps :: Set (AddrRange IPv4),
    keepalive :: Maybe Word16
  }
  deriving (Eq, Show)

compile ::
  Map PeerName PrivateKey -> Map PeerName IPv4 -> Network -> Map PeerName CompiledPeer
compile keys addrs net =
  let
    pubs = Map.map derivePublicKey keys
    contrib =
      Map.unionsWith (Map.unionWith Set.union) $
        [segmentContribution (network net) addrs seg | seg <- Map.elems (segments net)]
   in
    Map.mapWithKey
      ( \p spec ->
          CompiledPeer
            { ifacePrivKey = keys Map.! p,
              ifaceAddress = addrs Map.! p,
              ifaceListenPort = listenPort spec,
              peerEntries =
                Map.fromList
                  [ (t, CompiledPeerEntry (pubs Map.! t) (WgForge.Spec.endpoint spec) ips (persistentKeepalive spec))
                  | (t, ips) <- Map.toList (contrib Map.! p)
                  ]
            }
      )
      (peers net)

-- | A single directed @(source, target, AllowedIPs)@ edge contributed by a segment.
type Edge = (PeerName, PeerName, Set (AddrRange IPv4))

-- | Per-source @AllowedIPs@ keyed by target peer, as folded across segments.
type Contribution = Map PeerName (Map PeerName (Set (AddrRange IPv4)))

-- | Whether a leaf reaches only the central node ('Isolation', hub-and-spoke)
--   or also its sibling leaves through the central ('Transit', relay).
data LeafKind = Isolation | Transit

-- | Directed @AllowedIPs@ edges a single segment contributes to each source peer.
segmentContribution ::
  NetworkSpec ->
  Map PeerName IPv4 ->
  SegmentSpec ->
  Map PeerName (Map PeerName (Set (AddrRange IPv4)))
segmentContribution _ addrs (FullMesh ps) =
  fromEdges (fullMeshEdges addrs ps)
segmentContribution net addrs (HubSpoke hubs spokes mode) =
  fromEdges (centralLeafEdges net addrs Isolation hubs spokes mode)
segmentContribution net addrs (Relay relays clients mode) =
  fromEdges (centralLeafEdges net addrs Transit relays clients mode)

-- | Fold a directed edge list into the nested per-source contribution map,
--   unioning @AllowedIPs@ whenever an edge repeats.
fromEdges :: [Edge] -> Contribution
fromEdges =
  Map.fromListWith (Map.unionWith Set.union)
    . map (\(s, t, r) -> (s, Map.singleton t r))

-- | The @/32@ host range of a peer's allocated address.
hostOf :: Map PeerName IPv4 -> PeerName -> AddrRange IPv4
hostOf addrs p = makeAddrRange (addrs Map.! p) 32

-- | Symmetric @/32@ edges between every distinct pair of the given peers.
fullMeshEdges :: Map PeerName IPv4 -> [PeerName] -> [Edge]
fullMeshEdges addrs ps =
  [ (s, t, Set.singleton (hostOf addrs t))
  | s <- ps,
    t <- ps,
    s /= t
  ]

-- | Edges shared by hub-and-spoke and relay topologies: central→leaf @/32@,
--   leaf→central via 'leafRoutes', plus the central↔central mesh.
centralLeafEdges ::
  NetworkSpec -> Map PeerName IPv4 -> LeafKind -> [PeerName] -> [PeerName] -> AllowedIpsMode -> [Edge]
centralLeafEdges net addrs kind centrals leaves mode =
  [(c, l, Set.singleton (hostOf addrs l)) | c <- centrals, l <- leaves]
    ++ [(l, c, leafRoutes net mode (peersRoute c l)) | c <- centrals, l <- leaves]
    ++ fullMeshEdges addrs centrals
 where
  peersRoute central leaf = case kind of
    Isolation -> Set.singleton (hostOf addrs central)
    Transit ->
      Set.insert
        (hostOf addrs central)
        (Set.fromList [hostOf addrs s | s <- leaves, s /= leaf])

-- | The @AllowedIPs@ a leaf advertises for its central peer. @subnet@/@internet@
--   route a broad range regardless of topology; the hub-vs-relay distinction
--   lives entirely in the caller-supplied @peersRoute@, used only in 'Peers' mode.
leafRoutes :: NetworkSpec -> AllowedIpsMode -> Set (AddrRange IPv4) -> Set (AddrRange IPv4)
leafRoutes net mode peersRoute = case mode of
  Subnet -> Set.singleton (cidr net)
  Internet -> Set.singleton (makeAddrRange (toIPv4w 0) 0)
  Peers -> peersRoute
