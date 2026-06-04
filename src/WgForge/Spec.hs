module WgForge.Spec (
  Network (..),
  NetworkSpec (..),
  PeerName (..),
  PeerSpec (..),
  Endpoint (..),
  HostOrIp (..),
  Port (..),
  SegmentName (..),
  SegmentSpec (..),
  AllowedIpsMode (..),
)
where

import Data.IP (AddrRange, IPv4)
import Data.Map (Map)
import Data.Text (Text)
import Data.Word (Word16)

-- | Top-level parsed network document.
data Network = Network
  { network :: NetworkSpec,
    peers :: Map PeerName PeerSpec,
    segments :: Map SegmentName SegmentSpec
  }
  deriving (Eq, Show)

-- | Global network metadata.
data NetworkSpec = NetworkSpec
  { name :: Maybe Text,
    -- | Address pool for automatic IP allocation.
    cidr :: AddrRange IPv4
  }
  deriving (Eq, Show)

-- | Unique peer identifier.
newtype PeerName = PeerName Text deriving (Eq, Ord, Show)

-- | Per-peer configuration declared in the spec.
data PeerSpec = PeerSpec
  { -- | Public endpoint; absent for peers behind NAT.
    endpoint :: Maybe Endpoint,
    listenPort :: Maybe Port,
    -- | Keepalive interval in seconds.
    persistentKeepalive :: Maybe Word16,
    -- | Static address; allocated automatically if absent.
    address :: Maybe IPv4,
    tags :: [Text]
  }
  deriving (Eq, Show)

-- | A reachable host/port pair used as a WireGuard endpoint.
data Endpoint = Endpoint {host :: HostOrIp, port :: Port}
  deriving (Eq, Show)

-- | Endpoint address as either a DNS hostname or a literal IPv4.
data HostOrIp = HostName Text | HostIp IPv4
  deriving (Eq, Show)

-- | UDP port number.
newtype Port = Port Word16
  deriving (Eq, Ord, Show)

-- | Unique segment identifier.
newtype SegmentName = SegmentName Text deriving (Eq, Ord, Show)

-- | Topology of a network segment.
data SegmentSpec
  = -- | Every peer connects to every other peer.
    FullMesh
      [PeerName]
  | -- | Spokes connect only through hubs.
    HubSpoke
      -- | Hubs
      [PeerName]
      -- | Spokes
      [PeerName]
      AllowedIpsMode
  | -- | Hubs relay traffic; spokes are leaf nodes.
    Relay
      -- | Relays
      [PeerName]
      -- | Leaves
      [PeerName]
      AllowedIpsMode
  deriving (Eq, Show)

-- | Controls what goes into @AllowedIPs@ for peers in a segment.
data AllowedIpsMode
  = -- | Only peer addresses.
    Peers
  | -- | The entire subnet CIDR.
    Subnet
  | -- | Default route (@0.0.0.0/0@); routes all traffic through the tunnel.
    Internet
  deriving (Eq, Ord, Show)
