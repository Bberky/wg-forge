module WgForge.Error (SpecError (..), ValidationError (..)) where

import Data.IP (IPv4)

import WgForge.Spec

-- | Errors produced while loading a network spec.
data SpecError
  = -- | File could not be read (missing, permissions, ...).
    SpecIoError String
  | -- | Input is not well-formed YAML.
    YamlSyntaxError String
  | -- | YAML is well-formed but does not match the spec schema.
    SpecParseError String
  deriving (Eq, Show)

data ValidationError
  = -- | Segment does not contain enough peers to satisfy its topology requirements.
    InsufficientPeers SegmentName String
  | -- | Peer is assigned to both hub and spoke roles, or to both relay and client roles.
    PeerBothRoles SegmentName PeerName
  | -- | Peer is missing an endpoint where endpoint is required (e.g. for a hub or relay).
    MissingEndpoint PeerName
  | -- | A tunnel (A, B) exists in a segment but neither peer has an endpoint.
    NatPairInMesh SegmentName PeerName PeerName
  | -- | A peer is declared in 'specPeers' but referenced by no segment,
    --   so the compiled output would contain no tunnels for it.
    IslandPeer PeerName
  | -- | A peer is assigned a static address outside the network CIDR.
    AddressOutOfCidr PeerName IPv4
  | -- | A peer is assigned a static address that is reserved.
    AddressIsReserved PeerName IPv4
  | -- | Two peers are assigned the same static address.
    AddressCollision PeerName PeerName IPv4
  | -- | The network CIDR is too small to accommodate all peers.
    CidrOverflow Int Int
  deriving (Eq, Show)
