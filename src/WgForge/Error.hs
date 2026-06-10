module WgForge.Error (SpecError (..), ValidationError (..)) where

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
  deriving (Eq, Show)
