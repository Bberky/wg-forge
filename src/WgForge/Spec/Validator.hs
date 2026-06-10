module WgForge.Spec.Validator (
  validateNetwork,
  validateSegmentSpec,
  validatePeerRoles,
) where

import Data.Foldable (traverse_)
import Data.List (intersect)
import Data.List.NonEmpty (NonEmpty)
import Validation (Validation, failureIf)

import WgForge.Error (ValidationError (InsufficientPeers, PeerBothRoles))
import WgForge.Spec (Network, SegmentName, SegmentSpec (FullMesh, HubSpoke, Relay))

validateNetwork :: Network -> Validation (NonEmpty ValidationError) Network
validateNetwork = undefined

-- | Validate that each segment has enough peers to satisfy its topology requirements,
-- and that no peer is assigned to conflicting roles.
validateSegmentSpec ::
  SegmentName -> SegmentSpec -> Validation (NonEmpty ValidationError) SegmentSpec
validateSegmentSpec sn seg@(FullMesh peers) =
  seg
    <$ failureIf (length peers < 2) (InsufficientPeers sn "requires at least 2 peers")
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

-- | Validate that no peer is assigned to conflicting roles within the same segment.
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
