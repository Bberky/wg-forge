{-# LANGUAGE OverloadedStrings #-}

module Spec.ValidatorSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Test.Hspec
import Validation (Validation (..))

import WgForge.Error (ValidationError (..))
import WgForge.Spec (
  AllowedIpsMode (..),
  PeerName (..),
  SegmentName (..),
  SegmentSpec (..),
 )
import WgForge.Spec.Validator (validatePeerRoles, validateSegmentSpec)

sn :: SegmentName
sn = SegmentName "seg"

alice, bob, carol, dave :: PeerName
alice = PeerName "alice"
bob = PeerName "bob"
carol = PeerName "carol"
dave = PeerName "dave"

failureErrors :: Validation (NonEmpty e) a -> [e]
failureErrors (Failure es) = NE.toList es
failureErrors (Success _) = []

spec :: Spec
spec = do
  describe "validateSegmentSpec" $ do
    describe "FullMesh" $ do
      it "rejects empty peer list" $
        validateSegmentSpec sn (FullMesh [])
          `shouldBe` Failure (InsufficientPeers sn "requires at least 2 peers" :| [])
      it "rejects a single peer" $
        validateSegmentSpec sn (FullMesh [alice])
          `shouldBe` Failure (InsufficientPeers sn "requires at least 2 peers" :| [])
      it "accepts two peers" $ do
        let seg = FullMesh [alice, bob]
        validateSegmentSpec sn seg `shouldBe` Success seg

    describe "HubSpoke" $ do
      it "rejects empty hub list" $
        validateSegmentSpec sn (HubSpoke [] [alice] Peers)
          `shouldBe` Failure (InsufficientPeers sn "requires at least 1 hub" :| [])
      it "rejects empty spoke list" $
        -- regression: old code required >= 2 spokes; one spoke must now be valid
        validateSegmentSpec sn (HubSpoke [alice] [] Peers)
          `shouldBe` Failure (InsufficientPeers sn "requires at least 1 spoke" :| [])
      it "accepts 1 hub and 1 spoke" $ do
        let seg = HubSpoke [alice] [bob] Peers
        validateSegmentSpec sn seg `shouldBe` Success seg
      it "accepts multiple hubs and spokes" $ do
        let seg = HubSpoke [alice, bob] [carol, dave] Peers
        validateSegmentSpec sn seg `shouldBe` Success seg
      it "rejects a peer in both hub and spoke roles" $
        validateSegmentSpec sn (HubSpoke [alice] [alice] Peers)
          `shouldBe` Failure (PeerBothRoles sn alice :| [])
      it "accumulates errors for multiple role conflicts" $ do
        let result = validateSegmentSpec sn (HubSpoke [alice, bob] [alice, bob] Peers)
        failureErrors result
          `shouldMatchList` [PeerBothRoles sn alice, PeerBothRoles sn bob]
      it "accumulates both a count error and a role-conflict error" $ do
        -- no hubs, bob appears in both lists
        let result = validateSegmentSpec sn (HubSpoke [] [bob] Peers)
        -- InsufficientPeers (no hubs); no role conflict since hubs list is empty
        failureErrors result
          `shouldMatchList` [InsufficientPeers sn "requires at least 1 hub"]

    describe "Relay" $ do
      it "rejects empty relay list" $
        validateSegmentSpec sn (Relay [] [alice] Peers)
          `shouldBe` Failure (InsufficientPeers sn "requires at least 1 relay" :| [])
      it "rejects empty client list" $
        -- regression: old code required >= 2 clients; one client must now be valid
        validateSegmentSpec sn (Relay [alice] [] Peers)
          `shouldBe` Failure (InsufficientPeers sn "requires at least 1 client" :| [])
      it "accepts 1 relay and 1 client" $ do
        let seg = Relay [alice] [bob] Peers
        validateSegmentSpec sn seg `shouldBe` Success seg
      it "rejects a peer in both relay and client roles" $
        -- regression: old validateSegmentSpec never called validatePeerRoles for Relay
        validateSegmentSpec sn (Relay [alice] [alice] Peers)
          `shouldBe` Failure (PeerBothRoles sn alice :| [])
      it "accumulates errors for multiple relay/client conflicts" $ do
        let result = validateSegmentSpec sn (Relay [alice, bob] [alice, bob] Peers)
        failureErrors result
          `shouldMatchList` [PeerBothRoles sn alice, PeerBothRoles sn bob]

  describe "validatePeerRoles" $ do
    it "FullMesh always succeeds" $ do
      let seg = FullMesh [alice, bob, carol]
      validatePeerRoles sn seg `shouldBe` Success seg

    describe "HubSpoke" $ do
      it "succeeds with disjoint hubs and spokes" $ do
        let seg = HubSpoke [alice, bob] [carol, dave] Peers
        validatePeerRoles sn seg `shouldBe` Success seg
      it "reports one conflict" $
        validatePeerRoles sn (HubSpoke [alice, bob] [bob, carol] Peers)
          `shouldBe` Failure (PeerBothRoles sn bob :| [])
      it "reports multiple conflicts" $ do
        let result = validatePeerRoles sn (HubSpoke [alice, bob] [alice, bob] Peers)
        failureErrors result
          `shouldMatchList` [PeerBothRoles sn alice, PeerBothRoles sn bob]

    describe "Relay" $ do
      it "succeeds with disjoint relays and clients" $ do
        let seg = Relay [alice] [bob, carol] Peers
        validatePeerRoles sn seg `shouldBe` Success seg
      it "reports one conflict" $
        validatePeerRoles sn (Relay [alice, bob] [bob, carol] Peers)
          `shouldBe` Failure (PeerBothRoles sn bob :| [])
      it "reports multiple conflicts" $ do
        let result = validatePeerRoles sn (Relay [alice, bob] [alice, bob] Peers)
        failureErrors result
          `shouldMatchList` [PeerBothRoles sn alice, PeerBothRoles sn bob]
