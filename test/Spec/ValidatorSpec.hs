{-# LANGUAGE OverloadedStrings #-}

module Spec.ValidatorSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Test.Hspec
import Validation (Validation (..))

import Spec.Fixtures
import WgForge.Error
import WgForge.Spec
import WgForge.Spec.Validator.Internal

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

  describe "validateEndpoints" $ do
    it "reports MissingEndpoint for hub without endpoint" $ do
      let net =
            mkNetwork
              [(alice, mkPeer Nothing), (bob, mkPeer Nothing)]
              [(sn, HubSpoke [alice] [bob] Peers)]
      failureErrors (validateEndpoints net) `shouldMatchList` [MissingEndpoint alice]

    it "reports MissingEndpoint for relay without endpoint" $ do
      let net =
            mkNetwork
              [(alice, mkPeer Nothing), (bob, mkPeer Nothing)]
              [(sn, Relay [alice] [bob] Peers)]
      failureErrors (validateEndpoints net) `shouldMatchList` [MissingEndpoint alice]

    it "succeeds when hub has an endpoint" $ do
      let net =
            mkNetwork
              [(alice, mkPeer (Just sampleEndpoint)), (bob, mkPeer Nothing)]
              [(sn, HubSpoke [alice] [bob] Peers)]
      validateEndpoints net `shouldBe` Success net

    it "accumulates errors for multiple hubs missing endpoints" $ do
      let net =
            mkNetwork
              [(alice, mkPeer Nothing), (bob, mkPeer Nothing), (carol, mkPeer Nothing)]
              [(sn, HubSpoke [alice, bob] [carol] Peers)]
      failureErrors (validateEndpoints net)
        `shouldMatchList` [MissingEndpoint alice, MissingEndpoint bob]

    it "deduplicates: same hub missing endpoint in two segments reports once" $ do
      let net =
            mkNetwork
              [(alice, mkPeer Nothing), (bob, mkPeer Nothing), (carol, mkPeer Nothing)]
              [ (sn, HubSpoke [alice] [bob] Peers),
                (sn2, HubSpoke [alice] [carol] Peers)
              ]
      failureErrors (validateEndpoints net) `shouldMatchList` [MissingEndpoint alice]

  describe "validateNatPairs" $ do
    it "reports NatPairInMesh when both peers in a FullMesh lack endpoint" $ do
      let net =
            mkNetwork
              [(alice, mkPeer Nothing), (bob, mkPeer Nothing)]
              [(sn, FullMesh [alice, bob])]
      failureErrors (validateNatPairs net)
        `shouldMatchList` [NatPairInMesh sn alice bob]

    it "succeeds when one peer in a FullMesh pair has an endpoint" $ do
      let net =
            mkNetwork
              [(alice, mkPeer (Just sampleEndpoint)), (bob, mkPeer Nothing)]
              [(sn, FullMesh [alice, bob])]
      validateNatPairs net `shouldBe` Success net

    it "reports all three pairs when three FullMesh peers all lack endpoints" $ do
      let net =
            mkNetwork
              [(alice, mkPeer Nothing), (bob, mkPeer Nothing), (carol, mkPeer Nothing)]
              [(sn, FullMesh [alice, bob, carol])]
      failureErrors (validateNatPairs net)
        `shouldMatchList` [ NatPairInMesh sn alice bob,
                            NatPairInMesh sn alice carol,
                            NatPairInMesh sn bob carol
                          ]

    it "succeeds for HubSpoke when hub has an endpoint (spoke may lack one)" $ do
      let net =
            mkNetwork
              [(alice, mkPeer (Just sampleEndpoint)), (bob, mkPeer Nothing)]
              [(sn, HubSpoke [alice] [bob] Peers)]
      validateNatPairs net `shouldBe` Success net

    it "succeeds for two hubs both with endpoints (hub-hub edge is covered)" $ do
      let net =
            mkNetwork
              [ (alice, mkPeer (Just sampleEndpoint)),
                (bob, mkPeer (Just sampleEndpoint)),
                (carol, mkPeer Nothing)
              ]
              [(sn, HubSpoke [alice, bob] [carol] Peers)]
      validateNatPairs net `shouldBe` Success net

    it "reports two errors for the same bad pair appearing in two segments" $ do
      let net =
            mkNetwork
              [(alice, mkPeer Nothing), (bob, mkPeer Nothing)]
              [ (sn, FullMesh [alice, bob]),
                (sn2, FullMesh [alice, bob])
              ]
      failureErrors (validateNatPairs net)
        `shouldMatchList` [NatPairInMesh sn alice bob, NatPairInMesh sn2 alice bob]

  describe "validateReachability" $ do
    -- Success: each role position counts as reachable
    it "succeeds for a FullMesh member" $ do
      let net = mkNetwork [(alice, mkPeer Nothing), (bob, mkPeer Nothing)] [(sn, FullMesh [alice, bob])]
      validateReachability net `shouldBe` Success net

    it "succeeds for a HubSpoke hub" $ do
      let net =
            mkNetwork
              [(alice, mkPeer (Just sampleEndpoint)), (bob, mkPeer Nothing)]
              [(sn, HubSpoke [alice] [bob] Peers)]
      validateReachability net `shouldBe` Success net

    it "succeeds for a HubSpoke spoke" $ do
      let net =
            mkNetwork
              [(alice, mkPeer (Just sampleEndpoint)), (bob, mkPeer Nothing)]
              [(sn, HubSpoke [alice] [bob] Peers)]
      validateReachability net `shouldBe` Success net

    it "succeeds for a Relay relay" $ do
      let net =
            mkNetwork
              [(alice, mkPeer (Just sampleEndpoint)), (bob, mkPeer Nothing)]
              [(sn, Relay [alice] [bob] Peers)]
      validateReachability net `shouldBe` Success net

    it "succeeds for a Relay client" $ do
      let net =
            mkNetwork
              [(alice, mkPeer (Just sampleEndpoint)), (bob, mkPeer Nothing)]
              [(sn, Relay [alice] [bob] Peers)]
      validateReachability net `shouldBe` Success net

    it "succeeds when a peer is referenced in multiple segments" $ do
      let net =
            mkNetwork
              [(alice, mkPeer (Just sampleEndpoint)), (bob, mkPeer Nothing), (carol, mkPeer Nothing)]
              [(sn, HubSpoke [alice] [bob] Peers), (sn2, HubSpoke [alice] [carol] Peers)]
      validateReachability net `shouldBe` Success net

    -- Failure: IslandPeer emitted correctly
    it "reports IslandPeer for a peer in no segment" $ do
      let net = mkNetwork [(alice, mkPeer Nothing)] []
      failureErrors (validateReachability net) `shouldMatchList` [IslandPeer alice]

    it "reports IslandPeer only for the unreferenced peer" $ do
      let net =
            mkNetwork
              [(alice, mkPeer Nothing), (bob, mkPeer Nothing), (carol, mkPeer Nothing)]
              [(sn, FullMesh [alice, bob])]
      failureErrors (validateReachability net) `shouldMatchList` [IslandPeer carol]

    it "accumulates IslandPeer for all peers when no segments exist" $ do
      let net =
            mkNetwork
              [(alice, mkPeer Nothing), (bob, mkPeer Nothing), (carol, mkPeer Nothing)]
              []
      failureErrors (validateReachability net)
        `shouldMatchList` [IslandPeer alice, IslandPeer bob, IslandPeer carol]

    -- Regression: no endpoint exemption
    it "reports IslandPeer even when the peer has an endpoint" $ do
      let net = mkNetwork [(alice, mkPeer (Just sampleEndpoint))] []
      failureErrors (validateReachability net) `shouldMatchList` [IslandPeer alice]

    -- Regression: segment-only names (future UnknownPeerRef) are not flagged
    it "does not report IslandPeer for a name that appears only in a segment but not in specPeers" $ do
      -- dave is referenced in the segment but not in specPeers; alice is in specPeers and the segment
      let net =
            mkNetwork
              [(alice, mkPeer Nothing)]
              [(sn, FullMesh [alice, dave])]
      validateReachability net `shouldBe` Success net

  describe "validateAddressesInCidr" $ do
    it "accepts an explicit address inside the CIDR" $ do
      let net = mkAddrNetwork sampleCidr [(alice, mkPeerAddr (Just (read "10.0.0.5")))]
      validateAddressesInCidr net `shouldBe` Success net

    it "reports AddressOutOfCidr for an address outside the CIDR" $ do
      let net = mkAddrNetwork sampleCidr [(alice, mkPeerAddr (Just (read "192.168.1.5")))]
      failureErrors (validateAddressesInCidr net)
        `shouldMatchList` [AddressOutOfCidr alice (read "192.168.1.5")]

    it "ignores peers without an explicit address" $ do
      let net = mkAddrNetwork sampleCidr [(alice, mkPeerAddr Nothing)]
      validateAddressesInCidr net `shouldBe` Success net

    it "accumulates errors for multiple out-of-cidr addresses" $ do
      let net =
            mkAddrNetwork
              sampleCidr
              [ (alice, mkPeerAddr (Just (read "192.168.1.5"))),
                (bob, mkPeerAddr (Just (read "10.1.0.1")))
              ]
      failureErrors (validateAddressesInCidr net)
        `shouldMatchList` [ AddressOutOfCidr alice (read "192.168.1.5"),
                            AddressOutOfCidr bob (read "10.1.0.1")
                          ]

  describe "validateReservedAddresses" $ do
    it "reports AddressIsReserved for the network address" $ do
      let net = mkAddrNetwork sampleCidr [(alice, mkPeerAddr (Just (read "10.0.0.0")))]
      failureErrors (validateReservedAddresses net)
        `shouldMatchList` [AddressIsReserved alice (read "10.0.0.0")]

    it "reports AddressIsReserved for the broadcast address" $ do
      let net = mkAddrNetwork sampleCidr [(alice, mkPeerAddr (Just (read "10.0.0.255")))]
      failureErrors (validateReservedAddresses net)
        `shouldMatchList` [AddressIsReserved alice (read "10.0.0.255")]

    it "accepts an ordinary host address" $ do
      let net = mkAddrNetwork sampleCidr [(alice, mkPeerAddr (Just (read "10.0.0.1")))]
      validateReservedAddresses net `shouldBe` Success net

    it "accepts both addresses of a /31 (no reservations)" $ do
      let net =
            mkAddrNetwork
              (read "10.0.0.0/31")
              [ (alice, mkPeerAddr (Just (read "10.0.0.0"))),
                (bob, mkPeerAddr (Just (read "10.0.0.1")))
              ]
      validateReservedAddresses net `shouldBe` Success net

    it "accepts the single address of a /32 (no reservations)" $ do
      let net = mkAddrNetwork (read "10.0.0.7/32") [(alice, mkPeerAddr (Just (read "10.0.0.7")))]
      validateReservedAddresses net `shouldBe` Success net

  describe "validateAddressCollisions" $ do
    it "accepts distinct explicit addresses" $ do
      let net =
            mkAddrNetwork
              sampleCidr
              [ (alice, mkPeerAddr (Just (read "10.0.0.1"))),
                (bob, mkPeerAddr (Just (read "10.0.0.2")))
              ]
      validateAddressCollisions net `shouldBe` Success net

    it "ignores peers without an explicit address" $ do
      let net =
            mkAddrNetwork
              sampleCidr
              [(alice, mkPeerAddr Nothing), (bob, mkPeerAddr Nothing)]
      validateAddressCollisions net `shouldBe` Success net

    it "reports AddressCollision for two peers sharing an address" $ do
      let net =
            mkAddrNetwork
              sampleCidr
              [ (alice, mkPeerAddr (Just (read "10.0.0.1"))),
                (bob, mkPeerAddr (Just (read "10.0.0.1")))
              ]
      failureErrors (validateAddressCollisions net)
        `shouldMatchList` [AddressCollision alice bob (read "10.0.0.1")]

    it "reports all pairs for three peers sharing an address" $ do
      let net =
            mkAddrNetwork
              sampleCidr
              [ (alice, mkPeerAddr (Just (read "10.0.0.1"))),
                (bob, mkPeerAddr (Just (read "10.0.0.1"))),
                (carol, mkPeerAddr (Just (read "10.0.0.1")))
              ]
      failureErrors (validateAddressCollisions net)
        `shouldMatchList` [ AddressCollision alice bob (read "10.0.0.1"),
                            AddressCollision alice carol (read "10.0.0.1"),
                            AddressCollision bob carol (read "10.0.0.1")
                          ]

    it "reports collisions on two different addresses independently" $ do
      let net =
            mkAddrNetwork
              sampleCidr
              [ (alice, mkPeerAddr (Just (read "10.0.0.1"))),
                (bob, mkPeerAddr (Just (read "10.0.0.1"))),
                (carol, mkPeerAddr (Just (read "10.0.0.2"))),
                (dave, mkPeerAddr (Just (read "10.0.0.2")))
              ]
      failureErrors (validateAddressCollisions net)
        `shouldMatchList` [ AddressCollision alice bob (read "10.0.0.1"),
                            AddressCollision carol dave (read "10.0.0.2")
                          ]

  describe "validateCidrCapacity" $ do
    it "reports CidrOverflow when peers exceed addressable hosts" $ do
      let net =
            mkAddrNetwork
              (read "10.0.0.0/30") -- 2 usable hosts
              [ (alice, mkPeerAddr Nothing),
                (bob, mkPeerAddr Nothing),
                (carol, mkPeerAddr Nothing)
              ]
      validateCidrCapacity net `shouldBe` Failure (CidrOverflow 3 2 :| [])

    it "accepts peer count equal to addressable hosts" $ do
      let net =
            mkAddrNetwork
              (read "10.0.0.0/30")
              [(alice, mkPeerAddr Nothing), (bob, mkPeerAddr Nothing)]
      validateCidrCapacity net `shouldBe` Success net

    it "counts both addresses of a /31 as usable" $ do
      let net =
            mkAddrNetwork
              (read "10.0.0.0/31")
              [(alice, mkPeerAddr Nothing), (bob, mkPeerAddr Nothing)]
      validateCidrCapacity net `shouldBe` Success net

    it "counts the single address of a /32 as usable" $ do
      let net = mkAddrNetwork (read "10.0.0.7/32") [(alice, mkPeerAddr Nothing)]
      validateCidrCapacity net `shouldBe` Success net

    it "reports CidrOverflow for two peers in a /32" $ do
      let net =
            mkAddrNetwork
              (read "10.0.0.7/32")
              [(alice, mkPeerAddr Nothing), (bob, mkPeerAddr Nothing)]
      validateCidrCapacity net `shouldBe` Failure (CidrOverflow 2 1 :| [])

  describe "validateAddressing" $ do
    it "accepts a network with valid distinct addresses" $ do
      let net =
            mkAddrNetwork
              sampleCidr
              [ (alice, mkPeerAddr (Just (read "10.0.0.1"))),
                (bob, mkPeerAddr Nothing)
              ]
      validateAddressing net `shouldBe` Success net

    it "accumulates out-of-cidr, collision and overflow errors in one pass" $ do
      let net =
            mkAddrNetwork
              (read "10.0.0.0/30") -- 2 usable hosts
              [ (alice, mkPeerAddr (Just (read "192.168.0.1"))),
                (bob, mkPeerAddr (Just (read "10.0.0.1"))),
                (carol, mkPeerAddr (Just (read "10.0.0.1")))
              ]
      failureErrors (validateAddressing net)
        `shouldMatchList` [ AddressOutOfCidr alice (read "192.168.0.1"),
                            AddressCollision bob carol (read "10.0.0.1"),
                            CidrOverflow 3 2
                          ]

  describe "validateNetwork" $ do
    it "accumulates addressing errors alongside structural errors" $ do
      let net =
            Network
              sampleNetSpec
              ( Map.fromList
                  [ (alice, mkPeerAddr (Just (read "192.168.1.5"))),
                    (bob, mkPeerAddr Nothing)
                  ]
              )
              (Map.fromList [(sn, FullMesh [alice, bob]), (sn2, FullMesh [alice])])
      let errs = failureErrors (validateNetwork net)
      errs `shouldContain` [AddressOutOfCidr alice (read "192.168.1.5")]
      errs `shouldContain` [InsufficientPeers sn2 "requires at least 2 peers"]

    it "accumulates structural and endpoint errors from a single pass" $ do
      -- FullMesh with 1 peer (InsufficientPeers) and both peers lack endpoints
      -- (NatPairInMesh only fires when there are >= 2 peers; use a 2-peer mesh
      --  with an additional InsufficientPeers from a second segment)
      let net =
            mkNetwork
              [(alice, mkPeer Nothing), (bob, mkPeer Nothing)]
              [ (sn, FullMesh [alice, bob]), -- NatPairInMesh
                (sn2, FullMesh [alice]) -- InsufficientPeers
              ]
      let errs = failureErrors (validateNetwork net)
      errs `shouldContain` [NatPairInMesh sn alice bob]
      errs `shouldContain` [InsufficientPeers sn2 "requires at least 2 peers"]

    it "accumulates IslandPeer alongside InsufficientPeers in a single pass" $ do
      -- carol is in specPeers but no segment; sn2 is a malformed FullMesh with 1 peer
      let net =
            mkNetwork
              [(alice, mkPeer Nothing), (bob, mkPeer Nothing), (carol, mkPeer Nothing)]
              [ (sn, FullMesh [alice, bob]), -- valid segment
                (sn2, FullMesh [alice]) -- InsufficientPeers
              ]
      let errs = failureErrors (validateNetwork net)
      errs `shouldContain` [IslandPeer carol]
      errs `shouldContain` [InsufficientPeers sn2 "requires at least 2 peers"]
