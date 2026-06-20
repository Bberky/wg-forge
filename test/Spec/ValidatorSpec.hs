{-# LANGUAGE OverloadedStrings #-}

module Spec.ValidatorSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec
import Validation (Validation (Success))

import Spec.Fixtures (
  alice,
  bob,
  carol,
  dave,
  failureErrors,
  mkAddrNetwork,
  mkNetwork,
  mkPeer,
  mkPeerAddr,
  sampleCidr,
  sampleEndpoint,
  sampleNetSpec,
  sn,
  sn2,
 )
import WgForge.Error (ValidationError (..))
import WgForge.Spec (AllowedIpsMode (..), Network (..), SegmentSpec (..))
import WgForge.Spec.Validator (validateNetwork)

-- | The public validator runs and accumulates every rule at once, so per-rule
--   tests build a network that triggers the rule under test and then inspect the
--   accumulated errors filtered down to the relevant constructor. Acceptance
--   tests either assert full 'Success' (when the network is otherwise valid) or
--   that the rule's constructor is absent from the accumulated errors.
spec :: Spec
spec = describe "WgForge.Spec.Validator.validateNetwork" $ do
  describe "segment peer counts (InsufficientPeers)" $ do
    it "rejects a full-mesh with no peers" $
      insufficient (mkNetwork [] [(sn, FullMesh [])])
        `shouldMatchList` [InsufficientPeers sn "requires at least 2 peers"]
    it "rejects a full-mesh with a single peer" $
      insufficient (mkNetwork [(alice, mkPeer Nothing)] [(sn, FullMesh [alice])])
        `shouldMatchList` [InsufficientPeers sn "requires at least 2 peers"]
    it "accepts a full-mesh with two peers" $ do
      let net =
            mkNetwork
              [(alice, mkPeer (Just sampleEndpoint)), (bob, mkPeer Nothing)]
              [(sn, FullMesh [alice, bob])]
      validateNetwork net `shouldBe` Success net

    it "rejects a hub-and-spoke with no hubs" $
      insufficient (mkNetwork [(alice, mkPeer Nothing)] [(sn, HubSpoke [] [alice] Peers)])
        `shouldMatchList` [InsufficientPeers sn "requires at least 1 hub"]
    it "rejects a hub-and-spoke with no spokes" $
      insufficient (mkNetwork [(alice, mkPeer (Just sampleEndpoint))] [(sn, HubSpoke [alice] [] Peers)])
        `shouldMatchList` [InsufficientPeers sn "requires at least 1 spoke"]
    it "accepts a hub-and-spoke with one hub and one spoke" $ do
      let net =
            mkNetwork
              [(alice, mkPeer (Just sampleEndpoint)), (bob, mkPeer Nothing)]
              [(sn, HubSpoke [alice] [bob] Peers)]
      validateNetwork net `shouldBe` Success net

    it "rejects a relay with no relays" $
      insufficient (mkNetwork [(alice, mkPeer Nothing)] [(sn, Relay [] [alice] Peers)])
        `shouldMatchList` [InsufficientPeers sn "requires at least 1 relay"]
    it "rejects a relay with no clients" $
      insufficient (mkNetwork [(alice, mkPeer (Just sampleEndpoint))] [(sn, Relay [alice] [] Peers)])
        `shouldMatchList` [InsufficientPeers sn "requires at least 1 client"]
    it "accepts a relay with one relay and one client" $ do
      let net =
            mkNetwork
              [(alice, mkPeer (Just sampleEndpoint)), (bob, mkPeer Nothing)]
              [(sn, Relay [alice] [bob] Peers)]
      validateNetwork net `shouldBe` Success net

  describe "role conflicts (PeerBothRoles)" $ do
    it "rejects a peer that is both hub and spoke" $
      roleConflicts
        (mkNetwork [(alice, mkPeer (Just sampleEndpoint))] [(sn, HubSpoke [alice] [alice] Peers)])
        `shouldMatchList` [PeerBothRoles sn alice]
    it "rejects a peer that is both relay and client" $
      roleConflicts
        (mkNetwork [(alice, mkPeer (Just sampleEndpoint))] [(sn, Relay [alice] [alice] Peers)])
        `shouldMatchList` [PeerBothRoles sn alice]
    it "accumulates a conflict for every dual-role peer in a hub-and-spoke" $
      roleConflicts
        ( mkNetwork
            [(alice, mkPeer (Just sampleEndpoint)), (bob, mkPeer (Just sampleEndpoint))]
            [(sn, HubSpoke [alice, bob] [alice, bob] Peers)]
        )
        `shouldMatchList` [PeerBothRoles sn alice, PeerBothRoles sn bob]
    it "accumulates a conflict for every dual-role peer in a relay" $
      roleConflicts
        ( mkNetwork
            [(alice, mkPeer (Just sampleEndpoint)), (bob, mkPeer (Just sampleEndpoint))]
            [(sn, Relay [alice, bob] [alice, bob] Peers)]
        )
        `shouldMatchList` [PeerBothRoles sn alice, PeerBothRoles sn bob]
    it "accepts disjoint hub and spoke roles" $
      roleConflicts
        ( mkNetwork
            [ (alice, mkPeer (Just sampleEndpoint)),
              (bob, mkPeer (Just sampleEndpoint)),
              (carol, mkPeer Nothing),
              (dave, mkPeer Nothing)
            ]
            [(sn, HubSpoke [alice, bob] [carol, dave] Peers)]
        )
        `shouldBe` []

  describe "endpoint requirements (MissingEndpoint)" $ do
    it "requires an endpoint on a hub" $
      missingEndpoints
        ( mkNetwork
            [(alice, mkPeer Nothing), (bob, mkPeer Nothing)]
            [(sn, HubSpoke [alice] [bob] Peers)]
        )
        `shouldMatchList` [MissingEndpoint alice]
    it "requires an endpoint on a relay" $
      missingEndpoints
        ( mkNetwork
            [(alice, mkPeer Nothing), (bob, mkPeer Nothing)]
            [(sn, Relay [alice] [bob] Peers)]
        )
        `shouldMatchList` [MissingEndpoint alice]
    it "accepts a hub that has an endpoint" $ do
      let net =
            mkNetwork
              [(alice, mkPeer (Just sampleEndpoint)), (bob, mkPeer Nothing)]
              [(sn, HubSpoke [alice] [bob] Peers)]
      validateNetwork net `shouldBe` Success net
    it "accumulates a missing endpoint for every hub lacking one" $
      missingEndpoints
        ( mkNetwork
            [(alice, mkPeer Nothing), (bob, mkPeer Nothing), (carol, mkPeer Nothing)]
            [(sn, HubSpoke [alice, bob] [carol] Peers)]
        )
        `shouldMatchList` [MissingEndpoint alice, MissingEndpoint bob]
    it "reports a hub missing an endpoint across two segments only once" $
      missingEndpoints
        ( mkNetwork
            [(alice, mkPeer Nothing), (bob, mkPeer Nothing), (carol, mkPeer Nothing)]
            [(sn, HubSpoke [alice] [bob] Peers), (sn2, HubSpoke [alice] [carol] Peers)]
        )
        `shouldMatchList` [MissingEndpoint alice]

  describe "NAT pairing (NatPairInMesh)" $ do
    it "rejects a full-mesh pair where both peers lack an endpoint" $
      natPairs (mkNetwork [(alice, mkPeer Nothing), (bob, mkPeer Nothing)] [(sn, FullMesh [alice, bob])])
        `shouldMatchList` [NatPairInMesh sn alice bob]
    it "accepts a full-mesh pair when one peer has an endpoint" $ do
      let net =
            mkNetwork
              [(alice, mkPeer (Just sampleEndpoint)), (bob, mkPeer Nothing)]
              [(sn, FullMesh [alice, bob])]
      validateNetwork net `shouldBe` Success net
    it "reports every endpoint-less full-mesh pair" $
      natPairs
        ( mkNetwork
            [(alice, mkPeer Nothing), (bob, mkPeer Nothing), (carol, mkPeer Nothing)]
            [(sn, FullMesh [alice, bob, carol])]
        )
        `shouldMatchList` [ NatPairInMesh sn alice bob,
                            NatPairInMesh sn alice carol,
                            NatPairInMesh sn bob carol
                          ]
    it "accepts hub-and-spoke when the hub has an endpoint" $ do
      let net =
            mkNetwork
              [(alice, mkPeer (Just sampleEndpoint)), (bob, mkPeer Nothing)]
              [(sn, HubSpoke [alice] [bob] Peers)]
      validateNetwork net `shouldBe` Success net
    it "accepts two hubs that both have endpoints" $ do
      let net =
            mkNetwork
              [ (alice, mkPeer (Just sampleEndpoint)),
                (bob, mkPeer (Just sampleEndpoint)),
                (carol, mkPeer Nothing)
              ]
              [(sn, HubSpoke [alice, bob] [carol] Peers)]
      validateNetwork net `shouldBe` Success net
    it "reports the same bad pair separately in each segment" $
      natPairs
        ( mkNetwork
            [(alice, mkPeer Nothing), (bob, mkPeer Nothing)]
            [(sn, FullMesh [alice, bob]), (sn2, FullMesh [alice, bob])]
        )
        `shouldMatchList` [NatPairInMesh sn alice bob, NatPairInMesh sn2 alice bob]

  describe "reachability (IslandPeer)" $ do
    it "reports a peer that appears in no segment" $
      islands (mkNetwork [(alice, mkPeer Nothing)] [])
        `shouldMatchList` [IslandPeer alice]
    it "reports only the unreferenced peer" $
      islands
        ( mkNetwork
            [(alice, mkPeer Nothing), (bob, mkPeer Nothing), (carol, mkPeer Nothing)]
            [(sn, FullMesh [alice, bob])]
        )
        `shouldMatchList` [IslandPeer carol]
    it "reports every peer when there are no segments" $
      islands
        ( mkNetwork
            [(alice, mkPeer Nothing), (bob, mkPeer Nothing), (carol, mkPeer Nothing)]
            []
        )
        `shouldMatchList` [IslandPeer alice, IslandPeer bob, IslandPeer carol]
    it "reports an island even when it has an endpoint" $
      islands (mkNetwork [(alice, mkPeer (Just sampleEndpoint))] [])
        `shouldMatchList` [IslandPeer alice]
    it "does not flag a name that appears only in a segment, not the peer map" $
      islands (mkNetwork [(alice, mkPeer Nothing)] [(sn, FullMesh [alice, dave])])
        `shouldBe` []

  describe "addresses in CIDR (AddressOutOfCidr)" $ do
    it "accepts an explicit address inside the CIDR" $
      outOfCidr (mkAddrNetwork sampleCidr [(alice, mkPeerAddr (Just (read "10.0.0.5")))])
        `shouldBe` []
    it "rejects an explicit address outside the CIDR" $
      outOfCidr (mkAddrNetwork sampleCidr [(alice, mkPeerAddr (Just (read "192.168.1.5")))])
        `shouldMatchList` [AddressOutOfCidr alice (read "192.168.1.5")]
    it "ignores peers without an explicit address" $
      outOfCidr (mkAddrNetwork sampleCidr [(alice, mkPeerAddr Nothing)])
        `shouldBe` []
    it "accumulates every out-of-CIDR address" $
      outOfCidr
        ( mkAddrNetwork
            sampleCidr
            [ (alice, mkPeerAddr (Just (read "192.168.1.5"))),
              (bob, mkPeerAddr (Just (read "10.1.0.1")))
            ]
        )
        `shouldMatchList` [ AddressOutOfCidr alice (read "192.168.1.5"),
                            AddressOutOfCidr bob (read "10.1.0.1")
                          ]

  describe "reserved addresses (AddressIsReserved)" $ do
    it "rejects the network address" $
      reserved (mkAddrNetwork sampleCidr [(alice, mkPeerAddr (Just (read "10.0.0.0")))])
        `shouldMatchList` [AddressIsReserved alice (read "10.0.0.0")]
    it "rejects the broadcast address" $
      reserved (mkAddrNetwork sampleCidr [(alice, mkPeerAddr (Just (read "10.0.0.255")))])
        `shouldMatchList` [AddressIsReserved alice (read "10.0.0.255")]
    it "accepts an ordinary host address" $
      reserved (mkAddrNetwork sampleCidr [(alice, mkPeerAddr (Just (read "10.0.0.1")))])
        `shouldBe` []
    it "treats both addresses of a /31 as usable" $
      reserved
        ( mkAddrNetwork
            (read "10.0.0.0/31")
            [(alice, mkPeerAddr (Just (read "10.0.0.0"))), (bob, mkPeerAddr (Just (read "10.0.0.1")))]
        )
        `shouldBe` []
    it "treats the single address of a /32 as usable" $
      reserved (mkAddrNetwork (read "10.0.0.7/32") [(alice, mkPeerAddr (Just (read "10.0.0.7")))])
        `shouldBe` []

  describe "address collisions (AddressCollision)" $ do
    it "accepts distinct explicit addresses" $
      collisions
        ( mkAddrNetwork
            sampleCidr
            [(alice, mkPeerAddr (Just (read "10.0.0.1"))), (bob, mkPeerAddr (Just (read "10.0.0.2")))]
        )
        `shouldBe` []
    it "ignores peers without an explicit address" $
      collisions (mkAddrNetwork sampleCidr [(alice, mkPeerAddr Nothing), (bob, mkPeerAddr Nothing)])
        `shouldBe` []
    it "rejects two peers sharing an address" $
      collisions
        ( mkAddrNetwork
            sampleCidr
            [(alice, mkPeerAddr (Just (read "10.0.0.1"))), (bob, mkPeerAddr (Just (read "10.0.0.1")))]
        )
        `shouldMatchList` [AddressCollision alice bob (read "10.0.0.1")]
    it "reports every colliding pair for three peers on one address" $
      collisions
        ( mkAddrNetwork
            sampleCidr
            [ (alice, mkPeerAddr (Just (read "10.0.0.1"))),
              (bob, mkPeerAddr (Just (read "10.0.0.1"))),
              (carol, mkPeerAddr (Just (read "10.0.0.1")))
            ]
        )
        `shouldMatchList` [ AddressCollision alice bob (read "10.0.0.1"),
                            AddressCollision alice carol (read "10.0.0.1"),
                            AddressCollision bob carol (read "10.0.0.1")
                          ]
    it "reports collisions on different addresses independently" $
      collisions
        ( mkAddrNetwork
            sampleCidr
            [ (alice, mkPeerAddr (Just (read "10.0.0.1"))),
              (bob, mkPeerAddr (Just (read "10.0.0.1"))),
              (carol, mkPeerAddr (Just (read "10.0.0.2"))),
              (dave, mkPeerAddr (Just (read "10.0.0.2")))
            ]
        )
        `shouldMatchList` [ AddressCollision alice bob (read "10.0.0.1"),
                            AddressCollision carol dave (read "10.0.0.2")
                          ]

  describe "CIDR capacity (CidrOverflow)" $ do
    it "rejects more peers than addressable hosts" $
      overflow
        ( mkAddrNetwork
            (read "10.0.0.0/30") -- 2 usable hosts
            [(alice, mkPeerAddr Nothing), (bob, mkPeerAddr Nothing), (carol, mkPeerAddr Nothing)]
        )
        `shouldMatchList` [CidrOverflow 3 2]
    it "accepts a peer count equal to the addressable hosts" $
      overflow
        (mkAddrNetwork (read "10.0.0.0/30") [(alice, mkPeerAddr Nothing), (bob, mkPeerAddr Nothing)])
        `shouldBe` []
    it "counts both addresses of a /31 as usable" $
      overflow
        (mkAddrNetwork (read "10.0.0.0/31") [(alice, mkPeerAddr Nothing), (bob, mkPeerAddr Nothing)])
        `shouldBe` []
    it "counts the single address of a /32 as usable" $
      overflow (mkAddrNetwork (read "10.0.0.7/32") [(alice, mkPeerAddr Nothing)])
        `shouldBe` []
    it "rejects two peers in a /32" $
      overflow
        (mkAddrNetwork (read "10.0.0.7/32") [(alice, mkPeerAddr Nothing), (bob, mkPeerAddr Nothing)])
        `shouldMatchList` [CidrOverflow 2 1]

  describe "unknown peer references (UnknownPeerRef)" $ do
    it "reports a segment peer that is not declared" $
      unknownRefs
        ( mkNetwork
            [(alice, mkPeer Nothing), (bob, mkPeer Nothing)]
            [(sn, FullMesh [alice, bob, dave])]
        )
        `shouldMatchList` [UnknownPeerRef sn dave]
    it "accepts segments that reference only declared peers" $
      unknownRefs
        ( mkNetwork
            [(alice, mkPeer (Just sampleEndpoint)), (bob, mkPeer Nothing)]
            [(sn, FullMesh [alice, bob])]
        )
        `shouldBe` []

  describe "cross-rule accumulation" $ do
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
      let es = failureErrors (validateNetwork net)
      es `shouldContain` [AddressOutOfCidr alice (read "192.168.1.5")]
      es `shouldContain` [InsufficientPeers sn2 "requires at least 2 peers"]

    it "accumulates structural and endpoint errors from a single pass" $ do
      let net =
            mkNetwork
              [(alice, mkPeer Nothing), (bob, mkPeer Nothing)]
              [ (sn, FullMesh [alice, bob]), -- NatPairInMesh
                (sn2, FullMesh [alice]) -- InsufficientPeers
              ]
      let es = failureErrors (validateNetwork net)
      es `shouldContain` [NatPairInMesh sn alice bob]
      es `shouldContain` [InsufficientPeers sn2 "requires at least 2 peers"]

    it "accumulates IslandPeer alongside InsufficientPeers in a single pass" $ do
      let net =
            mkNetwork
              [(alice, mkPeer Nothing), (bob, mkPeer Nothing), (carol, mkPeer Nothing)]
              [ (sn, FullMesh [alice, bob]), -- valid segment
                (sn2, FullMesh [alice]) -- InsufficientPeers
              ]
      let es = failureErrors (validateNetwork net)
      es `shouldContain` [IslandPeer carol]
      es `shouldContain` [InsufficientPeers sn2 "requires at least 2 peers"]

-- | All accumulated validation errors for a network.
errs :: Network -> [ValidationError]
errs = failureErrors . validateNetwork

-- | The accumulated errors narrowed to a single constructor, so a per-rule test
--   can ignore the unrelated errors 'validateNetwork' may also report.
insufficient, roleConflicts, missingEndpoints, natPairs, islands :: Network -> [ValidationError]
insufficient net = [e | e@InsufficientPeers{} <- errs net]
roleConflicts net = [e | e@PeerBothRoles{} <- errs net]
missingEndpoints net = [e | e@MissingEndpoint{} <- errs net]
natPairs net = [e | e@NatPairInMesh{} <- errs net]
islands net = [e | e@IslandPeer{} <- errs net]

outOfCidr, reserved, collisions, overflow, unknownRefs :: Network -> [ValidationError]
outOfCidr net = [e | e@AddressOutOfCidr{} <- errs net]
reserved net = [e | e@AddressIsReserved{} <- errs net]
collisions net = [e | e@AddressCollision{} <- errs net]
overflow net = [e | e@CidrOverflow{} <- errs net]
unknownRefs net = [e | e@UnknownPeerRef{} <- errs net]
