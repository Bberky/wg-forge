{-# LANGUAGE OverloadedStrings #-}

module Spec.AllocatorSpec (spec) where

import Data.IP (AddrRange, IPv4, fromIPv4w, makeAddrRange, toIPv4)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import qualified Data.Set as Set
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Spec.Fixtures
import WgForge.Allocator (allocate)
import WgForge.Cidr (broadcastAddress, networkAddress)
import WgForge.Spec

spec :: Spec
spec = describe "WgForge.Allocator.allocate" $ do
  describe "properties" $ do
    it "is insensitive to peer map insertion order" $ property prop_orderInsensitive
    it "allocates exactly the input peers (completeness)" $ property prop_completeness
    it "assigns pairwise-distinct addresses (injectivity)" $ property prop_injectivity
    it "keeps every address in range and never reserved (containment)" $ property prop_containment
    it "preserves explicit addresses" $ property prop_explicitPreserved
    it "is stable for peers sorting before a removed dynamic peer" $ property prop_prefixStability

  describe "concrete cases" $ do
    it "gives the first dynamic peer .1, not the network address, on a /24" $
      allocate range24 (Map.fromList [(alice, mkPeer Nothing)])
        `shouldBe` Map.fromList [(alice, ip 10 0 0 1)]

    it "never hands out the broadcast address on a /24" $ do
      let pcs = Map.fromList [(p, mkPeer Nothing) | p <- manyNames 253]
          addrs = Map.elems (allocate range24 pcs)
      addrs `shouldSatisfy` notElem (ip 10 0 0 255)
      addrs `shouldSatisfy` notElem (ip 10 0 0 0)

    it "uses both addresses of a /31 (no reservation skip)" $
      Set.fromList
        (Map.elems (allocate range31 (Map.fromList [(alice, mkPeer Nothing), (bob, mkPeer Nothing)])))
        `shouldBe` Set.fromList [ip 10 0 0 0, ip 10 0 0 1]

    it "skips an explicitly used address when allocating dynamics" $
      allocate
        range24
        ( Map.fromList
            [ (alice, mkPeer Nothing),
              (bob, mkPeerAddr (Just (ip 10 0 0 1))),
              (carol, mkPeer Nothing)
            ]
        )
        `shouldBe` Map.fromList
          [ (alice, ip 10 0 0 2),
            (bob, ip 10 0 0 1),
            (carol, ip 10 0 0 3)
          ]

-- Properties --------------------------------------------------------------

prop_orderInsensitive :: AllocCase -> Property
prop_orderInsensitive (AllocCase r pcs) =
  forAll (shuffle (Map.toList pcs)) $ \shuffled ->
    allocate r (Map.fromList shuffled) === allocate r pcs

prop_completeness :: AllocCase -> Bool
prop_completeness (AllocCase r pcs) =
  Map.keysSet (allocate r pcs) == Map.keysSet pcs

prop_injectivity :: AllocCase -> Bool
prop_injectivity (AllocCase r pcs) =
  let addrs = Map.elems (allocate r pcs)
   in length addrs == Set.size (Set.fromList addrs)

prop_containment :: AllocCase -> Bool
prop_containment (AllocCase r pcs) =
  all inRange (Map.elems (allocate r pcs))
 where
  lo = w (networkAddress r)
  hi = w (broadcastAddress r)
  inRange a = let x = w a in x > lo && x < hi

prop_explicitPreserved :: AllocCase -> Bool
prop_explicitPreserved (AllocCase r pcs) =
  let result = allocate r pcs
   in and [Map.lookup nm result == Just a | (nm, spec') <- Map.toList pcs, Just a <- [address spec']]

prop_prefixStability :: AllocCase -> Property
prop_prefixStability (AllocCase r pcs) =
  not (null dynKeys) ==> forAll (elements dynKeys) $ \victim ->
    let full = allocate r pcs
        reduced = allocate r (Map.delete victim pcs)
        preds = filter (< victim) (Map.keys pcs)
     in all (\k -> Map.lookup k full == Map.lookup k reduced) preds
 where
  dynKeys = [nm | (nm, spec') <- Map.toList pcs, isNothing (address spec')]

-- Helpers -----------------------------------------------------------------

w :: IPv4 -> Word
w = fromIntegral . fromIPv4w

ip :: Int -> Int -> Int -> Int -> IPv4
ip a b c d = toIPv4 [a, b, c, d]

range24 :: AddrRange IPv4
range24 = makeAddrRange (ip 10 0 0 0) 24

range31 :: AddrRange IPv4
range31 = makeAddrRange (ip 10 0 0 0) 31

manyNames :: Int -> [PeerName]
manyNames n = [PeerName (T.pack (pad i)) | i <- [1 .. n]]
 where
  pad i = let s = show i in replicate (4 - length s) '0' ++ s
