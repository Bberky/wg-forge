{-# LANGUAGE OverloadedStrings #-}

module Spec.DiffSpec (spec) where

import qualified Data.ByteString as BS
import Data.Either (isLeft, isRight)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Test.QuickCheck

import WgForge.CLI
import WgForge.Diff
import WgForge.Spec (PeerName (..))

spec :: Spec
spec = describe "WgForge.Diff" $ do
  describe "normalize" $ do
    it "drops PrivateKey and PublicKey lines, keeps everything else" $
      normalize sampleConf `shouldBe` strippedConf

  describe "diffConfigs" $ do
    it "is all-Unchanged when desired equals on-disk" $
      let m = Map.fromList [(PeerName "a", sampleConf), (PeerName "b", sampleConf)]
       in all (== Unchanged) (Map.elems (diffConfigs m m)) `shouldBe` True

    it "ignores differences confined to key lines" $
      diffConfigs
        (Map.singleton (PeerName "a") sampleConf)
        (Map.singleton (PeerName "a") sampleConfOtherKeys)
        `shouldBe` Map.singleton (PeerName "a") Unchanged

    it "flags a peer present only in the spec as Added" $
      diffConfigs (Map.singleton (PeerName "a") sampleConf) Map.empty
        `shouldBe` Map.singleton (PeerName "a") Added

    it "flags a peer present only on disk as Removed" $
      diffConfigs Map.empty (Map.singleton (PeerName "a") sampleConf)
        `shouldBe` Map.singleton (PeerName "a") Removed

    it "flags a non-key body change as Modified" $
      diffConfigs
        (Map.singleton (PeerName "a") sampleConf)
        (Map.singleton (PeerName "a") (sampleConf <> "Address = 10.0.0.9\n"))
        `shouldBe` Map.singleton (PeerName "a") Modified

  describe "isDirty" $ do
    it "is False when every peer is Unchanged" $
      isDirty (Map.fromList [(PeerName "a", Unchanged), (PeerName "b", Unchanged)]) `shouldBe` False

    it "is True when any peer changed" $
      isDirty (Map.fromList [(PeerName "a", Unchanged), (PeerName "b", Added)]) `shouldBe` True

    it "a config map diffed against itself is never dirty (property)" $
      property prop_selfClean

  describe "diff (integration)" $ do
    it "is clean right after generate and dirty after adding a peer" $
      withSystemTempDirectory "wgf-diff" $ \dir -> do
        let specPath = dir </> "network.yaml"
            outDir = dir </> "out"
            keyDir = dir </> "keys"
        BS.writeFile specPath sampleSpec
        _ <- dispatch (Generate (GenerateOptions specPath outDir keyDir))

        clean <- dispatch (Diff (DiffOptions outDir False specPath))
        isRight clean `shouldBe` True

        BS.writeFile specPath sampleSpecExtraPeer
        dirty <- dispatch (Diff (DiffOptions outDir False specPath))
        isLeft dirty `shouldBe` True

-- | Diffing any config map against itself yields no changes, regardless of the
--   (key-masked) contents.
prop_selfClean :: [(String, String)] -> Bool
prop_selfClean kvs =
  let m = Map.fromList [(PeerName (T.pack k), T.pack v) | (k, v) <- kvs]
   in all (== Unchanged) (Map.elems (diffConfigs m m))

-- | A rendered config with both a private and a public key line.
sampleConf :: Text
sampleConf =
  T.unlines
    [ "[Interface]",
      "# node-a",
      "PrivateKey = aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa=",
      "Address = 10.0.0.1",
      "",
      "[Peer]",
      "# node-b",
      "PublicKey = bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb=",
      "AllowedIPs = 10.0.0.2/32"
    ]

-- | 'sampleConf' with the key lines removed.
strippedConf :: Text
strippedConf =
  T.unlines
    [ "[Interface]",
      "# node-a",
      "Address = 10.0.0.1",
      "",
      "[Peer]",
      "# node-b",
      "AllowedIPs = 10.0.0.2/32"
    ]

-- | 'sampleConf' with different key material but identical everything else.
sampleConfOtherKeys :: Text
sampleConfOtherKeys =
  T.unlines
    [ "[Interface]",
      "# node-a",
      "PrivateKey = cccccccccccccccccccccccccccccccccccccccccc=",
      "Address = 10.0.0.1",
      "",
      "[Peer]",
      "# node-b",
      "PublicKey = dddddddddddddddddddddddddddddddddddddddddd=",
      "AllowedIPs = 10.0.0.2/32"
    ]

-- | A minimal valid two-peer full-mesh spec.
sampleSpec :: BS.ByteString
sampleSpec =
  "network:\n\
  \  name: test-net\n\
  \  cidr: 10.0.0.0/24\n\
  \peers:\n\
  \  node-a:\n\
  \    endpoint: a.example.com:51820\n\
  \  node-b:\n\
  \    endpoint: b.example.com:51820\n\
  \segments:\n\
  \  mesh:\n\
  \    topology: full-mesh\n\
  \    peers: [node-a, node-b]\n"

-- | 'sampleSpec' with a third peer added to the mesh.
sampleSpecExtraPeer :: BS.ByteString
sampleSpecExtraPeer =
  "network:\n\
  \  name: test-net\n\
  \  cidr: 10.0.0.0/24\n\
  \peers:\n\
  \  node-a:\n\
  \    endpoint: a.example.com:51820\n\
  \  node-b:\n\
  \    endpoint: b.example.com:51820\n\
  \  node-c:\n\
  \    endpoint: c.example.com:51820\n\
  \segments:\n\
  \  mesh:\n\
  \    topology: full-mesh\n\
  \    peers: [node-a, node-b, node-c]\n"
