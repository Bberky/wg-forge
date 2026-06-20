{-# LANGUAGE OverloadedStrings #-}

module Spec.OutputSpec (spec) where

import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Either (isLeft)
import qualified Data.Map.Strict as Map
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (fileMode, getFileStatus, modificationTimeHiRes)
import Test.Hspec

import Spec.Fixtures (alice, bob, mkNetwork, mkPeer, sampleEndpoint, sn)
import WgForge.Allocator (allocate)
import WgForge.Compiler (CompiledPeer, compile)
import WgForge.Keystore (ensureKeys)
import WgForge.Output (
  WriteStat (..),
  readConfigs,
  scaffold,
  summarizeWrites,
  writeConfigs,
  writeQrPng,
 )
import WgForge.Spec (PeerName, SegmentSpec (..), cidr, network, peers)

spec :: Spec
spec = describe "WgForge.Output" $ do
  describe "writeConfigs" $ do
    it "writes one config per peer and reports each as written" $
      withProject $ \dir compiled -> do
        r <- writeConfigs (dir </> "out") compiled
        case r of
          Left e -> expectationFailure (show e)
          Right stats -> do
            stats `shouldMatchList` [Written, Written]
            doesFileExist (dir </> "out" </> "alice.conf") >>= (`shouldBe` True)
            doesFileExist (dir </> "out" </> "bob.conf") >>= (`shouldBe` True)

    it "is idempotent: a second run rewrites nothing" $
      withProject $ \dir compiled -> do
        let out = dir </> "out"
        _ <- writeConfigs out compiled
        mtime1 <- modificationTimeHiRes <$> getFileStatus (out </> "alice.conf")
        r2 <- writeConfigs out compiled
        mtime2 <- modificationTimeHiRes <$> getFileStatus (out </> "alice.conf")
        fmap (all (== Unchanged)) r2 `shouldBe` Right True
        mtime2 `shouldBe` mtime1 -- untouched on disk
  describe "readConfigs" $ do
    it "reads back exactly the peers that were written" $
      withProject $ \dir compiled -> do
        let out = dir </> "out"
        _ <- writeConfigs out compiled
        r <- readConfigs out
        fmap Map.keysSet r `shouldBe` Right (Map.keysSet compiled)

    it "reads an empty map from an empty directory" $
      withSystemTempDirectory "wgf-out" $ \dir -> do
        r <- readConfigs dir
        r `shouldBe` Right Map.empty

  describe "summarizeWrites" $
    it "summarises a mix of written and unchanged" $
      summarizeWrites [Written, Unchanged, Written] `shouldBe` "3 configs (2 written, 1 unchanged)"

  describe "scaffold" $ do
    it "creates network.yaml, out/ and a 0700 keys/" $
      withSystemTempDirectory "wgf-init" $ \root -> do
        let dir = root </> "proj"
        scaffold False dir `shouldReturn` Right ()
        doesFileExist (dir </> "network.yaml") >>= (`shouldBe` True)
        doesDirectoryExist (dir </> "out") >>= (`shouldBe` True)
        mode <- fileMode <$> getFileStatus (dir </> "keys")
        (mode .&. 0o777) `shouldBe` 0o700

    it "refuses a non-empty directory without force" $
      withSystemTempDirectory "wgf-init" $ \dir -> do
        BS.writeFile (dir </> "something") "x"
        r <- scaffold False dir
        r `shouldSatisfy` isLeft

    it "scaffolds into a non-empty directory when forced" $
      withSystemTempDirectory "wgf-init" $ \dir -> do
        BS.writeFile (dir </> "something") "x"
        scaffold True dir `shouldReturn` Right ()
        doesFileExist (dir </> "network.yaml") >>= (`shouldBe` True)

  describe "writeQrPng" $ do
    it "writes the given bytes to disk" $
      withSystemTempDirectory "wgf-qr" $ \dir -> do
        let path = dir </> "peer.png"
            bytes = BL.pack [137, 80, 78, 71]
        writeQrPng path bytes `shouldReturn` Right ()
        onDisk <- BL.readFile path
        onDisk `shouldBe` bytes

    it "surfaces a write into a missing directory as a FileError" $
      withSystemTempDirectory "wgf-qr" $ \dir -> do
        r <- writeQrPng (dir </> "missing" </> "peer.png") (BL.pack [0])
        r `shouldSatisfy` isLeft

-- | Run an action with a temp project: a compiled two-peer full-mesh network
--   plus the directory it lives in. Keys come from the public 'ensureKeys'.
withProject :: (FilePath -> Map.Map PeerName CompiledPeer -> IO a) -> IO a
withProject act =
  withSystemTempDirectory "wgf-out" $ \dir -> do
    let net =
          mkNetwork
            [(alice, mkPeer (Just sampleEndpoint)), (bob, mkPeer Nothing)]
            [(sn, FullMesh [alice, bob])]
        addrs = allocate (cidr (network net)) (peers net)
    keys <- ensureKeys (dir </> "keys") (Map.keys (peers net)) >>= either (fail . show) pure
    act dir (compile keys addrs net)
