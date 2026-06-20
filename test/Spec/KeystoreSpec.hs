{-# LANGUAGE OverloadedStrings #-}

module Spec.KeystoreSpec (spec) where

import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (fileMode, getFileStatus, modificationTimeHiRes)
import Test.Hspec

import Spec.Fixtures (alice, bob, carol)
import WgForge.Error (KeystoreError (MalformedKey))
import WgForge.Key (encodePrivateKey)
import WgForge.Keystore (ensureKeys, ensureKeystoreDir, generatePrivateKey, keyPath, loadKey)

spec :: Spec
spec = describe "WgForge.Keystore" $ do
  describe "ensureKeystoreDir" $ do
    it "creates the directory with mode 0700" $
      withSystemTempDirectory "wgf" $ \dir -> do
        let sub = dir </> "keystore"
        ensureKeystoreDir sub
        mode <- fileMode <$> getFileStatus sub
        (mode .&. 0o777) `shouldBe` 0o700

    it "is idempotent on an existing directory" $
      withSystemTempDirectory "wgf" $ \dir -> do
        ensureKeystoreDir dir
        ensureKeystoreDir dir
        mode <- fileMode <$> getFileStatus dir
        (mode .&. 0o777) `shouldBe` 0o700

  describe "ensureKeys" $ do
    it "creates a key file with mode 0600 and wg-genkey byte format" $
      withSystemTempDirectory "wgf" $ \dir -> do
        r <- ensureKeys dir [alice]
        case r of
          Left e -> expectationFailure (show e)
          Right m -> Map.keys m `shouldBe` [alice]
        let path = keyPath dir alice
        content <- BS.readFile path
        BS.length content `shouldBe` 45 -- 44 base64 chars + trailing newline
        BS.last content `shouldBe` 10 -- '\n'
        mode <- fileMode <$> getFileStatus path
        (mode .&. 0o777) `shouldBe` 0o600

    it "does not rewrite an existing key on re-run (idempotency)" $
      withSystemTempDirectory "wgf" $ \dir -> do
        _ <- ensureKeys dir [alice]
        let path = keyPath dir alice
        contentBefore <- BS.readFile path
        mtime1 <- modificationTimeHiRes <$> getFileStatus path
        _ <- ensureKeys dir [alice]
        contentAfter <- BS.readFile path
        mtime2 <- modificationTimeHiRes <$> getFileStatus path
        contentAfter `shouldBe` contentBefore
        mtime2 `shouldBe` mtime1

    it "preserves a pre-placed key (with trailing newline)" $
      withSystemTempDirectory "wgf" $ \dir -> do
        ensureKeystoreDir dir
        pre <- generatePrivateKey
        let path = keyPath dir alice
            encoded = encodePrivateKey pre
        BS.writeFile path (encoded <> "\n")
        r <- ensureKeys dir [alice]
        case r of
          Left e -> expectationFailure (show e)
          Right m -> fmap encodePrivateKey (Map.lookup alice m) `shouldBe` Just encoded

    it "creates keys for multiple peers" $
      withSystemTempDirectory "wgf" $ \dir -> do
        r <- ensureKeys dir [alice, bob, carol]
        case r of
          Left e -> expectationFailure (show e)
          Right m -> Map.keys m `shouldBe` [alice, bob, carol]
        mapM_ (\p -> doesFileExist (keyPath dir p) >>= (`shouldBe` True)) [alice, bob, carol]

  describe "loadKey" $ do
    it "returns Right Nothing for a missing key file" $
      withSystemTempDirectory "wgf" $ \dir -> do
        ensureKeystoreDir dir
        r <- loadKey dir alice
        case r of
          Right Nothing -> pure ()
          other -> expectationFailure ("expected Right Nothing, got " ++ show other)

    it "reports a malformed key file" $
      withSystemTempDirectory "wgf" $ \dir -> do
        ensureKeystoreDir dir
        let path = keyPath dir alice
        BS.writeFile path "not a valid key!!!"
        r <- loadKey dir alice
        case r of
          Left (MalformedKey p _) -> p `shouldBe` path
          other -> expectationFailure ("expected MalformedKey, got " ++ show other)
