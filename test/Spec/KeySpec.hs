{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Spec.KeySpec (spec) where

import Crypto.Error (throwCryptoError)
import qualified Crypto.PubKey.Curve25519 as Curve25519
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Test.Hspec
import Test.QuickCheck

import WgForge.Key (
  PrivateKey (..),
  decodePrivateKey,
  derivePublicKey,
  encodePrivateKey,
  encodePublicKey,
 )

spec :: Spec
spec = describe "WgForge.Key" $ do
  describe "derivePublicKey (known-answer test)" $
    it "matches a real 'wg genkey | wg pubkey' pair" $
      let Right priv = decodePrivateKey "GEtMFljNTXfN+YEDDFoa8k6ZCQPyCQf7OswTykMbhlg="
       in encodePublicKey (derivePublicKey priv) `shouldBe` "RPX7RM1Qx5rfr4BJ/gfhkxwJOI4bZ7D3ev2STI66SVM="

  describe "base64 codec" $ do
    it "round-trips a private key through encode/decode" $
      property prop_roundTrip
    it "encodes a private key to 44 base64 characters" $
      property prop_encodedLength

  describe "Show" $
    it "redacts the private key" $
      property prop_showRedacts

prop_roundTrip :: Property
prop_roundTrip = forAll genPrivateKey $ \pk ->
  let enc = encodePrivateKey pk
   in (encodePrivateKey <$> decodePrivateKey enc) === Right enc

prop_encodedLength :: Property
prop_encodedLength = forAll genPrivateKey $ \pk ->
  BS.length (encodePrivateKey pk) === 44

prop_showRedacts :: Property
prop_showRedacts = forAll genPrivateKey $ \pk ->
  show pk === "PrivateKey <redacted>"

-- | A private key built from 32 random bytes (no clamping needed for the codec).
genPrivateKey :: Gen PrivateKey
genPrivateKey = do
  bytes <- vectorOf 32 (arbitrary :: Gen Word8)
  pure (PrivateKey (throwCryptoError (Curve25519.secretKey (BS.pack bytes))))
