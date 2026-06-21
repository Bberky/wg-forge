-- | This module provides utilities for handling keys, both private and public.
--   It includes functions for deriving public keys from private keys, encoding and decoding keys in base64 format.
module WgForge.Key (
  PrivateKey (..),
  PublicKey (..),
  derivePublicKey,
  decodePrivateKey,
  encodePrivateKey,
  encodePublicKey,
) where

import Crypto.Error (eitherCryptoError)
import qualified Crypto.PubKey.Curve25519 as Curve25519
import qualified Data.ByteArray.Encoding as BAE
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Coerce (coerce)

-- | A Curve25519 secret key. Its 'Show' instance is redacted to keep the
--   secret out of logs and error output.
newtype PrivateKey = PrivateKey Curve25519.SecretKey deriving (Eq)

-- | A Curve25519 public key, derived from a 'PrivateKey'.
newtype PublicKey = PublicKey Curve25519.PublicKey deriving (Eq)

-- | Redacting 'Show' so a secret never leaks into logs, errors, or test output.
instance Show PrivateKey where
  show _ = "PrivateKey <redacted>"

-- | Public keys are safe to show and render as their base64 form.
instance Show PublicKey where
  show = BS8.unpack . encodePublicKey

-- | Derive the public key corresponding to a given private key.
derivePublicKey :: PrivateKey -> PublicKey
derivePublicKey = PublicKey . Curve25519.toPublic . coerce

-- | Decode a base64-encoded private key. Returns 'Left' with a defect
-- description on invalid base64 or a non-32-byte payload.
decodePrivateKey :: ByteString -> Either String PrivateKey
decodePrivateKey bs = do
  raw <- BAE.convertFromBase BAE.Base64 bs :: Either String ByteString
  sk <-
    either (const (Left "key is not 32 bytes")) Right (eitherCryptoError (Curve25519.secretKey raw))
  return $ PrivateKey sk

-- | Encode a private key as a base64 string.
encodePrivateKey :: PrivateKey -> ByteString
encodePrivateKey = BAE.convertToBase BAE.Base64 . (coerce :: PrivateKey -> Curve25519.SecretKey)

-- | Encode a public key as a base64 string.
encodePublicKey :: PublicKey -> ByteString
encodePublicKey = BAE.convertToBase BAE.Base64 . (coerce :: PublicKey -> Curve25519.PublicKey)
