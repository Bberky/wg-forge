module WgForge.Key (
  PrivateKey (..),
  PublicKey (..),
  derivePublicKey,
  decodePrivateKey,
  encodePrivateKey,
  encodePublicKey,
) where

import Crypto.Error (maybeCryptoError)
import qualified Crypto.PubKey.Curve25519 as Curve25519
import qualified Data.ByteArray.Encoding as BAE
import Data.ByteString (ByteString)
import Data.Coerce (coerce)

newtype PrivateKey = PrivateKey Curve25519.SecretKey
newtype PublicKey = PublicKey Curve25519.PublicKey

-- | Derive the public key corresponding to a given private key.
derivePublicKey :: PrivateKey -> PublicKey
derivePublicKey = PublicKey . Curve25519.toPublic . coerce

-- | Decode a base64-encoded private key, returning 'Nothing' if the input is invalid.
decodePrivateKey :: ByteString -> Maybe PrivateKey
decodePrivateKey bs = do
  raw <- either (const Nothing) Just (BAE.convertFromBase BAE.Base64 bs :: Either String ByteString)
  sk <- maybeCryptoError $ Curve25519.secretKey raw
  return $ PrivateKey sk

-- | Encode a private key as a base64 string.
encodePrivateKey :: PrivateKey -> ByteString
encodePrivateKey = BAE.convertToBase BAE.Base64 . (coerce :: PrivateKey -> Curve25519.SecretKey)

-- | Encode a public key as a base64 string.
encodePublicKey :: PublicKey -> ByteString
encodePublicKey = BAE.convertToBase BAE.Base64 . (coerce :: PublicKey -> Curve25519.PublicKey)
