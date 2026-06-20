-- | This module provides functions for managing a keystore of private keys for peers.
module WgForge.Keystore (
  ensureKeystoreDir,
  generatePrivateKey,
  loadKey,
  ensureKeys,
  ensureKey,
  writeKey,
  keyPath,
) where

import Control.Exception (IOException, try)
import qualified Crypto.PubKey.Curve25519 as Curve25519
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (isSpace)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (hClose)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Files (setFileMode)
import System.Posix.IO (
  OpenFileFlags (..),
  OpenMode (..),
  defaultFileFlags,
  fdToHandle,
  openFd,
 )

import WgForge.Error
import WgForge.Key
import WgForge.Spec

-- | Ensure the keystore directory exists with mode @0700@.
-- Sets the mode explicitly after creation to avoid the umask window.
ensureKeystoreDir :: FilePath -> IO ()
ensureKeystoreDir dir = do
  createDirectoryIfMissing True dir
  setFileMode dir 0o700

-- | Generate a new Curve25519 private key.
-- The PRNG used is the system's secure random number generator.
generatePrivateKey :: IO PrivateKey
generatePrivateKey = PrivateKey <$> Curve25519.generateSecretKey

-- | Load a private key for a peer.
-- Returns 'Right Nothing' if the key file does not exist,
-- 'Right (Just pk)' on success, and 'Left err' on IO or parsing errors.
loadKey :: FilePath -> PeerName -> IO (Either KeystoreError (Maybe PrivateKey))
loadKey dir peerName = do
  let path = keyPath dir peerName
  result <- try (BS.readFile path) :: IO (Either IOException ByteString)
  case result of
    Left e
      | isDoesNotExistError e -> return $ Right Nothing
      | otherwise -> return $ Left (KeyIoError path (show e))
    Right bs ->
      case decodePrivateKey (trim bs) of
        Left err -> return $ Left (MalformedKey path err)
        Right pk -> return $ Right (Just pk)
 where
  trim = fst . BS8.spanEnd isSpace . BS8.dropWhile isSpace

-- | Write a private key to @path@ with mode @0600@, never overwriting an
-- existing file (@O_CREAT | O_EXCL@). The payload is base64 plus a single
-- trailing newline, byte-identical to @wg genkey@.
writeKey :: FilePath -> PrivateKey -> IO (Maybe KeystoreError)
writeKey path pk = do
  result <- try go :: IO (Either IOException ())
  return $ either (Just . KeyIoError path . show) (const Nothing) result
 where
  go = do
    fd <- openFd path WriteOnly defaultFileFlags{creat = Just 0o600, exclusive = True}
    handle <- fdToHandle fd
    BS.hPut handle (encodePrivateKey pk <> BS8.singleton '\n')
    hClose handle

-- | Ensure a private key exists for a peer, generating and persisting one if
-- absent. Existing keys are never regenerated.
ensureKey :: FilePath -> PeerName -> IO (Either KeystoreError PrivateKey)
ensureKey dir peerName = do
  let path = keyPath dir peerName
  result <- loadKey dir peerName
  case result of
    Left err -> return $ Left err
    Right (Just pk) -> return $ Right pk
    Right Nothing -> do
      pk <- generatePrivateKey
      writeRes <- writeKey path pk
      return $ maybe (Right pk) Left writeRes

-- | Ensure private keys exist for the given peers, creating any that are
-- missing. Returns the first error encountered, or the full key map on success.
-- Keystore directory is created if it does not exist.
ensureKeys :: FilePath -> [PeerName] -> IO (Either KeystoreError (Map PeerName PrivateKey))
ensureKeys dir peerNames = do
  ensureKeystoreDir dir
  results <- mapM (ensureKey dir) peerNames
  return $ Map.fromList . zip peerNames <$> sequence results

-- | Get the path to a peer's key file in the keystore directory.
keyPath :: FilePath -> PeerName -> FilePath
keyPath dir (PeerName pn) = dir </> (T.unpack pn ++ ".key")
