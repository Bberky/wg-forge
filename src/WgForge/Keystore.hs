module WgForge.Keystore (
  ensureKeystoreDir,
  generatePrivateKey,
  loadKey,
  ensureKeys,
  ensureKey,
  writeKey,
  keyPath,
) where

import Control.Monad (unless)
import qualified Crypto.PubKey.Curve25519 as Curve25519
import qualified Data.ByteString as BS
import Data.Either (partitionEithers)
import qualified Data.Map.Strict as Map
import Data.Text (unpack)
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))
import System.Posix.Directory (createDirectory)
import System.Posix.IO (
  OpenFileFlags (..),
  OpenMode (..),
  defaultFileFlags,
  fdToHandle,
  openFd,
 )

import WgForge.Error (KeystoreError (MalformedKey))
import WgForge.Key (PrivateKey (PrivateKey), decodePrivateKey, encodePrivateKey)
import WgForge.Spec (PeerName (..))

-- | Check that the keystore directory exists and if not, create it with permissions 700.
ensureKeystoreDir :: FilePath -> IO ()
ensureKeystoreDir dir = do
  exists <- doesDirectoryExist dir
  unless exists $ createDirectory dir 0o700

-- | Generate a new Curve25519 private key.
-- The PRNG used is the system's secure random number generator.
generatePrivateKey :: IO PrivateKey
generatePrivateKey = PrivateKey <$> Curve25519.generateSecretKey

-- | Load a private key from the specified file path.
-- Returns an error if the file does not exist or if the key is malformed.
loadKey :: FilePath -> PeerName -> IO (Either KeystoreError (Maybe PrivateKey))
loadKey dir peerName = do
  let path = keyPath dir peerName
  bs <- BS.readFile path
  case decodePrivateKey bs of
    Nothing -> return $ Left (MalformedKey path "Invalid key format")
    Just pk -> return $ Right (Just pk)

-- | Write a private key to the specified file path with permissions 600.
-- Only creates the file if it does not already exist, to avoid overwriting existing keys.
writeKey :: FilePath -> PrivateKey -> IO (Maybe KeystoreError)
writeKey path pk = do
  fd <- openFd path WriteOnly defaultFileFlags{creat = Just 0o600, exclusive = True}
  handle <- fdToHandle fd
  BS.hPut handle (encodePrivateKey pk)
  return Nothing

-- | Ensure that a private key exists for the given peer name.
-- If the key does not exist, generate a new one and write it to the keystore.
-- Returns the private key or an error if the operation fails.
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

-- | Ensure that private keys exist for the given peer names.
-- If a key does not exist, generate a new one and write it to the keystore.
-- Returns a map of peer names to their private keys or an error if any operation fails.
ensureKeys :: FilePath -> [PeerName] -> IO (Either [KeystoreError] (Map.Map PeerName PrivateKey))
ensureKeys dir peerNames = do
  results <- mapM (ensureKey dir) peerNames
  let (errors, keys) = partitionEithers results
  if null errors
    then return $ Right (Map.fromList (zip peerNames keys))
    else return $ Left errors

-- | Get the path to a peer's key file in the keystore directory.
keyPath :: FilePath -> PeerName -> FilePath
keyPath dir (PeerName name) = dir </> (unpack name ++ ".key")
