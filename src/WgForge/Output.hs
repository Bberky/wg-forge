{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Everything wg-forge writes to disk, kept out of the CLI orchestration: the
--   idempotent per-peer config writes for @generate@ and the project scaffold
--   for @init@. This is the IO counterpart to the pure "WgForge.Renderer",
--   mirroring "WgForge.Keystore" — the CLI decides what to do, the mechanics
--   live here. Failures surface as 'FileError'.
module WgForge.Output (
  -- * generate
  WriteStat (..),
  writeConfigs,
  summarizeWrites,

  -- * init
  scaffold,
) where

import Control.Exception (IOException, try)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.FileEmbed (embedFile)
import qualified Data.Map.Strict as Map
import Data.Text (unpack)
import Data.Text.Encoding (encodeUtf8)
import System.Directory (
  createDirectoryIfMissing,
  doesDirectoryExist,
  doesFileExist,
  listDirectory,
  renameFile,
 )
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)

import WgForge.Compiler (CompiledPeer)
import WgForge.Error (FileError (FileError))
import WgForge.Keystore (ensureKeystoreDir)
import WgForge.Renderer (renderConfig)
import WgForge.Spec (PeerName (..))

-- | Outcome of writing a single config file.
data WriteStat = Written | Unchanged
  deriving (Eq, Show)

-- | Render and write every peer's config into @dir@, creating the directory
--   first. Writes are idempotent (see 'writeConfigIfChanged'); the traversal
--   stops at the first IO failure.
writeConfigs :: FilePath -> Map.Map PeerName CompiledPeer -> IO (Either FileError [WriteStat])
writeConfigs dir compiled =
  ensureDir dir >>= \case
    Left err -> pure (Left err)
    Right () -> go (Map.toAscList compiled)
 where
  go [] = pure (Right [])
  go (pc : rest) =
    writeConfigIfChanged dir pc >>= \case
      Left err -> pure (Left err)
      Right stat -> fmap (stat :) <$> go rest

-- | A compact one-line summary, e.g. @3 configs (2 written, 1 unchanged)@.
summarizeWrites :: [WriteStat] -> String
summarizeWrites stats =
  show total ++ " configs (" ++ show written ++ " written, " ++ show unchanged ++ " unchanged)"
 where
  total = length stats
  written = length (filter (== Written) stats)
  unchanged = total - written

-- | Write a peer's config idempotently: skip when the on-disk bytes already
--   match, otherwise write to a temp file and atomically rename it into place.
writeConfigIfChanged :: FilePath -> (PeerName, CompiledPeer) -> IO (Either FileError WriteStat)
writeConfigIfChanged dir (pn@(PeerName name), cp) = do
  let target = dir </> (unpack name ++ ".conf")
      bytes = encodeUtf8 (renderConfig pn cp)
  result <- try (go target bytes) :: IO (Either IOException WriteStat)
  pure (first (FileError target . show) result)
 where
  go target bytes = do
    exists <- doesFileExist target
    same <- if exists then (== bytes) <$> BS.readFile target else pure False
    if same
      then pure Unchanged
      else do
        (tmp, h) <- openTempFile dir (unpack name ++ ".conf.tmp")
        BS.hPut h bytes
        hClose h
        renameFile tmp target
        pure Written

-- | Scaffold a project at @path@: a starter @network.yaml@, an @out/@
--   directory, and a @0700@ @keys/@ keystore. Refuses an existing non-empty
--   directory unless @force@ is set; any IO failure surfaces as a 'FileError'.
scaffold :: Bool -> FilePath -> IO (Either FileError ())
scaffold force path = do
  nonEmpty <- dirNonEmpty path
  if nonEmpty && not force
    then pure (Left (FileError path "refusing to initialize non-empty directory (use --force)"))
    else first (FileError path . show) <$> (try go :: IO (Either IOException ()))
 where
  dirNonEmpty p = do
    exists <- doesDirectoryExist p
    if exists then not . null <$> listDirectory p else pure False
  go = do
    createDirectoryIfMissing True path
    BS.writeFile (path </> "network.yaml") template
    createDirectoryIfMissing True (path </> "out")
    ensureKeystoreDir (path </> "keys")

-- | Create a directory (and parents), surfacing IO failures as 'FileError'.
ensureDir :: FilePath -> IO (Either FileError ())
ensureDir dir =
  first (FileError dir . show)
    <$> (try (createDirectoryIfMissing True dir) :: IO (Either IOException ()))

-- | The commented full-mesh starter spec written by @init@, embedded at
--   compile time from @data/network.template.yaml@.
template :: ByteString
template = $(embedFile "data/network.template.yaml")
