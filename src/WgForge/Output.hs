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

  -- * diff
  readConfigs,

  -- * init
  scaffold,

  -- * qr
  writeQrPng,
) where

import Control.Exception (IOException, try)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.FileEmbed (embedFile)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as TIO
import System.Directory (
  createDirectoryIfMissing,
  doesDirectoryExist,
  doesFileExist,
  listDirectory,
  renameFile,
 )
import System.FilePath (dropExtension, takeExtension, (</>))
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
writeConfigs :: FilePath -> Map PeerName CompiledPeer -> IO (Either FileError [WriteStat])
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
  let target = dir </> (T.unpack name ++ ".conf")
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
        (tmp, h) <- openTempFile dir (T.unpack name ++ ".conf.tmp")
        BS.hPut h bytes
        hClose h
        renameFile tmp target
        pure Written

-- | Read every @\<peer\>.conf@ in @dir@ back into memory, keyed by peer name,
--   for @diff@ to compare against the spec. A non-existent directory yields an
--   empty map (a never-generated project, where every peer is \"added\"); any IO
--   failure surfaces as a 'FileError'.
readConfigs :: FilePath -> IO (Either FileError (Map PeerName Text))
readConfigs dir = do
  first (FileError dir . show) <$> (try go :: IO (Either IOException (Map PeerName Text)))
 where
  go = do
    names <- listDirectory dir
    Map.fromList <$> mapM readOne (filter ((== ".conf") . takeExtension) names)
  readOne name = do
    text <- TIO.readFile (dir </> name)
    pure (PeerName (T.pack (dropExtension name)), text)

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

-- | Write PNG bytes to a path for @qr --output@, surfacing IO failures (e.g. a
--   missing parent directory) as 'FileError'. The pure encoding lives in
--   "WgForge.QR"; this is just the disk edge.
writeQrPng :: FilePath -> BL.ByteString -> IO (Either FileError ())
writeQrPng path bytes =
  first (FileError path . show)
    <$> (try (BL.writeFile path bytes) :: IO (Either IOException ()))

-- | Create a directory (and parents), surfacing IO failures as 'FileError'.
ensureDir :: FilePath -> IO (Either FileError ())
ensureDir dir =
  first (FileError dir . show)
    <$> (try (createDirectoryIfMissing True dir) :: IO (Either IOException ()))

-- | The commented full-mesh starter spec written by @init@, embedded at
--   compile time from @data/network.template.yaml@.
template :: ByteString
template = $(embedFile "data/network.template.yaml")
