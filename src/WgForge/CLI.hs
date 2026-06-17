{-# LANGUAGE TemplateHaskell #-}

-- | The IO shell around the pure compiler core: run the parsed command through
--   the parse → validate → compile → render → write pipeline and map every
--   outcome to an exit code. The option parser lives in "WgForge.CLI.Options".
module WgForge.CLI (
  run,
  dispatch,
  module WgForge.CLI.Options,
) where

import Control.Exception (IOException, try)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.FileEmbed (embedFile)
import qualified Data.Map.Strict as Map
import Data.Text (unpack)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as TIO
import Options.Applicative (execParser)
import System.Directory (
  createDirectoryIfMissing,
  doesDirectoryExist,
  doesFileExist,
  listDirectory,
  renameFile,
 )
import System.Exit (exitWith)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile, stderr)
import Validation (Validation (Failure, Success))

import WgForge.Allocator (allocate)
import WgForge.CLI.Options
import WgForge.CLI.Report (AppError (..), exitCodeFor, renderAppError)
import WgForge.Compiler (CompiledPeer, compile)
import WgForge.Keystore (ensureKeys, ensureKeystoreDir)
import WgForge.Renderer (renderConfig)
import WgForge.Spec (Network (..), NetworkSpec (cidr), PeerName (..))
import WgForge.Spec.Parser (parseNetworkFile)
import WgForge.Spec.Validator (validateNetwork)

-- | Parse argv, run the chosen command, and exit with the mapped code,
--   printing any error to stderr.
run :: IO ()
run = do
  opts <- execParser parseOpts
  result <- dispatch (optCommand opts)
  case result of
    Left err -> do
      TIO.hPutStrLn stderr (renderAppError err)
      exitWith (exitCodeFor err)
    Right () -> pure ()

-- | Route a parsed command to its handler.
dispatch :: Command -> IO (Either AppError ())
dispatch (Init o) = runInit o
dispatch (Validate f) = runValidate f
dispatch (Generate o) = runGenerate o

-- | Short-circuiting bind for the @IO (Either AppError a)@ pipeline: run the
--   first step, stop on a 'Left', otherwise feed the result to the next step.
(>>?) :: IO (Either e a) -> (a -> IO (Either e b)) -> IO (Either e b)
m >>? k = m >>= either (pure . Left) k

infixl 1 >>?

-- | Traverse a list with a fallible IO action, stopping at the first error.
traverseE :: (a -> IO (Either e b)) -> [a] -> IO (Either e [b])
traverseE f = go
 where
  go [] = pure (Right [])
  go (x : xs) = f x >>? \y -> go xs >>? \ys -> pure (Right (y : ys))

-- | Parse and validate a spec file, accumulating all validation errors.
loadValidated :: FilePath -> IO (Either AppError Network)
loadValidated f =
  fmap (first AppSpec) (parseNetworkFile f) >>? \net ->
    case validateNetwork net of
      Success v -> pure (Right v)
      Failure es -> pure (Left (AppValidation es))

-- | @validate@: parse + validate, silent on success.
runValidate :: FilePath -> IO (Either AppError ())
runValidate f = loadValidated f >>? \_ -> pure (Right ())

-- | @generate@: the full pipeline → one @\<peer\>.conf@ per peer plus keys.
runGenerate :: GenerateOptions -> IO (Either AppError ())
runGenerate (GenerateOptions spec out keyDir) =
  loadValidated spec >>? \net -> do
    let addrs = allocate (cidr (network net)) (peers net)
    fmap (first AppKeystore) (ensureKeys keyDir (Map.keys (peers net))) >>? \keys -> do
      let compiled = compile keys addrs net
      ensureDir out >>? \() ->
        traverseE (writeConfigIfChanged out) (Map.toAscList compiled) >>? \stats -> do
          putStrLn (summarize stats)
          pure (Right ())

-- | Outcome of writing a single config file.
data WriteStat = Written | Unchanged

-- | Create a directory (and parents), surfacing IO failures as 'AppIO'.
ensureDir :: FilePath -> IO (Either AppError ())
ensureDir dir =
  first (AppIO dir . show)
    <$> (try (createDirectoryIfMissing True dir) :: IO (Either IOException ()))

-- | Write a peer's config idempotently: skip when the on-disk bytes already
--   match, otherwise write to a temp file and atomically rename it into place.
writeConfigIfChanged :: FilePath -> (PeerName, CompiledPeer) -> IO (Either AppError WriteStat)
writeConfigIfChanged dir (pn@(PeerName name), cp) = do
  let target = dir </> (unpack name ++ ".conf")
      bytes = encodeUtf8 (renderConfig pn cp)
  result <- try (go target bytes) :: IO (Either IOException WriteStat)
  pure (first (AppIO target . show) result)
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

-- | A compact one-line summary, e.g. @3 configs (2 written, 1 unchanged)@.
summarize :: [WriteStat] -> String
summarize stats =
  show total ++ " configs (" ++ show written ++ " written, " ++ show unchanged ++ " unchanged)"
 where
  total = length stats
  written = length (filter isWritten stats)
  unchanged = total - written
  isWritten Written = True
  isWritten Unchanged = False

-- | @init@: scaffold a project directory with a starter spec, @out/@, @keys/@.
runInit :: InitOptions -> IO (Either AppError ())
runInit (InitOptions path force) = do
  nonEmpty <- dirNonEmpty path
  if nonEmpty && not force
    then pure (Left (AppIO path "refusing to initialize non-empty directory (use --force)"))
    else do
      result <- try (scaffold path) :: IO (Either IOException ())
      case result of
        Left e -> pure (Left (AppIO path (show e)))
        Right () -> do
          putStrLn ("Initialized wg-forge project in " ++ path)
          pure (Right ())
 where
  dirNonEmpty p = do
    exists <- doesDirectoryExist p
    if exists then not . null <$> listDirectory p else pure False
  scaffold p = do
    createDirectoryIfMissing True p
    BS.writeFile (p </> "network.yaml") initTemplate
    createDirectoryIfMissing True (p </> "out")
    ensureKeystoreDir (p </> "keys")

-- | The commented full-mesh starter spec written by @init@, embedded at
--   compile time from @data/network.template.yaml@.
initTemplate :: ByteString
initTemplate = $(embedFile "data/network.template.yaml")
