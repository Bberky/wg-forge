{-# LANGUAGE OverloadedStrings #-}

-- | The IO shell around the pure compiler core: parse argv, run the
--   parse → validate → compile → render → write pipeline, and map every
--   outcome to an exit code.
module WgForge.CLI (
  run,
  dispatch,
  Options (..),
  Command (..),
  InitOptions (..),
  GenerateOptions (..),
  parseOpts,
) where

import Control.Exception (IOException, try)
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as TIO
import Options.Applicative (
  CommandFields,
  Mod,
  Parser,
  ParserInfo,
  command,
  execParser,
  fullDesc,
  header,
  help,
  helper,
  hsubparser,
  info,
  long,
  metavar,
  progDesc,
  short,
  strOption,
  switch,
  value,
 )
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
import WgForge.CLI.Report (AppError (..), exitCodeFor, renderAppError)
import WgForge.Compiler (CompiledPeer, compile)
import WgForge.Keystore (ensureKeys, ensureKeystoreDir)
import WgForge.Renderer (renderConfig)
import WgForge.Spec (Network (..), NetworkSpec (cidr), PeerName (..))
import WgForge.Spec.Parser (parseNetworkFile)
import WgForge.Spec.Validator (validateNetwork)

newtype Options = Options
  { optCommand :: Command
  }

data Command
  = Init InitOptions
  | Validate FilePath
  | Generate GenerateOptions

data InitOptions = InitOptions
  { initPath :: FilePath,
    initForce :: Bool
  }

data GenerateOptions = GenerateOptions
  { genSpec :: FilePath,
    genOutDir :: FilePath,
    genKeyDir :: FilePath
  }

initOpts :: Parser Command
initOpts =
  Init
    <$> ( InitOptions
            <$> strOption (long "path" <> short 'p' <> metavar "DIR" <> help "Path to initialize the project in")
            <*> switch (long "force" <> short 'f' <> help "Force initialization even if the directory is not empty")
        )

initCmd :: Mod CommandFields Command
initCmd = command "init" (info initOpts (progDesc "Initialize a new wg-forge project"))

generateOpts :: Parser Command
generateOpts =
  Generate
    <$> ( GenerateOptions
            <$> strOption
              (long "spec" <> short 's' <> metavar "FILE" <> help "Path to the network specification YAML file")
            <*> strOption
              ( long "out"
                  <> short 'o'
                  <> metavar "DIR"
                  <> value "out"
                  <> help "Output directory for generated WireGuard configurations (default: out)"
              )
            <*> strOption
              ( long "keys"
                  <> short 'k'
                  <> metavar "DIR"
                  <> value "keys"
                  <> help "Directory containing WireGuard private keys (default: keys)"
              )
        )

generateCmd :: Mod CommandFields Command
generateCmd =
  command
    "generate"
    (info generateOpts (progDesc "Generate configurations from a network specification"))

validateOpts :: Parser Command
validateOpts =
  Validate
    <$> strOption
      ( long "spec"
          <> short 's'
          <> metavar "FILE"
          <> help "Path to the network specification YAML file to validate"
      )

validateCmd :: Mod CommandFields Command
validateCmd =
  command
    "validate"
    (info validateOpts (progDesc "Validate a network specification YAML file"))

programOpts :: Parser Options
programOpts =
  Options <$> hsubparser (initCmd <> generateCmd <> validateCmd)

parseOpts :: ParserInfo Options
parseOpts =
  info
    (helper <*> programOpts)
    ( fullDesc
        <> header "wg-forge: a WireGuard© configuration generator"
        <> progDesc
          "Generate WireGuard© configurations from a high-level network specification, with support for validation, key management, and multiple network topologies."
    )

run :: IO ()
run = do
  opts <- execParser parseOpts
  result <- dispatch (optCommand opts)
  case result of
    Left err -> do
      TIO.hPutStrLn stderr (renderAppError err)
      exitWith (exitCodeFor err)
    Right () -> pure ()

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
    TIO.writeFile (p </> "network.yaml") initTemplate
    createDirectoryIfMissing True (p </> "out")
    ensureKeystoreDir (p </> "keys")

-- | A commented full-mesh starter spec written by @init@.
initTemplate :: Text
initTemplate =
  "# wg-forge network specification\n\
  \#\n\
  \# A starter two-peer full mesh. Edit the peers and segments below,\n\
  \# then run: wg-forge generate --spec network.yaml\n\
  \\n\
  \network:\n\
  \  name: my-network\n\
  \  cidr: 10.0.0.0/24\n\
  \\n\
  \peers:\n\
  \  node-a:\n\
  \    endpoint: a.example.com:51820\n\
  \    listenPort: 51820\n\
  \\n\
  \  node-b:\n\
  \    endpoint: b.example.com:51820\n\
  \    listenPort: 51820\n\
  \\n\
  \segments:\n\
  \  mesh:\n\
  \    topology: full-mesh\n\
  \    peers: [node-a, node-b]\n"
