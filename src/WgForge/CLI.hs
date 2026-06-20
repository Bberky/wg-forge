-- | The IO shell around the pure compiler core: route the parsed command
--   through the parse → validate → compile → render → write pipeline and map
--   every outcome to an exit code. The option parser lives in
--   "WgForge.CLI.Options"; everything written to disk (config files and the
--   @init@ scaffold) in "WgForge.Output".
module WgForge.CLI (
  run,
  dispatch,
  module WgForge.CLI.Options,
) where

import Control.Exception (IOException, try)
import Control.Monad (unless)
import Data.Bifunctor (first)
import qualified Data.ByteString.Char8 as BS8
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Options.Applicative (customExecParser)
import System.Exit (exitWith)
import System.FilePath (takeDirectory, (</>))
import System.IO (stderr)
import Validation (Validation (Failure, Success))

import WgForge.Allocator
import WgForge.CLI.Options
import WgForge.CLI.Report
import WgForge.Compiler
import WgForge.Diff
import WgForge.Error
import WgForge.Key
import WgForge.Keystore
import WgForge.Output
import WgForge.QR
import WgForge.Renderer
import WgForge.Spec
import WgForge.Spec.Parser
import WgForge.Spec.Validator

-- | Parse argv, run the chosen command, and exit with the mapped code,
--   printing any error to stderr.
run :: IO ()
run = do
  opts <- customExecParser parsePrefs parseOpts
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
dispatch (Diff o) = runDiff o
dispatch (QR o) = runQR o

-- | Short-circuiting bind for the @IO (Either AppError a)@ pipeline: run the
--   first step, stop on a 'Left', otherwise feed the result to the next step.
(>>?) :: IO (Either e a) -> (a -> IO (Either e b)) -> IO (Either e b)
m >>? k = m >>= either (pure . Left) k

infixl 1 >>?

-- | Lift a filesystem failure into the CLI error taxonomy.
fromFileError :: FileError -> AppError
fromFileError (FileError path details) = AppIO path details

-- | Parse and validate a spec file, accumulating all validation errors.
loadValidated :: FilePath -> IO (Either AppError Network)
loadValidated f =
  fmap (first AppSpec) (parseNetworkFile f) >>? \(net, dups) ->
    case maybe (Success net) Failure (nonEmpty dups) *> validateNetwork net of
      Success v -> pure (Right v)
      Failure es -> pure (Left (AppValidation es))

-- | @validate@: parse + validate, silent on success.
runValidate :: FilePath -> IO (Either AppError ())
runValidate f = loadValidated f >>? \_ -> pure (Right ())

-- | @generate@: the full pipeline → one @\<peer\>.conf@ per peer plus keys.
--   Output directories are resolved relative to the spec's own directory (the
--   project root), so the result is independent of the current directory; an
--   absolute @--out@/@--keys@ overrides this.
runGenerate :: GenerateOptions -> IO (Either AppError ())
runGenerate (GenerateOptions spec out keyDir) =
  loadValidated spec >>? \net -> do
    let base = takeDirectory spec
        outDir = base </> out
        keysDir = base </> keyDir
        addrs = allocate (cidr (network net)) (peers net)
    fmap (first AppKeystore) (ensureKeys keysDir (Map.keys (peers net))) >>? \keys -> do
      let compiled = compile keys addrs net
      fmap (first fromFileError) (writeConfigs outDir compiled) >>? \stats -> do
        putStrLn (summarizeWrites stats)
        pure (Right ())

-- | @diff@: report how the spec would change the on-disk configs, without
--   writing anything or touching the keystore. The full report goes to stdout;
--   the result is 'AppDiffDirty' (exit 4) when they differ, @Right ()@ (exit 0)
--   when they match. Out dir is resolved like @generate@'s.
runDiff :: DiffOptions -> IO (Either AppError ())
runDiff (DiffOptions out quiet spec) =
  loadValidated spec >>? \net -> do
    let outDir = takeDirectory spec </> out
        addrs = allocate (cidr (network net)) (peers net)
        keys = placeholderKey <$ peers net
        compiled = compile keys addrs net
        desired = Map.mapWithKey renderConfig compiled
    fmap (first fromFileError) (readConfigs outDir) >>? \disk -> do
      let report = diffConfigs desired disk
      unless quiet (TIO.putStrLn (renderReport report))
      pure (if isDirty report then Left AppDiffDirty else Right ())

-- | A fixed, arbitrary private key used only so 'compile' is total during a
--   diff. The @PrivateKey@/@PublicKey@ lines it produces are masked out before
--   comparison (see "WgForge.Diff"), so the actual key material is irrelevant —
--   diff never reads the real keystore.
placeholderKey :: PrivateKey
placeholderKey =
  either error id (decodePrivateKey (BS8.pack "GEtMFljNTXfN+YEDDFoa8k6ZCQPyCQf7OswTykMbhlg="))

-- | @init@: scaffold a project directory with a starter spec, @out/@, @keys/@.
runInit :: InitOptions -> IO (Either AppError ())
runInit (InitOptions force path) =
  fmap (first fromFileError) (scaffold force path) >>? \() -> do
    putStrLn ("Initialized wg-forge project in " ++ path)
    pure (Right ())

-- | @qr@: encode a peer config as a QR code. Without @--output@ the code is
--   printed to the terminal; with it, a PNG is written to the given path and
--   nothing is printed on success.
runQR :: QROptions -> IO (Either AppError ())
runQR (QROptions output confPath) = do
  readResult <- try (TIO.readFile confPath) :: IO (Either IOException Text)
  case readResult of
    Left err -> pure (Left (AppIO confPath (show err)))
    Right conf -> case encodeToQr conf of
      Nothing -> pure (Left (AppQR "Failed to encode QR: content too long for any QR version"))
      Just qr -> case output of
        Nothing -> Right () <$ TIO.putStrLn (renderQrToAnsii qr)
        Just path -> first fromFileError <$> writeQrPng path (encodeQrToPng qr)
