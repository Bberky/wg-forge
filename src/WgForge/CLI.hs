-- | The 'WgForge.CLI' module defines the main entry point for the wg-forge command-line interface,
--   including command parsing, dispatching to handlers, and error reporting.
--   It re-exports the 'Options' and 'Report' modules for convenience.
module WgForge.CLI (
  run,
  module WgForge.CLI.Options,
  module WgForge.CLI.Report,
) where

import Control.Exception (IOException, try)
import Control.Monad (unless)
import Data.Bifunctor (Bifunctor (second), first)
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
  result <- dispatch (opts.cmd)
  case result of
    Left err -> do
      TIO.hPutStrLn stderr (renderAppError err)
      exitWith (exitCodeFor err)
    Right () -> pure ()

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

loadValidated :: FilePath -> IO (Either AppError Network)
loadValidated f =
  fmap (first AppSpec) (parseNetworkFile f) >>? \(net, dups) ->
    case maybe (Success net) Failure (nonEmpty dups) *> validateNetwork net of
      Success v -> pure (Right v)
      Failure es -> pure (Left (AppValidation es))

runValidate :: FilePath -> IO (Either AppError ())
runValidate f = fmap (second (const ())) (loadValidated f)

runGenerate :: GenerateOptions -> IO (Either AppError ())
runGenerate (GenerateOptions outPath keysPath specPath) =
  loadValidated specPath >>? \net -> do
    let base = takeDirectory specPath
        dest = base </> outPath
        keysDir = base </> keysPath
        addrs = allocate (cidr (network net)) (peers net)
    fmap (first AppKeystore) (ensureKeys keysDir (Map.keys (peers net))) >>? \keys -> do
      let compiled = compile keys addrs net
      fmap (first AppFile) (writeConfigs dest compiled) >>? \stats -> do
        putStrLn (summarizeWrites stats)
        pure (Right ())

runDiff :: DiffOptions -> IO (Either AppError ())
runDiff (DiffOptions outPath q specPath) =
  loadValidated specPath >>? \net -> do
    let dest = takeDirectory specPath </> outPath
        addrs = allocate (cidr (network net)) (peers net)
        keys = placeholderKey <$ peers net
        compiled = compile keys addrs net
        desired = Map.mapWithKey renderConfig compiled
    fmap (first AppFile) (readConfigs dest) >>? \disk -> do
      let report = diffConfigs desired disk
      unless q (TIO.putStrLn (renderReport report))
      pure (if isDirty report then Left AppDiffDirty else Right ())

placeholderKey :: PrivateKey
placeholderKey =
  either error id (decodePrivateKey "GEtMFljNTXfN+YEDDFoa8k6ZCQPyCQf7OswTykMbhlg=")

runInit :: InitOptions -> IO (Either AppError ())
runInit (InitOptions forceInit dest) =
  fmap (first AppFile) (scaffold forceInit dest) >>? \_ -> do
    putStrLn ("Initialized wg-forge project in " ++ dest)
    pure (Right ())

runQR :: QROptions -> IO (Either AppError ())
runQR (QROptions output confPath) = do
  readResult <- try (TIO.readFile confPath) :: IO (Either IOException Text)
  let qr = first (AppIO confPath . show) readResult >>= encodeQr
  either (pure . Left) (emitQr output) qr
 where
  encodeQr conf =
    maybe
      (Left (AppQR "Failed to encode QR: content too long for any QR version"))
      Right
      (encodeToQr conf)
  emitQr Nothing q = Right () <$ TIO.putStrLn (renderQrToAnsii q)
  emitQr (Just dest) q = first AppFile <$> writeQrPng dest (encodeQrToPng q)
