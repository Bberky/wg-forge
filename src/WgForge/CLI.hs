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

import Data.Bifunctor (first)
import qualified Data.Map.Strict as Map
import qualified Data.Text.IO as TIO
import Options.Applicative (execParser)
import System.Exit (exitWith)
import System.FilePath (takeDirectory, (</>))
import System.IO (stderr)
import Validation (Validation (Failure, Success))

import WgForge.Allocator (allocate)
import WgForge.CLI.Options
import WgForge.CLI.Report (AppError (..), exitCodeFor, renderAppError)
import WgForge.Compiler (compile)
import WgForge.Error (FileError (FileError))
import WgForge.Keystore (ensureKeys)
import WgForge.Output (scaffold, summarizeWrites, writeConfigs)
import WgForge.Spec (Network (..), NetworkSpec (cidr))
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

-- | Lift a filesystem failure into the CLI error taxonomy.
fromFileError :: FileError -> AppError
fromFileError (FileError path details) = AppIO path details

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

-- | @init@: scaffold a project directory with a starter spec, @out/@, @keys/@.
runInit :: InitOptions -> IO (Either AppError ())
runInit (InitOptions path force) =
  fmap (first fromFileError) (scaffold force path) >>? \() -> do
    putStrLn ("Initialized wg-forge project in " ++ path)
    pure (Right ())
