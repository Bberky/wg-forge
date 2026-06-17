module WgForge.CLI (
  run,
) where

import Data.Text (unpack)
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
  progDesc,
  short,
  strOption,
  switch,
 )
import System.Exit (exitWith)

import WgForge.CLI.Report (AppError, exitCodeFor, renderAppError)

data Options = Options
  { optVerbose :: Bool,
    optCommand :: Command
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

verbose :: Parser Bool
verbose = switch (long "verbose" <> short 'v' <> help "Enable verbose output")

initOpts :: Parser Command
initOpts =
  Init
    <$> ( InitOptions
            <$> strOption (long "path" <> short 'p' <> help "Path to initialize the project in")
            <*> switch (long "force" <> short 'f' <> help "Force initialization even if the directory is not empty")
        )

initCmd :: Mod CommandFields Command
initCmd = command "init" (info initOpts (progDesc "Initialize a new wg-forge project"))

programOpts :: Parser Options
programOpts =
  Options <$> verbose <*> hsubparser initCmd

parseOpts :: ParserInfo Options
parseOpts =
  info
    (helper <*> programOpts)
    ( fullDesc
        <> progDesc "wg-forge: a WireGuard configuration generator"
        <> header "wg-forge - generate WireGuard configs from a network specification"
    )

run :: IO ()
run = do
  opts <- execParser parseOpts
  result <- dispatch (optCommand opts)
  case result of
    Left err -> do
      putStrLn . unpack $ renderAppError err
      exitWith (exitCodeFor err)
    Right () -> return ()

dispatch :: Command -> IO (Either AppError ())
dispatch = undefined
