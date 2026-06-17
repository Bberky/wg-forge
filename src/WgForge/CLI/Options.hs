-- | Command-line surface: the parsed 'Command' tree and the
--   @optparse-applicative@ parser that produces it.
module WgForge.CLI.Options (
  Options (..),
  Command (..),
  InitOptions (..),
  GenerateOptions (..),
  parseOpts,
  parsePrefs,
) where

import Options.Applicative (
  CommandFields,
  Mod,
  Parser,
  ParserInfo,
  ParserPrefs,
  command,
  fullDesc,
  header,
  help,
  helper,
  hsubparser,
  info,
  long,
  metavar,
  prefs,
  progDesc,
  short,
  showHelpOnEmpty,
  strOption,
  switch,
  value,
 )

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
                  <> help
                    "Output directory for generated configurations, relative to the spec unless absolute (default: out)"
              )
            <*> strOption
              ( long "keys"
                  <> short 'k'
                  <> metavar "DIR"
                  <> value "keys"
                  <> help "Directory for WireGuard private keys, relative to the spec unless absolute (default: keys)"
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

-- | Parser preferences: when invoked with no arguments, show the full help
--   text (with the command list) instead of a terse @Missing: COMMAND@ usage.
parsePrefs :: ParserPrefs
parsePrefs = prefs showHelpOnEmpty

-- | The top-level parser, including @--help@.
parseOpts :: ParserInfo Options
parseOpts =
  info
    (helper <*> programOpts)
    ( fullDesc
        <> header "wg-forge: a WireGuard© configuration generator"
        <> progDesc
          "Generate WireGuard© configurations from a high-level network specification, with support for validation, key management, and multiple network topologies."
    )
