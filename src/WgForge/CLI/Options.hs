-- | Command-line surface: the parsed 'Command' tree and the
--   @optparse-applicative@ parser that produces it.
module WgForge.CLI.Options (
  Options (..),
  Command (..),
  InitOptions (..),
  GenerateOptions (..),
  DiffOptions (..),
  QROptions (..),
  parseOpts,
  parsePrefs,
) where

import Options.Applicative (
  CommandFields,
  Mod,
  Parser,
  ParserInfo,
  ParserPrefs,
  argument,
  command,
  fullDesc,
  header,
  help,
  helper,
  hsubparser,
  info,
  long,
  metavar,
  optional,
  prefs,
  progDesc,
  short,
  showHelpOnEmpty,
  str,
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
  | Diff DiffOptions
  | QR QROptions

data InitOptions = InitOptions
  { initPath :: FilePath,
    initForce :: Bool
  }

data GenerateOptions = GenerateOptions
  { genSpec :: FilePath,
    genOutDir :: FilePath,
    genKeyDir :: FilePath
  }

data DiffOptions = DiffOptions
  { diffSpec :: FilePath,
    diffOutDir :: FilePath
  }

data QROptions = QROptions
  { qrOutput :: Maybe FilePath,
    qrPeerConfig :: FilePath
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

diffOpts :: Parser Command
diffOpts =
  Diff
    <$> ( DiffOptions
            <$> strOption
              (long "spec" <> short 's' <> metavar "FILE" <> help "Path to the network specification YAML file")
            <*> strOption
              ( long "out"
                  <> short 'o'
                  <> metavar "DIR"
                  <> value "out"
                  <> help
                    "Directory of generated configurations to compare against, relative to the spec unless absolute (default: out)"
              )
        )

diffCmd :: Mod CommandFields Command
diffCmd =
  command
    "diff"
    (info diffOpts (progDesc "Show how the specification differs from the configurations on disk"))

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

qrOpts :: Parser Command
qrOpts =
  QR
    <$> ( QROptions
            <$> optional
              ( strOption
                  ( long "output"
                      <> short 'o'
                      <> metavar "FILE"
                      <> help
                        "Path to save the QR code image (PNG format). If not provided, the QR code will be printed to the terminal."
                  )
              )
            <*> argument
              str
              ( metavar "FILE"
                  <> help "Path to the WireGuard peer configuration file to encode as a QR code"
              )
        )

qrCmd :: Mod CommandFields Command
qrCmd = command "qr" (info qrOpts (progDesc "Generate a QR code from a peer config file"))

programOpts :: Parser Options
programOpts =
  Options <$> hsubparser (initCmd <> validateCmd <> generateCmd <> diffCmd <> qrCmd)

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
