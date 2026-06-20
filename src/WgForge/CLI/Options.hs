-- | Command-line options for wg-forge.
--  This module defines the data types representing the command-line options and
--  the parsers for those options using the optparse-applicative library.
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

import Options.Applicative

-- | The top-level options data type, representing the command and its associated options.
newtype Options = Options
  { cmd :: Command
  }

-- | The different commands supported by wg-forge, each with its own associated options.
data Command
  = -- | Initialize a new wg-forge project
    Init InitOptions
  | -- | Validate a network specification YAML file
    Validate FilePath
  | -- | Generate configurations from a network specification
    Generate GenerateOptions
  | -- | Show how the specification differs from the configurations on disk
    Diff DiffOptions
  | -- | Generate a QR code from a peer config file
    QR QROptions

-- | Options for the 'init' command.
data InitOptions = InitOptions
  { -- | Whether to force initialization even if the directory is not empty.
    force :: Bool,
    -- | The path to initialize the project in.
    path :: FilePath
  }

-- | Options for the 'generate' command.
data GenerateOptions = GenerateOptions
  { -- | The output directory for the generated configurations.
    outDir :: FilePath,
    -- | The directory containing the WireGuard keys.
    keyDir :: FilePath,
    -- | The path to the network specification YAML file.
    spec :: FilePath
  }

-- | Options for the 'diff' command.
data DiffOptions = DiffOptions
  { -- | The output directory for the generated configurations.
    outDir :: FilePath,
    -- | Whether to suppress output of unchanged files.
    quiet :: Bool,
    -- | The path to the network specification YAML file.
    spec :: FilePath
  }

-- | Options for the 'qr' command.
data QROptions = QROptions
  { -- | The path to save the QR code image.
    out :: Maybe FilePath,
    -- | The path to the WireGuard peer configuration file.
    peerConf :: FilePath
  }

initOpts :: Parser Command
initOpts =
  Init
    <$> ( InitOptions
            <$> switch (long "force" <> short 'f' <> help "Force initialization even if the directory is not empty")
            <*> strOption (long "path" <> short 'p' <> metavar "DIR" <> help "Path to initialize the project in")
        )

initCmd :: Mod CommandFields Command
initCmd = command "init" (info initOpts (progDesc "Initialize a new wg-forge project"))

generateOpts :: Parser Command
generateOpts = Generate <$> (GenerateOptions <$> outOpt <*> keysOpt <*> specArg)

generateCmd :: Mod CommandFields Command
generateCmd =
  command
    "generate"
    (info generateOpts (progDesc "Generate configurations from a network specification"))

diffOpts :: Parser Command
diffOpts =
  Diff
    <$> ( DiffOptions
            <$> outOpt
            <*> switch
              ( long "quiet"
                  <> short 'q'
                  <> help "Suppress output of unchanged files"
              )
            <*> specArg
        )

diffCmd :: Mod CommandFields Command
diffCmd =
  command
    "diff"
    (info diffOpts (progDesc "Show how the specification differs from the configurations on disk"))

validateOpts :: Parser Command
validateOpts = Validate <$> specArg

validateCmd :: Mod CommandFields Command
validateCmd = command "validate" (info validateOpts (progDesc "Validate a network specification YAML file"))

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

specArg :: Parser FilePath
specArg = argument str (metavar "FILE" <> help "Path to the network specification YAML file")

outOpt :: Parser FilePath
outOpt =
  strOption
    ( long "out"
        <> short 'o'
        <> metavar "DIR"
        <> value "out"
        <> help
          "Directory of generated configurations to compare against, relative to the spec unless absolute (default: out)"
    )

keysOpt :: Parser FilePath
keysOpt =
  strOption
    ( long "keys"
        <> short 'k'
        <> metavar "DIR"
        <> value "keys"
        <> help "Directory for WireGuard private keys, relative to the spec unless absolute (default: keys)"
    )
