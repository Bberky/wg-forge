-- | Defines the 'AppError' type for representing errors in the wg-forge CLI application,
--   along with functions to render these errors as human-readable text,
--   and to determine appropriate exit codes.
module WgForge.CLI.Report (
  AppError (..),
  renderAppError,
  exitCodeFor,
) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (ExitCode (..))

import WgForge.Error
import WgForge.Spec

-- | The 'AppError' type represents the various kinds of errors that can occur in the wg-forge CLI.
data AppError
  = AppSpec SpecError
  | AppValidation (NonEmpty ValidationError)
  | AppKeystore KeystoreError
  | AppIO FilePath String
  | AppFile FileError
  | AppQR String
  | AppDiffDirty

-- | Render an 'AppError' as human-readable text.
renderAppError :: AppError -> Text
renderAppError (AppSpec err) =
  "Specification error" <> case err of
    YamlSyntaxError details -> T.pack $ ": YAML syntax error at " <> details
    SpecParseError details -> T.pack $ ": Specification parse error: " <> details
    SpecIoError details -> T.pack $ ": Specification I/O error: " <> details
renderAppError (AppValidation err) = "Validation error: " <> T.pack (show (NE.toList err))
renderAppError (AppKeystore err) =
  "Keystore error" <> case err of
    KeyIoError path details -> T.pack $ ": I/O error at " <> path <> ": " <> details
    MalformedKey path details -> T.pack $ ": Malformed key at " <> path <> ": " <> details
    MissingKey (PeerName peer) -> T.pack $ ": Missing key for peer " <> show peer
renderAppError (AppIO path details) = T.pack $ "I/O error at " <> path <> ": " <> details
renderAppError (AppFile (FileError path details)) = T.pack $ "File error at " <> path <> ": " <> details
renderAppError (AppQR details) = T.pack $ "QR code error: " <> details
renderAppError AppDiffDirty = "diff: on-disk configuration differs from the specification"

-- | Determine the appropriate exit code for a given 'AppError'.
exitCodeFor :: AppError -> ExitCode
exitCodeFor (AppSpec specErr) = case specErr of
  YamlSyntaxError _ -> ExitFailure 2
  SpecParseError _ -> ExitFailure 2
  _ -> ExitFailure 3
exitCodeFor (AppValidation _) = ExitFailure 2
exitCodeFor (AppKeystore _) = ExitFailure 3
exitCodeFor (AppIO _ _) = ExitFailure 3
exitCodeFor (AppFile _) = ExitFailure 3
exitCodeFor (AppQR _) = ExitFailure 3
exitCodeFor AppDiffDirty = ExitFailure 4
