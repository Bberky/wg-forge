module WgForge.CLI.Report (
  AppError (..),
  renderAppError,
  exitCodeFor,
) where

import Data.List.NonEmpty (NonEmpty, toList)
import Data.Text (Text, pack)
import System.Exit (ExitCode (ExitFailure))

import WgForge.Error
import WgForge.Spec (PeerName (PeerName))

data AppError
  = AppSpec SpecError
  | AppValidation (NonEmpty ValidationError)
  | AppKeystore KeystoreError
  | AppIO FilePath String
  | AppQR String

renderAppError :: AppError -> Text
renderAppError (AppSpec err) =
  pack "Specification error" <> case err of
    YamlSyntaxError details -> pack $ ": YAML syntax error at " <> details
    SpecParseError details -> pack $ ": Specification parse error: " <> details
    SpecIoError details -> pack $ ": Specification I/O error: " <> details
renderAppError (AppValidation err) = pack "Validation error: " <> pack (show (toList err))
renderAppError (AppKeystore err) =
  pack "Keystore error" <> case err of
    KeyIoError path details -> pack $ ": I/O error at " <> path <> ": " <> details
    MalformedKey path details -> pack $ ": Malformed key at " <> path <> ": " <> details
    MissingKey (PeerName peer) -> pack $ ": Missing key for peer " <> show peer
renderAppError (AppIO path details) = pack $ "I/O error at " <> path <> ": " <> details
renderAppError (AppQR details) = pack $ "QR code error: " <> details

exitCodeFor :: AppError -> ExitCode
exitCodeFor (AppSpec specErr) = case specErr of
  YamlSyntaxError _ -> ExitFailure 2
  SpecParseError _ -> ExitFailure 2
  _ -> ExitFailure 3
exitCodeFor (AppValidation _) = ExitFailure 2
exitCodeFor (AppKeystore _) = ExitFailure 3
exitCodeFor (AppIO _ _) = ExitFailure 3
