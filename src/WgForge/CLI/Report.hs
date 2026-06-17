module WgForge.CLI.Report (
  AppError (..),
  renderAppError,
  exitCodeFor,
) where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text, pack)
import System.Exit (ExitCode (ExitSuccess))

import WgForge.Error (KeystoreError, SpecError, ValidationError)

data AppError
  = AppSpec SpecError
  | AppValidation (NonEmpty ValidationError)
  | AppKeystore KeystoreError
  | AppIO FilePath String

renderAppError :: AppError -> Text
renderAppError (AppSpec _) = pack "Specification error"
renderAppError (AppValidation _) = pack "Validation errors"
renderAppError (AppKeystore _) = pack "Keystore error"
renderAppError (AppIO _ _) = pack "I/O error at"

exitCodeFor :: AppError -> ExitCode
exitCodeFor _ = ExitSuccess
