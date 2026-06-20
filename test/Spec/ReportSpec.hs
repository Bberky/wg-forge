{-# LANGUAGE OverloadedStrings #-}

module Spec.ReportSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text as T
import System.Exit (ExitCode (ExitFailure))
import Test.Hspec

import WgForge.CLI.Report (AppError (..), exitCodeFor, renderAppError)
import WgForge.Error (FileError (..), KeystoreError (..), SpecError (..), ValidationError (..))
import WgForge.Spec (PeerName (..))

spec :: Spec
spec = describe "WgForge.CLI.Report" $ do
  describe "exitCodeFor" $ do
    it "maps YAML syntax errors to 2 (spec validation)" $
      exitCodeFor (AppSpec (YamlSyntaxError "x")) `shouldBe` ExitFailure 2
    it "maps spec parse errors to 2 (spec validation)" $
      exitCodeFor (AppSpec (SpecParseError "x")) `shouldBe` ExitFailure 2
    it "maps spec I/O errors to 3 (IO error)" $
      exitCodeFor (AppSpec (SpecIoError "x")) `shouldBe` ExitFailure 3
    it "maps validation errors to 2 (spec validation)" $
      exitCodeFor (AppValidation (MissingEndpoint (PeerName "alice") :| [])) `shouldBe` ExitFailure 2
    it "maps keystore errors to 3 (IO error)" $
      exitCodeFor (AppKeystore (MissingKey (PeerName "alice"))) `shouldBe` ExitFailure 3
    it "maps IO errors to 3 (IO error)" $
      exitCodeFor (AppIO "p" "d") `shouldBe` ExitFailure 3
    it "maps file errors to 3 (IO error)" $
      exitCodeFor (AppFile (FileError "p" "d")) `shouldBe` ExitFailure 3
    it "maps QR errors to 3 (IO error)" $
      exitCodeFor (AppQR "d") `shouldBe` ExitFailure 3
    it "maps a dirty diff to 4" $
      exitCodeFor AppDiffDirty `shouldBe` ExitFailure 4

  describe "renderAppError" $ do
    it "lists every accumulated validation error" $ do
      let txt =
            renderAppError
              (AppValidation (DuplicatePeerName (PeerName "alice") :| [IslandPeer (PeerName "carol")]))
      txt `shouldSatisfy` T.isInfixOf "alice"
      txt `shouldSatisfy` T.isInfixOf "carol"
    it "includes the path and detail of a file error" $ do
      let txt = renderAppError (AppFile (FileError "/tmp/out" "boom"))
      txt `shouldSatisfy` T.isInfixOf "/tmp/out"
      txt `shouldSatisfy` T.isInfixOf "boom"
    it "renders a dirty diff as a non-empty message" $
      renderAppError AppDiffDirty `shouldSatisfy` (not . T.null)
