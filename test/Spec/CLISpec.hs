{-# LANGUAGE OverloadedStrings #-}

module Spec.CLISpec (spec) where

import Control.Monad ((>=>))
import qualified Data.ByteString as BS
import Options.Applicative (defaultPrefs, execParserPure, getParseResult)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (getFileStatus, modificationTimeHiRes)
import Test.Hspec

import WgForge.CLI

-- | Drive the option parser over an argv vector, returning the parsed command.
parse :: [String] -> Maybe Command
parse args = optCommand <$> getParseResult (execParserPure defaultPrefs parseOpts args)

-- | A minimal valid two-peer full-mesh spec (parses and validates cleanly).
sampleSpec :: BS.ByteString
sampleSpec =
  "network:\n\
  \  name: test-net\n\
  \  cidr: 10.0.0.0/24\n\
  \peers:\n\
  \  node-a:\n\
  \    endpoint: a.example.com:51820\n\
  \    listenPort: 51820\n\
  \  node-b:\n\
  \    endpoint: b.example.com:51820\n\
  \    listenPort: 51820\n\
  \segments:\n\
  \  mesh:\n\
  \    topology: full-mesh\n\
  \    peers: [node-a, node-b]\n"

spec :: Spec
spec = describe "WgForge.CLI" $ do
  describe "parseOpts" $ do
    it "parses validate with a spec path" $
      case parse ["validate", "--spec", "x.yaml"] of
        Just (Validate f) -> f `shouldBe` "x.yaml"
        _ -> expectationFailure "expected Validate"

    it "defaults generate's out and keys directories" $
      case parse ["generate", "--spec", "x.yaml"] of
        Just (Generate g) -> do
          genSpec g `shouldBe` "x.yaml"
          genOutDir g `shouldBe` "out"
          genKeyDir g `shouldBe` "keys"
        _ -> expectationFailure "expected Generate"

    it "honours explicit generate -o/-k overrides" $
      case parse ["generate", "-s", "x.yaml", "-o", "cfg", "-k", "secrets"] of
        Just (Generate g) -> do
          genOutDir g `shouldBe` "cfg"
          genKeyDir g `shouldBe` "secrets"
        _ -> expectationFailure "expected Generate"

    it "parses init with and without --force" $ do
      case parse ["init", "--path", "proj"] of
        Just (Init o) -> do
          initPath o `shouldBe` "proj"
          initForce o `shouldBe` False
        _ -> expectationFailure "expected Init"
      case parse ["init", "--path", "proj", "--force"] of
        Just (Init o) -> initForce o `shouldBe` True
        _ -> expectationFailure "expected Init"

    it "fails when validate's required --spec is missing" $
      isNothingCmd (parse ["validate"]) `shouldBe` True

  describe "generate (integration)" $ do
    it "writes one config per peer and is byte-stable across reruns" $
      withSystemTempDirectory "wgf-cli" $ \dir -> do
        let specPath = dir </> "network.yaml"
            outDir = dir </> "out"
            keyDir = dir </> "keys"
            confA = outDir </> "node-a.conf"
            confB = outDir </> "node-b.conf"
        BS.writeFile specPath sampleSpec

        r1 <- dispatch (Generate (GenerateOptions specPath outDir keyDir))
        isRightUnit r1 `shouldBe` True
        mapM_ (doesFileExist >=> (`shouldBe` True)) [confA, confB]

        bytesA1 <- BS.readFile confA
        mtimeA1 <- modificationTimeHiRes <$> getFileStatus confA

        r2 <- dispatch (Generate (GenerateOptions specPath outDir keyDir))
        isRightUnit r2 `shouldBe` True

        bytesA2 <- BS.readFile confA
        mtimeA2 <- modificationTimeHiRes <$> getFileStatus confA
        bytesA2 `shouldBe` bytesA1 -- byte-identical
        mtimeA2 `shouldBe` mtimeA1 -- not rewritten (mtime untouched)
    it "resolves the default out/keys relative to the spec, not the CWD" $
      withSystemTempDirectory "wgf-cli" $ \dir -> do
        let specPath = dir </> "network.yaml"
        BS.writeFile specPath sampleSpec

        -- Defaults ("out", "keys") are relative; they must land beside the
        -- spec at <dir>/out and <dir>/keys, regardless of the process CWD.
        r <- dispatch (Generate (GenerateOptions specPath "out" "keys"))
        isRightUnit r `shouldBe` True
        mapM_
          (doesFileExist >=> (`shouldBe` True))
          [ dir </> "out" </> "node-a.conf",
            dir </> "out" </> "node-b.conf",
            dir </> "keys" </> "node-a.key"
          ]
 where
  isNothingCmd Nothing = True
  isNothingCmd _ = False
  isRightUnit (Right ()) = True
  isRightUnit _ = False
