{-# LANGUAGE ScopedTypeVariables #-}

-- | A thin harness for driving the public CLI entry point ('run') from tests.
--   Since @dispatch@ and the per-command handlers are private, 'run' is the only
--   public way to exercise the full command pipeline end-to-end.
module Spec.CliHarness (runCli) where

import Control.Exception (bracket, try)
import Data.Either (fromLeft)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.Environment (withArgs)
import System.Exit (ExitCode (ExitSuccess))
import System.IO (hClose, hFlush, readFile', stderr, stdout)
import System.IO.Temp (withSystemTempFile)

import WgForge.CLI (run)

-- | Run the CLI over an argv vector, returning the resulting exit code together
--   with everything the command wrote to stderr. A normal return counts as
--   'ExitSuccess'; an 'exitWith' (the failure path) is caught and reported.
--   Both standard streams are redirected to temp files for the duration so the
--   test log stays clean; only stderr is read back, for error assertions.
runCli :: [String] -> IO (ExitCode, String)
runCli args =
  withSystemTempFile "wgf-stdout" $ \_ outH ->
    withSystemTempFile "wgf-stderr" $ \errPath errH -> do
      code <-
        bracket
          ( do
              savedOut <- hDuplicate stdout
              savedErr <- hDuplicate stderr
              hDuplicateTo outH stdout
              hDuplicateTo errH stderr
              pure (savedOut, savedErr)
          )
          ( \(savedOut, savedErr) -> do
              hFlush stdout
              hFlush stderr
              hDuplicateTo savedOut stdout
              hClose savedOut
              hDuplicateTo savedErr stderr
              hClose savedErr
          )
          (\_ -> fromLeft ExitSuccess <$> (try (withArgs args run) :: IO (Either ExitCode ())))
      hClose errH
      errText <- readFile' errPath
      pure (code, errText)
