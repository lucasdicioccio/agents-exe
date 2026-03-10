module Agq.Run
  ( runCmd
  , captureCmd
  , captureCmdBoth
  , runGit
  , runGh
  , runAgentsExe
  ) where

import Data.Text (Text)
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.ByteString as BS
import System.Exit (ExitCode(..))
import System.Process

-- | Run a command, streaming output to stdout/stderr. Returns ExitCode.
runCmd :: FilePath -> [String] -> IO ExitCode
runCmd cmd args = do
  let p = (proc cmd args) { std_in = NoStream }
  (_, _, _, ph) <- createProcess p
  waitForProcess ph

-- | Run a command capturing stdout as Text. Streams stderr.
captureCmd :: FilePath -> [String] -> IO (ExitCode, Text)
captureCmd cmd args = do
  let p = (proc cmd args) { std_in = NoStream, std_out = CreatePipe }
  (_, mout, _, ph) <- createProcess p
  out <- case mout of
    Nothing -> return ""
    Just h  -> do
      bs <- BS.hGetContents h
      return (TextEncoding.decodeUtf8 bs)
  ec <- waitForProcess ph
  return (ec, out)

-- | Run a command capturing both stdout and stderr as Text.
captureCmdBoth :: FilePath -> [String] -> IO (ExitCode, Text, Text)
captureCmdBoth cmd args = do
  let p = (proc cmd args) { std_in = NoStream, std_out = CreatePipe, std_err = CreatePipe }
  (_, mout, merr, ph) <- createProcess p
  out <- case mout of
    Nothing -> return ""
    Just h  -> fmap TextEncoding.decodeUtf8 (BS.hGetContents h)
  err <- case merr of
    Nothing -> return ""
    Just h  -> fmap TextEncoding.decodeUtf8 (BS.hGetContents h)
  ec <- waitForProcess ph
  return (ec, out, err)

-- | Run git with given args, streaming output.
runGit :: [String] -> IO ExitCode
runGit = runCmd "git"

-- | Run gh with given args, capturing stdout.
runGh :: [String] -> IO (ExitCode, Text)
runGh = captureCmd "gh"

-- | Run agents-exe with given args, capturing stdout.
runAgentsExe :: [String] -> IO (ExitCode, Text)
runAgentsExe = captureCmd "agents-exe"
