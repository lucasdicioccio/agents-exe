{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Opt-in build-command capability for the DeveloperToolbox.

Runs the argv configured in the toolbox description (no shell) and returns
its exit code and combined stdout/stderr. The command is fixed by the
configuration; the model cannot choose or alter it.
-}
module System.Agents.Tools.DeveloperToolbox.BuildCommand (
    executeBuildCommand,
) where

import Control.Exception (IOException, try)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)

import System.Agents.Base (DeveloperToolboxDescription (..))
import System.Agents.Tools.DeveloperToolbox.Types

-- | Seconds before a running build is abandoned.
buildTimeoutSeconds :: Int
buildTimeoutSeconds = 600

-- | Keep only the tail of very long output (errors come last).
maxOutputChars :: Int
maxOutputChars = 20000

executeBuildCommand :: Toolbox -> IO (Either DeveloperToolError Text)
executeBuildCommand toolbox =
    case toolbox.toolboxConfig.developerToolboxBuildCommand of
        Just (prog : args) -> do
            r <-
                try $
                    timeout (buildTimeoutSeconds * 1000000) $
                        readProcessWithExitCode (Text.unpack prog) (map Text.unpack args) ""
            pure $ case r of
                Left (e :: IOException) ->
                    Left $ ValidationError $ "Failed to run build command: " <> Text.pack (show e)
                Right Nothing ->
                    Left $ ValidationError $ "Build command timed out after " <> Text.pack (show buildTimeoutSeconds) <> "s"
                Right (Just (code, out, err)) ->
                    Right $
                        Text.unlines
                            [ "command: " <> Text.unwords (prog : args)
                            , "exit code: " <> renderCode code
                            , "output:"
                            , tailChars (Text.pack (out <> err))
                            ]
        _ -> pure $ Left $ ValidationError "No buildCommand configured for this toolbox"
  where
    renderCode ExitSuccess = "0"
    renderCode (ExitFailure n) = Text.pack (show n)
    tailChars t
        | Text.length t > maxOutputChars = "[...truncated...]\n" <> Text.takeEnd maxOutputChars t
        | otherwise = t
