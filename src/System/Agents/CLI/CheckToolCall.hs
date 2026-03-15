{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Module for the 'check-tool-call' command handler.

This command validates a tool call payload against a tool's schema.
It reads a JSON payload from stdin and validates it against the
schema of the specified tool.

Usage:
    cat payload.json | agents-exe check-tool-call --tool <path-to-tool>

The tool path should point to a bash tool script that follows the
standard describe|run protocol.
-}
module System.Agents.CLI.CheckToolCall (
    handleCheckToolCall,
    CheckToolCallOptions (..),
) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Exit (exitFailure)
import System.IO (stderr)

import qualified Prod.Tracer as Prod
import qualified System.Agents.ToolRegistration as ToolReg
import System.Agents.Tools.Bash (ScriptDescription (..), ScriptInfo (..), loadScript)
import System.Agents.Tools.Validation (formatValidationErrors, validateToolInput)

-- | Options for the check-tool-call command
data CheckToolCallOptions = CheckToolCallOptions
    { toolPath :: FilePath
    -- ^ Path to the tool script to validate against
    }
    deriving (Show, Eq)

{- | Handle the check-tool-call command.

Reads a JSON payload from stdin and validates it against the tool's schema.
Returns exit code 0 if valid, 1 if invalid.
-}
handleCheckToolCall :: CheckToolCallOptions -> IO ()
handleCheckToolCall opts = do
    -- Load the tool to get its schema
    toolResult <- loadScript Prod.silent opts.toolPath
    case toolResult of
        Left err -> do
            Text.hPutStrLn stderr $ "Failed to load tool: " <> Text.pack (show err)
            exitFailure
        Right scriptDesc -> do
            -- Read JSON payload from stdin
            input <- LByteString.getContents
            case Aeson.decode' input of
                Nothing -> do
                    Text.hPutStrLn stderr "Error: Invalid JSON input from stdin"
                    exitFailure
                Just value -> do
                    -- Get the tool schema and validate
                    let schema = ToolReg.mapArg <$> scriptDesc.scriptInfo.scriptArgs
                    let errors = validateToolInput schema value
                    if null errors
                        then do
                            Text.putStrLn "✓ Tool call payload is valid"
                        else do
                            let toolName =
                                    Text.pack opts.toolPath
                                        <> " ("
                                        <> scriptDesc.scriptInfo.scriptSlug
                                        <> ")"
                            Text.putStrLn $ formatValidationErrors toolName errors
                            exitFailure

