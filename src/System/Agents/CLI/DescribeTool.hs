{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Module for the standalone 'describe' command handler.

The describe command validates and displays tool registration information
for bash tool scripts without requiring an agent configuration.
-}
module System.Agents.CLI.DescribeTool (
    handleDescribeTool,
    DescribeToolOptions (..),
    OutputFormat (..),
) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as LByteString
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Exit (exitFailure)
import System.IO (stderr)
import qualified Prod.Tracer as Prod

import System.Agents.Tools.Bash (ScriptArg (..), ScriptArgArity (..), ScriptArgCallingMode (..), ScriptDescription (..), ScriptInfo (..))
import qualified System.Agents.Tools.Bash as Bash

-- | Output format for tool description
data OutputFormat
    = FormatJson
    | FormatPretty
    deriving (Show, Eq)

-- | Options for the describe tool command
data DescribeToolOptions = DescribeToolOptions
    { describeToolPath :: FilePath
    -- ^ Path to the tool script to describe
    , describeOutputFormat :: OutputFormat
    -- ^ Output format (json or pretty)
    , describeCheckOnly :: Bool
    -- ^ Only check validity, don't output description
    }
    deriving (Show, Eq)

-- | Handle the describe tool command: validate and display tool info
handleDescribeTool ::
    -- | Options for describe command
    DescribeToolOptions ->
    IO ()
handleDescribeTool opts = do
    -- Load the script using the Bash module
    result <- Bash.loadScript Prod.silent opts.describeToolPath
    case result of
        Left err -> do
            Text.hPutStrLn stderr $ "Error loading tool: " <> Text.pack (show err)
            exitFailure
        Right scriptDesc -> do
            if opts.describeCheckOnly
                then do
                    Text.putStrLn $ "Tool '" <> scriptDesc.scriptInfo.scriptSlug <> "' is valid."
                else case opts.describeOutputFormat of
                    FormatJson ->
                        LByteString.writeFile "/dev/stdout" $
                            Aeson.encodePretty scriptDesc.scriptInfo
                    FormatPretty ->
                        printPrettyDescription scriptDesc

-- | Print a human-readable description of the tool
printPrettyDescription :: ScriptDescription -> IO ()
printPrettyDescription desc = do
    let info = desc.scriptInfo
    Text.putStrLn $ "Tool: " <> info.scriptSlug
    Text.putStrLn $ "Description: " <> info.scriptDescription
    Text.putStrLn $ "Path: " <> Text.pack desc.scriptPath
    Text.putStrLn ""
    Text.putStrLn "Arguments:"
    if null info.scriptArgs
        then Text.putStrLn "  (none)"
        else mapM_ printArg info.scriptArgs
    case info.scriptEmptyResultBehavior of
        Nothing -> pure ()
        Just behavior -> do
            Text.putStrLn ""
            Text.putStrLn $ "Empty result behavior: " <> Text.pack (show behavior)
  where
    printArg :: ScriptArg -> IO ()
    printArg arg = do
        Text.putStrLn $ "  - " <> arg.argName
        Text.putStrLn $ "      Description: " <> arg.argDescription
        Text.putStrLn $ "      Type: " <> arg.argTypeString
        Text.putStrLn $ "      Arity: " <> Text.pack (show arg.argTypeArity)
        Text.putStrLn $ "      Mode: " <> Text.pack (show arg.argCallingMode)
        Text.putStrLn ""

