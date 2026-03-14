{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Module for the standalone 'describe-tool' command handler.

The describe-tool command validates and displays tool registration information
for bash tool scripts without requiring an agent configuration. This enables
rapid iteration when developing new tools.

Example usage:

> agents-exe describe-tool /path/to/tool-script

Outputs the parsed 'ScriptInfo' as formatted JSON on success, or a helpful
error message on failure.
-}
module System.Agents.CLI.DescribeTool (
    handleDescribeTool,
    DescribeToolOptions (..),
    OutputFormat (..),
) where

import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as LByteString
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.IO as Text
import qualified Prod.Tracer as Prod
import System.Exit (exitFailure)
import System.IO (stderr, stdout)

import System.Agents.Tools.Bash (ScriptArg (..), ScriptDescription (..), ScriptInfo (..))
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
            Text.hPutStrLn stderr $ formatError err
            exitFailure
        Right scriptDesc -> do
            if opts.describeCheckOnly
                then do
                    Text.putStrLn $ "Tool '" <> scriptDesc.scriptInfo.scriptSlug <> "' is valid."
                else case opts.describeOutputFormat of
                    FormatJson -> do
                        LByteString.hPutStr stdout $ Aeson.encodePretty scriptDesc.scriptInfo
                        -- Add trailing newline after JSON output
                        Text.putStrLn ""
                    FormatPretty ->
                        printPrettyDescription scriptDesc

-- | Format an error for display
formatError :: Bash.InvalidScriptError -> Text
formatError (Bash.InvalidScriptError path code errOutput) =
    Text.unlines
        [ "Error: Tool script failed during describe"
        , "  Path: " <> Text.pack path
        , "  Exit code: " <> Text.pack (show code)
        , "  Stderr: " <> Text.decodeUtf8With lenientDecode errOutput
        ]
formatError (Bash.InvalidDescriptionError path jsonErr) =
    Text.unlines
        [ "Error: Tool returned invalid JSON description"
        , "  Path: " <> Text.pack path
        , "  Parse error: " <> Text.pack jsonErr
        , ""
        , "Hint: Run the tool with 'describe' argument manually to see its output:"
        , "  " <> Text.pack path <> " describe"
        ]

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
