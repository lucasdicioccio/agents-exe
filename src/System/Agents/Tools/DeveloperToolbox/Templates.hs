{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Template generation for the DeveloperToolbox.

This module provides config-based templates for creating new tool scripts
in various languages.
-}
module System.Agents.Tools.DeveloperToolbox.Templates (
    -- * Config-based tool templates
    makeToolTemplateFromConfig,
    makeBashToolTemplateFromConfig,
    makePythonToolTemplateFromConfig,
    makeHaskellToolTemplateFromConfig,

    -- * Template helpers
    toolConfigToAeson,
    defaultDeveloperToolboxDescription,
) where

import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString.Lazy as LByteString
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding

import System.Agents.Base (
    BuiltinToolboxDescription (..),
    DeveloperToolCapability (..),
    DeveloperToolboxDescription (..),
 )
import System.Agents.Tools.DeveloperToolbox.Types (
    ScriptArg (..),
    ToolConfig (..),
 )

-- Config-Based Tool Templates
-------------------------------------------------------------------------------

-- | Generate tool content from config.
makeToolTemplateFromConfig :: Text -> ToolConfig -> Text
makeToolTemplateFromConfig language config = case language of
    "python" -> makePythonToolTemplateFromConfig config
    "haskell" -> makeHaskellToolTemplateFromConfig config
    _ -> makeBashToolTemplateFromConfig config

-- | Create a bash tool from config.
makeBashToolTemplateFromConfig :: ToolConfig -> Text
makeBashToolTemplateFromConfig config =
    Text.unlines $
        [ "#!/bin/bash"
        , ""
        , "# NOTE: This script must be executable (chmod +x) to be loaded as a tool."
        , ""
        , "# " <> toolConfigSlug config <> " - " <> toolConfigDescription config
        , ""
        , descLine
        , "    cat <<'EOF'"
        , "{"
        , "  \"slug\": \"" <> toolConfigSlug config <> "\","
        , "  \"description\": \"" <> toolConfigDescription config <> "\","
        , "  \"args\": ["
        ]
            ++ formatArgs (toolConfigArgs config)
            ++ ["  ],"]
            ++ formatEmptyResult (toolConfigEmptyResult config)
            ++ [ "}"
               , "EOF"
               , "    exit 0"
               , "fi"
               , ""
               , "# Parse arguments for 'run' command"
               ]
            ++ parseArgsCode (toolConfigArgs config)
            ++ [ ""
               , "# Main logic here"
               , execLine
               ]
  where
    -- Build the if statement line properly without escaping issues
    descLine = "if [ \"" <> "$" <> "1\" == \"describe\" ]; then"
    execLine = "echo \"Tool " <> toolConfigSlug config <> " executed\""

    formatArgs [] = ["  "]
    formatArgs args =
        let formatted = zipWith (formatArg (length args)) [0 ..] args
         in formatted

    formatArg total idx arg =
        let isLast = idx == total - 1
            comma = if isLast then "" else ","
         in "    {"
                <> "      \"name\": \""
                <> scriptArgName arg
                <> "\","
                <> "      \"description\": \""
                <> scriptArgDescription arg
                <> "\","
                <> "      \"type\": \""
                <> scriptArgType arg
                <> "\","
                <> "      \"backing_type\": \""
                <> scriptArgBackingType arg
                <> "\","
                <> "      \"arity\": \""
                <> scriptArgArity arg
                <> "\","
                <> "      \"mode\": \""
                <> scriptArgMode arg
                <> "\""
                <> "    }"
                <> comma

    formatEmptyResult Nothing = ["  \"empty-result\": { \"tag\": \"AddMessage\", \"contents\": \"No results\" }"]
    formatEmptyResult (Just val) = ["  \"empty-result\": " <> TextEncoding.decodeUtf8 (LByteString.toStrict (AesonPretty.encodePretty val))]

    parseArgsCode [] = []
    parseArgsCode args = concatMap parseOneArg args

    parseOneArg arg =
        let varName = Text.toUpper $ Text.replace "-" "_" (scriptArgName arg)
            argFlag = "--" <> scriptArgName arg
         in case scriptArgMode arg of
                "stdin" ->
                    [ varName <> "=$(cat)"
                    ]
                "positional" ->
                    [ "# Positional argument: " <> scriptArgName arg
                    ]
                "dashdashequal" ->
                    [ varName <> "=\"\""
                    , "while [[ " <> "$" <> "# -gt 0 ]]; do"
                    , "    case " <> "$" <> "1 in"
                    , "        " <> argFlag <> "=*)"
                    , "            " <> varName <> "=\"${" <> "1#*=}\""
                    , "            shift"
                    , "            ;;"
                    , "        *)"
                    , "            shift"
                    , "            ;;"
                    , "    esac"
                    , "done"
                    ]
                _ ->
                    -- dashdashspace (default)
                    [ varName <> "=\"\""
                    , "while [[ " <> "$" <> "# -gt 0 ]]; do"
                    , "    case " <> "$" <> "1 in"
                    , "        " <> argFlag <> ")"
                    , "            " <> varName <> "=\"" <> "$" <> "2\""
                    , "            shift 2"
                    , "            ;;"
                    , "        *)"
                    , "            shift"
                    , "            ;;"
                    , "    esac"
                    , "done"
                    ]

-- | Create a Python tool from config.
makePythonToolTemplateFromConfig :: ToolConfig -> Text
makePythonToolTemplateFromConfig config =
    let argsJson = TextEncoding.decodeUtf8 $ LByteString.toStrict $ AesonPretty.encodePretty (toolConfigArgs config)
        emptyResultJson = case toolConfigEmptyResult config of
            Nothing -> "{\"tag\": \"AddMessage\", \"contents\": \"No results\"}"
            Just val -> TextEncoding.decodeUtf8 $ LByteString.toStrict $ AesonPretty.encodePretty val
     in Text.unlines
            [ "#!/usr/bin/env python3"
            , ""
            , "\"\"\"" <> toolConfigSlug config <> " - " <> toolConfigDescription config <> "\"\"\""
            , ""
            , "import json"
            , "import sys"
            , ""
            , "DESCRIPTION = {"
            , "    \"slug\": \"" <> toolConfigSlug config <> "\","
            , "    \"description\": \"" <> toolConfigDescription config <> "\","
            , "    \"args\": " <> argsJson <> ","
            , "    \"empty-result\": " <> emptyResultJson
            , "}"
            , ""
            , ""
            , "def main():"
            , "    if len(sys.argv) > 1 and sys.argv[1] == \"describe\":"
            , "        print(json.dumps(DESCRIPTION))"
            , "        sys.exit(0)"
            , ""
            , "    # Parse arguments"
            , "    # TODO: Implement argument parsing"
            , ""
            , "    # Main logic here"
            , "    print(\"Tool " <> toolConfigSlug config <> " executed\")"
            , ""
            , ""
            , "if __name__ == \"__main__\":"
            , "    main()"
            ]

-- | Create a Haskell tool from config.
makeHaskellToolTemplateFromConfig :: ToolConfig -> Text
makeHaskellToolTemplateFromConfig config =
    let descValue = TextEncoding.decodeUtf8 (LByteString.toStrict (AesonPretty.encodePretty (toolConfigToAeson config)))
     in Text.unlines
            [ "#!/usr/bin/env runhaskell"
            , "{-# LANGUAGE OverloadedStrings #-}"
            , ""
            , "-- | " <> toolConfigSlug config <> " - " <> toolConfigDescription config
            , ""
            , "import qualified Data.Aeson as Aeson"
            , "import qualified Data.ByteString.Lazy as LBS"
            , "import Data.Text (Text)"
            , "import qualified Data.Text as Text"
            , "import qualified Data.Text.IO as Text"
            , "import System.Environment (getArgs)"
            , ""
            , "main :: IO ()"
            , "main = do"
            , "    args <- getArgs"
            , "    case args of"
            , "        (\"describe\":_) -> do"
            , "            LBS.putStr $ Aeson.encode description"
            , "        (\"run\":_) -> do"
            , "            -- TODO: Implement argument parsing and main logic"
            , "            Text.putStrLn \"Tool " <> toolConfigSlug config <> " executed\""
            , "        _ -> do"
            , "            Text.hPutStrLn stderr \"Usage: " <> toolConfigSlug config <> " describe|run\""
            , ""
            , "description :: Aeson.Value"
            , "description = " <> descValue
            ]

-- | Convert ToolConfig to Aeson Value for template generation.
toolConfigToAeson :: ToolConfig -> Aeson.Value
toolConfigToAeson config =
    Aeson.object
        [ "slug" .= toolConfigSlug config
        , "description" .= toolConfigDescription config
        , "args" .= toolConfigArgs config
        , "empty-result" .= toolConfigEmptyResult config
        ]

-------------------------------------------------------------------------------
-- Default Configuration
-------------------------------------------------------------------------------

-- | Default developer toolbox configuration for agent templates.
defaultDeveloperToolboxDescription :: BuiltinToolboxDescription
defaultDeveloperToolboxDescription =
    DeveloperToolbox $
        DeveloperToolboxDescription
            { developerToolboxName = "developer"
            , developerToolboxDescription = "Tools for developing agents and tools"
            , developerToolboxCapabilities =
                [ DevToolShowSpec
                , DevToolValidateAgent
                , DevToolCreateAgent
                , DevToolCreateTool
                , DevToolReadFileRange
                , DevToolWriteFileRange
                , DevToolPatchFile
                ]
            , developerToolboxActivation = Nothing -- Uses default: AlwaysActivated
            , developerToolboxFileSandbox = Nothing -- Uses default: deny all
            , developerToolboxBuildCommand = Nothing
            }

