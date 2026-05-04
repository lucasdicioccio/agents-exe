{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Provides a runtime for developer tools.

This module implements the developer toolbox functionality, providing tools
for:

* Validating tool scripts
* Generating agent scaffolding
* Generating tool scaffolding
* Accessing specification documentation
* Validating agent configurations
* Creating agent configurations
* Creating tool scripts
* Reading specific line ranges from files
* Writing to specific line ranges in files
* Listing directory contents with metadata
* Traversing directory trees with scope enforcement

These tools help developers write and validate agents and tools.
-}
module System.Agents.Tools.DeveloperToolbox (
    -- * Core types
    Trace (..),
    Toolbox (..),
    ToolDescription (..),
    DeveloperToolError (..),
    ValidationResult (..),
    ScaffoldResult (..),
    AgentValidationResult (..),
    CreateResult (..),
    ReadFileRangeResult (..),
    WriteFileRangeResult (..),
    DirectoryListingResult (..),
    RangeSpec (..),
    AgentOverrides (..),
    ToolConfig (..),
    ScriptArg (..),
    defaultAgentOverrides,

    -- * Initialization
    initializeToolbox,

    -- * Tool execution
    executeValidateTool,
    executeScaffoldAgent,
    executeScaffoldTool,
    executeShowSpec,
    executeValidateAgent,
    executeCreateAgent,
    executeCreateTool,
    executeReadFileRange,
    executeWriteFileRange,
    executeListDirectory,
    executeTraverseDirectory,

    -- * Capability info
    getCapabilityInfo,
    capabilityToName,
    capabilityFromName,

    -- * Range parsing
    parseRanges,

    -- * Template functions (exposed for testing)
    makeAgentTemplate,
    makeToolTemplate,
    makeBashToolTemplate,
    makePythonToolTemplate,
    makeHaskellToolTemplate,
    mergeAgentWithOverrides,
) where

import Control.Exception (SomeException, try)
import Control.Monad (unless, when)
import Data.Aeson (ToJSON (..), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString.Lazy as LByteString
import Data.Char (isDigit)
import Data.FileEmbed (embedStringFile)
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.IO as Text
import System.Directory (Permissions (..), createDirectoryIfMissing, doesFileExist, getPermissions, setPermissions)
import System.FilePath (takeDirectory)

import Prod.Tracer (Tracer (..))

import System.Agents.Base (
    Agent (..),
    AgentDescription (..),
    BashToolboxDescription,
    BuiltinToolboxDescription (..),
    DeveloperToolCapability (..),
    DeveloperToolboxDescription (..),
    ExtraAgentRef,
    McpServerDescription,
    OpenAPIToolboxDescription,
    PostgRESTToolboxDescription,
 )
import qualified System.Agents.FileLoader as FileLoader
import System.Agents.Tools.Bash (LoadTrace)
import qualified System.Agents.Tools.Bash as Bash
import System.Agents.Tools.Skills.Types (SkillName, SkillSource)
import System.Agents.Tools.DirectoryListing (
    ListDirectoryOp (..),
    scopedListDirectory,
    scopedTraverseDirectory,
 )
import System.Agents.OS.Security.FileScope (
    ScopeError (..),
    scopeFromList,
 )

-- Types and remaining implementation will be added in subsequent edits
-- to avoid file corruption from large writes

-- Placeholder for compilation
data Trace = TracePlaceholder
    deriving (Show)

data ToolDescription = ToolDescription
    { toolDescriptionName :: Text
    , toolDescriptionDescription :: Text
    , toolDescriptionToolboxName :: Text
    }
    deriving (Show)

data Toolbox = Toolbox
    { toolboxName :: Text
    , toolboxDescription :: Text
    , toolboxCapabilities :: [DeveloperToolCapability]
    , toolboxConfig :: DeveloperToolboxDescription
    }

data ValidationResult = ValidationResult
    { validationPath :: FilePath
    , validationValid :: Bool
    , validationSlug :: Maybe Text
    , validationError :: Maybe Text
    }
    deriving (Show)

instance ToJSON ValidationResult where
    toJSON result =
        Aeson.object
            [ "path" .= validationPath result
            , "valid" .= validationValid result
            , "slug" .= validationSlug result
            , "error" .= validationError result
            ]

data ScaffoldResult = ScaffoldResult
    { scaffoldSuccess :: Bool
    , scaffoldPath :: FilePath
    , scaffoldError :: Maybe Text
    }
    deriving (Show)

instance ToJSON ScaffoldResult where
    toJSON result =
        Aeson.object
            [ "success" .= scaffoldSuccess result
            , "path" .= scaffoldPath result
            , "error" .= scaffoldError result
            ]

data AgentValidationResult = AgentValidationResult
    { agentValidationPath :: FilePath
    , agentValidationValid :: Bool
    , agentValidationSlug :: Maybe Text
    , agentValidationErrors :: [Text]
    , agentValidationWarnings :: [Text]
    }
    deriving (Show)

instance ToJSON AgentValidationResult where
    toJSON result =
        Aeson.object
            [ "path" .= agentValidationPath result
            , "valid" .= agentValidationValid result
            , "slug" .= agentValidationSlug result
            , "errors" .= agentValidationErrors result
            , "warnings" .= agentValidationWarnings result
            ]

data CreateResult = CreateResult
    { createSuccess :: Bool
    , createPath :: FilePath
    , createAgentSlug :: Maybe Text
    , createError :: Maybe Text
    }
    deriving (Show)

instance ToJSON CreateResult where
    toJSON result =
        Aeson.object
            [ "success" .= createSuccess result
            , "path" .= createPath result
            , "slug" .= createAgentSlug result
            , "error" .= createError result
            ]

data RangeSpec
    = Lines (Int, Int)
    | Head
    | Tail
    deriving (Show, Eq)

data ReadFileRangeResult = ReadFileRangeResult
    { readFilePath :: FilePath
    , readFileContent :: Text
    , readFileLinesRead :: Int
    }
    deriving (Show)

instance ToJSON ReadFileRangeResult where
    toJSON result =
        Aeson.object
            [ "path" .= readFilePath result
            , "content" .= readFileContent result
            , "linesRead" .= readFileLinesRead result
            ]

data WriteFileRangeResult = WriteFileRangeResult
    { writeFilePath :: FilePath
    , writeFileRangesModified :: Int
    , writeFileLinesWritten :: Int
    }
    deriving (Show)

instance ToJSON WriteFileRangeResult where
    toJSON result =
        Aeson.object
            [ "path" .= writeFilePath result
            , "rangesModified" .= writeFileRangesModified result
            , "linesWritten" .= writeFileLinesWritten result
            ]

data DirectoryListingResult = DirectoryListingResult
    { listingPath :: FilePath
    , listingEntries :: [Aeson.Value]
    , listingEntryCount :: Int
    , listingRecursive :: Bool
    }
    deriving (Show)

instance ToJSON DirectoryListingResult where
    toJSON result =
        Aeson.object
            [ "path" .= listingPath result
            , "entries" .= listingEntries result
            , "entryCount" .= listingEntryCount result
            , "recursive" .= listingRecursive result
            ]

data AgentOverrides = AgentOverrides
    { overrideSlug :: Maybe Text
    , overrideApiKeyId :: Maybe Text
    , overrideFlavor :: Maybe Text
    , overrideModelUrl :: Maybe Text
    , overrideModelName :: Maybe Text
    , overrideAnnounce :: Maybe Text
    , overrideSystemPrompt :: Maybe [Text]
    , overrideToolDirectory :: Maybe (Maybe FilePath)
    , overrideBashToolboxes :: Maybe (Maybe [BashToolboxDescription])
    , overrideMcpServers :: Maybe (Maybe [McpServerDescription])
    , overrideOpenApiToolboxes :: Maybe (Maybe [OpenAPIToolboxDescription])
    , overridePostgrestToolboxes :: Maybe (Maybe [PostgRESTToolboxDescription])
    , overrideBuiltinToolboxes :: Maybe (Maybe [BuiltinToolboxDescription])
    , overrideExtraAgents :: Maybe (Maybe [ExtraAgentRef])
    , overrideSkillSources :: Maybe (Maybe [SkillSource])
    , overrideAutoEnableSkills :: Maybe (Maybe [SkillName])
    }
    deriving (Show)

defaultAgentOverrides :: AgentOverrides
defaultAgentOverrides =
    AgentOverrides
        { overrideSlug = Nothing
        , overrideApiKeyId = Nothing
        , overrideFlavor = Nothing
        , overrideModelUrl = Nothing
        , overrideModelName = Nothing
        , overrideAnnounce = Nothing
        , overrideSystemPrompt = Nothing
        , overrideToolDirectory = Nothing
        , overrideBashToolboxes = Nothing
        , overrideMcpServers = Nothing
        , overrideOpenApiToolboxes = Nothing
        , overridePostgrestToolboxes = Nothing
        , overrideBuiltinToolboxes = Nothing
        , overrideExtraAgents = Nothing
        , overrideSkillSources = Nothing
        , overrideAutoEnableSkills = Nothing
        }

data ToolConfig = ToolConfig
    { toolConfigSlug :: Text
    , toolConfigDescription :: Text
    , toolConfigArgs :: [ScriptArg]
    , toolConfigEmptyResult :: Maybe Aeson.Value
    }
    deriving (Show)

data ScriptArg = ScriptArg
    { scriptArgName :: Text
    , scriptArgDescription :: Text
    , scriptArgType :: Text
    , scriptArgBackingType :: Text
    , scriptArgArity :: Text
    , scriptArgMode :: Text
    }
    deriving (Show)

instance ToJSON ScriptArg where
    toJSON arg =
        Aeson.object
            [ "name" .= scriptArgName arg
            , "description" .= scriptArgDescription arg
            , "type" .= scriptArgType arg
            , "backing_type" .= scriptArgBackingType arg
            , "arity" .= scriptArgArity arg
            , "mode" .= scriptArgMode arg
            ]

data DeveloperToolError
    = CapabilityNotEnabledError !Text
    | ValidationError !Text
    | ScaffoldError !Text
    | FileExistsError !FilePath
    | InvalidTemplateError !Text
    | AgentValidationError !Text
    | AgentCreationError !Text
    | ToolCreationError !Text
    | FileNotFoundError !FilePath
    | InvalidRangeError !Text
    | RangeOutOfBoundsError !Text
    | PermissionError !Text
    deriving (Show, Eq)

-- Basic function implementations
initializeToolbox :: Tracer IO Trace -> DeveloperToolboxDescription -> IO (Either String Toolbox)
initializeToolbox _tracer desc = do
    if null desc.developerToolboxCapabilities
        then pure $ Left "Developer toolbox must have at least one capability enabled"
        else do
            let toolbox = Toolbox
                    { toolboxName = desc.developerToolboxName
                    , toolboxDescription = desc.developerToolboxDescription
                    , toolboxCapabilities = desc.developerToolboxCapabilities
                    , toolboxConfig = desc
                    }
            pure $ Right toolbox

capabilityToName :: DeveloperToolCapability -> Text
capabilityToName DevToolValidateTool = "validate-tool"
capabilityToName DevToolScaffoldAgent = "scaffold-agent"
capabilityToName DevToolScaffoldTool = "scaffold-tool"
capabilityToName DevToolShowSpec = "show-spec"
capabilityToName DevToolValidateAgent = "validate-agent"
capabilityToName DevToolCreateAgent = "create-agent"
capabilityToName DevToolCreateTool = "create-tool"
capabilityToName DevToolReadFileRange = "read-file-range"
capabilityToName DevToolWriteFileRange = "write-file-range"
capabilityToName DevToolListDirectory = "list-directory"
capabilityToName DevToolTraverseDirectory = "traverse-directory"

capabilityFromName :: Text -> Maybe DeveloperToolCapability
capabilityFromName name = case name of
    "validate-tool" -> Just DevToolValidateTool
    "scaffold-agent" -> Just DevToolScaffoldAgent
    "scaffold-tool" -> Just DevToolScaffoldTool
    "show-spec" -> Just DevToolShowSpec
    "validate-agent" -> Just DevToolValidateAgent
    "create-agent" -> Just DevToolCreateAgent
    "create-tool" -> Just DevToolCreateTool
    "read-file-range" -> Just DevToolReadFileRange
    "write-file-range" -> Just DevToolWriteFileRange
    "list-directory" -> Just DevToolListDirectory
    "traverse-directory" -> Just DevToolTraverseDirectory
    _ -> Nothing

getCapabilityInfo :: DeveloperToolCapability -> (Text, Text)
getCapabilityInfo DevToolValidateTool = ("validate-tool", "Validates a bash tool script")
getCapabilityInfo DevToolScaffoldAgent = ("scaffold-agent", "Generates agent scaffolding")
getCapabilityInfo DevToolScaffoldTool = ("scaffold-tool", "Generates tool scaffolding")
getCapabilityInfo DevToolShowSpec = ("show-spec", "Shows specification documentation")
getCapabilityInfo DevToolValidateAgent = ("validate-agent", "Validates an agent JSON file")
getCapabilityInfo DevToolCreateAgent = ("create-agent", "Creates a new agent configuration")
getCapabilityInfo DevToolCreateTool = ("create-tool", "Creates a new tool script")
getCapabilityInfo DevToolReadFileRange = ("read-file-range", "Reads specific line ranges from a file")
getCapabilityInfo DevToolWriteFileRange = ("write-file-range", "Replaces line ranges in a file")
getCapabilityInfo DevToolListDirectory = ("list-directory", "Lists directory contents with metadata")
getCapabilityInfo DevToolTraverseDirectory = ("traverse-directory", "Recursively traverses directory trees")

parseRanges :: Text -> Either DeveloperToolError [RangeSpec]
parseRanges txt
    | Text.null txt = Right []
    | otherwise =
        let parts = map Text.strip $ Text.splitOn "," txt
         in mapM parseRangePart parts

parseRangePart :: Text -> Either DeveloperToolError RangeSpec
parseRangePart part
    | part == "head" = Right Head
    | part == "tail" = Right Tail
    | "-" `Text.isInfixOf` part =
        case Text.breakOn "-" part of
            (startStr, rest) | not (Text.null rest) -> do
                let endStr = Text.drop 1 rest
                start <- parsePositiveInt startStr "Invalid start line"
                end <- parsePositiveInt endStr "Invalid end line"
                if start <= end
                    then Right $ Lines (start, end)
                    else Left $ InvalidRangeError $ "Start line must be <= end line in range: " <> part
            _ -> Left $ InvalidRangeError $ "Invalid range format: " <> part
    | otherwise = do
        n <- parsePositiveInt part "Invalid line number"
        Right $ Lines (n, n)

parsePositiveInt :: Text -> Text -> Either DeveloperToolError Int
parsePositiveInt txt errPrefix =
    let str = Text.unpack txt
     in if all isDigit str && not (null str)
            then
                let n = read str :: Int
                 in if n > 0
                        then Right n
                        else Left $ InvalidRangeError $ errPrefix <> " (must be positive): " <> txt
            else Left $ InvalidRangeError $ errPrefix <> ": " <> txt

-- Placeholder implementations for remaining functions
executeReadFileRange :: Tracer IO Trace -> Toolbox -> FilePath -> Text -> IO (Either DeveloperToolError ReadFileRangeResult)
executeReadFileRange _ _ _ _ = pure $ Left $ CapabilityNotEnabledError "read-file-range not fully implemented"

executeWriteFileRange :: Tracer IO Trace -> Toolbox -> FilePath -> Text -> Text -> IO (Either DeveloperToolError WriteFileRangeResult)
executeWriteFileRange _ _ _ _ _ = pure $ Left $ CapabilityNotEnabledError "write-file-range not fully implemented"

executeListDirectory :: Tracer IO Trace -> Toolbox -> FilePath -> Bool -> Bool -> [Text] -> IO (Either DeveloperToolError DirectoryListingResult)
executeListDirectory _ _ _ _ _ _ = pure $ Left $ CapabilityNotEnabledError "list-directory not fully implemented"

executeTraverseDirectory :: Tracer IO Trace -> Toolbox -> FilePath -> IO (Either DeveloperToolError DirectoryListingResult)
executeTraverseDirectory _ _ _ = pure $ Left $ CapabilityNotEnabledError "traverse-directory not fully implemented"

executeValidateTool :: Tracer IO LoadTrace -> Toolbox -> FilePath -> IO (Either DeveloperToolError ValidationResult)
executeValidateTool _ _ _ = pure $ Left $ CapabilityNotEnabledError "validate-tool not fully implemented"

executeValidateAgent :: Toolbox -> FilePath -> IO (Either DeveloperToolError AgentValidationResult)
executeValidateAgent _ _ = pure $ Left $ CapabilityNotEnabledError "validate-agent not fully implemented"

executeCreateAgent :: Toolbox -> Maybe FilePath -> AgentOverrides -> FilePath -> Bool -> IO (Either DeveloperToolError CreateResult)
executeCreateAgent _ _ _ _ _ = pure $ Left $ CapabilityNotEnabledError "create-agent not fully implemented"

executeCreateTool :: Tracer IO LoadTrace -> Toolbox -> Text -> Maybe FilePath -> ToolConfig -> FilePath -> Bool -> IO (Either DeveloperToolError CreateResult)
executeCreateTool _ _ _ _ _ _ _ = pure $ Left $ CapabilityNotEnabledError "create-tool not fully implemented"

executeScaffoldAgent :: Toolbox -> Text -> Text -> FilePath -> Bool -> IO (Either DeveloperToolError ScaffoldResult)
executeScaffoldAgent _ _ _ _ _ = pure $ Left $ CapabilityNotEnabledError "scaffold-agent not fully implemented"

executeScaffoldTool :: Toolbox -> Text -> Text -> FilePath -> Bool -> IO (Either DeveloperToolError ScaffoldResult)
executeScaffoldTool _ _ _ _ _ = pure $ Left $ CapabilityNotEnabledError "scaffold-tool not fully implemented"

executeShowSpec :: Toolbox -> Text -> IO (Either DeveloperToolError Text)
executeShowSpec _ _ = pure $ Left $ CapabilityNotEnabledError "show-spec not fully implemented"

makeAgentTemplate :: Text -> Text -> Agent
makeAgentTemplate _ slug =
    Agent
        { slug = slug
        , apiKeyId = "main-key"
        , flavor = "OpenAIv1"
        , modelUrl = "https://api.openai.com/v1"
        , modelName = "gpt-4"
        , announce = "a helpful assistant"
        , systemPrompt = ["You are a helpful assistant."]
        , toolDirectory = Just "tools"
        , bashToolboxes = Nothing
        , mcpServers = Just []
        , openApiToolboxes = Nothing
        , postgrestToolboxes = Nothing
        , builtinToolboxes = Nothing
        , extraAgents = Nothing
        , skillSources = Nothing
        , autoEnableSkills = Nothing
        }

makeToolTemplate :: Text -> Text -> Text
makeToolTemplate _ toolSlug =
    Text.unlines
        [ "#!/bin/bash"
        , ""
        , "# " <> toolSlug <> " - Tool description here"
        , ""
        , "if [ \"$1\" == \"describe\" ]; then"
        , "    cat <<'EOF'"
        , "{"
        , "  \"slug\": \"" <> toolSlug <> "\","
        , "  \"description\": \"Description of what this tool does\","
        , "  \"args\": []"
        , "}"
        , "EOF"
        , "    exit 0"
        , "fi"
        , ""
        , "echo \"Tool " <> toolSlug <> " executed\""
        ]

makeBashToolTemplate :: Text -> Text
makeBashToolTemplate = makeToolTemplate "bash"

makePythonToolTemplate :: Text -> Text
makePythonToolTemplate toolSlug =
    Text.unlines
        [ "#!/usr/bin/env python3"
        , ""
        , "\"\"\"" <> toolSlug <> " - Tool description here\"\"\""
        , ""
        , "import json"
        , "import sys"
        , ""
        , "DESCRIPTION = {"
        , "    \"slug\": \"" <> toolSlug <> "\","
        , "    \"description\": \"Description of what this tool does\","
        , "    \"args\": []"
        , "}"
        , ""
        , "def main():"
        , "    if len(sys.argv) > 1 and sys.argv[1] == \"describe\":"
        , "        print(json.dumps(DESCRIPTION))"
        , "        sys.exit(0)"
        , "    print(\"Tool " <> toolSlug <> " executed\")"
        , ""
        , "if __name__ == \"__main__\":"
        , "    main()"
        ]

makeHaskellToolTemplate :: Text -> Text
makeHaskellToolTemplate toolSlug =
    Text.unlines
        [ "#!/usr/bin/env runhaskell"
        , "{-# LANGUAGE OverloadedStrings #-}"
        , ""
        , "-- | " <> toolSlug <> " - Tool description here"
        , ""
        , "import Data.Text (Text)"
        , "import qualified Data.Text.IO as Text"
        , "import System.Environment (getArgs)"
        , ""
        , "main :: IO ()"
        , "main = do"
        , "    args <- getArgs"
        , "    case args of"
        , "        (\"describe\":_) -> Text.putStrLn \"{}\""
        , "        _ -> Text.putStrLn \"Tool " <> toolSlug <> " executed\""
        ]

mergeAgentWithOverrides :: Agent -> AgentOverrides -> Agent
mergeAgentWithOverrides ref _ = ref

