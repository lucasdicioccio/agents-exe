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
* Reading specific line ranges from files (with scope validation)
* Writing to specific line ranges in files (with scope validation)

These tools help developers write and validate agents and tools.

File scope security:
File operations (read-file-range, write-file-range) respect configured scope
boundaries defined in the developerToolboxFileScope field. Paths are validated
against glob patterns before access is granted.
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

    -- * Scope validation
    scopeErrorToToolError,
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
    DeveloperFileScope (..),
    DeveloperToolCapability (..),
    DeveloperToolboxDescription (..),
    ExtraAgentRef,
    McpServerDescription,
    OpenAPIToolboxDescription,
    PostgRESTToolboxDescription,
    defaultDeveloperFileScope,
 )
import qualified System.Agents.FileLoader as FileLoader
import System.Agents.OS.Security.FileScope (
    FileScope (..),
    ScopeError (..),
    compileGlob,
    validateFileAccess,
 )
import System.Agents.Tools.Bash (LoadTrace)
import qualified System.Agents.Tools.Bash as Bash
import System.Agents.Tools.Skills.Types (SkillName, SkillSource)

-------------------------------------------------------------------------------
-- Core Types
-------------------------------------------------------------------------------

{- | Trace events for monitoring developer toolbox operations.

These events allow tracking of:
* Tool validation attempts
* Scaffold generation
* Spec access
* Agent validation
* Agent creation
* Tool creation
* File range read operations
* File range write operations
* Scope validation failures
-}
data Trace
    = -- | Tool validation started
      ValidateToolStartedTrace !FilePath
    | -- | Tool validation completed
      ValidateToolCompletedTrace !FilePath !Bool
    | -- | Scaffold agent started
      ScaffoldAgentStartedTrace !Text !FilePath
    | -- | Scaffold agent completed
      ScaffoldAgentCompletedTrace !Text !FilePath
    | -- | Scaffold tool started
      ScaffoldToolStartedTrace !Text !FilePath
    | -- | Scaffold tool completed
      ScaffoldToolCompletedTrace !Text !FilePath
    | -- | Show spec started
      ShowSpecStartedTrace !Text
    | -- | Show spec completed
      ShowSpecCompletedTrace !Text
    | -- | Agent validation started
      ValidateAgentStartedTrace !FilePath
    | -- | Agent validation completed
      ValidateAgentCompletedTrace !FilePath !Bool
    | -- | Agent creation started
      CreateAgentStartedTrace !(Maybe FilePath) !FilePath
    | -- | Agent creation completed
      CreateAgentCompletedTrace !FilePath !Bool
    | -- | Tool creation started
      CreateToolStartedTrace !Text !(Maybe FilePath) !FilePath
    | -- | Tool creation completed
      CreateToolCompletedTrace !FilePath !Bool
    | -- | Read file range started
      ReadFileRangeStartedTrace !FilePath !Text
    | -- | Read file range completed
      ReadFileRangeCompletedTrace !FilePath !Int
    | -- | Write file range started
      WriteFileRangeStartedTrace !FilePath !Text
    | -- | Write file range completed
      WriteFileRangeCompletedTrace !FilePath !Int !Int
    | -- | Scope validation failed
      ScopeValidationFailedTrace !FilePath !Text
    | -- | Error during operation
      DeveloperToolErrorTrace !Text !Text
    deriving (Show)

{- | Description of a developer tool.

Contains metadata about a specific developer tool.
-}
data ToolDescription = ToolDescription
    { toolDescriptionName :: Text
    -- ^ Name of the tool
    , toolDescriptionDescription :: Text
    -- ^ Human-readable description
    , toolDescriptionToolboxName :: Text
    -- ^ Name of the toolbox this tool belongs to
    }
    deriving (Show)

{- | Runtime state for a developer toolbox.

The toolbox maintains:
* Toolbox name and description
* List of enabled capabilities
* File scope configuration for restricting file access
* The original configuration description used to create this toolbox
-}
data Toolbox = Toolbox
    { toolboxName :: Text
    , toolboxDescription :: Text
    , toolboxCapabilities :: [DeveloperToolCapability]
    , toolboxFileScope :: FileScope
    -- ^ File scope for restricting file operations
    , toolboxConfig :: DeveloperToolboxDescription
    -- ^ Original configuration description used to create this toolbox
    }

-- | Result of a tool validation.
data ValidationResult = ValidationResult
    { validationPath :: FilePath
    , validationValid :: Bool
    , validationSlug :: Maybe Text
    , validationError :: Maybe Text
    }
    deriving (Show)

-- | JSON serialization for ValidationResult.
instance ToJSON ValidationResult where
    toJSON result =
        Aeson.object
            [ "path" .= validationPath result
            , "valid" .= validationValid result
            , "slug" .= validationSlug result
            , "error" .= validationError result
            ]

-- | Result of a scaffolding operation.
data ScaffoldResult = ScaffoldResult
    { scaffoldSuccess :: Bool
    , scaffoldPath :: FilePath
    , scaffoldError :: Maybe Text
    }
    deriving (Show)

-- | JSON serialization for ScaffoldResult.
instance ToJSON ScaffoldResult where
    toJSON result =
        Aeson.object
            [ "success" .= scaffoldSuccess result
            , "path" .= scaffoldPath result
            , "error" .= scaffoldError result
            ]

-- | Result of an agent validation.
data AgentValidationResult = AgentValidationResult
    { agentValidationPath :: FilePath
    , agentValidationValid :: Bool
    , agentValidationSlug :: Maybe Text
    , agentValidationErrors :: [Text]
    , agentValidationWarnings :: [Text]
    }
    deriving (Show)

-- | JSON serialization for AgentValidationResult.
instance ToJSON AgentValidationResult where
    toJSON result =
        Aeson.object
            [ "path" .= agentValidationPath result
            , "valid" .= agentValidationValid result
            , "slug" .= agentValidationSlug result
            , "errors" .= agentValidationErrors result
            , "warnings" .= agentValidationWarnings result
            ]

-- | Result of a create operation (for agents or tools).
data CreateResult = CreateResult
    { createSuccess :: Bool
    , createPath :: FilePath
    , createAgentSlug :: Maybe Text
    , createError :: Maybe Text
    }
    deriving (Show)

-- | JSON serialization for CreateResult.
instance ToJSON CreateResult where
    toJSON result =
        Aeson.object
            [ "success" .= createSuccess result
            , "path" .= createPath result
            , "slug" .= createAgentSlug result
            , "error" .= createError result
            ]

-- | Range specification for file operations.
data RangeSpec
    = -- | 1-based, inclusive line range (start, end)
      Lines (Int, Int)
    | -- | Before line 1 (prepend)
      Head
    | -- | After last line (append)
      Tail
    deriving (Show, Eq)

-- | Result of a read file range operation.
data ReadFileRangeResult = ReadFileRangeResult
    { readFilePath :: FilePath
    , readFileContent :: Text
    , readFileLinesRead :: Int
    }
    deriving (Show)

-- | JSON serialization for ReadFileRangeResult.
instance ToJSON ReadFileRangeResult where
    toJSON result =
        Aeson.object
            [ "path" .= readFilePath result
            , "content" .= readFileContent result
            , "linesRead" .= readFileLinesRead result
            ]

-- | Result of a write file range operation.
data WriteFileRangeResult = WriteFileRangeResult
    { writeFilePath :: FilePath
    , writeFileRangesModified :: Int
    , writeFileLinesWritten :: Int
    }
    deriving (Show)

-- | JSON serialization for WriteFileRangeResult.
instance ToJSON WriteFileRangeResult where
    toJSON result =
        Aeson.object
            [ "path" .= writeFilePath result
            , "rangesModified" .= writeFileRangesModified result
            , "linesWritten" .= writeFileLinesWritten result
            ]

-- | Override parameters for creating an agent from reference or scratch.
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

-- | Default overrides (all Nothing).
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

-- | Configuration for creating a tool.
data ToolConfig = ToolConfig
    { toolConfigSlug :: Text
    , toolConfigDescription :: Text
    , toolConfigArgs :: [ScriptArg]
    , toolConfigEmptyResult :: Maybe Aeson.Value
    }
    deriving (Show)

-- | Description of a script argument for tool creation.
data ScriptArg = ScriptArg
    { scriptArgName :: Text
    , scriptArgDescription :: Text
    , scriptArgType :: Text
    , scriptArgBackingType :: Text
    , scriptArgArity :: Text -- "single" | "optional"
    , scriptArgMode :: Text -- "positional" | "dashdashspace" | "dashdashequal" | "stdin"
    }
    deriving (Show)

-- | JSON serialization for ScriptArg.
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

-- | Errors that can occur during developer tool operations.
data DeveloperToolError
    = -- | The requested capability is not enabled
      CapabilityNotEnabledError !Text
    | -- | Error during tool validation
      ValidationError !Text
    | -- | Error during scaffolding
      ScaffoldError !Text
    | -- | File already exists
      FileExistsError !FilePath
    | -- | Invalid template or language
      InvalidTemplateError !Text
    | -- | Error during agent validation
      AgentValidationError !Text
    | -- | Error during agent creation
      AgentCreationError !Text
    | -- | Error during tool creation
      ToolCreationError !Text
    | -- | File not found
      FileNotFoundError !FilePath
    | -- | Invalid range format
      InvalidRangeError !Text
    | -- | Range out of bounds
      RangeOutOfBoundsError !Text
    | -- | Permission denied
      PermissionError !Text
    | -- | Scope validation error (file access denied)
      ScopeError !ScopeError
    deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Scope Error Conversion
-------------------------------------------------------------------------------

-- | Convert a ScopeError to a DeveloperToolError.
scopeErrorToToolError :: ScopeError -> DeveloperToolError
scopeErrorToToolError = ScopeError

-- | Convert ScopeError to a user-friendly error message.
scopeErrorToMessage :: ScopeError -> Text
scopeErrorToMessage (PathNotAllowed path) =
    "File access denied: path not in allowed patterns: " <> Text.pack path
scopeErrorToMessage (PathNotAbsolute path) =
    "File access denied: path must be absolute: " <> Text.pack path
scopeErrorToMessage (PathEscapesSandbox path) =
    "File access denied: path escapes sandbox: " <> Text.pack path
scopeErrorToMessage (CreateNotAllowed path) =
    "File creation denied: creating new files not allowed: " <> Text.pack path
scopeErrorToMessage RangeOpsNotAllowed =
    "Range operations not allowed by file scope configuration"
scopeErrorToMessage (FileNotFound path) =
    "File not found: " <> Text.pack path
scopeErrorToMessage (ScopeIOError path msg) =
    "IO error accessing file '" <> Text.pack path <> "': " <> Text.pack msg

-------------------------------------------------------------------------------
-- Initialization
-------------------------------------------------------------------------------

{- | Initialize a developer toolbox from a description.

This function creates a 'Toolbox' value from a 'DeveloperToolboxDescription',
validating the configuration and preparing the runtime state.

Returns an error if the configuration is invalid.
-}
initializeToolbox ::
    Tracer IO Trace ->
    DeveloperToolboxDescription ->
    IO (Either String Toolbox)
initializeToolbox _tracer desc = do
    -- Validate that we have at least one capability
    if null desc.developerToolboxCapabilities
        then pure $ Left "Developer toolbox must have at least one capability enabled"
        else do
            let fileScope = convertFileScope (fromMaybe defaultDeveloperFileScope desc.developerToolboxFileScope)
            let toolbox =
                    Toolbox
                        { toolboxName = desc.developerToolboxName
                        , toolboxDescription = desc.developerToolboxDescription
                        , toolboxCapabilities = desc.developerToolboxCapabilities
                        , toolboxFileScope = fileScope
                        , toolboxConfig = desc
                        }
            pure $ Right toolbox

-- | Convert DeveloperFileScope to FileScope.
convertFileScope :: DeveloperFileScope -> FileScope
convertFileScope devScope =
    FileScope
        { allowedPatterns = map (compileGlob . Text.unpack) (devFileScopeAllowedPatterns devScope)
        , allowRangeOps = devFileScopeAllowRangeOps devScope
        , allowCreate = devFileScopeAllowCreate devScope
        }

-------------------------------------------------------------------------------
-- Capability Info
-------------------------------------------------------------------------------

-- | Get the name for a capability.
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

-- | Convert a capability name text to the corresponding DeveloperToolCapability.
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
    _ -> Nothing

-- | Get information about a capability (name and description).
getCapabilityInfo :: DeveloperToolCapability -> (Text, Text)
getCapabilityInfo DevToolValidateTool =
    ( "validate-tool"
    , "Validates a bash tool script by loading its description"
    )
getCapabilityInfo DevToolScaffoldAgent =
    ( "scaffold-agent"
    , "Generates agent scaffolding from a template (openai, mistral, ollama)"
    )
getCapabilityInfo DevToolScaffoldTool =
    ( "scaffold-tool"
    , "Generates tool scaffolding in a language (bash, python, haskell)"
    )
getCapabilityInfo DevToolShowSpec =
    ( "show-spec"
    , "Shows specification documentation (bash-tools)"
    )
getCapabilityInfo DevToolValidateAgent =
    ( "validate-agent"
    , "Validates an agent JSON configuration file"
    )
getCapabilityInfo DevToolCreateAgent =
    ( "create-agent"
    , "Creates a new agent configuration from scratch or from a reference"
    )
getCapabilityInfo DevToolCreateTool =
    ( "create-tool"
    , "Creates a new tool script from scratch or from a reference"
    )
getCapabilityInfo DevToolReadFileRange =
    ( "read-file-range"
    , "Reads specific line ranges from a file (e.g., '1-10', '5', 'head', 'tail')"
    )
getCapabilityInfo DevToolWriteFileRange =
    ( "write-file-range"
    , "Replaces line ranges in a file with new content (use '---' to separate multiple ranges)"
    )

-------------------------------------------------------------------------------
-- Range Parsing
-------------------------------------------------------------------------------

{- | Parse a ranges string into a list of RangeSpec.

Supports formats:
- Single number: "5" -> Lines (5, 5)
- Range: "1-10" -> Lines (1, 10)
- Special: "head" -> Head, "tail" -> Tail
- Multiple: "1-10,20-30" -> [Lines (1, 10), Lines (20, 30)]

Returns Left with error message if parsing fails.
-}
parseRanges :: Text -> Either DeveloperToolError [RangeSpec]
parseRanges txt
    | Text.null txt = Right []
    | otherwise =
        let parts = map Text.strip $ Text.splitOn "," txt
         in mapM parseRangePart parts

-- | Parse a single range part.
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

-- | Parse a positive integer from Text.
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

-------------------------------------------------------------------------------
-- File Range Operations with Scope Validation
-------------------------------------------------------------------------------

{- | Execute read file range operation with scope validation.

Reads specific line ranges from a file and returns them with line numbers
prepended as "{line_num}\t{line_content}".

Parameters:
- path: Path to the file to read
- ranges: Comma-separated line ranges (e.g., "1-10", "5", "head", "tail")

This operation validates the file path against the configured file scope
before reading. If the path is not in the allowed patterns, the operation
is rejected with a ScopeError.

Returns Right with ReadFileRangeResult on success, Left with error on failure.
-}
executeReadFileRange ::
    Tracer IO Trace ->
    Toolbox ->
    FilePath ->
    Text ->
    IO (Either DeveloperToolError ReadFileRangeResult)
executeReadFileRange tracer toolbox filePath rangesTxt = do
    if DevToolReadFileRange `notElem` toolboxCapabilities toolbox
        then pure $ Left $ CapabilityNotEnabledError "read-file-range"
        else do
            runTracer tracer (ReadFileRangeStartedTrace filePath rangesTxt)

            -- Validate path against file scope
            scopeResult <- validateFileAccess Nothing (toolboxFileScope toolbox) filePath True
            case scopeResult of
                Left scopeErr -> do
                    let err = scopeErrorToToolError scopeErr
                    runTracer tracer (ScopeValidationFailedTrace filePath (scopeErrorToMessage scopeErr))
                    runTracer tracer (DeveloperToolErrorTrace "read-file-range" $ Text.pack $ show err)
                    pure $ Left err
                Right _canonicalPath -> do
                    -- Path is allowed, proceed with the operation
                    -- Parse ranges
                    case parseRanges rangesTxt of
                        Left err -> pure $ Left err
                        Right ranges -> do
                            -- Read file and extract lines
                            result <- try $ Text.readFile filePath
                            case result of
                                Left (e :: SomeException) -> do
                                    let err = PermissionError $ Text.pack $ show e
                                    runTracer tracer (DeveloperToolErrorTrace "read-file-range" $ Text.pack $ show e)
                                    pure $ Left err
                                Right content -> do
                                    let allLines = Text.lines content
                                    let resultLines = extractLines allLines ranges
                                    let output = Text.unlines $ map formatLineWithNumber resultLines
                                    let linesRead = length resultLines

                                    runTracer tracer (ReadFileRangeCompletedTrace filePath linesRead)

                                    pure $
                                        Right $
                                            ReadFileRangeResult
                                                { readFilePath = filePath
                                                , readFileContent = output
                                                , readFileLinesRead = linesRead
                                                }

{- | Extract lines based on range specifications.
Returns list of (lineNumber, lineContent) pairs.
-}
extractLines :: [Text] -> [RangeSpec] -> [(Int, Text)]
extractLines allLines ranges =
    concatMap (extractRange allLines) ranges

-- | Extract lines for a single range.
extractRange :: [Text] -> RangeSpec -> [(Int, Text)]
extractRange _ Head = []
extractRange _ Tail = []
extractRange allLines (Lines (start, end)) =
    let totalLines = length allLines
        actualStart = max 1 start
        actualEnd = min end totalLines
     in if actualStart > actualEnd || actualStart > totalLines
            then []
            else zip [actualStart ..] (take (actualEnd - actualStart + 1) $ drop (actualStart - 1) allLines)

-- | Format a line with its line number.
formatLineWithNumber :: (Int, Text) -> Text
formatLineWithNumber (n, line) = Text.pack (show n) <> "\t" <> line

{- | Execute write file range operation with scope validation.

Replaces line ranges in a file with new content. Multiple ranges are applied
bottom-to-top so earlier line numbers stay valid. Content blocks for multiple
ranges are separated by a line containing only "---".

Parameters:
- path: Path to the file to modify
- ranges: Comma-separated ranges (e.g., "1-10", "5", "head", "tail")
- content: Replacement text (or multiple blocks separated by "---")

This operation validates the file path against the configured file scope
before writing. If the path is not in the allowed patterns or if creating
new files is not allowed, the operation is rejected with a ScopeError.

Returns Right with WriteFileRangeResult on success, Left with error on failure.
-}
executeWriteFileRange ::
    Tracer IO Trace ->
    Toolbox ->
    FilePath ->
    Text ->
    Text ->
    IO (Either DeveloperToolError WriteFileRangeResult)
executeWriteFileRange tracer toolbox filePath rangesTxt contentTxt = do
    if DevToolWriteFileRange `notElem` toolboxCapabilities toolbox
        then pure $ Left $ CapabilityNotEnabledError "write-file-range"
        else do
            runTracer tracer (WriteFileRangeStartedTrace filePath rangesTxt)

            -- Check if file exists first
            fileExists <- doesFileExist filePath

            -- Validate path against file scope
            -- For write operations, we need to check allowCreate if file doesn't exist
            let isRead = False
            scopeResult <- validateFileAccess Nothing (toolboxFileScope toolbox) filePath isRead
            case scopeResult of
                Left scopeErr -> do
                    let err = scopeErrorToToolError scopeErr
                    runTracer tracer (ScopeValidationFailedTrace filePath (scopeErrorToMessage scopeErr))
                    runTracer tracer (DeveloperToolErrorTrace "write-file-range" $ Text.pack $ show err)
                    pure $ Left err
                Right _canonicalPath -> do
                    -- Path is allowed, proceed with the operation
                    -- Parse ranges
                    case parseRanges rangesTxt of
                        Left err -> pure $ Left err
                        Right ranges -> do
                            -- Split content by "---" separator
                            let contentBlocks = map Text.strip $ Text.splitOn "---" contentTxt

                            if length contentBlocks /= length ranges && length ranges > 1
                                then
                                    pure $
                                        Left $
                                            InvalidRangeError $
                                                "Number of content blocks ("
                                                    <> Text.pack (show $ length contentBlocks)
                                                    <> ") must match number of ranges ("
                                                    <> Text.pack (show $ length ranges)
                                                    <> "). Use '---' to separate blocks."
                                else do
                                    -- Check if file exists (already done above, but for clarity)
                                    if not fileExists && not (any isHeadOrTail ranges)
                                        then do
                                            let err = FileNotFoundError filePath
                                            runTracer tracer (DeveloperToolErrorTrace "write-file-range" $ Text.pack $ show err)
                                            pure $ Left err
                                        else do
                                            -- Read existing file or start empty
                                            existingContent <-
                                                if fileExists
                                                    then Text.readFile filePath
                                                    else pure ""

                                            let existingLines = Text.lines existingContent

                                            -- Pair ranges with content blocks
                                            let rangeContentPairs = zip ranges (if null contentBlocks then [""] else contentBlocks)

                                            -- Sort by start line in descending order (bottom-to-top)
                                            let sortedPairs = sortOn (negate . rangeStartKey) rangeContentPairs

                                            -- Apply each range replacement
                                            let finalLines = foldl (applyRangeReplacement existingLines) existingLines sortedPairs

                                            -- Preserve trailing newline if original had one
                                            let hasTrailingNewline = not (Text.null existingContent) && Text.last existingContent == '\n'
                                            let output =
                                                    if hasTrailingNewline || null existingLines
                                                        then Text.unlines finalLines
                                                        else Text.unlines finalLines

                                            -- Write result
                                            writeResult <- try $ do
                                                createDirectoryIfMissing True (takeDirectory filePath)
                                                Text.writeFile filePath output

                                            case writeResult of
                                                Left (e :: SomeException) -> do
                                                    let err = PermissionError $ Text.pack $ show e
                                                    runTracer tracer (DeveloperToolErrorTrace "write-file-range" $ Text.pack $ show e)
                                                    pure $ Left err
                                                Right () -> do
                                                    let rangesModified = length ranges
                                                    let linesWritten = length $ Text.lines contentTxt

                                                    runTracer tracer (WriteFileRangeCompletedTrace filePath rangesModified linesWritten)

                                                    pure $
                                                        Right $
                                                            WriteFileRangeResult
                                                                { writeFilePath = filePath
                                                                , writeFileRangesModified = rangesModified
                                                                , writeFileLinesWritten = linesWritten
                                                                }
  where
    isHeadOrTail Head = True
    isHeadOrTail Tail = True
    isHeadOrTail _ = False

    -- Get sort key for range (for bottom-to-top ordering)
    rangeStartKey :: (RangeSpec, Text) -> Int
    rangeStartKey (Head, _) = 0
    rangeStartKey (Lines (n, _), _) = n
    rangeStartKey (Tail, _) = maxBound

-- | Apply a range replacement to the lines.
applyRangeReplacement :: [Text] -> [Text] -> (RangeSpec, Text) -> [Text]
applyRangeReplacement originalLines currentLines (range, content) =
    let newLines = Text.lines content
     in case range of
            Head -> newLines ++ currentLines
            Tail -> currentLines ++ newLines
            Lines (start, end) ->
                let totalLines = length originalLines
                    actualStart = max 1 start
                    actualEnd = min end totalLines
                 in if actualStart > actualEnd || actualStart > length currentLines
                        then currentLines
                        else
                            let before = take (actualStart - 1) currentLines
                                after = drop actualEnd currentLines
                             in before ++ newLines ++ after

-------------------------------------------------------------------------------
-- Tool Execution - Validation
-------------------------------------------------------------------------------

{- | Execute tool validation.

This function validates a bash tool script by attempting to load its
description using the standard 'describe' command.

Returns:
* 'Right ValidationResult' on successful validation (valid or invalid)
* 'Left DeveloperToolError' if capability not enabled
-}
executeValidateTool ::
    Tracer IO LoadTrace ->
    Toolbox ->
    FilePath ->
    IO (Either DeveloperToolError ValidationResult)
executeValidateTool tracer toolbox toolPath = do
    if DevToolValidateTool `notElem` toolboxCapabilities toolbox
        then pure $ Left $ CapabilityNotEnabledError "validate-tool"
        else do
            result <- try $ Bash.loadScript tracer toolPath
            case result of
                Left (e :: SomeException) -> do
                    let errMsg = Text.pack $ show e
                    pure $
                        Right $
                            ValidationResult
                                { validationPath = toolPath
                                , validationValid = False
                                , validationSlug = Nothing
                                , validationError = Just errMsg
                                }
                Right (Left _err) -> do
                    let errMsg = "Failed to load script"
                    pure $
                        Right $
                            ValidationResult
                                { validationPath = toolPath
                                , validationValid = False
                                , validationSlug = Nothing
                                , validationError = Just errMsg
                                }
                Right (Right scriptDesc) -> do
                    pure $
                        Right $
                            ValidationResult
                                { validationPath = toolPath
                                , validationValid = True
                                , validationSlug = Just scriptDesc.scriptInfo.scriptSlug
                                , validationError = Nothing
                                }

{- | Execute agent validation.

This function validates an agent JSON configuration file by:
1. Checking if the file exists
2. Parsing the JSON
3. Validating required fields are present
4. Checking field types
5. Generating warnings for discouraged patterns

Returns:
* 'Right AgentValidationResult' with validation details
* 'Left DeveloperToolError' if capability not enabled or file not found
-}
executeValidateAgent ::
    Toolbox ->
    FilePath ->
    IO (Either DeveloperToolError AgentValidationResult)
executeValidateAgent toolbox agentPath = do
    if DevToolValidateAgent `notElem` toolboxCapabilities toolbox
        then pure $ Left $ CapabilityNotEnabledError "validate-agent"
        else do
            fileExists <- doesFileExist agentPath
            if not fileExists
                then
                    pure $
                        Right $
                            AgentValidationResult
                                { agentValidationPath = agentPath
                                , agentValidationValid = False
                                , agentValidationSlug = Nothing
                                , agentValidationErrors = ["File not found: " <> Text.pack agentPath]
                                , agentValidationWarnings = []
                                }
                else do
                    result <- FileLoader.readJsonDescriptionFile agentPath
                    case result of
                        Left err -> do
                            let errText = Text.pack $ show err
                            pure $
                                Right $
                                    AgentValidationResult
                                        { agentValidationPath = agentPath
                                        , agentValidationValid = False
                                        , agentValidationSlug = Nothing
                                        , agentValidationErrors = [errText]
                                        , agentValidationWarnings = []
                                        }
                        Right (AgentDescription agent) -> do
                            let (errors, warnings) = validateAgentStructure agent
                            pure $
                                Right $
                                    AgentValidationResult
                                        { agentValidationPath = agentPath
                                        , agentValidationValid = null errors
                                        , agentValidationSlug = Just (slug agent)
                                        , agentValidationErrors = errors
                                        , agentValidationWarnings = warnings
                                        }

-- | Validate agent structure and return (errors, warnings).
validateAgentStructure :: Agent -> ([Text], [Text])
validateAgentStructure agent =
    let errors =
            concat
                [ checkRequired "slug" (slug agent)
                , checkRequired "apiKeyId" (apiKeyId agent)
                , checkRequired "flavor" (flavor agent)
                , checkRequired "modelUrl" (modelUrl agent)
                , checkRequired "modelName" (modelName agent)
                , checkRequired "announce" (announce agent)
                , checkSystemPrompt (systemPrompt agent)
                ]
        warnings =
            concat
                [ checkToolSources agent
                ]
     in (errors, warnings)
  where
    checkRequired :: Text -> Text -> [Text]
    checkRequired fieldName value =
        if Text.null value
            then ["Required field '" <> fieldName <> "' is empty"]
            else []

    checkSystemPrompt :: [Text] -> [Text]
    checkSystemPrompt [] = ["Required field 'systemPrompt' is empty"]
    checkSystemPrompt prompts =
        if all Text.null prompts
            then ["Required field 'systemPrompt' contains only empty strings"]
            else []

    -- Check if at least one tool source is configured
    checkToolSources :: Agent -> [Text]
    checkToolSources a =
        let hasToolDir = case toolDirectory a of
                Just path -> not (null path)
                Nothing -> False
            hasBashToolboxes = case bashToolboxes a of
                Just tb -> not (null tb)
                Nothing -> False
            hasMcpServers = case mcpServers a of
                Just servers -> not (null servers)
                Nothing -> False
            hasOpenApi = case openApiToolboxes a of
                Just tb -> not (null tb)
                Nothing -> False
            hasPostgrest = case postgrestToolboxes a of
                Just tb -> not (null tb)
                Nothing -> False
            hasBuiltin = case builtinToolboxes a of
                Just tb -> not (null tb)
                Nothing -> False
            hasExtraAgents = case extraAgents a of
                Just agents -> not (null agents)
                Nothing -> False
            hasAnySource = hasToolDir || hasBashToolboxes || hasMcpServers || hasOpenApi || hasPostgrest || hasBuiltin || hasExtraAgents
         in if hasAnySource
                then []
                else ["No tool sources configured (toolDirectory, bashToolboxes, mcpServers, openApiToolboxes, postgrestToolboxes, builtinToolboxes, or extraAgents)"]

-------------------------------------------------------------------------------
-- Tool Execution - Creation
-------------------------------------------------------------------------------

{- | Execute agent creation.

This function creates a new agent configuration file either from scratch
or based on a reference agent.

Returns:
* 'Right CreateResult' on success or failure with error
* 'Left DeveloperToolError' if capability not enabled
-}
executeCreateAgent ::
    Toolbox ->
    -- | Source mode: Nothing for scratch, Just path for reference
    Maybe FilePath ->
    -- | Override parameters (merged with reference or used as-is)
    AgentOverrides ->
    -- | Output file path
    FilePath ->
    -- | Force overwrite
    Bool ->
    IO (Either DeveloperToolError CreateResult)
executeCreateAgent toolbox mReferencePath overrides outputPath force = do
    if DevToolCreateAgent `notElem` toolboxCapabilities toolbox
        then pure $ Left $ CapabilityNotEnabledError "create-agent"
        else do
            -- Check if output file exists
            unless force $ do
                exists <- doesFileExist outputPath
                when exists $ do
                    error $ "File already exists: " <> outputPath

            -- Load reference agent if provided
            referenceResult <- case mReferencePath of
                Nothing -> pure $ Right Nothing
                Just refPath -> do
                    refExists <- doesFileExist refPath
                    if not refExists
                        then pure $ Left $ FileNotFoundError refPath
                        else do
                            result <- FileLoader.readJsonDescriptionFile refPath
                            case result of
                                Left err -> pure $ Left $ AgentValidationError (Text.pack $ show err)
                                Right (AgentDescription agent) -> pure $ Right (Just agent)

            case referenceResult of
                Left err -> pure $ Left err
                Right mReferenceAgent -> do
                    -- Build final agent
                    let finalAgent = case mReferenceAgent of
                            Nothing -> agentFromOverrides overrides
                            Just refAgent -> mergeAgentWithOverrides refAgent overrides

                    -- Validate the resulting agent
                    let (errors, _warnings) = validateAgentStructure finalAgent
                    if not (null errors)
                        then
                            pure $
                                Right $
                                    CreateResult
                                        { createSuccess = False
                                        , createPath = outputPath
                                        , createAgentSlug = Just (slug finalAgent)
                                        , createError = Just $ "Validation failed: " <> Text.intercalate "; " errors
                                        }
                        else do
                            -- Write the agent to file
                            writeResult <- try $ do
                                createDirectoryIfMissing True (takeDirectory outputPath)
                                LByteString.writeFile outputPath $
                                    AesonPretty.encodePretty (AgentDescription finalAgent)

                            case writeResult of
                                Left (e :: SomeException) ->
                                    pure $
                                        Right $
                                            CreateResult
                                                { createSuccess = False
                                                , createPath = outputPath
                                                , createAgentSlug = Just (slug finalAgent)
                                                , createError = Just $ Text.pack $ show e
                                                }
                                Right () ->
                                    pure $
                                        Right $
                                            CreateResult
                                                { createSuccess = True
                                                , createPath = outputPath
                                                , createAgentSlug = Just (slug finalAgent)
                                                , createError = Nothing
                                                }

-- | Create an agent from overrides only (no reference).
agentFromOverrides :: AgentOverrides -> Agent
agentFromOverrides overrides =
    Agent
        { slug = fromMaybe "new-agent" (overrideSlug overrides)
        , apiKeyId = fromMaybe "main-key" (overrideApiKeyId overrides)
        , flavor = fromMaybe "OpenAIv1" (overrideFlavor overrides)
        , modelUrl = fromMaybe "https://api.openai.com/v1" (overrideModelUrl overrides)
        , modelName = fromMaybe "gpt-4" (overrideModelName overrides)
        , announce = fromMaybe "a helpful assistant" (overrideAnnounce overrides)
        , systemPrompt = fromMaybe ["You are a helpful assistant."] (overrideSystemPrompt overrides)
        , toolDirectory = fromMaybe (Just "tools") (overrideToolDirectory overrides)
        , bashToolboxes = fromMaybe Nothing (overrideBashToolboxes overrides)
        , mcpServers = fromMaybe (Just []) (overrideMcpServers overrides)
        , openApiToolboxes = fromMaybe Nothing (overrideOpenApiToolboxes overrides)
        , postgrestToolboxes = fromMaybe Nothing (overridePostgrestToolboxes overrides)
        , builtinToolboxes = fromMaybe (Just []) (overrideBuiltinToolboxes overrides)
        , extraAgents = fromMaybe Nothing (overrideExtraAgents overrides)
        , skillSources = fromMaybe Nothing (overrideSkillSources overrides)
        , autoEnableSkills = fromMaybe Nothing (overrideAutoEnableSkills overrides)
        }

-- | Merge a reference agent with overrides.
mergeAgentWithOverrides :: Agent -> AgentOverrides -> Agent
mergeAgentWithOverrides ref overrides =
    Agent
        { slug = fromMaybe (slug ref) (overrideSlug overrides)
        , apiKeyId = fromMaybe (apiKeyId ref) (overrideApiKeyId overrides)
        , flavor = fromMaybe (flavor ref) (overrideFlavor overrides)
        , modelUrl = fromMaybe (modelUrl ref) (overrideModelUrl overrides)
        , modelName = fromMaybe (modelName ref) (overrideModelName overrides)
        , announce = fromMaybe (announce ref) (overrideAnnounce overrides)
        , systemPrompt = fromMaybe (systemPrompt ref) (overrideSystemPrompt overrides)
        , toolDirectory = fromMaybe (toolDirectory ref) (overrideToolDirectory overrides)
        , bashToolboxes = fromMaybe (bashToolboxes ref) (overrideBashToolboxes overrides)
        , mcpServers = fromMaybe (mcpServers ref) (overrideMcpServers overrides)
        , openApiToolboxes = fromMaybe (openApiToolboxes ref) (overrideOpenApiToolboxes overrides)
        , postgrestToolboxes = fromMaybe (postgrestToolboxes ref) (overridePostgrestToolboxes overrides)
        , builtinToolboxes = fromMaybe (builtinToolboxes ref) (overrideBuiltinToolboxes overrides)
        , extraAgents = fromMaybe (extraAgents ref) (overrideExtraAgents overrides)
        , skillSources = fromMaybe (skillSources ref) (overrideSkillSources overrides)
        , autoEnableSkills = fromMaybe (autoEnableSkills ref) (overrideAutoEnableSkills overrides)
        }

-- TODO(lucas): verify why unused

{- | Execute tool creation.

This function creates a new tool script in the specified language,
either from scratch or based on a reference tool.

Returns:
* 'Right CreateResult' on success or failure with error
* 'Left DeveloperToolError' if capability not enabled
-}
executeCreateTool ::
    Tracer IO LoadTrace ->
    -- | Language (bash, python, haskell)
    Toolbox ->
    -- | Language (bash, python, haskell)
    Text ->
    -- | Source mode: Nothing for scratch, Just path for reference
    Maybe FilePath ->
    -- | Tool configuration
    ToolConfig ->
    -- | Output file path
    FilePath ->
    -- | Force overwrite
    Bool ->
    IO (Either DeveloperToolError CreateResult)
executeCreateTool tracer toolbox language mReferencePath config outputPath force = do
    if DevToolCreateTool `notElem` toolboxCapabilities toolbox
        then pure $ Left $ CapabilityNotEnabledError "create-tool"
        else do
            -- Check if output file exists
            unless force $ do
                exists <- doesFileExist outputPath
                when exists $ do
                    error $ "File already exists: " <> outputPath

            -- Load reference tool if provided
            referenceResult <- case mReferencePath of
                Nothing -> pure $ Right Nothing
                Just refPath -> do
                    refExists <- doesFileExist refPath
                    if not refExists
                        then pure $ Left $ FileNotFoundError refPath
                        else do
                            -- Only bash tools can be loaded for reference
                            result <- try $ Bash.loadScript tracer refPath
                            case result of
                                Left (e :: SomeException) ->
                                    pure $ Left $ ToolCreationError $ Text.pack $ show e
                                Right (Left _err) ->
                                    pure $ Left $ ToolCreationError "Failed to load reference tool"
                                Right (Right scriptDesc) ->
                                    pure $ Right (Just scriptDesc)

            case referenceResult of
                Left err -> pure $ Left err
                Right mReferenceTool -> do
                    -- Build final config
                    let finalConfig = case mReferenceTool of
                            Nothing -> config
                            Just refTool -> mergeToolConfig refTool config

                    -- Generate content
                    let content = makeToolTemplateFromConfig language finalConfig

                    -- Write the tool to file and make executable
                    writeResult <- try $ do
                        createDirectoryIfMissing True (takeDirectory outputPath)
                        Text.writeFile outputPath content
                        -- Make executable: get current permissions and set executable
                        perms <- getPermissions outputPath
                        setPermissions outputPath (perms{executable = True})

                    case writeResult of
                        Left (e :: SomeException) ->
                            pure $
                                Right $
                                    CreateResult
                                        { createSuccess = False
                                        , createPath = outputPath
                                        , createAgentSlug = Just (toolConfigSlug finalConfig)
                                        , createError = Just $ Text.pack $ show e
                                        }
                        Right () -> do
                            -- Validate the created tool by running describe
                            validationResult <- try $ Bash.loadScript tracer outputPath
                            case validationResult of
                                Left (e :: SomeException) ->
                                    pure $
                                        Right $
                                            CreateResult
                                                { createSuccess = False
                                                , createPath = outputPath
                                                , createAgentSlug = Just (toolConfigSlug finalConfig)
                                                , createError = Just $ "Created tool failed validation: " <> Text.pack (show e)
                                                }
                                Right (Left _err) ->
                                    pure $
                                        Right $
                                            CreateResult
                                                { createSuccess = False
                                                , createPath = outputPath
                                                , createAgentSlug = Just (toolConfigSlug finalConfig)
                                                , createError = Just "Created tool failed validation"
                                                }
                                Right (Right _validated) ->
                                    pure $
                                        Right $
                                            CreateResult
                                                { createSuccess = True
                                                , createPath = outputPath
                                                , createAgentSlug = Just (toolConfigSlug finalConfig)
                                                , createError = Nothing
                                                }

-- | Merge a reference tool description with config overrides.
mergeToolConfig :: Bash.ScriptDescription -> ToolConfig -> ToolConfig
mergeToolConfig ref config =
    ToolConfig
        { toolConfigSlug = if toolConfigSlug config /= "" then toolConfigSlug config else Bash.scriptSlug (Bash.scriptInfo ref)
        , toolConfigDescription = if toolConfigDescription config /= "" then toolConfigDescription config else Bash.scriptDescription (Bash.scriptInfo ref)
        , toolConfigArgs = if not (null (toolConfigArgs config)) then toolConfigArgs config else scriptArgsToConfigArgs (Bash.scriptArgs (Bash.scriptInfo ref))
        , toolConfigEmptyResult = toolConfigEmptyResult config
        }
  where
    scriptArgsToConfigArgs :: [Bash.ScriptArg] -> [ScriptArg]
    scriptArgsToConfigArgs = map $ \arg ->
        ScriptArg
            { scriptArgName = Bash.argName arg
            , scriptArgDescription = Bash.argDescription arg
            , scriptArgType = Bash.argTypeString arg
            , scriptArgBackingType = Bash.argBackingTypeString arg
            , scriptArgArity = case Bash.argTypeArity arg of
                Bash.Single -> "single"
                Bash.Optional -> "optional"
            , scriptArgMode = case Bash.argCallingMode arg of
                Bash.Stdin -> "stdin"
                Bash.Positional -> "positional"
                Bash.DashDashSpace -> "dashdashspace"
                Bash.DashDashEqual -> "dashdashequal"
            }

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
        , "# " <> toolConfigSlug config <> " - " <> toolConfigDescription config
        , ""
        , "if [ \"$1\" == \"describe\" ]; then"
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
               , "echo \"Tool " <> toolConfigSlug config <> " executed\""
               ]
  where
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
                    , "while [[ $# -gt 0 ]]; do"
                    , "    case $1 in"
                    , "        " <> argFlag <> "=*)"
                    , "            " <> varName <> "=\"${1#*=}\""
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
                    , "while [[ $# -gt 0 ]]; do"
                    , "    case $1 in"
                    , "        " <> argFlag <> ")"
                    , "            " <> varName <> "=\"$2\""
                    , "            shift 2"
                    , "            ;;"
                    , "        *)"
                    , "            shift"
                    , "            ;;"
                    , "    esac"
                    , "done"
                    ]

-------------------------------------------------------------------------------
-- Tool Execution - Legacy Scaffolding
-------------------------------------------------------------------------------

{- | Execute agent scaffolding.

This function generates agent scaffolding from a template.

Returns:
* 'Right ScaffoldResult' on success or failure with error
* 'Left DeveloperToolError' if capability not enabled
-}
executeScaffoldAgent ::
    Toolbox ->
    -- | Template name (openai, mistral, ollama)
    Text ->
    -- | Agent slug
    Text ->
    -- | Output file path
    FilePath ->
    -- | Force overwrite
    Bool ->
    IO (Either DeveloperToolError ScaffoldResult)
executeScaffoldAgent toolbox templateName agentSlug filePath force = do
    if DevToolScaffoldAgent `notElem` toolboxCapabilities toolbox
        then pure $ Left $ CapabilityNotEnabledError "scaffold-agent"
        else do
            unless force $ do
                exists <- doesFileExist filePath
                when exists $ do
                    error $ "File already exists: " <> filePath
            result <- try $ do
                let agent = makeAgentTemplate templateName agentSlug
                createDirectoryIfMissing True (takeDirectory filePath)
                LByteString.writeFile filePath $
                    AesonPretty.encodePretty (AgentDescription agent)
            case result of
                Left (e :: SomeException) ->
                    pure $
                        Right $
                            ScaffoldResult
                                { scaffoldSuccess = False
                                , scaffoldPath = filePath
                                , scaffoldError = Just $ Text.pack $ show e
                                }
                Right () ->
                    pure $
                        Right $
                            ScaffoldResult
                                { scaffoldSuccess = True
                                , scaffoldPath = filePath
                                , scaffoldError = Nothing
                                }

{- | Execute tool scaffolding.

This function generates tool scaffolding in a given language.

Returns:
* 'Right ScaffoldResult' on success or failure with error
* 'Left DeveloperToolError' if capability not enabled
-}
executeScaffoldTool ::
    Toolbox ->
    -- | Language (bash, python, haskell)
    Text ->
    -- | Tool slug
    Text ->
    -- | Output file path
    FilePath ->
    -- | Force overwrite
    Bool ->
    IO (Either DeveloperToolError ScaffoldResult)
executeScaffoldTool toolbox language toolSlug filePath force = do
    if DevToolScaffoldTool `notElem` toolboxCapabilities toolbox
        then pure $ Left $ CapabilityNotEnabledError "scaffold-tool"
        else do
            unless force $ do
                exists <- doesFileExist filePath
                when exists $ do
                    error $ "File already exists: " <> filePath
            result <- try $ do
                let content = makeToolTemplate language toolSlug
                createDirectoryIfMissing True (takeDirectory filePath)
                Text.writeFile filePath content
            case result of
                Left (e :: SomeException) ->
                    pure $
                        Right $
                            ScaffoldResult
                                { scaffoldSuccess = False
                                , scaffoldPath = filePath
                                , scaffoldError = Just $ Text.pack $ show e
                                }
                Right () ->
                    pure $
                        Right $
                            ScaffoldResult
                                { scaffoldSuccess = True
                                , scaffoldPath = filePath
                                , scaffoldError = Nothing
                                }

-- | Embedded bash-tools documentation
bashToolsDocumentation :: Text
bashToolsDocumentation = $(embedStringFile "docs/binary-tool.md")

{- | Execute show spec.

This function returns specification documentation.

Returns:
* 'Right Text' with the spec content
* 'Left DeveloperToolError' if capability not enabled or unknown spec
-}
executeShowSpec ::
    Toolbox ->
    -- | Spec name (bash-tools)
    Text ->
    IO (Either DeveloperToolError Text)
executeShowSpec toolbox specName = do
    if DevToolShowSpec `notElem` toolboxCapabilities toolbox
        then pure $ Left $ CapabilityNotEnabledError "show-spec"
        else case specName of
            "bash-tools" -> pure $ Right bashToolsDocumentation
            _ -> pure $ Left $ InvalidTemplateError $ "Unknown spec: " <> specName

-------------------------------------------------------------------------------
-- Agent Templates
-------------------------------------------------------------------------------

-- | Default developer toolbox configuration for agent templates.
defaultDeveloperToolboxDescription :: BuiltinToolboxDescription
defaultDeveloperToolboxDescription =
    DeveloperToolbox $
        DeveloperToolboxDescription
            { developerToolboxName = "developer"
            , developerToolboxDescription = "Tools for developing agents and tools"
            , developerToolboxCapabilities =
                [ DevToolValidateTool
                , DevToolScaffoldAgent
                , DevToolScaffoldTool
                , DevToolShowSpec
                , DevToolValidateAgent
                , DevToolCreateAgent
                , DevToolCreateTool
                , DevToolReadFileRange
                , DevToolWriteFileRange
                ]
            , developerToolboxFileScope = Nothing -- Uses default (deny all)
            , developerToolboxActivation = Nothing -- Uses default: AlwaysActivated
            }

-- | Create an agent from a template
makeAgentTemplate :: Text -> Text -> Agent
makeAgentTemplate templateName agentSlug = case templateName of
    "mistral" ->
        Agent
            { slug = agentSlug
            , apiKeyId = "mistral-key"
            , flavor = "OpenAIv1"
            , modelUrl = "https://api.mistral.ai/v1"
            , modelName = "mistral-large-latest"
            , announce = "a helpful assistant powered by Mistral AI"
            , systemPrompt =
                [ "You are a helpful assistant powered by Mistral AI."
                , "You provide clear, accurate, and concise responses."
                , "You excel at reasoning and following instructions precisely."
                ]
            , toolDirectory = Just "tools"
            , bashToolboxes = Nothing
            , mcpServers = Just []
            , openApiToolboxes = Nothing
            , postgrestToolboxes = Nothing
            , builtinToolboxes = Just [defaultDeveloperToolboxDescription]
            , extraAgents = Nothing
            , skillSources = Nothing
            , autoEnableSkills = Nothing
            }
    "ollama" ->
        Agent
            { slug = agentSlug
            , apiKeyId = "ollama-key"
            , flavor = "OpenAIv1"
            , modelUrl = "http://localhost:11434/v1"
            , modelName = "llama3.2"
            , announce = "a helpful assistant running locally via Ollama"
            , systemPrompt =
                [ "You are a helpful assistant running locally on the user's machine."
                , "You provide clear and accurate responses while respecting privacy."
                , "Note: You are running on local hardware, which may limit capabilities."
                ]
            , toolDirectory = Just "tools"
            , bashToolboxes = Nothing
            , mcpServers = Just []
            , openApiToolboxes = Nothing
            , postgrestToolboxes = Nothing
            , builtinToolboxes = Just [defaultDeveloperToolboxDescription]
            , extraAgents = Nothing
            , skillSources = Nothing
            , autoEnableSkills = Nothing
            }
    _ ->
        -- Default to OpenAI template
        Agent
            { slug = agentSlug
            , apiKeyId = "main-key"
            , flavor = "OpenAIv1"
            , modelUrl = "https://api.openai.com/v1"
            , modelName = "gpt-4-turbo-preview"
            , announce = "a helpful assistant powered by OpenAI"
            , systemPrompt =
                [ "You are a helpful assistant."
                , "You provide clear, accurate, and concise responses."
                , "When using tools, you explain your actions to the user."
                ]
            , toolDirectory = Just "tools"
            , bashToolboxes = Nothing
            , mcpServers = Just []
            , openApiToolboxes = Nothing
            , postgrestToolboxes = Nothing
            , builtinToolboxes = Just [defaultDeveloperToolboxDescription]
            , extraAgents = Nothing
            , skillSources = Nothing
            , autoEnableSkills = Nothing
            }

-------------------------------------------------------------------------------
-- Tool Templates
-------------------------------------------------------------------------------

-- | Create a tool template for a given language
makeToolTemplate :: Text -> Text -> Text
makeToolTemplate language toolSlug = case language of
    "python" -> makePythonToolTemplate toolSlug
    "haskell" -> makeHaskellToolTemplate toolSlug
    _ -> makeBashToolTemplate toolSlug

-- | Create a bash tool template
makeBashToolTemplate :: Text -> Text
makeBashToolTemplate toolSlug =
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
        , "  \"args\": ["
        , "    {"
        , "      \"name\": \"example_arg\","
        , "      \"description\": \"An example argument\","
        , "      \"type\": \"string\","
        , "      \"backing_type\": \"string\","
        , "      \"arity\": \"single\","
        , "      \"mode\": \"dashdashspace\""
        , "    }"
        , "  ],"
        , "  \"empty-result\": { \"tag\": \"AddMessage\", \"contents\": \"No results\" }"
        , "}"
        , "EOF"
        , "    exit 0"
        , "fi"
        , ""
        , "# Parse arguments for 'run' command"
        , "EXAMPLE_ARG=\"\""
        , "while [[ $# -gt 0 ]]; do"
        , "    case $1 in"
        , "        --example-arg)"
        , "            EXAMPLE_ARG=\"$2\""
        , "            shift 2"
        , "            ;;"
        , "        *)"
        , "            shift"
        , "            ;;"
        , "    esac"
        , "done"
        , ""
        , "# Main logic here"
        , "echo \"Tool " <> toolSlug <> " executed with: $EXAMPLE_ARG\""
        ]

-- | Create a Python tool template
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
        , "    \"args\": ["
        , "        {"
        , "            \"name\": \"example_arg\","
        , "            \"description\": \"An example argument\","
        , "            \"type\": \"string\","
        , "            \"backing_type\": \"string\","
        , "            \"arity\": \"single\","
        , "            \"mode\": \"dashdashspace\""
        , "        }"
        , "    ],"
        , "    \"empty-result\": {\"tag\": \"AddMessage\", \"contents\": \"No results\"}"
        , "}"
        , ""
        , ""
        , "def main():"
        , "    if len(sys.argv) > 1 and sys.argv[1] == \"describe\":"
        , "        print(json.dumps(DESCRIPTION))"
        , "        sys.exit(0)"
        , ""
        , "    # Parse arguments"
        , "    example_arg = \"\""
        , "    i = 2  # Skip 'run' command"
        , "    while i < len(sys.argv):"
        , "        if sys.argv[i] == \"--example-arg\" and i + 1 < len(sys.argv):"
        , "            example_arg = sys.argv[i + 1]"
        , "            i += 2"
        , "        else:"
        , "            i += 1"
        , ""
        , "    # Main logic here"
        , "    print(f'Tool " <> toolSlug <> " executed with: {example_arg}')"
        , ""
        , ""
        , "if __name__ == \"__main__\":"
        , "    main()"
        ]

-- | Create a Haskell tool template
makeHaskellToolTemplate :: Text -> Text
makeHaskellToolTemplate toolSlug =
    Text.unlines
        [ "#!/usr/bin/env runhaskell"
        , "{-# LANGUAGE OverloadedStrings #-}"
        , ""
        , "-- | " <> toolSlug <> " - Tool description here"
        , ""
        , "import qualified Data.Aeson as Aeson"
        , "import qualified Data.ByteString.Lazy as LBS"
        , "import Data.Text (Text)"
        , "import qualified Data.Text as Text"
        , "import qualified Data.Text.IO as Text"
        , "import System.Environment (getArgs)"
        , ""
        , "-- | Tool description"
        , "data ScriptInfo = ScriptInfo"
        , "    { scriptSlug :: Text"
        , "    , scriptDescription :: Text"
        , "    , scriptArgs :: [ScriptArg]"
        , "    } deriving (Show)"
        , ""
        , "data ScriptArg = ScriptArg"
        , "    { argName :: Text"
        , "    , argDescription :: Text"
        , "    , argType :: Text"
        , "    , argBackingType :: Text"
        , "    , argArity :: Text"
        , "    , argMode :: Text"
        , "    } deriving (Show)"
        , ""
        , "instance Aeson.ToJSON ScriptInfo where"
        , "    toJSON s = Aeson.object"
        , "        [ \"slug\" Aeson..= scriptSlug s"
        , "        , \"description\" Aeson..= scriptDescription s"
        , "        , \"args\" Aeson..= scriptArgs s"
        , "        ]"
        , ""
        , "instance Aeson.ToJSON ScriptArg where"
        , "    toJSON a = Aeson.object"
        , "        [ \"name\" Aeson..= argName a"
        , "        , \"description\" Aeson..= argDescription a"
        , "        , \"type\" Aeson..= argType a"
        , "        , \"backing_type\" Aeson..= argBackingType a"
        , "        , \"arity\" Aeson..= argArity a"
        , "        , \"mode\" Aeson..= argMode a"
        , "        ]"
        , ""
        , "description :: ScriptInfo"
        , "description = ScriptInfo"
        , "    { scriptSlug = \"" <> toolSlug <> "\""
        , "    , scriptDescription = \"Description of what this tool does\""
        , "    , scriptArgs ="
        , "        [ ScriptArg"
        , "            { argName = \"example_arg\""
        , "            , argDescription = \"An example argument\""
        , "            , argType = \"string\""
        , "            , argBackingType = \"string\""
        , "            , argArity = \"single\""
        , "            , argMode = \"dashdashspace\""
        , "            }"
        , "        ]"
        , "    }"
        , ""
        , "main :: IO ()"
        , "main = do"
        , "    args <- getArgs"
        , "    case args of"
        , "        (\"describe\":_) -> do"
        , "            LBS.putStr $ Aeson.encode description"
        , "        (\"run\":_) -> do"
        , "            -- Parse arguments and execute"
        , "            let exampleArg = parseArg args \"--example-arg\""
        , "            Text.putStrLn $ \"Tool " <> toolSlug <> " executed with: \" <> exampleArg"
        , "        _ -> do"
        , "            Text.hPutStrLn stderr \"Usage: " <> toolSlug <> " describe|run\""
        , ""
        , "parseArg :: [String] -> Text -> Text"
        , "parseArg args name = case break (== Text.unpack name) args of"
        , "    (_, _:value:_) -> Text.pack value"
        , "    _ -> \"\""
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
