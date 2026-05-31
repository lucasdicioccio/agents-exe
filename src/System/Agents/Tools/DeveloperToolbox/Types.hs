{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Core types for the DeveloperToolbox module.

This module defines the fundamental types used across all DeveloperToolbox
capabilities including trace events, error types, result types, and
toolbox configuration.
-}
module System.Agents.Tools.DeveloperToolbox.Types (
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
    RangeEditResult (..),
    PatchResult (..),
    PatchError (..),
    Hunk (..),
    RangeSpec (..),
    AgentOverrides (..),
    ToolConfig (..),
    ScriptArg (..),
    Snapshot (..),
    SnapshotRef (..),
    SnapshotStore,
    RestoreResult (..),
    defaultAgentOverrides,
    makeSnapshot,
    emptySnapshotStore,

    -- * Re-exported capability types
    DeveloperToolCapability (..),
) where

import Data.Aeson (ToJSON (..), (.=))
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (fromStrict)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Digest.Pure.MD5 (md5)
import System.Agents.Base (
    BashToolboxDescription,
    BuiltinToolboxDescription (..),
    DeveloperToolCapability (..),
    DeveloperToolboxDescription (..),
    ExtraAgentRef,
    McpServerDescription,
    OpenAPIToolboxDescription,
    PostgRESTToolboxDescription,
 )
import System.Agents.FileSandbox (FileSandbox)
import System.Agents.Tools.Skills.Types (SkillName, SkillSource)
import Control.Concurrent.STM (TVar, newTVarIO)

-------------------------------------------------------------------------------
-- Snapshot Types
-------------------------------------------------------------------------------

{- | A reference to a snapshot, using MD5 hash of the content.
This serves as both identifier and content verification.
-}
newtype SnapshotRef = SnapshotRef { unSnapshotRef :: Text }
    deriving (Show, Eq, Ord)

-- | JSON serialization for SnapshotRef.
instance ToJSON SnapshotRef where
    toJSON (SnapshotRef ref) = Aeson.toJSON ref

{- | A snapshot of file content stored in memory.
Snapshots are keyed by their MD5 hash for deduplication and integrity.
-}
data Snapshot = Snapshot
    { snapshotRef :: SnapshotRef
    -- ^ MD5 hash reference for this snapshot
    , snapshotContent :: ByteString
    -- ^ Raw file content
    , snapshotSize :: Int
    -- ^ Size in bytes
    }
    deriving (Show)

-- | Create a snapshot from file content, computing the MD5 hash.
makeSnapshot :: ByteString -> Snapshot
makeSnapshot content =
    let hash = md5 (fromStrict content)
        hashText = Text.pack (show hash)
     in Snapshot
            { snapshotRef = SnapshotRef hashText
            , snapshotContent = content
            , snapshotSize = BS.length content
            }

-- | In-memory store for snapshots, keyed by reference.
type SnapshotStore = TVar (Map SnapshotRef Snapshot)

-- | Create an empty snapshot store.
emptySnapshotStore :: IO SnapshotStore
emptySnapshotStore = newTVarIO Map.empty

-------------------------------------------------------------------------------
-- Trace Events
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
* Patch file operations
* Snapshot operations
* Restore operations
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
    | -- | Patch file started
      PatchFileStartedTrace !FilePath !Int
    | -- | Patch file completed
      PatchFileCompletedTrace !FilePath !Int !Int
    | -- | Patch file error
      PatchFileErrorTrace !Text !Text
    | -- | Snapshot taken before edit
      SnapshotTakenTrace !FilePath !SnapshotRef
    | -- | File restored from snapshot
      FileRestoredTrace !FilePath !SnapshotRef
    | -- | Snapshot not found
      SnapshotNotFoundTrace !SnapshotRef
    | -- | Snapshot mismatch detected (optimistic locking)
      SnapshotMismatchTrace !FilePath !SnapshotRef !SnapshotRef
    | -- | Error during operation
      DeveloperToolErrorTrace !Text !Text
    deriving (Show)

-------------------------------------------------------------------------------
-- Tool Description
-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
-- Toolbox State
-------------------------------------------------------------------------------

{- | Runtime state for a developer toolbox.

The toolbox maintains:
* Toolbox name and description
* List of enabled capabilities
* The original configuration description used to create this toolbox
* Optional file sandbox for file-related capabilities
* Optional snapshot store for snapshot-enabled operations
-}
data Toolbox = Toolbox
    { toolboxName :: Text
    , toolboxDescription :: Text
    , toolboxCapabilities :: [DeveloperToolCapability]
    , toolboxConfig :: DeveloperToolboxDescription
    -- ^ Original configuration description used to create this toolbox
    , toolboxFileSandbox :: Maybe FileSandbox
    -- ^ Optional file sandbox for read-file-range, write-file-range, patch-file
    , toolboxSnapshotStore :: Maybe SnapshotStore
    -- ^ Optional snapshot store for snapshot-enabled file operations
    }

-------------------------------------------------------------------------------
-- Result Types
-------------------------------------------------------------------------------

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
    | -- | Entire file (overwrite/read all)
      Whole
    | -- | Insert after specified line (single line or end of range)
      After Int
    deriving (Show, Eq)

-- | Result of a read file range operation.
--
-- Includes metadata fields to help distinguish between:
-- - Empty files (totalLineCount = 0, totalFileSize = 0)
-- - Out-of-bounds ranges (linesRead = 0, but totalLineCount > 0)
-- - Successfully read ranges (linesRead > 0)
data ReadFileRangeResult = ReadFileRangeResult
    { readFilePath :: FilePath
    -- ^ Path to the file that was read
    , readFileContent :: Text
    -- ^ Content read (with line numbers prepended as "N\tcontent")
    , readFileLinesRead :: Int
    -- ^ Number of lines actually returned in the content
    , readFileTotalSize :: Int
    -- ^ Total file size in bytes
    , readFileTotalLines :: Int
    -- ^ Total number of lines in the file
    , readFileRangesParsed :: [Text]
    -- ^ Normalized range specifications that were applied
    }
    deriving (Show)

-- | JSON serialization for ReadFileRangeResult.
instance ToJSON ReadFileRangeResult where
    toJSON result =
        Aeson.object
            [ "path" .= readFilePath result
            , "content" .= readFileContent result
            , "linesRead" .= readFileLinesRead result
            , "totalFileSize" .= readFileTotalSize result
            , "totalLineCount" .= readFileTotalLines result
            , "rangesParsed" .= readFileRangesParsed result
            ]

-- | Per-range edit result providing detailed feedback for each edit operation.
data RangeEditResult = RangeEditResult
    { rangeEditSpec :: Text
    -- ^ Original range spec (e.g., "5", "1-3", "head", "tail")
    , rangeEditOriginalStart :: Int
    -- ^ Original start line (1-based, before any edits)
    , rangeEditOriginalEnd :: Int
    -- ^ Original end line (1-based, before any edits)
    , rangeEditLinesWritten :: Int
    -- ^ Number of lines written for this range
    , rangeEditFinalStartLine :: Maybe Int
    -- ^ Final start line after all edits applied (Nothing for deletions)
    , rangeEditFinalEndLine :: Maybe Int
    -- ^ Final end line after all edits applied (Nothing for deletions)
    , rangeEditOperation :: Text
    -- ^ Operation type: "prepend", "append", "replace", "delete", "overwrite", "skipped-out-of-bounds", "insert-after"
    }
    deriving (Show)

-- | JSON serialization for RangeEditResult.
instance ToJSON RangeEditResult where
    toJSON result =
        let originalLines = Text.pack (show (rangeEditOriginalStart result)) <> "-" <> Text.pack (show (rangeEditOriginalEnd result))
         in Aeson.object
                [ "range" .= rangeEditSpec result
                , "originalLines" .= originalLines
                , "linesWritten" .= rangeEditLinesWritten result
                , "finalStartLine" .= rangeEditFinalStartLine result
                , "finalEndLine" .= rangeEditFinalEndLine result
                , "operation" .= rangeEditOperation result
                ]

-- | Result of a write file range operation with detailed per-range feedback.
--
-- Includes both before and after snapshot references for optimistic locking.
-- The beforeSnapshotRef references the state of the file before the edit,
-- while afterSnapshotRef references the state after the edit.
data WriteFileRangeResult = WriteFileRangeResult
    { writeFilePath :: FilePath
    -- ^ Path to the file that was modified
    , writeFileRangesModified :: Int
    -- ^ Number of ranges that were processed
    , writeFileLinesWritten :: Int
    -- ^ Total number of lines written across all ranges
    , writeFileFinalLineCount :: Int
    -- ^ Total lines in file after all edits
    , writeFileRangeResults :: [RangeEditResult]
    -- ^ Detailed results for each range
    , writeFileBeforeSnapshotRef :: Maybe SnapshotRef
    -- ^ Reference to snapshot of file content before edit (if snapshot enabled)
    , writeFileAfterSnapshotRef :: Maybe SnapshotRef
    -- ^ Reference to snapshot of file content after edit (if snapshot enabled)
    }
    deriving (Show)

-- | JSON serialization for WriteFileRangeResult.
instance ToJSON WriteFileRangeResult where
    toJSON result =
        Aeson.object
            [ "path" .= writeFilePath result
            , "rangesModified" .= writeFileRangesModified result
            , "linesWritten" .= writeFileLinesWritten result
            , "finalLineCount" .= writeFileFinalLineCount result
            , "rangeResults" .= writeFileRangeResults result
            , "beforeSnapshotRef" .= writeFileBeforeSnapshotRef result
            , "afterSnapshotRef" .= writeFileAfterSnapshotRef result
            ]

-- | Result of a patch file operation.
--
-- Includes both before and after snapshot references for optimistic locking.
data PatchResult = PatchResult
    { patchFilePath :: FilePath
    -- ^ Path to the file that was patched
    , patchHunksApplied :: Int
    -- ^ Number of hunks successfully applied
    , patchHunksRejected :: Int
    -- ^ Number of hunks rejected
    , patchLinesChanged :: Int
    -- ^ Total number of lines changed
    , patchBeforeSnapshotRef :: Maybe SnapshotRef
    -- ^ Reference to snapshot of file content before patch (if snapshot enabled)
    , patchAfterSnapshotRef :: Maybe SnapshotRef
    -- ^ Reference to snapshot of file content after patch (if snapshot enabled)
    }
    deriving (Show)

-- | JSON serialization for PatchResult.
instance ToJSON PatchResult where
    toJSON result =
        Aeson.object
            [ "path" .= patchFilePath result
            , "hunksApplied" .= patchHunksApplied result
            , "hunksRejected" .= patchHunksRejected result
            , "linesChanged" .= patchLinesChanged result
            , "beforeSnapshotRef" .= patchBeforeSnapshotRef result
            , "afterSnapshotRef" .= patchAfterSnapshotRef result
            ]

-- | Result of a restore file operation.
data RestoreResult = RestoreResult
    { restoreFilePath :: FilePath
    , restoreSnapshotRef :: SnapshotRef
    , restoreSuccess :: Bool
    , restoreError :: Maybe Text
    }
    deriving (Show)

-- | JSON serialization for RestoreResult.
instance ToJSON RestoreResult where
    toJSON result =
        Aeson.object
            [ "path" .= restoreFilePath result
            , "snapshotRef" .= restoreSnapshotRef result
            , "success" .= restoreSuccess result
            , "error" .= restoreError result
            ]

-- | Errors that can occur during patch operations.
data PatchError
    = -- | Invalid diff format
      PatchParseError !Text
    | -- | Line N context doesn't match
      PatchContextMismatch !Int !Text
    | -- | Hunks at lines X and Y overlap
      PatchHunkOverlap !Int !Int
    | -- | File to patch not found
      PatchFileNotFound !FilePath
    | -- | Line number out of valid range
      PatchInvalidLineNumber !Int
    deriving (Show, Eq)

{- | A hunk represents a single change section in a unified diff.
The context lines must match exactly for the hunk to be applied.
-}
data Hunk = Hunk
    { hunkOldStart :: Int
    -- ^ Starting line number in the original file (1-based)
    , hunkOldCount :: Int
    -- ^ Number of lines the hunk applies to in the original file
    , hunkNewStart :: Int
    -- ^ Starting line number in the new file (1-based)
    , hunkNewCount :: Int
    -- ^ Number of lines the hunk produces in the new file
    , hunkContextBefore :: [Text]
    -- ^ Context lines before the changed lines (no prefix)
    , hunkRemovedLines :: [Text]
    -- ^ Lines to remove (from original file, - prefix in diff)
    , hunkAddedLines :: [Text]
    -- ^ Lines to add (to new file, + prefix in diff)
    , hunkContextAfter :: [Text]
    -- ^ Context lines after the changed lines (no prefix)
    }
    deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Agent/Tool Configuration Types
-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
-- Error Types
-------------------------------------------------------------------------------

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
    | -- | File access denied by sandbox
      FileAccessDeniedError !FilePath !Text
    | -- | Error during patch operation
      PatchValidationError !PatchError
    | -- | Snapshot not found
      SnapshotNotFoundError !SnapshotRef
    | -- | Restore operation failed
      RestoreError !Text
    | -- | Snapshot mismatch (optimistic locking failure)
      -- Expected snapshot doesn't match actual file content
      SnapshotMismatchError
        { snapshotMismatchExpected :: !SnapshotRef
        -- ^ Expected snapshot reference provided by caller
        , snapshotMismatchActual :: !SnapshotRef
        -- ^ Actual snapshot of current file content
        }
    deriving (Show, Eq)

