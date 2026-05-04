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
* Reading specific line ranges from a file
* Writing to specific line ranges in files
* Applying unified diff patches to files
* Restoring files from snapshots
* Getting comprehensive help documentation
* Listing directory contents with metadata
* Traversing directory trees with scope enforcement

These tools help developers write and validate agents and tools.

The patch-file capability provides atomic, context-aware file modifications
using unified diff format, addressing the fragility issues of write-file-range
when making multiple edits.

The snapshot capability enables rollback by storing file content in memory
before edits. When snapshot is enabled, write-file-range and patch-file
operations automatically save the original file content keyed by MD5 hash.
The restore-file capability can then restore the file to any saved snapshot.

The help capability returns comprehensive documentation for all enabled
capabilities, providing detailed parameter descriptions and usage examples.
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
    RangeEditResult (..),
    PatchResult (..),
    PatchError (..),
    Hunk (..),
    DirectoryListingResult (..),
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
    executePatchFile,
    executeRestoreFile,
    executeHelp,
    executeListDirectory,
    executeTraverseDirectory,

    -- * Capability info
    getCapabilityInfo,
    capabilityToName,
    capabilityFromName,

    -- * Range parsing
    parseRanges,
    parseRangePart,
    parsePositiveInt,

    -- * Patch parsing and validation
    parseUnifiedDiff,
    validateHunk,
    validateAllHunks,
    applyHunk,

    -- * Template functions (exposed for testing)
    makeAgentTemplate,
    makeToolTemplate,
    makeBashToolTemplate,
    makePythonToolTemplate,
    makeHaskellToolTemplate,
    makeToolTemplateFromConfig,
    makeBashToolTemplateFromConfig,
    makePythonToolTemplateFromConfig,
    makeHaskellToolTemplateFromConfig,
    mergeAgentWithOverrides,
    mergeToolConfig,
    agentFromOverrides,

    -- * Read file range helpers (exposed for testing)
    extractLines,
    extractRange,
    formatLineWithNumber,

    -- * Help text generation
    buildHelpText,

    -- * Default configuration
    defaultDeveloperToolboxDescription,

    -- * Re-exports from Types (includes DeveloperToolCapability from Base)
    DeveloperToolCapability (..),
) where

-- Re-export all types
import System.Agents.Tools.DeveloperToolbox.Types

-- Re-export initialization
import System.Agents.Tools.DeveloperToolbox.Init

-- Re-export capability execution functions

import System.Agents.Tools.DeveloperToolbox.Create
import System.Agents.Tools.DeveloperToolbox.Directory
import System.Agents.Tools.DeveloperToolbox.Help
import System.Agents.Tools.DeveloperToolbox.Patch
import System.Agents.Tools.DeveloperToolbox.Read
import System.Agents.Tools.DeveloperToolbox.Restore
import System.Agents.Tools.DeveloperToolbox.Scaffold
import System.Agents.Tools.DeveloperToolbox.Spec
import System.Agents.Tools.DeveloperToolbox.Validate
import System.Agents.Tools.DeveloperToolbox.Write

-- Re-export capabilities info
import System.Agents.Tools.DeveloperToolbox.Capabilities

-- Re-export range parsing
import System.Agents.Tools.DeveloperToolbox.Range

-- Re-export templates
import System.Agents.Tools.DeveloperToolbox.Templates
