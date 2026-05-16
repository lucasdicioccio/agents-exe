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

These tools help developers write and validate agents and tools.

The patch-file capability provides atomic, context-aware file modifications
using unified diff format, addressing the fragility issues of write-file-range
when making multiple edits.

This module re-exports functionality from the sub-modules for convenience.
For more granular imports, use the sub-modules directly.
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
    executePatchFile,

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
import System.Agents.Tools.DeveloperToolbox.Patch
import System.Agents.Tools.DeveloperToolbox.Read
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
