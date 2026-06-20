{-# LANGUAGE OverloadedStrings #-}

{- |
Capability information for the DeveloperToolbox.

This module provides utilities for working with DeveloperToolCapabilities,
including converting between capability names and their representations.
-}
module System.Agents.Tools.DeveloperToolbox.Capabilities (
    -- * Capability info
    getCapabilityInfo,
    capabilityToName,
    capabilityFromName,
) where

import Data.Text (Text)

import System.Agents.Base (DeveloperToolCapability (..))

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
capabilityToName DevToolPatchFile = "patch-file"
capabilityToName DevToolHelp = "help"
capabilityToName DevToolSnapshot = "snapshot"
capabilityToName DevToolRestoreFile = "restore-file"
capabilityToName DevToolListDirectory = "list-directory"
capabilityToName DevToolTraverseDirectory = "traverse-directory"

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
    "patch-file" -> Just DevToolPatchFile
    "help" -> Just DevToolHelp
    "snapshot" -> Just DevToolSnapshot
    "restore-file" -> Just DevToolRestoreFile
    "list-directory" -> Just DevToolListDirectory
    "traverse-directory" -> Just DevToolTraverseDirectory
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
    , "Replaces specific lines in a file. Ranges are comma-separated line numbers or ranges (e.g., '2,5,8' or '1-3'). Takes a list of content blocks, where each block corresponds to one range. Use empty blocks to delete lines. Multiple edits are processed sequentially with position tracking."
    )
getCapabilityInfo DevToolPatchFile =
    ( "patch-file"
    , "Applies a unified diff patch to a file atomically with context validation"
    )
getCapabilityInfo DevToolHelp =
    ( "help"
    , "Returns detailed documentation for all activated developer toolbox capabilities"
    )
getCapabilityInfo DevToolSnapshot =
    ( "snapshot"
    , "Deprecated, no-op: snapshotting is now always on for write-file-range and patch-file (file content is stored in RAM before/after every edit, enabling rollback via restore-file). Listing this capability is no longer required and has no effect."
    )
getCapabilityInfo DevToolRestoreFile =
    ( "restore-file"
    , "Restores a file to a previous version using a snapshot reference (MD5 hash) returned by write-file-range or patch-file."
    )
getCapabilityInfo DevToolListDirectory =
    ( "list-directory"
    , "Lists directory contents with metadata (type, size, permissions, modification time)"
    )
getCapabilityInfo DevToolTraverseDirectory =
    ( "traverse-directory"
    , "Recursively traverses a directory tree and returns all entries within scope"
    )
