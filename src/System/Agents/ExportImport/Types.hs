{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- |
-- Module      : System.Agents.ExportImport.Types
-- Description : Core data types for exporting and importing agent configurations
-- Copyright   : (c) 2024 Lucas DiCioccio
-- License     : Apache-2.0
--
-- This module defines the core data types for the agent export/import system.
-- It supports exporting complete agent configurations including:
--
-- * Agent configurations (from 'System.Agents.Base')
-- * Tool scripts (from 'System.Agents.Tools.Bash')
-- * MCP server configurations
--
-- The package format supports two modes:
--
-- 1. __Embedded mode__: Tool content is embedded directly in the JSON (self-contained)
-- 2. __Reference mode__: Tools are referenced by path (for git-based repos)
--
-- == Example Package Structure
--
-- A typical export package in JSON format:
--
-- > {
-- >   "metadata": {
-- >     "version": "1.0.0",
-- >     "created_at": "2024-01-15T10:30:00Z",
-- >     "description": "Production agent configurations",
-- >     "source": "https://github.com/org/agents"
-- >   },
-- >   "agents": [...],
-- >   "tools": [...],
-- >   "mcp_servers": [...]
-- > }
--
-- == Namespace Support
--
-- Namespaces allow organizing multiple agents/tools in a hierarchical structure,
-- useful for git-based multi-agent repositories. See 'System.Agents.ExportImport.Namespace'
-- for parsing and conversion functions.
module System.Agents.ExportImport.Types (
    -- * Schema Version
    currentSchemaVersion,
    SchemaVersion (..),

    -- * Export Package Types
    ExportPackage (..),
    PackageMetadata (..),

    -- * Agent Export
    AgentExport (..),

    -- * Tool Export
    ToolExport (..),
    ToolContentMode (..),

    -- * MCP Server Export
    McpServerExport (..),
) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import System.Posix.Types (CMode)

import System.Agents.Base (Agent, McpServerDescription)

-------------------------------------------------------------------------------
-- Schema Version
-------------------------------------------------------------------------------

-- | The current schema version for export packages.
-- This follows semantic versioning for compatibility tracking.
--
-- === Version History
--
-- * __1.0.0__: Initial format supporting agents, tools, and MCP servers
--
-- Future versions should maintain backward compatibility where possible,
-- and include migration logic for breaking changes.
currentSchemaVersion :: SchemaVersion
currentSchemaVersion = SchemaVersion 1 0 0

-- | Semantic version for the export package schema.
-- Used for compatibility checking during import.
data SchemaVersion = SchemaVersion
    { major :: !Int
    , minor :: !Int
    , patch :: !Int
    }
    deriving (Show, Eq, Ord, Generic)

-------------------------------------------------------------------------------
-- Export Package Types
-------------------------------------------------------------------------------

-- | A complete exportable package containing agent configurations,
-- tools, and MCP server definitions.
--
-- This type supports both embedded and reference modes for tool content.
-- Use 'ToolContentMode' to specify how tools are stored.
data ExportPackage = ExportPackage
    { packageMetadata :: !PackageMetadata
    -- ^ Package metadata including version and creation info
    , packageAgents :: ![AgentExport]
    -- ^ Agent configurations with their associated tools
    , packageTools :: ![ToolExport]
    -- ^ Standalone tools not attached to specific agents
    , packageMcpServers :: ![McpServerExport]
    -- ^ MCP server configurations
    }
    deriving (Show, Eq, Generic)

-- | Metadata about the export package.
data PackageMetadata = PackageMetadata
    { packageSchemaVersion :: !SchemaVersion
    -- ^ Schema version for compatibility checking
    , packageCreatedAt :: !UTCTime
    -- ^ Timestamp when the package was created
    , packageDescription :: !(Maybe Text)
    -- ^ Optional human-readable description
    , packageSource :: !(Maybe Text)
    -- ^ Optional source information (e.g., git URL)
    }
    deriving (Show, Eq, Generic)

-------------------------------------------------------------------------------
-- Agent Export
-------------------------------------------------------------------------------

-- | An exported agent configuration with optional namespace and embedded tools.
data AgentExport = AgentExport
    { agentConfig :: !Agent
    -- ^ The agent configuration (from 'System.Agents.Base')
    , agentNamespace :: !(Maybe Text)
    -- ^ Optional namespace for organizing agents in git repos.
    -- Example: @"team1/project-a"@ or @"org/department"@
    , agentTools :: ![ToolExport]
    -- ^ Tools embedded with this agent. These are the tools that would
    -- be found in the agent's 'toolDirectory'.
    }
    deriving (Show, Eq, Generic)

-------------------------------------------------------------------------------
-- Tool Export
-------------------------------------------------------------------------------

-- | Mode for storing tool content in an export package.
data ToolContentMode
    = -- | Tool content is embedded directly as a base64-encoded ByteString.
      -- This creates a self-contained package but results in larger files.
      EmbeddedContent !ByteString
    | -- | Tool is referenced by a path relative to the package.
      -- This is used for git-based repositories where tools are stored
      -- as separate files.
      ReferencedContent !FilePath
    deriving (Show, Eq, Generic)

-- | An exported tool with its metadata and content.
data ToolExport = ToolExport
    { toolName :: !Text
    -- ^ The name of the tool (typically the filename without extension)
    , toolContent :: !ToolContentMode
    -- ^ How the tool content is stored (embedded or referenced)
    , toolPermissions :: !CMode
    -- ^ POSIX file permissions for the tool (e.g., executable permissions)
    , toolNamespace :: !(Maybe Text)
    -- ^ Optional namespace for organizing tools in git repos
    , toolMetadata :: !(Maybe Text)
    -- ^ Optional additional metadata (e.g., git commit hash, description)
    }
    deriving (Show, Eq, Generic)

-------------------------------------------------------------------------------
-- MCP Server Export
-------------------------------------------------------------------------------

-- | An exported MCP server configuration with optional namespace.
data McpServerExport = McpServerExport
    { mcpConfig :: !McpServerDescription
    -- ^ The MCP server configuration (from 'System.Agents.Base')
    , mcpNamespace :: !(Maybe Text)
    -- ^ Optional namespace for organizing servers in git repos
    }
    deriving (Show, Eq, Generic)

