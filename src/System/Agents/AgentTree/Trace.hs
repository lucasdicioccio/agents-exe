{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- | OS-native agent tree management.

This module provides agent discovery, configuration loading, and tree
building using the OS-native ECS architecture. It replaces the legacy
Runtime-per-agent model with pure OS-native operations.
-}
module System.Agents.AgentTree.Trace (
    -- * Agent tree types
    AgentConfigTree (..),
    agentRootDir,

    -- * Configuration graph types
    ReferenceValidationTrace (..),

    -- * Tracing
    TreeTrace (..),

    -- * Re-export Agent type for field access (without constructor to avoid name shadowing)
    Agent,
    -- Re-export field accessors individually (avoiding those that conflict with ApiKey)
    slug,
    announce,
    systemPrompt,
    flavor,
    modelUrl,
    modelName,
    -- apiKeyId is excluded due to conflict with ApiKey.apiKeyId - use .apiKeyId instead
    toolDirectory,
    bashToolboxes,
    mcpServers,
    openApiToolboxes,
    postgrestToolboxes,
    builtinToolboxes,
    extraAgents,
    skillSources,
    autoEnableSkills,
) where

import qualified Data.Set as Set
import qualified System.FilePath as FilePath

import System.Agents.Base (
    Agent,
    AgentSlug,
    McpServerDescription,
    OpenAPIToolboxDescription (..),
    PostgRESTToolboxDescription (..),
    -- Export field accessors (except apiKeyId which conflicts)

    announce,
    autoEnableSkills,
    bashToolboxes,
    builtinToolboxes,
    extraAgents,
    flavor,
    mcpServers,
    modelName,
    modelUrl,
    openApiToolboxes,
    postgrestToolboxes,
    skillSources,
    slug,
    systemPrompt,
    toolDirectory,
 )
import qualified System.Agents.FileLoader as FileLoader

-- OS Core imports

import qualified System.Agents.AgentTree.ToolLoader as ToolLoader
import qualified System.Agents.Runtime.Trace as Runtime
import qualified System.Agents.Tools.BashToolbox as BashToolbox
import qualified System.Agents.Tools.DeveloperToolbox as DeveloperToolbox
import qualified System.Agents.Tools.LuaToolbox as LuaToolbox
import qualified System.Agents.Tools.McpToolbox as McpToolbox
import qualified System.Agents.Tools.McpToolbox as McpTools
import qualified System.Agents.Tools.OpenAPIToolbox as OpenAPIToolbox
import qualified System.Agents.Tools.OpenAPIToolbox as OpenApiToolbox
import qualified System.Agents.Tools.PostgRESToolbox as PostgREST
import qualified System.Agents.Tools.PostgRESToolbox as PostgRESToolbox
import qualified System.Agents.Tools.SqliteToolbox as SqliteToolbox
import qualified System.Agents.Tools.SystemToolbox as SystemToolbox

-------------------------------------------------------------------------------
-- Trace Types
-------------------------------------------------------------------------------

-- | Trace events for agent tree operations.
data TreeTrace
    = McpTrace McpServerDescription McpTools.Trace
    | OpenAPITrace OpenAPIToolboxDescription OpenAPIToolbox.Trace
    | PostgRESTrace PostgRESTToolboxDescription PostgREST.Trace
    | DataLoadingTrace FileLoader.Trace
    | ConfigLoadedTrace AgentConfigTree
    | CyclicReferencesWarning [[AgentSlug]]
    | ReferenceValidationTrace ReferenceValidationTrace
    | RuntimeTrace Runtime.Trace
    | BashToolboxTrace BashToolbox.Trace
    | McpToolboxTrace McpToolbox.Trace
    | OpenApiToolboxTrace OpenApiToolbox.Trace
    | PostgRESToolboxTrace PostgRESToolbox.Trace
    | SqliteToolboxTrace SqliteToolbox.Trace
    | SystemToolboxTrace SystemToolbox.Trace
    | DeveloperToolboxTrace DeveloperToolbox.Trace
    | LuaToolboxTrace LuaToolbox.Trace
    | ToolLoaderTrace ToolLoader.Trace
    deriving (Show)

-- | Configuration tree from file loading (intermediate structure)
data AgentConfigTree = AgentConfigTree
    { agentConfigFile :: FilePath
    , agentConfig :: Agent
    , agentConfigChildren :: [AgentConfigTree]
    }
    deriving (Show)

agentRootDir :: AgentConfigTree -> FilePath
agentRootDir agent = FilePath.takeDirectory agent.agentConfigFile

-- | Trace events for reference validation phase
data ReferenceValidationTrace
    = ValidationStarted (Set.Set AgentSlug)
    | ValidationComplete
    | DuplicateSlugDetected AgentSlug [FilePath]
    | SelfReferenceDetected AgentSlug FilePath AgentSlug
    deriving (Show)
