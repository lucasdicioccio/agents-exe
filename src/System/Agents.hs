{-# LANGUAGE OverloadedStrings #-}

{- | Main module for the agents system.

This module re-exports the core functionality needed to build and run agents
with various tool integrations including:

* LLM interactions (OpenAI)
* Tool definitions and registration
* Session management
* Runtime infrastructure
* OpenAPI tool integration
* Recursive agent calls (sub-agent sessions)

For OpenAPI tool integration, use 'System.Agents.Tools.OpenAPIToolbox':

@
import System.Agents
import qualified System.Agents.Tools.OpenAPIToolbox as OpenAPI

main :: IO ()
main = do
    let config = OpenAPI.Config
            { configUrl = "https://petstore.swagger.io/v2/swagger.json"
            , configBaseUrl = "https://petstore.swagger.io/v2"
            , configHeaders = mempty
            , configToken = Nothing
            }

    result <- OpenAPI.initializeToolbox tracer config
    case result of
        Left err -> print err
        Right toolbox -> do
            -- Register all tools from the OpenAPI spec
            regResult <- registerOpenAPITools toolbox
            case regResult of
                Left err -> putStrLn $ "Registration failed: " ++ err
                Right registrations -> do
                    -- Use with agent runtime...
                    pure ()
@

For recursive agent calls (agents calling other agents as tools),
use the 'SubAgentSessionConfig' and 'turnAgentRuntimeIntoIOToolWithCallbacks':

@
import System.Agents

-- Create a sub-agent configuration with callbacks
let subAgentConfig = defaultSubAgentConfig
        { subAgentOnProgress = Just myProgressCallback
        , subAgentStore = Just sessionStore
        }

-- Convert a runtime to a tool with callback support
let toolReg = turnAgentRuntimeIntoIOToolWithCallbacks
        subAgentConfig
        parentTracer
        subAgentRuntime
        parentSlug
        parentAgentId
@

See the documentation for 'System.Agents.AgentTree.OneShotTool' for more details
on configuring sub-agent callbacks and tracking.
-}
module System.Agents (
    -- * OpenAPI Toolbox
    module System.Agents.Tools.OpenAPIToolbox,

    -- * Tool registration
    module System.Agents.ToolRegistration,

    -- * Core tools
    module System.Agents.Tools,

    -- * Tool schema
    module System.Agents.ToolSchema,

    -- * Recursive agent calls (sub-agent sessions)
    -- | These types and functions enable agents to call other agents as tools,
    -- with support for parent-child session tracking, progress callbacks,
    -- and correlation tracing.
    --
    -- See "System.Agents.AgentTree.OneShotTool" for detailed documentation.
    SubAgentSessionConfig (..),
    defaultSubAgentConfig,
    turnAgentRuntimeIntoIOTool,
    turnAgentRuntimeIntoIOToolWithCallbacks,

    -- * Session construction helpers
    -- | Functions for creating and inspecting sessions with parent-child relationships.
    mkChildSession,
    isChildSession,
) where

import System.Agents.AgentTree.OneShotTool (
    SubAgentSessionConfig (..),
    defaultSubAgentConfig,
    turnAgentRuntimeIntoIOTool,
    turnAgentRuntimeIntoIOToolWithCallbacks,
 )
import System.Agents.Session.Base (isChildSession, mkChildSession)
import System.Agents.ToolRegistration
import System.Agents.ToolSchema
import System.Agents.Tools
import System.Agents.Tools.OpenAPIToolbox

