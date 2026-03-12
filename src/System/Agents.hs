{-# LANGUAGE OverloadedStrings #-}

{- | Main module for the agents system.

This module re-exports the core functionality needed to build and run agents
with various tool integrations including:

* LLM interactions (OpenAI)
* Tool definitions and registration
* Session management
* Runtime infrastructure
* OpenAPI tool integration

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
-}
module System.Agents (
    -- * OpenAPI Toolbox (main addition for this release)
    module System.Agents.Tools.OpenAPIToolbox,

    -- * Tool registration
    module System.Agents.ToolRegistration,

    -- * Core tools
    module System.Agents.Tools,

    -- * Tool schema
    module System.Agents.ToolSchema,
) where

import System.Agents.ToolRegistration
import System.Agents.ToolSchema
import System.Agents.Tools
import System.Agents.Tools.OpenAPIToolbox
