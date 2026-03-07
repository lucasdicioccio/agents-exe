{-# LANGUAGE OverloadedRecordDot #-}

module System.Agents.Tools.Base where

-------------------------------------------------------------------------------
import Data.Aeson (Value)
import qualified Data.Aeson.Types as Aeson
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import Prod.Tracer (Tracer)

import qualified System.Agents.MCP.Base as Mcp
import qualified System.Agents.Tools.Bash as BashTools
import qualified System.Agents.Tools.IO as IOTools
import System.Agents.Tools.OpenAPI.Types (ToolResult)
import qualified System.Agents.Tools.McpToolbox as McpTools
import System.Agents.Tools.Context (ToolExecutionContext)
import System.Agents.Tools.Trace (ToolTrace)

-------------------------------------------------------------------------------
-- | Definition of a tool, parameterized by the call type.
--
-- The 'toolRun' function receives a 'ToolExecutionContext' instead of a generic
-- runtime value. This provides tools with access to session metadata (session ID,
-- conversation ID, turn ID, etc.) without exposing these details to the LLM.
data Tool call
    = Tool
    { toolDef :: ToolDef
    , toolRun :: Tracer IO ToolTrace -> ToolExecutionContext -> Aeson.Value -> IO (CallResult call)
    }

-- | Tool definition without the execution function.
-- Used for identifying and describing the tool.
data ToolDef
    = BashTool !BashTools.ScriptDescription
    | MCPTool !McpTools.ToolDescription
    | IOTool !IOTools.IOScriptDescription
    | OpenAPITool !Text !Text  -- ^ Toolbox name and operation ID
    deriving (Show)

-------------------------------------------------------------------------------
-- | Result of a tool call.
--
-- This type represents the various outcomes that can occur when executing
-- a tool through the agent system. Each constructor pairs the call context
-- with the specific result or error type.
data CallResult call
    = BlobToolSuccess call ByteString
    -- ^ Successful execution returning raw bytes (e.g., bash/IO tool output)
    | ToolNotFound call
    -- ^ Tool was not found in the registered tools
    | BashToolError call BashTools.RunScriptError
    -- ^ Bash script execution failed
    | IOToolError call IOTools.RunError
    -- ^ IO tool execution failed
    | McpToolResult call Mcp.CallToolResult
    -- ^ MCP tool executed successfully with structured result
    | McpToolError call String
    -- ^ MCP tool execution failed
    | OpenAPIToolResult call ToolResult
    -- ^ OpenAPI tool executed successfully with structured result
    | OpenAPIToolError call String
    -- ^ OpenAPI tool execution failed
    deriving (Show)

-------------------------------------------------------------------------------
-- | Explicit helper to map on the result of a CallResult.
mapCallResult :: (a -> b) -> CallResult a -> CallResult b
mapCallResult f c =
    case c of
        (ToolNotFound v) -> ToolNotFound (f v)
        (BashToolError v e) -> BashToolError (f v) e
        (IOToolError v e) -> IOToolError (f v) e
        (BlobToolSuccess v b) -> BlobToolSuccess (f v) b
        (McpToolResult v b) -> McpToolResult (f v) b
        (McpToolError v b) -> McpToolError (f v) b
        (OpenAPIToolResult v r) -> OpenAPIToolResult (f v) r
        (OpenAPIToolError v e) -> OpenAPIToolError (f v) e

-- | Explicit helper to map on the results a Tool makes.
mapToolResult :: (a -> b) -> Tool a -> Tool b
mapToolResult f (Tool d run) =
    Tool d (\tracer ctx v -> fmap (mapCallResult f) (run tracer ctx v))

-------------------------------------------------------------------------------
{- | Extracts the call definition out of a CallResut.
Note that a result is always bound to a CallResut, thus this function is total.
-}
extractCall :: CallResult call -> call
extractCall (ToolNotFound c) = c
extractCall (BashToolError c _) = c
extractCall (IOToolError c _) = c
extractCall (BlobToolSuccess c _) = c
extractCall (McpToolResult c _) = c
extractCall (McpToolError c _) = c
extractCall (OpenAPIToolResult c _) = c
extractCall (OpenAPIToolError c _) = c

