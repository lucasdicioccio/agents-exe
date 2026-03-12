{-# LANGUAGE OverloadedRecordDot #-}

module System.Agents.Tools.Base where

-------------------------------------------------------------------------------
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LByteString
import Data.Text (Text)
import qualified Data.Text as Text
import Prod.Tracer (Tracer)

import qualified System.Agents.MCP.Base as Mcp
import qualified System.Agents.Tools.Bash as BashTools
import qualified System.Agents.Tools.IO as IOTools
import System.Agents.Tools.OpenAPI.Types (ToolResult)
import qualified System.Agents.Tools.McpToolbox as McpTools
import qualified System.Agents.Tools.PostgREST.Types as PostgRESTypes
import qualified System.Agents.Tools.SqliteToolbox as SqliteTools
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
    | PostgRESTool !Text !Text  -- ^ Toolbox name and table path
    | SqliteTool !SqliteTools.ToolDescription  -- ^ SQLite tool description
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
    | PostgRESToolResult call PostgRESTypes.ToolResult
    -- ^ PostgREST tool executed successfully with structured result
    | PostgRESToolError call String
    -- ^ PostgREST tool execution failed
    | SqliteToolResult call SqliteTools.QueryResult
    -- ^ SQLite tool executed successfully with query results
    | SqliteToolError call SqliteTools.QueryError
    -- ^ SQLite tool execution failed
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
        (PostgRESToolResult v r) -> PostgRESToolResult (f v) r
        (PostgRESToolError v e) -> PostgRESToolError (f v) e
        (SqliteToolResult c r) -> SqliteToolResult (f c) r
        (SqliteToolError c e) -> SqliteToolError (f c) e

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
extractCall (PostgRESToolResult c _) = c
extractCall (PostgRESToolError c _) = c
extractCall (SqliteToolResult c _) = c
extractCall (SqliteToolError c _) = c

-------------------------------------------------------------------------------
-- Byte Counting Helpers
-------------------------------------------------------------------------------

-- | Calculate the byte size of a tool response for tracking purposes.
--
-- This function converts various tool result types to their JSON representation
-- and returns the size in bytes. Used for byte usage tracking in sessions.
--
-- Note: For BlobToolSuccess, we count the raw ByteString length directly.
-- For other result types, we encode to JSON and count the bytes.
callResultByteSize :: CallResult call -> Int
callResultByteSize (BlobToolSuccess _ bs) =
    -- Raw bytes - count the ByteString length directly
    BS.length bs
callResultByteSize (ToolNotFound _) =
    -- Fixed small size for not-found errors
    20
callResultByteSize (BashToolError _ _) =
    -- Encode error details as JSON and count
    -- The actual error will be serialized when stored
    100  -- Approximate, actual size depends on error details
callResultByteSize (IOToolError _ _) =
    100  -- Approximate
callResultByteSize (McpToolResult _ result) =
    fromIntegral (LByteString.length (Aeson.encode result))
callResultByteSize (McpToolError _ err) =
    fromIntegral (LByteString.length (Aeson.encode (Aeson.String (Text.pack err))))
callResultByteSize (OpenAPIToolResult _ result) =
    fromIntegral (LByteString.length (Aeson.encode result))
callResultByteSize (OpenAPIToolError _ err) =
    fromIntegral (LByteString.length (Aeson.encode (Aeson.String (Text.pack err))))
callResultByteSize (PostgRESToolResult _ result) =
    fromIntegral (LByteString.length (Aeson.encode result))
callResultByteSize (PostgRESToolError _ err) =
    fromIntegral (LByteString.length (Aeson.encode (Aeson.String (Text.pack err))))
callResultByteSize (SqliteToolResult _ result) =
    fromIntegral (LByteString.length (Aeson.encode result))
callResultByteSize (SqliteToolError _ err) =
    fromIntegral (LByteString.length (Aeson.encode (Aeson.String (Text.pack $ show err))))

-- | Calculate total bytes for a list of tool responses.
--
-- Helper function for calculating tool byte usage when processing
-- multiple tool results in a single step.
sumToolResponseBytes :: [CallResult call] -> Int
sumToolResponseBytes = sum . map callResultByteSize

