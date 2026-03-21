{-# LANGUAGE OverloadedRecordDot #-}

module System.Agents.Tools.Base where

-------------------------------------------------------------------------------
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LByteString
import Data.Text (Text)
import qualified Data.Text as Text
import Prod.Tracer (Tracer)

import qualified System.Agents.MCP.Base as Mcp
import qualified System.Agents.Tools.Bash as BashTools
import System.Agents.Tools.Context (ToolExecutionContext)
import qualified System.Agents.Tools.DeveloperToolbox as DeveloperTools
import qualified System.Agents.Tools.IO as IOTools
import qualified System.Agents.Tools.McpToolbox as McpTools
import System.Agents.Tools.OpenAPI.Types (ToolResult)
import qualified System.Agents.Tools.PostgREST.Types as PostgRESTypes
import qualified System.Agents.Tools.Skills.Types as SkillTypes
import qualified System.Agents.Tools.SqliteToolbox as SqliteTools
import qualified System.Agents.Tools.SystemToolbox as SystemTools
import System.Agents.Tools.Trace (ToolTrace)

-------------------------------------------------------------------------------

{- | Definition of a tool, parameterized by the call type.

The 'toolRun' function receives a 'ToolExecutionContext' instead of a generic
runtime value. This provides tools with access to session metadata (session ID,
conversation ID, turn ID, etc.) without exposing these details to the LLM.
-}
data Tool call
    = Tool
    { toolDef :: ToolDef
    , toolRun :: Tracer IO ToolTrace -> ToolExecutionContext -> Aeson.Value -> IO (CallResult call)
    }

{- | Tool definition without the execution function.
Used for identifying and describing the tool.
-}
data ToolDef
    = BashTool !BashTools.ScriptDescription
    | MCPTool !McpTools.ToolDescription
    | IOTool !IOTools.IOScriptDescription
    | -- | Toolbox name and operation ID
      OpenAPITool !Text !Text
    | -- | Toolbox name and table path
      PostgRESTool !Text !Text
    | -- | SQLite tool description
      SqliteTool !SqliteTools.ToolDescription
    | -- | System tool description
      SystemTool !SystemTools.ToolDescription
    | -- | Developer tool description
      DeveloperTool !DeveloperTools.ToolDescription
    | -- | Lua tool: toolbox name (Lua scripts are anonymous)
      LuaTool !Text
    | -- | Skill tool: skill name and action (describe, enable, disable)
      SkillTool !SkillTypes.SkillName !Text
    | -- | Skill script tool: skill name and script name
      SkillScriptTool !SkillTypes.SkillName !SkillTypes.ScriptName
    | -- | Skill list tool
      SkillListTool
    deriving (Show)

-------------------------------------------------------------------------------

{- | Result of a tool call.

This type represents the various outcomes that can occur when executing
a tool through the agent system. Each constructor pairs the call context
with the specific result or error type.
-}
data CallResult call
    = -- | Successful execution returning raw bytes (e.g., bash/IO tool output)
      BlobToolSuccess call ByteString
    | -- | Tool was not found in the registered tools
      ToolNotFound call
    | -- | Bash script execution failed
      BashToolError call BashTools.RunScriptError
    | -- | IO tool execution failed
      IOToolError call IOTools.RunError
    | -- | MCP tool executed successfully with structured result
      McpToolResult call Mcp.CallToolResult
    | -- | MCP tool execution failed
      McpToolError call String
    | -- | OpenAPI tool executed successfully with structured result
      OpenAPIToolResult call ToolResult
    | -- | OpenAPI tool execution failed
      OpenAPIToolError call String
    | -- | PostgREST tool executed successfully with structured result
      PostgRESToolResult call PostgRESTypes.ToolResult
    | -- | PostgREST tool execution failed
      PostgRESToolError call String
    | -- | SQLite tool executed successfully with query results
      SqliteToolResult call SqliteTools.QueryResult
    | -- | SQLite tool execution failed
      SqliteToolError call SqliteTools.QueryError
    | -- | System tool executed successfully with query results
      SystemToolResult call SystemTools.QueryResult
    | -- | System tool execution failed
      SystemToolError call SystemTools.QueryError
    | -- | Developer tool validation result
      DeveloperToolResult call DeveloperTools.ValidationResult
    | -- | Developer tool scaffold result
      DeveloperToolScaffoldResult call DeveloperTools.ScaffoldResult
    | -- | Developer tool spec result
      DeveloperToolSpecResult call Text
    | -- | Developer tool execution failed
      DeveloperToolError call DeveloperTools.DeveloperToolError
    | -- | Lua tool executed successfully with result
      LuaToolResult call Aeson.Value -- NOTE: the Aeson.Value is an array of results
    | -- | Lua tool execution failed
      LuaToolError call Text
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
        (SqliteToolResult callCtx r) -> SqliteToolResult (f callCtx) r
        (SqliteToolError callCtx e) -> SqliteToolError (f callCtx) e
        (SystemToolResult v r) -> SystemToolResult (f v) r
        (SystemToolError v e) -> SystemToolError (f v) e
        (DeveloperToolResult v r) -> DeveloperToolResult (f v) r
        (DeveloperToolScaffoldResult v r) -> DeveloperToolScaffoldResult (f v) r
        (DeveloperToolSpecResult v r) -> DeveloperToolSpecResult (f v) r
        (DeveloperToolError v e) -> DeveloperToolError (f v) e
        (LuaToolResult v r) -> LuaToolResult (f v) r
        (LuaToolError v e) -> LuaToolError (f v) e

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
extractCall (SystemToolResult c _) = c
extractCall (SystemToolError c _) = c
extractCall (DeveloperToolResult c _) = c
extractCall (DeveloperToolScaffoldResult c _) = c
extractCall (DeveloperToolSpecResult c _) = c
extractCall (DeveloperToolError c _) = c
extractCall (LuaToolResult c _) = c
extractCall (LuaToolError c _) = c

-------------------------------------------------------------------------------
-- Byte Counting Helpers
-------------------------------------------------------------------------------

{- | Calculate the byte size of a tool response for tracking purposes.

This function converts various tool result types to their JSON representation
and returns the size in bytes. Used for byte usage tracking in sessions.

Note: For BlobToolSuccess, we count the raw ByteString length directly.
For other result types, we encode to JSON and count the bytes.
-}
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
    100 -- Approximate, actual size depends on error details
callResultByteSize (IOToolError _ _) =
    100 -- Approximate
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
callResultByteSize (SystemToolResult _ result) =
    fromIntegral (LByteString.length (Aeson.encode result))
callResultByteSize (SystemToolError _ err) =
    fromIntegral (LByteString.length (Aeson.encode (Aeson.String (Text.pack $ show err))))
callResultByteSize (DeveloperToolResult _ result) =
    fromIntegral (LByteString.length (Aeson.encode result))
callResultByteSize (DeveloperToolScaffoldResult _ result) =
    fromIntegral (LByteString.length (Aeson.encode result))
callResultByteSize (DeveloperToolSpecResult _ content) =
    fromIntegral (LByteString.length (Aeson.encode (Aeson.String content)))
callResultByteSize (DeveloperToolError _ err) =
    fromIntegral (LByteString.length (Aeson.encode (Aeson.String (Text.pack $ show err))))
callResultByteSize (LuaToolResult _ result) =
    fromIntegral (LByteString.length (Aeson.encode result))
callResultByteSize (LuaToolError _ err) =
    fromIntegral (LByteString.length (Aeson.encode (Aeson.String err)))

{- | Calculate total bytes for a list of tool responses.

Helper function for calculating tool byte usage when processing
multiple tool results in a single step.
-}
sumToolResponseBytes :: [CallResult call] -> Int
sumToolResponseBytes = sum . map callResultByteSize
