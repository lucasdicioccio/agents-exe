{-# LANGUAGE OverloadedRecordDot #-}

{- | Provides some abstraction of tools that agents can use.

To date, there are two ways to provide tools:
* bash tools (arbitrary code running in a separate process, and defined with an external command)
* IO tools (arbitrary code running in the same process, and defined with Haskell code)
* MCP tools (tools exposed via Model Context Protocol)
* OpenAPI tools (tools generated from OpenAPI specifications)
* PostgREST tools (tools generated from PostgREST database APIs)
* System tools (tools providing system information)
* Developer tools (tools for writing/validating agents and tools)

Technically, we could implement bash-tools as a sepecific implementation of
IO-tools however having a separate constructor enables to surface a bit more
information (when announcing a tool to an LLM, when logging etc.).
Thus, a merge may happen at some point, but not just yet.
-}
module System.Agents.Tools (
    -- * Re-exports from Base
    Tool (..),
    ToolDef (..),
    CallResult (..),
    mapToolResult,
    mapCallResult,
    extractCall,

    -- * Re-exports from Trace
    module System.Agents.Tools.Trace,

    -- * Tool builders
    ioTool,
    bashTool,
    mcpTool,
    openapiTool,
    postgrestTool,
    systemTool,
    developerTool,
) where

import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as Aeson
import Data.ByteString.Char8 as CByteString
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Prod.Tracer (contramap)

-------------------------------------------------------------------------------

import qualified System.Agents.MCP.Client as McpClient
import System.Agents.Tools.Base
import qualified System.Agents.Tools.Bash as BashTools
import qualified System.Agents.Tools.DeveloperToolbox as DeveloperTools
import qualified System.Agents.Tools.IO as IOTools
import qualified System.Agents.Tools.McpToolbox as McpTools
import qualified System.Agents.Tools.OpenAPI.Converter as OpenAPI
import qualified System.Agents.Tools.OpenAPIToolbox as OpenAPIToolbox
import qualified System.Agents.Tools.PostgREST.Converter as PostgREST
import qualified System.Agents.Tools.PostgRESToolbox as PostgRESToolbox
import qualified System.Agents.Tools.SystemToolbox as SystemTools
import System.Agents.Tools.Trace

-------------------------------------------------------------------------------

{- | Builder for a tool based on a Bash script-description.

When executed, the script will have access to session context via environment
variables (see 'System.Agents.Tools.Bash.runValue' for details):

* @AGENT_SESSION_ID@ - The current session UUID
* @AGENT_CONVERSATION_ID@ - The conversation UUID
* @AGENT_TURN_ID@ - The current turn UUID
* @AGENT_AGENT_ID@ - The agent UUID (if available)
* @AGENT_SESSION_JSON@ - Full session as JSON (if available)

Scripts can use these variables for logging, audit trails, or context-aware
processing without requiring explicit parameters from the LLM.
-}
bashTool ::
    BashTools.ScriptDescription ->
    Tool ()
bashTool script =
    Tool
        { toolDef = BashTool script
        , toolRun = run
        }
  where
    call = ()
    run tracer ctx v = do
        ret <- BashTools.runValue (contramap BashToolsTrace tracer) script (Just ctx) v
        case ret of
            Left err -> pure $ BashToolError call err
            Right rsp -> pure $ BlobToolSuccess call rsp

-------------------------------------------------------------------------------

{- | Builder for a tool based on an MCP toolbox tool.

MCP tools are exposed via the Model Context Protocol and provide
standardized tool definitions with structured schemas.
-}
mcpTool ::
    McpTools.Toolbox ->
    McpTools.ToolDescription ->
    Tool ()
mcpTool toolbox desc =
    Tool
        { toolDef = MCPTool desc
        , toolRun = run
        }
  where
    call = ()
    run _tracer _ctx (Aeson.Object v) = do
        ret <- McpTools.callTool toolbox desc (Just v)
        case ret of
            (Just (Right rsp)) -> pure $ extractContentsFromToolCall rsp
            err -> pure $ McpToolError call (mconcat ["calling error: ", show err])
    run _tracer _ctx _ = do
        pure $ McpToolError call ("can only call McpTools with Aeson.Object")
    extractContentsFromToolCall :: McpClient.CallToolResultRsp -> CallResult ()
    extractContentsFromToolCall rsp =
        McpToolResult call rsp.getCallToolResult

-------------------------------------------------------------------------------

{- | Builder for an OpenAPI-based tool.

This creates a tool from an OpenAPI operation that can be executed
against an API endpoint. The tool handles:

* Path parameter substitution (parameters prefixed with 'p_')
* Query parameter construction (parameters prefixed with 'p_')
* Request body serialization (parameter 'b')
* HTTP method execution
* Response parsing

Example usage:

@
let toolbox = ... -- initialized OpenAPI toolbox
let apiTool = head (OpenAPIToolbox.toolboxTools toolbox)
let tool = openapiTool toolbox apiTool
-- Use tool with agent runtime...
@

The tool name and description are derived from the OpenAPI operation,
and the execution uses the HTTP runtime from the toolbox.
-}
openapiTool ::
    OpenAPIToolbox.Toolbox ->
    OpenAPI.OpenAPITool ->
    Tool ()
openapiTool toolbox apiTool =
    let opId = fromMaybe (OpenAPI.toolName apiTool) (OpenAPIToolbox.getOperationId (OpenAPI.toolOperation apiTool))
     in Tool
            { toolDef = OpenAPITool (OpenAPIToolbox.toolboxName toolbox) opId
            , toolRun = run
            }
  where
    call = ()
    run _tracer _ctx args = do
        result <- OpenAPIToolbox.handleToolCall toolbox apiTool args
        case result of
            Left err -> do
                pure $ OpenAPIToolError call (Text.unpack err)
            Right (_textResult, toolResult) -> do
                pure $ OpenAPIToolResult call toolResult

-------------------------------------------------------------------------------

{- | Builder for a PostgREST-based tool.

This creates a tool from a PostgREST table endpoint that can be executed
against a PostgREST API. The tool handles:

* Structured query parameters (filters, subset, ranking)
* Column-based row filtering
* Pagination (limit/offset)
* Column selection (select parameter)
* Ordering (order parameter)

Example usage:

@
let toolbox = ... -- initialized PostgREST toolbox
let prTool = head (PostgRESToolbox.toolboxTools toolbox)
let tool = postgrestTool toolbox prTool
-- Use tool with agent runtime...
@

The tool name format is: postgrest_{toolbox}_{table}
-}
postgrestTool ::
    PostgRESToolbox.Toolbox ->
    PostgREST.PostgRESTool ->
    Tool ()
postgrestTool toolbox prTool =
    Tool
        { toolDef = PostgRESTool (PostgRESToolbox.toolboxName toolbox) (PostgREST.prtPath prTool)
        , toolRun = run
        }
  where
    call = ()
    run _tracer _ctx args = do
        result <- PostgRESToolbox.handleToolCall toolbox prTool args
        case result of
            Left err -> do
                pure $ PostgRESToolError call (Text.unpack err)
            Right (_textResult, toolResult) -> do
                pure $ PostgRESToolResult call toolResult

-------------------------------------------------------------------------------

{- | Builder for a SystemToolbox-based tool.

This creates a tool that provides system information based on configured capabilities.
The tool accepts a single parameter 'capability' specifying which information to retrieve.
-}
systemTool :: SystemTools.Toolbox -> Tool ()
systemTool box =
    Tool
        { toolDef = SystemTool toolDesc
        , toolRun = run
        }
  where
    call = ()
    toolDesc =
        SystemTools.ToolDescription
            { SystemTools.toolDescriptionName = "system_info"
            , SystemTools.toolDescriptionDescription = box.toolboxDescription
            , SystemTools.toolDescriptionToolboxName = box.toolboxName
            }
    run _tracer _ctx (Aeson.Object v) = do
        case KeyMap.lookup (AesonKey.fromText "capability") v of
            Just (Aeson.String cap) -> do
                result <- SystemTools.executeQuery box cap
                case result of
                    Left err -> pure $ SystemToolError call err
                    Right rsp -> pure $ SystemToolResult call rsp
            _ -> pure $ SystemToolError call (SystemTools.SystemInfoError "Missing 'capability' parameter or invalid type")
    run _tracer _ctx _ = do
        pure $ SystemToolError call (SystemTools.SystemInfoError "Arguments must be a JSON object")

-------------------------------------------------------------------------------

{- | Builder for a DeveloperToolbox-based tool.

This creates a tool that provides developer utilities for writing and validating
agents and tools. The tool accepts parameters based on the capability:

* validate-tool: { "tool_path": "/path/to/tool.sh" }
* scaffold-agent: { "template": "openai", "slug": "my-agent", "file_path": "my-agent.json", "force": false }
* scaffold-tool: { "language": "bash", "slug": "my-tool", "file_path": "my-tool.sh", "force": false }
* show-spec: { "spec_name": "bash-tools" }
-}
developerTool :: DeveloperTools.Toolbox -> Tool ()
developerTool box =
    Tool
        { toolDef = DeveloperTool toolDesc
        , toolRun = run
        }
  where
    call = ()
    toolDesc =
        DeveloperTools.ToolDescription
            { DeveloperTools.toolDescriptionName = "developer_tools"
            , DeveloperTools.toolDescriptionDescription = box.toolboxDescription
            , DeveloperTools.toolDescriptionToolboxName = box.toolboxName
            }
    run _tracer _ctx (Aeson.Object v) = do
        case KeyMap.lookup (AesonKey.fromText "capability") v of
            Just (Aeson.String cap) -> executeDeveloperCapability box cap v
            _ -> pure $ DeveloperToolError call (DeveloperTools.ValidationError "Missing 'capability' parameter or invalid type")
    run _tracer _ctx _ = do
        pure $ DeveloperToolError call (DeveloperTools.ValidationError "Arguments must be a JSON object")

-- | Execute a developer tool capability
executeDeveloperCapability :: DeveloperTools.Toolbox -> Text.Text -> Aeson.Object -> IO (CallResult ())
executeDeveloperCapability box cap params = case cap of
    "validate-tool" -> do
        case KeyMap.lookup (AesonKey.fromText "tool_path") params of
            Just (Aeson.String toolPath) -> do
                result <- DeveloperTools.executeValidateTool box (Text.unpack toolPath)
                case result of
                    Left err -> pure $ DeveloperToolError () err
                    Right valResult -> pure $ DeveloperToolResult () valResult
            _ -> pure $ DeveloperToolError () (DeveloperTools.ValidationError "Missing 'tool_path' parameter")
    "scaffold-agent" -> do
        let mTemplate = case KeyMap.lookup (AesonKey.fromText "template") params of
                Just (Aeson.String t) -> t
                _ -> "openai"
        let mSlug = case KeyMap.lookup (AesonKey.fromText "slug") params of
                Just (Aeson.String s) -> s
                _ -> "new-agent"
        let mFilePath = case KeyMap.lookup (AesonKey.fromText "file_path") params of
                Just (Aeson.String fp) -> Text.unpack fp
                _ -> "new-agent.json"
        let mForce = case KeyMap.lookup (AesonKey.fromText "force") params of
                Just (Aeson.Bool f) -> f
                _ -> False
        result <- DeveloperTools.executeScaffoldAgent box mTemplate mSlug mFilePath mForce
        case result of
            Left err -> pure $ DeveloperToolError () err
            Right scaffoldResult -> pure $ DeveloperToolScaffoldResult () scaffoldResult
    "scaffold-tool" -> do
        let mLang = case KeyMap.lookup (AesonKey.fromText "language") params of
                Just (Aeson.String l) -> l
                _ -> "bash"
        let mSlug = case KeyMap.lookup (AesonKey.fromText "slug") params of
                Just (Aeson.String s) -> s
                _ -> "new-tool"
        let mFilePath = case KeyMap.lookup (AesonKey.fromText "file_path") params of
                Just (Aeson.String fp) -> Text.unpack fp
                _ -> "new-tool.sh"
        let mForce = case KeyMap.lookup (AesonKey.fromText "force") params of
                Just (Aeson.Bool f) -> f
                _ -> False
        result <- DeveloperTools.executeScaffoldTool box mLang mSlug mFilePath mForce
        case result of
            Left err -> pure $ DeveloperToolError () err
            Right scaffoldResult -> pure $ DeveloperToolScaffoldResult () scaffoldResult
    "show-spec" -> do
        case KeyMap.lookup (AesonKey.fromText "spec_name") params of
            Just (Aeson.String specName) -> do
                result <- DeveloperTools.executeShowSpec box specName
                case result of
                    Left err -> pure $ DeveloperToolError () err
                    Right content -> pure $ DeveloperToolSpecResult () content
            _ -> pure $ DeveloperToolError () (DeveloperTools.ValidationError "Missing 'spec_name' parameter")
    _ -> pure $ DeveloperToolError () (DeveloperTools.ValidationError $ "Unknown capability: " <> cap)

-------------------------------------------------------------------------------

{- | Builder for a tool based on an IO-tool script-description.

The IO action receives the full 'ToolExecutionContext', giving it direct access
to session metadata including:

* 'ctxSessionId' - The current session identifier
* 'ctxConversationId' - The conversation identifier
* 'ctxTurnId' - The current turn identifier
* 'ctxAgentId' - Optional agent identifier
* 'ctxFullSession' - Optional complete session data

This is a breaking change from the previous API where IO scripts received a
'ConversationId'. To migrate existing tools:

@
-- Before:
ioRun :: ConversationId -> MyArg -> IO ByteString
ioRun _convId arg = ...

-- After:
ioRun :: ToolExecutionContext -> MyArg -> IO ByteString
ioRun ctx arg = ...
-- Use ctx when needed, or ignore with _ctx if not needed
@

Example tool using context:

@
logTool :: IOScript LogArg ByteString
logTool = IOScript
    { description = IOScriptDescription "log-tool" "Logs with session context"
    , ioRun = \ctx arg -> do
        let sessionId = ctxSessionId ctx
        logWithSession sessionId arg.message
        pure "logged"
    }
@
-}
ioTool ::
    (Aeson.FromJSON llmArg) =>
    IOTools.IOScript llmArg ByteString ->
    Tool ()
ioTool script =
    Tool
        { toolDef = IOTool script.description
        , toolRun = run
        }
  where
    call = ()
    run tracer ctx v = do
        -- we trace the original input object
        let adaptTrace = IOToolsTrace . IOTools.adaptTraceInput (const v)
        ret <- IOTools.runValue (contramap adaptTrace tracer) script ctx v
        case ret of
            Left err -> pure $ IOToolError call err
            Right rsp -> pure $ BlobToolSuccess call rsp
