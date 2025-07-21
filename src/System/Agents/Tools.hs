{-# LANGUAGE OverloadedRecordDot #-}

{- | Provides some abstraction of tools that agents can use.

To date, there are two ways to provide tools:
* bash tools (arbitrary code running in a separate process, and defined with an external command)
* IO tools (arbitrary code running in the same process, and defined with Haskell code)

Technically, we could implement bash-tools as a sepecific implementation of
IO-tools however having a separate constructor enables to surface a bit more
information (when announcing a tool to an LLM, when logging etc.).
Thus, a merge may happen at some point, but not just yet.
-}
module System.Agents.Tools (
    module System.Agents.Tools.Trace,
    Tool (..),
    ToolDef (..),
    ioTool,
    bashTool,
    mcpTool,
    mapToolResult,
    mapCallResult,
    extractCall,
) where

import qualified Data.Aeson.Types as Aeson
import Data.ByteString.Char8 as CByteString
import Prod.Tracer (Tracer, contramap)

-------------------------------------------------------------------------------

import qualified System.Agents.MCP.Client as McpClient
import System.Agents.Tools.Base
import qualified System.Agents.Tools.Bash as BashTools
import qualified System.Agents.Tools.IO as IOTools
import qualified System.Agents.Tools.McpToolbox as McpTools
import System.Agents.Tools.Trace

-------------------------------------------------------------------------------

-- | Captures tools that are defined in a various ways but have a similar "run" interface.
data Tool rtVal call
    = Tool
    { toolDef :: ToolDef
    , toolRun :: Tracer IO ToolTrace -> rtVal -> Aeson.Value -> IO (CallResult call)
    }

data ToolDef
    = BashTool !BashTools.ScriptDescription
    | MCPTool !McpTools.ToolDescription
    | IOTool !IOTools.IOScriptDescription
    deriving (Show)

-------------------------------------------------------------------------------

-- | Builder for a tool based on a Bash script-description.
bashTool ::
    BashTools.ScriptDescription ->
    Tool a ()
bashTool script =
    Tool
        { toolDef = BashTool script
        , toolRun = run
        }
  where
    call = ()
    run tracer _ v = do
        ret <- BashTools.runValue (contramap BashToolsTrace tracer) script v
        case ret of
            Left err -> pure $ BashToolError call err
            Right rsp -> pure $ BlobToolSuccess call rsp

-------------------------------------------------------------------------------
mcpTool ::
    McpTools.Toolbox ->
    McpTools.ToolDescription ->
    Tool a ()
mcpTool toolbox desc =
    Tool
        { toolDef = MCPTool desc
        , toolRun = run
        }
  where
    call = ()
    run _ _ (Aeson.Object v) = do
        ret <- McpTools.callTool toolbox desc (Just v)
        case ret of
            (Just (Right rsp)) -> pure $ extractContentsFromToolCall rsp
            err -> pure $ McpToolError call (mconcat ["calling error: ", show err])
    run _ _ _ = do
        pure $ McpToolError call ("can only call McpTools with Aeson.Object")
    extractContentsFromToolCall :: McpClient.CallToolResultRsp -> CallResult ()
    extractContentsFromToolCall rsp =
        McpToolResult call rsp.getCallToolResult

-------------------------------------------------------------------------------

-- | Builder for a tool based on an IO-tool script-description.
ioTool ::
    (Aeson.FromJSON llmArg) =>
    IOTools.IOScript rtVal llmArg ByteString ->
    Tool rtVal ()
ioTool script =
    Tool
        { toolDef = IOTool script.description
        , toolRun = run
        }
  where
    call = ()
    run tracer runtimeValue v = do
        -- we trace the original input object
        let adaptTrace = IOToolsTrace . IOTools.adaptTraceInput (const v)
        ret <- IOTools.runValue (contramap adaptTrace tracer) script runtimeValue v
        case ret of
            Left err -> pure $ IOToolError call err
            Right rsp -> pure $ BlobToolSuccess call rsp

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

-- | Explicit helper to map on the results a Tool makes.
mapToolResult :: (a -> b) -> Tool x a -> Tool x b
mapToolResult f (Tool d run) =
    Tool d (\tracer rtval v -> fmap (mapCallResult f) (run tracer rtval v))
