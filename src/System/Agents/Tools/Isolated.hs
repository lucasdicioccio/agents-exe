{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Running bash and MCP tool calls outside the current process, for every
agent a host builds.

This is a property of the host, not of an agent's configuration: an agent's
own @toolCallPolicyConfig@ still decides whether a call is deferred, cached,
or run asynchronously, but once a covered call is executed it goes to the
'DeploymentRunner' ("System.Agents.Session.Isolation"), whatever that policy
says. There is no in-process fallback.

What it does not do:

* It covers 'BashTool' and 'MCPTool' only. Every other tool kind (skill
  scripts, Lua, SQLite, OpenAPI, PostgREST, system and developer tools,
  agents called as tools) still runs in-process.
* MCP servers are still started by the host when it loads an agent, to list
  their tools. Only the calls go to the runner.
* The worker that executes a call is the runner's: the envelope names the
  tool and carries its arguments, nothing more. Secret parameter values are
  not part of it (see 'contextSnapshot').
-}
module System.Agents.Tools.Isolated (
    ToolIsolation (..),
    toolIsolationFromSpec,
    isolatesToolDef,
    isolateToolCalls,
    isolatePortal,
) where

import qualified Data.Aeson as Aeson
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text

import System.Agents.Session.Base (
    DeploymentRunner (..),
    IsolationError (..),
    IsolationSpec (..),
    LlmToolCall,
    ToolCallDisposition (..),
    UserToolResponse (..),
    dockerRunner,
    localProcessRunner,
    mkIsolationEnvelope,
    newContinuationToken,
 )
import qualified System.Agents.Session.Compat as SessionCompat
import System.Agents.ToolRegistration (ToolRegistration (..))
import System.Agents.Tools.Base (Tool (..), ToolDef (..))
import System.Agents.Tools.Context (
    ToolCall (..),
    ToolExecutionContext,
    ToolPortal,
    ToolResult (..),
    contextSnapshot,
 )

{- | Where covered tool calls run: the specification written into each
call's envelope, and the runner that executes it.
-}
data ToolIsolation = ToolIsolation
    { tiSpec :: IsolationSpec
    , tiRunner :: DeploymentRunner
    }

{- | The runner for a specification. 'FunctionRunner' has no implementation
and is refused, rather than accepted and failing every call.
-}
toolIsolationFromSpec :: IsolationSpec -> Either Text ToolIsolation
toolIsolationFromSpec spec = case spec of
    Docker image
        | Text.null image -> Left "empty docker image"
        | otherwise -> Right $ ToolIsolation spec (dockerRunner image)
    LocalProcess path
        | null path -> Left "empty worker path"
        | otherwise -> Right $ ToolIsolation spec (localProcessRunner path)
    FunctionRunner _ -> Left "function runners are not implemented"

-- | The tool kinds a 'ToolIsolation' covers: bash scripts and MCP tools.
isolatesToolDef :: ToolDef -> Bool
isolatesToolDef def = case def of
    BashTool _ -> True
    MCPTool _ -> True
    _ -> False

{- | Whether the registered tool a call resolves to is a covered one. The
call is resolved the way the in-process path resolves it (first
registration that claims it), so the tool's kind decides, not its name.
-}
resolvesToIsolatedTool :: [ToolRegistration] -> ToolCall -> Bool
resolvesToIsolatedTool regs call =
    case Maybe.mapMaybe (\r -> r.findTool call) regs of
        (tool : _) -> isolatesToolDef tool.toolDef
        [] -> False

{- | Send covered calls to the runner instead of running them in-process.

A runner that fails (no Docker, a missing image, a worker that answers
nothing usable) fails the call: the error becomes the tool's result.
-}
isolateToolCalls ::
    Maybe ToolIsolation ->
    IO [ToolRegistration] ->
    (ToolExecutionContext -> LlmToolCall -> IO UserToolResponse) ->
    ToolExecutionContext ->
    LlmToolCall ->
    IO UserToolResponse
isolateToolCalls Nothing _ inProcess ctx call = inProcess ctx call
isolateToolCalls (Just isolation) getTools inProcess ctx call =
    case SessionCompat.parseToolCallFromLlmToolCall call of
        -- Not a call the in-process path can resolve to a tool either.
        Nothing -> inProcess ctx call
        Just parsed -> do
            regs <- getTools
            if resolvesToIsolatedTool regs parsed
                then do
                    token <- newContinuationToken
                    let envelope = mkIsolationEnvelope token call (contextSnapshot ctx) (RunIsolated isolation.tiSpec) Nothing
                    result <- isolation.tiRunner.drExecute envelope
                    pure $ case result of
                        Right response -> response
                        Left (IsolationError err) -> TextResponse ("isolation error: " <> err)
                else inProcess ctx call

{- | Refuse covered calls made through the tool portal (how a Lua script
calls other tools), which runs tools in-process.
-}
isolatePortal :: Maybe ToolIsolation -> IO [ToolRegistration] -> ToolPortal -> ToolPortal
isolatePortal Nothing _ portal = portal
isolatePortal (Just _) getTools portal = \mCtx call -> do
    regs <- getTools
    if resolvesToIsolatedTool regs call
        then
            pure $
                ToolResult
                    { resultData =
                        Aeson.object
                            [ "error" Aeson..= ("this tool only runs isolated on this host and cannot be called from another tool" :: Text)
                            , "toolName" Aeson..= call.callToolName
                            ]
                    , resultDuration = 0
                    , resultTraceId = "isolation-refused"
                    }
        else portal mCtx call
