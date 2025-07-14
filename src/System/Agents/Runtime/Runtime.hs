{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Agents.Runtime.Runtime (
    Runtime (..),
    addTracer,
    newRuntime,
    triggerRefreshTools,
) where

import Control.Concurrent.STM (STM, atomically, readTVar)
import Data.Either (rights)
import qualified Data.Text.Encoding as Text
import qualified Prod.Background as Background
import Prod.Tracer (Tracer, contramap, traceBoth)
import qualified System.Agents.HttpClient as HttpClient

import System.Agents.Base (AgentAnnounce, AgentId, AgentSlug, newAgentId)
import qualified System.Agents.LLMs.OpenAI as LLM
import System.Agents.ToolRegistration
import qualified System.Agents.Tools.BashToolbox as BashToolbox
import qualified System.Agents.Tools.McpToolbox as McpTools

import System.Agents.Runtime.Trace

-------------------------------------------------------------------------------

data Runtime
    = Runtime
    { agentSlug :: AgentSlug
    , agentId :: AgentId
    , agentAnnounce :: AgentAnnounce
    , agentTracer :: Tracer IO Trace
    , agentAuthenticatedHttpClientRuntime :: HttpClient.Runtime
    , agentModel :: LLM.Model
    , agentTools :: IO [ToolRegistration]
    , agentTriggerRefreshTools :: STM Bool
    }

{- | Adds an extra tracer to the runtime, hence returning a modified Runtime.

In current implementation (arbitrary) the extra tracer is ran before the
already-in-place one.
-}
addTracer :: Runtime -> (Tracer IO Trace) -> Runtime
addTracer rt t = rt{agentTracer = traceBoth t rt.agentTracer}

triggerRefreshTools :: Runtime -> IO Bool
triggerRefreshTools rt = atomically $ rt.agentTriggerRefreshTools

-- todo: directly ask for effects returning registrations
type ToolboxDirectory = FilePath
type IOToolBuilder = AgentSlug -> AgentId -> ToolRegistration
type McpToolConfig = McpTools.Toolbox

newRuntime ::
    AgentSlug ->
    AgentAnnounce ->
    Tracer IO Trace ->
    LLM.ApiKey ->
    LLM.Model ->
    ToolboxDirectory ->
    [IOToolBuilder] ->
    [McpToolConfig] ->
    IO (Either String Runtime)
newRuntime slug announce tracer apiKey model tooldir mkIoTools mcpToolboxes = do
    uid <- newAgentId
    let ioTools = [mk slug uid | mk <- mkIoTools]
    toolz <- BashToolbox.initializeBackroundToolbox (contramap (AgentTrace_Loading slug uid) tracer) tooldir
    case toolz of
        Left err -> pure $ Left (show err)
        Right toolbox -> do
            let auth = HttpClient.BearerToken $ Text.decodeUtf8 $ LLM.revealApiKey apiKey
            httpRt <- HttpClient.newRuntime auth
            let appendIOTools xs = ioTools <> xs
            let registerTools xs = fmap registerBashToolInLLM xs
            let bkgToolsWithIOTools = fmap (appendIOTools . registerTools) toolbox.tools

            let readTools = (<>) <$> Background.readBackgroundVal bkgToolsWithIOTools <*> readMcpToolsRegistrations
            let rt = Runtime slug uid announce tracer httpRt model readTools toolbox.triggerReload
            pure $ Right rt
  where
    readMcpToolsRegistrations :: IO [ToolRegistration]
    readMcpToolsRegistrations = do
        -- TODO: would be nice to collect the lefts and trace errors upon relaoding
        lists <- traverse (atomically . readTVar . McpTools.toolsList) $ mcpToolboxes
        let reg tb tds = rights [registerMcpToolInLLM tb td | td <- tds]
        pure $ mconcat $ zipWith reg mcpToolboxes lists
