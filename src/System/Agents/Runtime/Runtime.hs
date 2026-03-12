{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Agents.Runtime.Runtime (
    Runtime (..),
    addTracer,
    newRuntime,
    triggerRefreshTools,
) where

import Control.Concurrent.STM (STM, atomically, readTVar)
import Data.Either (partitionEithers, rights)
import qualified Data.Text.Encoding as Text
import qualified Prod.Background as Background
import Prod.Tracer (Tracer, contramap, runTracer, traceBoth)
import qualified System.Agents.HttpClient as HttpClient

import System.Agents.Base (
    AgentAnnounce,
    AgentId,
    AgentSlug,
    BuiltinToolboxDescription (..),
    SqliteToolboxDescription (..),
    newAgentId,
 )
import qualified System.Agents.LLMs.OpenAI as OpenAI
import System.Agents.ToolRegistration
import qualified System.Agents.Tools.BashToolbox as BashToolbox
import qualified System.Agents.Tools.McpToolbox as McpTools
import qualified System.Agents.Tools.SqliteToolbox as SqliteToolbox

import System.Agents.Runtime.Trace

-------------------------------------------------------------------------------

data Runtime
    = Runtime
    { agentSlug :: AgentSlug
    , agentId :: AgentId
    , agentAnnounce :: AgentAnnounce
    , agentTracer :: Tracer IO Trace
    , agentAuthenticatedHttpClientRuntime :: HttpClient.Runtime
    , agentModel :: OpenAI.Model
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
    OpenAI.ApiKey ->
    OpenAI.Model ->
    ToolboxDirectory ->
    [IOToolBuilder] ->
    [McpToolConfig] ->
    -- | OpenAPI tool registrations - initialized and ready to use
    [ToolRegistration] ->
    -- | Builtin toolbox descriptions - to be initialized
    [BuiltinToolboxDescription] ->
    IO (Either String Runtime)
newRuntime slug announce tracer apiKey model tooldir mkIoTools mcpToolboxes openApiToolRegs builtinDescriptions = do
    uid <- newAgentId
    let ioTools = [mk slug uid | mk <- mkIoTools]
    toolz <- BashToolbox.initializeBackroundToolbox (contramap (AgentTrace_Loading slug uid) tracer) tooldir
    case toolz of
        Left err -> pure $ Left (show err)
        Right toolbox -> do
            -- Initialize builtin toolboxes (SQLite)
            sqliteToolboxes <- initializeBuiltinToolboxes tracer builtinDescriptions

            let auth = HttpClient.BearerToken $ Text.decodeUtf8 $ OpenAI.revealApiKey apiKey
            httpRt <- HttpClient.newRuntime auth
            let appendIOTools xs = ioTools <> xs
            let registerTools xs = fmap registerBashToolInLLM xs
            let bkgToolsWithIOTools = fmap (appendIOTools . registerTools) toolbox.tools

            let readTools = (<>) <$> Background.readBackgroundVal bkgToolsWithIOTools <*> readAdditionalTools sqliteToolboxes
            let rt = Runtime slug uid announce tracer httpRt model readTools toolbox.triggerReload
            pure $ Right rt
  where
    readAdditionalTools :: [SqliteToolbox.Toolbox] -> IO [ToolRegistration]
    readAdditionalTools sqliteToolboxes = do
        mcpTools <- readMcpToolsRegistrations
        sqliteTools <- readSqliteToolsRegistrations tracer sqliteToolboxes
        -- OpenAPI tools are pre-registered and static
        pure $ mcpTools <> sqliteTools <> openApiToolRegs

    readMcpToolsRegistrations :: IO [ToolRegistration]
    readMcpToolsRegistrations = do
        -- TODO: would be nice to collect the lefts and trace errors upon relaoding
        lists <- traverse (atomically . readTVar . McpTools.toolsList) $ mcpToolboxes
        let reg tb tds = rights [registerMcpToolInLLM tb td | td <- tds]
        pure $ mconcat $ zipWith reg mcpToolboxes lists

{- | Initialize all builtin toolboxes from their descriptions.
Currently only supports SQLite toolboxes.
-}
initializeBuiltinToolboxes ::
    Tracer IO Trace ->
    [BuiltinToolboxDescription] ->
    IO [SqliteToolbox.Toolbox]
initializeBuiltinToolboxes tracer descriptions = do
    results <- traverse (initializeBuiltinToolbox tracer) descriptions
    let (errors, toolboxes) = partitionEithers results
    -- Trace any initialization errors
    mapM_ (\err -> runTracer tracer (BuiltinToolboxInitError "builtin" err)) errors
    pure $ concat toolboxes

{- | Initialize a single builtin toolbox.
Returns either an error message or a list of toolboxes (for SQLite, always a single toolbox).
-}
initializeBuiltinToolbox ::
    Tracer IO Trace ->
    BuiltinToolboxDescription ->
    IO (Either String [SqliteToolbox.Toolbox])
initializeBuiltinToolbox tracer (SqliteToolbox desc) = do
    let toolboxTracer = contramap (BuiltinToolboxTrace desc.sqliteToolboxName) tracer
    result <- SqliteToolbox.initializeToolbox toolboxTracer desc
    case result of
        Left err -> pure $ Left err
        Right toolbox -> pure $ Right [toolbox]

-- | Read SQLite tool registrations from initialized toolboxes.
readSqliteToolsRegistrations ::
    Tracer IO Trace ->
    [SqliteToolbox.Toolbox] ->
    IO [ToolRegistration]
readSqliteToolsRegistrations tracer toolboxes = do
    results <- traverse (registerSqliteToolsWithTracing tracer) toolboxes
    let (errors, registrations) = partitionEithers results
    -- Trace errors for each failed toolbox
    mapM_ (\err -> runTracer tracer (BuiltinToolboxInitError "sqlite" err)) errors
    pure $ concat registrations

-- | Register tools from a single SQLite toolbox with error tracing.
registerSqliteToolsWithTracing ::
    Tracer IO Trace ->
    SqliteToolbox.Toolbox ->
    IO (Either String [ToolRegistration])
registerSqliteToolsWithTracing tracer toolbox = do
    result <- registerSqliteTools toolbox
    case result of
        Left err -> do
            runTracer tracer (BuiltinToolboxInitError (SqliteToolbox.toolboxName toolbox) err)
            pure $ Left err
        Right regs -> pure $ Right regs
