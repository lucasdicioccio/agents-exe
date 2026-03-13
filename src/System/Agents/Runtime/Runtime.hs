{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Agents.Runtime.Runtime (
    Runtime (..),
    AgentTools,
    addTracer,
    newRuntime,
    triggerRefreshTools,
) where

import Control.Concurrent.STM (STM, TVar, atomically, newTVarIO, readTVar, readTVarIO, writeTVar)
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
    SystemToolboxDescription (..),
    newAgentId,
 )
import qualified System.Agents.LLMs.OpenAI as OpenAI
import System.Agents.ToolRegistration
import qualified System.Agents.Tools.BashToolbox as BashToolbox
import qualified System.Agents.Tools.McpToolbox as McpTools
import qualified System.Agents.Tools.SqliteToolbox as SqliteToolbox
import qualified System.Agents.Tools.SystemToolbox as SystemToolbox

import System.Agents.Runtime.Trace

-------------------------------------------------------------------------------

-- | Type alias for the mutable tool registrations storage
type AgentTools = TVar [ToolRegistration]

data Runtime
    = Runtime
    { agentSlug :: AgentSlug
    , agentId :: AgentId
    , agentAnnounce :: AgentAnnounce
    , agentTracer :: Tracer IO Trace
    , agentAuthenticatedHttpClientRuntime :: HttpClient.Runtime
    , agentModel :: OpenAI.Model
    , agentTools :: AgentTools
    -- ^ Mutable tool registrations - allows dynamic updates for sub-agent tools
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
            -- Initialize builtin toolboxes (SQLite and System)
            (sqliteToolboxes, systemToolboxes) <- initializeBuiltinToolboxes tracer builtinDescriptions

            let auth = HttpClient.BearerToken $ Text.decodeUtf8 $ OpenAI.revealApiKey apiKey
            httpRt <- HttpClient.newRuntime auth
            let appendIOTools xs = ioTools <> xs
            let registerTools xs = fmap registerBashToolInLLM xs
            let bkgToolsWithIOTools = fmap (appendIOTools . registerTools) toolbox.tools

            -- Create mutable TVar for tools, initialized with bash + IO + MCP + OpenAPI + SQLite + System tools
            toolsTVar <- newTVarIO []
            
            let readTools = (<>) <$> Background.readBackgroundVal bkgToolsWithIOTools <*> readAdditionalTools sqliteToolboxes systemToolboxes
            -- Initialize the TVar with the base tools
            baseTools <- readTools
            atomically $ writeTVar toolsTVar baseTools
            
            let rt = Runtime slug uid announce tracer httpRt model toolsTVar toolbox.triggerReload
            pure $ Right rt
  where
    readAdditionalTools :: [SqliteToolbox.Toolbox] -> [SystemToolbox.Toolbox] -> IO [ToolRegistration]
    readAdditionalTools sqliteToolboxes systemToolboxes = do
        mcpTools <- readMcpToolsRegistrations
        sqliteTools <- readSqliteToolsRegistrations tracer sqliteToolboxes
        systemTools <- readSystemToolsRegistrations tracer systemToolboxes
        -- OpenAPI tools are pre-registered and static
        pure $ mcpTools <> sqliteTools <> systemTools <> openApiToolRegs

    readMcpToolsRegistrations :: IO [ToolRegistration]
    readMcpToolsRegistrations = do
        -- TODO: would be nice to collect the lefts and trace errors upon relaoding
        lists <- traverse (atomically . readTVar . McpTools.toolsList) $ mcpToolboxes
        let reg tb tds = rights [registerMcpToolInLLM tb td | td <- tds]
        pure $ mconcat $ zipWith reg mcpToolboxes lists

{- | Initialize all builtin toolboxes from their descriptions.
Supports both SQLite and System toolboxes.
-}
initializeBuiltinToolboxes ::
    Tracer IO Trace ->
    [BuiltinToolboxDescription] ->
    IO ([SqliteToolbox.Toolbox], [SystemToolbox.Toolbox])
initializeBuiltinToolboxes tracer descriptions = do
    let sqliteDescs = [desc | SqliteToolbox desc <- descriptions]
    let systemDescs = [desc | SystemToolbox desc <- descriptions]
    
    sqliteResults <- traverse (initializeSqliteToolbox tracer) sqliteDescs
    systemResults <- traverse (initializeSystemToolbox tracer) systemDescs
    
    let (sqliteErrors, sqliteToolboxes) = partitionEithers sqliteResults
    let (systemErrors, systemToolboxes) = partitionEithers systemResults
    
    -- Trace errors for each failed toolbox
    mapM_ (\err -> runTracer tracer (BuiltinToolboxInitError "sqlite" err)) sqliteErrors
    mapM_ (\err -> runTracer tracer (BuiltinToolboxInitError "system" err)) systemErrors
    
    pure (sqliteToolboxes, systemToolboxes)

{- | Initialize a single SQLite builtin toolbox.
Returns either an error message or the initialized toolbox.
-}
initializeSqliteToolbox ::
    Tracer IO Trace ->
    SqliteToolboxDescription ->
    IO (Either String SqliteToolbox.Toolbox)
initializeSqliteToolbox tracer desc = do
    let toolboxTracer = contramap (BuiltinToolboxTrace desc.sqliteToolboxName) tracer
    SqliteToolbox.initializeToolbox toolboxTracer desc

{- | Initialize a single System builtin toolbox.
Returns either an error message or the initialized toolbox.
-}
initializeSystemToolbox ::
    Tracer IO Trace ->
    SystemToolboxDescription ->
    IO (Either String SystemToolbox.Toolbox)
initializeSystemToolbox tracer desc = do
    let toolboxTracer = contramap (SystemToolboxTrace desc.systemToolboxName) tracer
    SystemToolbox.initializeToolbox toolboxTracer desc

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

-- | Read System tool registrations from initialized toolboxes.
readSystemToolsRegistrations ::
    Tracer IO Trace ->
    [SystemToolbox.Toolbox] ->
    IO [ToolRegistration]
readSystemToolsRegistrations tracer toolboxes = do
    results <- traverse (registerSystemToolsWithTracing tracer) toolboxes
    let (errors, registrations) = partitionEithers results
    -- Trace errors for each failed toolbox
    mapM_ (\err -> runTracer tracer (BuiltinToolboxInitError "system" err)) errors
    pure $ concat registrations

-- | Register tools from a single System toolbox with error tracing.
registerSystemToolsWithTracing ::
    Tracer IO Trace ->
    SystemToolbox.Toolbox ->
    IO (Either String [ToolRegistration])
registerSystemToolsWithTracing tracer toolbox = do
    result <- registerSystemTools toolbox
    case result of
        Left err -> do
            runTracer tracer (BuiltinToolboxInitError (SystemToolbox.toolboxName toolbox) err)
            pure $ Left err
        Right regs -> pure $ Right regs

