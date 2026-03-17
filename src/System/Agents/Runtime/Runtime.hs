{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Agents.Runtime.Runtime (
    Runtime (..),
    AgentTools,
    addTracer,
    newRuntime,
    triggerRefreshTools,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Control.Concurrent.STM (STM, TVar, atomically, newTVarIO, readTVar, writeTVar)
import Control.Monad (forever)
import Data.Either (partitionEithers, rights)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Prod.Tracer (Tracer, contramap, runTracer, traceBoth)
import qualified System.Agents.HttpClient as HttpClient

import System.Agents.Base (
    AgentAnnounce,
    AgentId,
    AgentSlug,
    BashToolboxDescription (..),
    BuiltinToolboxDescription (..),
    DeveloperToolboxDescription (..),
    SqliteToolboxDescription (..),
    SystemToolboxDescription (..),
    newAgentId,
 )
import qualified System.Agents.LLMs.OpenAI as OpenAI
import System.Agents.ToolRegistration
import qualified System.Agents.Tools.BashToolbox as BashToolbox
import qualified System.Agents.Tools.DeveloperToolbox as DeveloperToolbox
import qualified System.Agents.Tools.McpToolbox as McpTools
import qualified System.Agents.Tools.Skills.Source as SkillsSource
import qualified System.Agents.Tools.Skills.Toolbox as SkillsToolbox
import System.Agents.Tools.Skills.Types (SkillSource)
import qualified System.Agents.Tools.Skills.Types as SkillsTypes
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
    , agentSkillsStore :: Maybe SkillsTypes.SkillsStore
    -- ^ Loaded skills store (if skills are configured)
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
type IOToolBuilder = AgentSlug -> AgentId -> ToolRegistration
type McpToolConfig = McpTools.Toolbox


{- | Create a new runtime with multiple bash tool sources.

This is the preferred way to create a runtime when you need to load
tools from multiple directories or single executables.

Example:

@
let bashSources =
        [ FileSystemDirectory $ FileSystemDirectoryDescription (Just "agents/" "./tools" Nothing
        , FileSystemDirectory $ FileSystemDirectoryDescription Nothing "./extra" (Just ".sh")
        , SingleTool $ SingleToolDescription "/path/to/special.sh"
        ]
result <- newRuntime slug announce tracer apiKey model bashSources ...
@
-}
newRuntime ::
    AgentSlug ->
    AgentAnnounce ->
    Tracer IO Trace ->
    OpenAI.ApiKey ->
    OpenAI.Model ->
    -- | Bash tool sources (directories and/or single tools)
    [BashToolboxDescription] ->
    [IOToolBuilder] ->
    [McpToolConfig] ->
    -- | OpenAPI tool registrations - initialized and ready to use
    [ToolRegistration] ->
    -- | Builtin toolbox descriptions - to be initialized
    [BuiltinToolboxDescription] ->
    -- | Skill sources - to be loaded
    [SkillSource] ->
    IO (Either String Runtime)
newRuntime slug announce tracer apiKey model bashSources mkIoTools mcpToolboxes openApiToolRegs builtinDescriptions skillSources = do
    uid <- newAgentId
    let ioTools = [mk slug uid | mk <- mkIoTools]

    -- Initialize bash tools from multiple sources
    multiToolboxResult <- BashToolbox.initializeMultiSourceToolbox (contramap (AgentTrace_Loading slug uid) tracer) "." bashSources

    case multiToolboxResult of
        Left err -> pure $ Left (show err)
        Right multiToolbox -> do
            -- Initialize builtin toolboxes (SQLite, System, and Developer)
            (sqliteToolboxes, systemToolboxes, developerToolboxes) <- initializeBuiltinToolboxes tracer builtinDescriptions

            -- Load skills from skill sources
            (mSkillsStore, skillErrors) <- loadSkills tracer skillSources

            -- Trace any skill loading errors
            mapM_ (\err -> runTracer tracer (SkillsToolboxInitError "skills" err)) skillErrors

            -- Register skill tools from the skills store
            -- Create meta tools (describe, enable, disable) for each skill
            let allSkillRegs = case mSkillsStore of
                    Just store ->
                        let skills = SkillsTypes.allSkills store
                            metaToolsPerSkill = map (SkillsToolbox.makeMetaTools store) skills
                            metaTools = concat metaToolsPerSkill
                            mListTool = SkillsToolbox.makeListSkillsTool store
                         in maybe metaTools (: metaTools) mListTool
                    Nothing -> []

            let auth = HttpClient.BearerToken $ Text.decodeUtf8 $ OpenAI.revealApiKey apiKey
            httpRt <- HttpClient.newRuntime auth

            -- Create mutable TVar for tools
            toolsTVar <- newTVarIO []

            -- Read tools from all sources
            let doWriteTools = do
                    baseTools <- readAllTools multiToolbox ioTools sqliteToolboxes systemToolboxes developerToolboxes mcpToolboxes openApiToolRegs allSkillRegs
                    atomically $ writeTVar toolsTVar baseTools

            doWriteTools
            _ <- async $ forever $ do
                threadDelay 1000000
                doWriteTools

            let rt = Runtime slug uid announce tracer httpRt model toolsTVar (BashToolbox.triggerAllReloads multiToolbox) mSkillsStore
            pure $ Right rt
  where
    readAllTools ::
        BashToolbox.MultiSourceBashTools ->
        [ToolRegistration] ->
        [SqliteToolbox.Toolbox] ->
        [SystemToolbox.Toolbox] ->
        [DeveloperToolbox.Toolbox] ->
        [McpToolConfig] ->
        [ToolRegistration] ->
        [ToolRegistration] ->
        IO [ToolRegistration]
    readAllTools multiToolbox ioTools sqliteToolboxes systemToolboxes developerToolboxes mcpToolConfigs openApiToolRegs' skillToolRegs = do
        -- Read bash tools from all sources
        bashScripts <- BashToolbox.readMultiSourceTools multiToolbox
        let bashTools = map registerBashToolInLLM bashScripts

        -- Read MCP tools
        mcpTools <- readMcpToolsRegistrations mcpToolConfigs

        -- Read SQLite tools
        sqliteTools <- readSqliteToolsRegistrations tracer sqliteToolboxes

        -- Read System tools
        systemTools <- readSystemToolsRegistrations tracer systemToolboxes

        -- Read Developer tools
        developerTools <- readDeveloperToolsRegistrations tracer developerToolboxes

        -- Combine all tools: bash + IO + MCP + OpenAPI + SQLite + System + Developer + Skills
        pure $ bashTools ++ ioTools ++ mcpTools ++ openApiToolRegs' ++ sqliteTools ++ systemTools ++ developerTools ++ skillToolRegs

    readMcpToolsRegistrations :: [McpToolConfig] -> IO [ToolRegistration]
    readMcpToolsRegistrations configs = do
        lists <- traverse (atomically . readTVar . McpTools.toolsList) configs
        let reg tb tds = rights [registerMcpToolInLLM tb td | td <- tds]
        pure $ mconcat $ zipWith reg configs lists

{- | Load skills from skill sources.

Returns:
* (Just SkillsStore, []) on success
* (Nothing, [errors]) on failure
-}
loadSkills ::
    Tracer IO Trace ->
    [SkillSource] ->
    IO (Maybe SkillsTypes.SkillsStore, [String])
loadSkills _tracer [] = pure (Nothing, [])
loadSkills tracer sources = do
    runTracer tracer (SkillsToolboxTrace "skills" (SkillsToolbox.SkillsLoadingTrace sources))
    result <- SkillsSource.loadSkillsFromSources sources
    case result of
        Left err -> pure (Nothing, [Text.unpack err])
        Right store ->
            if null (SkillsTypes.allSkills store)
                then pure (Nothing, ["No skills found in sources"])
                else pure (Just store, [])

{- | Initialize all builtin toolboxes from their descriptions.
Supports SQLite, System, and Developer toolboxes.
-}
initializeBuiltinToolboxes ::
    Tracer IO Trace ->
    [BuiltinToolboxDescription] ->
    IO ([SqliteToolbox.Toolbox], [SystemToolbox.Toolbox], [DeveloperToolbox.Toolbox])
initializeBuiltinToolboxes tracer descriptions = do
    let sqliteDescs = [desc | SqliteToolbox desc <- descriptions]
    let systemDescs = [desc | SystemToolbox desc <- descriptions]
    let developerDescs = [desc | DeveloperToolbox desc <- descriptions]

    sqliteResults <- traverse (initializeSqliteToolbox tracer) sqliteDescs
    systemResults <- traverse (initializeSystemToolbox tracer) systemDescs
    developerResults <- traverse (initializeDeveloperToolbox tracer) developerDescs

    let (sqliteErrors, sqliteToolboxes) = partitionEithers sqliteResults
    let (systemErrors, systemToolboxes) = partitionEithers systemResults
    let (developerErrors, developerToolboxes) = partitionEithers developerResults

    -- Trace errors for each failed toolbox
    mapM_ (\err -> runTracer tracer (BuiltinToolboxInitError "sqlite" err)) sqliteErrors
    mapM_ (\err -> runTracer tracer (BuiltinToolboxInitError "system" err)) systemErrors
    mapM_ (\err -> runTracer tracer (BuiltinToolboxInitError "developer" err)) developerErrors

    pure (sqliteToolboxes, systemToolboxes, developerToolboxes)

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

{- | Initialize a single Developer builtin toolbox.
Returns either an error message or the initialized toolbox.
-}
initializeDeveloperToolbox ::
    Tracer IO Trace ->
    DeveloperToolboxDescription ->
    IO (Either String DeveloperToolbox.Toolbox)
initializeDeveloperToolbox tracer desc = do
    let toolboxTracer = contramap (DeveloperToolboxTrace desc.developerToolboxName) tracer
    DeveloperToolbox.initializeToolbox toolboxTracer desc

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

-- | Read Developer tool registrations from initialized toolboxes.
readDeveloperToolsRegistrations ::
    Tracer IO Trace ->
    [DeveloperToolbox.Toolbox] ->
    IO [ToolRegistration]
readDeveloperToolsRegistrations tracer toolboxes = do
    results <- traverse (registerDeveloperToolsWithTracing tracer) toolboxes
    let (errors, registrations) = partitionEithers results
    -- Trace errors for each failed toolbox
    mapM_ (\err -> runTracer tracer (BuiltinToolboxInitError "developer" err)) errors
    pure $ concat registrations

-- | Register tools from a single Developer toolbox with error tracing.
registerDeveloperToolsWithTracing ::
    Tracer IO Trace ->
    DeveloperToolbox.Toolbox ->
    IO (Either String [ToolRegistration])
registerDeveloperToolsWithTracing tracer toolbox = do
    result <- registerDeveloperTools toolbox
    case result of
        Left err -> do
            runTracer tracer (BuiltinToolboxInitError (DeveloperToolbox.toolboxName toolbox) err)
            pure $ Left err
        Right regs -> pure $ Right regs
