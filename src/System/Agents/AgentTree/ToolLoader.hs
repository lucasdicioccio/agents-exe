{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Tool loading for agent trees.

This module provides functions to load and register tools from various
sources (bash toolboxes, MCP servers, OpenAPI toolboxes, PostgREST toolboxes,
builtin toolboxes, skills toolboxes) into OS-native agent nodes.

This is a crucial part of agent initialization that was missing from the
OS migration - the wireToolReferences function only loaded helper agent
tools, but not the actual toolboxes configured in the agent.

Note: This module does not import System.Agents.AgentTree to avoid cyclic
imports.
-}
module System.Agents.AgentTree.ToolLoader (
    -- * Tool loading
    loadAgentTools,

    -- * Loading errors
    LoadingError (..),
) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', readTVar)
import Control.Exception (SomeException, try)
import qualified Data.Aeson as Aeson
import Data.Either (partitionEithers)
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Text as Text
import Prod.Tracer (Tracer (..), contramap)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import qualified System.FilePath as FilePath
import qualified System.Process

import System.Agents.AgentTree.Trace (TreeTrace (..))
import System.Agents.Base (
    Agent (..),
    BashToolboxDescription (..),
    BuiltinToolboxDescription (..),
    DeveloperToolboxDescription (..),
    FileSystemDirectoryDescription (..),
    LuaToolboxDescription (..),
    McpServerDescription (..),
    McpSimpleBinaryConfiguration (..),
    OpenAPIServerDescription (..),
    OpenAPIServerOnDisk (..),
    OpenAPIToolboxDescription (..),
    PostgRESTServerDescription (..),
    PostgRESTServerOnDisk (..),
    PostgRESTToolboxDescription (..),
    SqliteToolboxDescription (..),
    SystemToolboxDescription (..),
 )
import System.Agents.ToolRegistration (ToolRegistration)
import qualified System.Agents.ToolRegistration as ToolReg
import qualified System.Agents.Tools.BashToolbox as BashToolbox
import qualified System.Agents.Tools.DeveloperToolbox as DeveloperToolbox
import qualified System.Agents.Tools.LuaToolbox as LuaToolbox
import qualified System.Agents.Tools.McpToolbox as McpToolbox
import qualified System.Agents.Tools.OpenAPIToolbox as OpenAPIToolbox
import qualified System.Agents.Tools.PostgRESToolbox as PostgRESToolbox
import qualified System.Agents.Tools.Skills.Source as SkillsSource
import qualified System.Agents.Tools.Skills.Toolbox as SkillsToolbox
import System.Agents.Tools.Skills.Types (
    SkillName,
    SkillSource (..),
    SkillsStore,
    allSkills,
 )
import qualified System.Agents.Tools.SqliteToolbox as SqliteToolbox
import qualified System.Agents.Tools.SystemToolbox as SystemToolbox

-- | Loading error type
data LoadingError
    = OtherError String
    | BashLoadingError String
    | McpLoadingError String
    | OpenAPILoadingError String
    | PostgRESTLoadingError String
    | SqliteLoadingError String
    | SystemLoadingError String
    | DeveloperLoadingError String
    | LuaLoadingError String
    | SkillsLoadingError String
    deriving (Show)

-------------------------------------------------------------------------------
-- Main Tool Loading Entry Point
-------------------------------------------------------------------------------

{- | Load all tools for an agent from its configuration.

This function loads tools from configured sources:
- Bash toolboxes (bashToolboxes field + legacy toolDirectory)
- MCP servers (mcpServers field)
- OpenAPI toolboxes (openApiToolboxes field)
- PostgREST toolboxes (postgrestToolboxes field)
- Builtin toolboxes (builtinToolboxes field): SQLite, System, Developer, Lua
- Skills toolboxes (skillSources field): Skills with progressive disclosure

Tools are loaded and appended to the node's tools TVar.

Returns a list of loading errors (empty if all tools loaded successfully).

Note: Bash toolboxes and Skills directories are resolved relative to the
execution's current working directory.
-}
loadAgentTools ::
    Tracer IO TreeTrace ->
    -- | Base directory for resolving relative paths (for OpenAPI/PostgREST on-disk configs)
    FilePath ->
    -- | Path to API keys file (for resolving ApiKey secret sources)
    FilePath ->
    -- | The agent configuration
    Agent ->
    -- | The agent node's tools TVar
    TVar [ToolRegistration] ->
    IO [LoadingError]
loadAgentTools tracer baseDir apiKeysFile agent toolsTVar = do
    errors <-
        catMaybes
            <$> sequence
                [ loadBashTools (contramap BashToolboxTrace tracer) agent toolsTVar
                , loadMcpServers (contramap McpToolboxTrace tracer) agent toolsTVar
                , loadOpenAPIToolboxes (contramap OpenApiToolboxTrace tracer) baseDir apiKeysFile agent toolsTVar
                , loadPostgRESToolboxes (contramap PostgRESToolboxTrace tracer) baseDir apiKeysFile agent toolsTVar
                , loadBuiltinToolboxes tracer agent toolsTVar
                , loadSkillsTools agent toolsTVar
                ]
    pure errors

-------------------------------------------------------------------------------
-- Bash Tool Loading
-------------------------------------------------------------------------------

{- | Load bash tools from the agent configuration.

Loads tools from:
1. The legacy toolDirectory field (if present)
2. The new bashToolboxes field (if present)

Note: Relative paths are resolved relative to the execution's current
working directory.

The activation configuration from each BashToolboxDescription is applied to
all scripts loaded from that source. This allows per-toolbox activation
control (e.g., progressive disclosure via on-demand activation).
-}
loadBashTools ::
    Tracer IO BashToolbox.Trace ->
    Agent ->
    TVar [ToolRegistration] ->
    IO (Maybe LoadingError)
loadBashTools tracer agent toolsTVar = do
    let descriptions = collectBashDescriptions agent

    if null descriptions
        then pure Nothing
        else do
            result <-
                BashToolbox.initializeMultiSourceToolbox
                    tracer
                    descriptions

            case result of
                Left (BashToolbox.LoadingError _msg _errs) -> do
                    pure $ Just $ BashLoadingError "Failed to load some bash tools"
                Right multiSourceTools -> do
                    -- readMultiSourceTools now returns [(Maybe Activation, [ScriptDescription])]
                    -- where each tuple contains the activation config for that source and its scripts
                    activationAndScripts <- BashToolbox.readMultiSourceTools multiSourceTools

                    -- Create registrations for each script, applying the source's activation config
                    let registrations = concatMap makeRegistrations activationAndScripts
                          where
                            makeRegistrations (mbActivation, scripts) =
                                map (ToolReg.registerBashToolInLLM mbActivation) scripts

                    atomically $ modifyTVar' toolsTVar (\existing -> existing ++ registrations)
                    pure Nothing

{- | Collect all bash toolbox descriptions from the agent config.
Note: Relative paths in bash toolboxes are resolved relative to the
execution's current working directory.
-}
collectBashDescriptions :: Agent -> [BashToolboxDescription]
collectBashDescriptions agent =
    let legacyDir = case toolDirectory agent of
            Just dir -> [FileSystemDirectory $ FileSystemDirectoryDescription Nothing dir Nothing Nothing]
            Nothing -> []
        toolboxDescs = fromMaybe [] (bashToolboxes agent)
     in legacyDir ++ toolboxDescs

-------------------------------------------------------------------------------
-- MCP Server Loading
-------------------------------------------------------------------------------

-- | Timeout for MCP server initialization (30 seconds in microseconds)
mcpInitTimeoutMicros :: Int
mcpInitTimeoutMicros = 30 * 1000 * 1000

{- | Load MCP servers from the agent configuration.

Each MCP server is started and its tools are registered.
If a server fails to start or times out, an error is returned
but processing continues for other servers.

The activation from McpSimpleBinaryConfiguration is propagated to the Toolbox
and applied to all tools from that server via registerMcpToolInLLM.
-}
loadMcpServers ::
    Tracer IO McpToolbox.Trace ->
    Agent ->
    TVar [ToolRegistration] ->
    IO (Maybe LoadingError)
loadMcpServers tracer agent toolsTVar = do
    let servers = fromMaybe [] (mcpServers agent)

    if null servers
        then pure Nothing
        else do
            errors <- mapM (loadMcpServer tracer toolsTVar) servers
            pure $ collectFirstError errors

{- | Load a single MCP server and register its tools.
Catches exceptions during initialization and returns a graceful error.
-}
loadMcpServer ::
    Tracer IO McpToolbox.Trace ->
    TVar [ToolRegistration] ->
    McpServerDescription ->
    IO (Maybe LoadingError)
loadMcpServer tracer toolsTVar (McpSimpleBinary config) = do
    let proc = System.Process.proc config.executable (map Text.unpack config.args)

    -- Try to initialize the MCP toolbox with activation from config
    initResult <- try $ McpToolbox.initializeMcpToolbox tracer config.name proc config.mcpActivation

    case initResult of
        Left (e :: SomeException) -> do
            -- Initialization failed (e.g., executable not found)
            let errMsg = Text.unpack config.name ++ ": Failed to initialize MCP server: " ++ show e
            pure $ Just $ McpLoadingError errMsg
        Right toolbox -> do
            -- Wait for initial tool discovery with timeout
            discoveryResult <- McpToolbox.waitForInitialDiscoveryTimeout mcpInitTimeoutMicros toolbox

            if not discoveryResult
                then do
                    -- Timeout waiting for tool discovery
                    let errMsg = Text.unpack config.name ++ ": Timeout waiting for MCP server tool discovery (" ++ show (mcpInitTimeoutMicros `div` 1000000) ++ "s)"
                    pure $ Just $ McpLoadingError errMsg
                else do
                    -- Get discovered tools
                    tools <- readTVarIO $ McpToolbox.toolsList toolbox

                    -- Register each tool with activation from toolbox
                    let registrations = map (ToolReg.registerMcpToolInLLM toolbox) tools
                    let (errs, regs) = partitionEithers registrations

                    -- Add successful registrations to the tools TVar
                    atomically $ modifyTVar' toolsTVar (\existing -> existing ++ regs)

                    -- Return first error if any registration failed
                    pure $ case errs of
                        (e : _) -> Just $ McpLoadingError e
                        [] -> Nothing
  where
    readTVarIO tvar = atomically $ readTVar tvar

-------------------------------------------------------------------------------
-- OpenAPI Toolbox Loading
-------------------------------------------------------------------------------

{- | Load OpenAPI toolboxes from the agent configuration.

Each OpenAPI toolbox fetches its spec and registers converted tools.

The activation from OpenAPIServerDescription is propagated to the Toolbox
and applied to all tools from that toolbox via registerOpenAPITool.
-}
loadOpenAPIToolboxes ::
    Tracer IO OpenAPIToolbox.Trace ->
    FilePath ->
    FilePath ->
    Agent ->
    TVar [ToolRegistration] ->
    IO (Maybe LoadingError)
loadOpenAPIToolboxes tracer baseDir apiKeysFile agent toolsTVar = do
    let toolboxes = fromMaybe [] (openApiToolboxes agent)

    if null toolboxes
        then pure Nothing
        else do
            errors <- mapM (loadOpenAPIToolbox tracer baseDir apiKeysFile toolsTVar) toolboxes
            pure $ collectFirstError errors

-- | Load a single OpenAPI toolbox and register its tools.
loadOpenAPIToolbox ::
    Tracer IO OpenAPIToolbox.Trace ->
    FilePath ->
    FilePath ->
    TVar [ToolRegistration] ->
    OpenAPIToolboxDescription ->
    IO (Maybe LoadingError)
loadOpenAPIToolbox tracer baseDir apiKeysFile toolsTVar description = do
    -- Resolve the description to get the actual config
    configResult <- resolveOpenAPIDescription baseDir description

    case configResult of
        Left err -> pure $ Just $ OpenAPILoadingError err
        Right config -> do
            -- Initialize the toolbox with API keys file for secret resolution
            initResult <- OpenAPIToolbox.initializeToolbox apiKeysFile tracer config

            case initResult of
                Left err -> pure $ Just $ OpenAPILoadingError (show err)
                Right toolbox -> do
                    -- Register all tools from the toolbox
                    -- Activation is extracted from openApiActivation in registerOpenAPITool
                    regResult <- ToolReg.registerOpenAPITools toolbox

                    case regResult of
                        Left err -> pure $ Just $ OpenAPILoadingError err
                        Right registrations -> do
                            atomically $ modifyTVar' toolsTVar (\existing -> existing ++ registrations)
                            pure Nothing

-- | Resolve an OpenAPI description to its configuration.
resolveOpenAPIDescription ::
    FilePath ->
    OpenAPIToolboxDescription ->
    IO (Either String OpenAPIToolbox.Config)
resolveOpenAPIDescription _baseDir (OpenAPIServer desc) =
    pure $
        Right $
            OpenAPIToolbox.Config
                { OpenAPIToolbox.configUrl = openApiSpecUrl desc
                , OpenAPIToolbox.configBaseUrl = openApiBaseUrl desc
                , OpenAPIToolbox.configHeaders = fromMaybe mempty (openApiHeaders desc)
                , OpenAPIToolbox.configToken = openApiToken desc
                , OpenAPIToolbox.configFilter = openApiFilter desc
                , OpenAPIToolbox.configSecrets = fromMaybe [] (openApiSecrets desc)
                , OpenAPIToolbox.configActivation = openApiActivation desc
                }
resolveOpenAPIDescription baseDir (OpenAPIServerOnDiskDescription (OpenAPIServerOnDisk path)) = do
    let fullPath = if FilePath.isRelative path then baseDir </> path else path
    result <- Aeson.eitherDecodeFileStrict' fullPath
    case result of
        Left err -> pure $ Left $ "Failed to load OpenAPI config from " ++ fullPath ++ ": " ++ err
        Right desc -> resolveOpenAPIDescription baseDir (OpenAPIServer desc)

-------------------------------------------------------------------------------
-- PostgREST Toolbox Loading
-------------------------------------------------------------------------------

{- | Load PostgREST toolboxes from the agent configuration.

Each PostgREST toolbox fetches its spec and registers converted tools.

The activation from PostgRESTServerDescription is propagated to the Toolbox
and applied to all tools from that toolbox via registerPostgRESTool.
-}
loadPostgRESToolboxes ::
    Tracer IO PostgRESToolbox.Trace ->
    FilePath ->
    FilePath ->
    Agent ->
    TVar [ToolRegistration] ->
    IO (Maybe LoadingError)
loadPostgRESToolboxes tracer baseDir apiKeysFile agent toolsTVar = do
    let toolboxes = fromMaybe [] (postgrestToolboxes agent)

    if null toolboxes
        then pure Nothing
        else do
            errors <- mapM (loadPostgRESTToolbox tracer baseDir apiKeysFile toolsTVar) toolboxes
            pure $ collectFirstError errors

-- | Load a single PostgREST toolbox and register its tools.
loadPostgRESTToolbox ::
    Tracer IO PostgRESToolbox.Trace ->
    FilePath ->
    FilePath ->
    TVar [ToolRegistration] ->
    PostgRESTToolboxDescription ->
    IO (Maybe LoadingError)
loadPostgRESTToolbox tracer baseDir apiKeysFile toolsTVar description = do
    -- Resolve the description to get the actual config
    configResult <- resolvePostgRESTDescription baseDir description

    case configResult of
        Left err -> pure $ Just $ PostgRESTLoadingError err
        Right config -> do
            -- Initialize the toolbox with API keys file for secret resolution
            initResult <- PostgRESToolbox.initializeToolbox apiKeysFile tracer config

            case initResult of
                Left err -> pure $ Just $ PostgRESTLoadingError (show err)
                Right toolbox -> do
                    -- Register all tools from the toolbox
                    -- Activation is extracted from postgrestActivation in registerPostgRESTool
                    regResult <- ToolReg.registerPostgRESTools toolbox

                    case regResult of
                        Left err -> pure $ Just $ PostgRESTLoadingError err
                        Right registrations -> do
                            atomically $ modifyTVar' toolsTVar (\existing -> existing ++ registrations)
                            pure Nothing

-- | Resolve a PostgREST description to its configuration.
resolvePostgRESTDescription ::
    FilePath ->
    PostgRESTToolboxDescription ->
    IO (Either String PostgRESToolbox.Config)
resolvePostgRESTDescription _baseDir (PostgRESTServer desc) =
    pure $
        Right $
            PostgRESToolbox.Config
                { PostgRESToolbox.configUrl = postgrestSpecUrl desc
                , PostgRESToolbox.configBaseUrl = postgrestBaseUrl desc
                , PostgRESToolbox.configHeaders = fromMaybe mempty (postgrestHeaders desc)
                , PostgRESToolbox.configToken = postgrestToken desc
                , PostgRESToolbox.configAllowedMethods = fromMaybe [] (postgrestAllowedMethods desc)
                , PostgRESToolbox.configFilter = postgrestFilter desc
                , PostgRESToolbox.configSecrets = fromMaybe [] (postgrestSecrets desc)
                , PostgRESToolbox.configActivation = postgrestActivation desc
                }
resolvePostgRESTDescription baseDir (PostgRESTServerOnDiskDescription (PostgRESTServerOnDisk path)) = do
    let fullPath = if FilePath.isRelative path then baseDir </> path else path
    result <- Aeson.eitherDecodeFileStrict' fullPath
    case result of
        Left err -> pure $ Left $ "Failed to load PostgREST config from " ++ fullPath ++ ": " ++ err
        Right desc -> resolvePostgRESTDescription baseDir (PostgRESTServer desc)

-------------------------------------------------------------------------------
-- Builtin Toolbox Loading
-------------------------------------------------------------------------------

{- | Load builtin toolboxes from the agent configuration.

Builtin toolboxes include:
- SQLite databases
- System information tools
- Developer tools
- Lua sandbox

These toolboxes support activation via their configuration fields:
- sqliteToolboxActivation
- systemToolboxActivation
- developerToolboxActivation
- luaToolboxActivation
-}
loadBuiltinToolboxes ::
    Tracer IO TreeTrace ->
    Agent ->
    TVar [ToolRegistration] ->
    IO (Maybe LoadingError)
loadBuiltinToolboxes tracer agent toolsTVar = do
    let toolboxes = fromMaybe [] (builtinToolboxes agent)

    if null toolboxes
        then pure Nothing
        else do
            errors <- mapM (loadBuiltinToolbox tracer toolsTVar) toolboxes
            pure $ collectFirstError errors

-- | Load a single builtin toolbox.
loadBuiltinToolbox ::
    Tracer IO TreeTrace ->
    TVar [ToolRegistration] ->
    BuiltinToolboxDescription ->
    IO (Maybe LoadingError)
loadBuiltinToolbox tracer toolsTVar (SqliteToolbox desc) = loadSqliteToolbox (contramap SqliteToolboxTrace tracer) toolsTVar desc
loadBuiltinToolbox tracer toolsTVar (SystemToolbox desc) = loadSystemToolbox (contramap SystemToolboxTrace tracer) toolsTVar desc
loadBuiltinToolbox tracer toolsTVar (DeveloperToolbox desc) = loadDeveloperToolbox (contramap DeveloperToolboxTrace tracer) toolsTVar desc
loadBuiltinToolbox tracer toolsTVar (LuaToolbox desc) = loadLuaToolbox (contramap LuaToolboxTrace tracer) toolsTVar desc

-------------------------------------------------------------------------------
-- SQLite Toolbox Loading
-------------------------------------------------------------------------------

-- | Load a SQLite toolbox and register its tools.
loadSqliteToolbox ::
    Tracer IO SqliteToolbox.Trace ->
    TVar [ToolRegistration] ->
    SqliteToolboxDescription ->
    IO (Maybe LoadingError)
loadSqliteToolbox tracer toolsTVar desc = do
    -- Initialize the toolbox
    initResult <- SqliteToolbox.initializeToolbox tracer desc

    case initResult of
        Left err -> pure $ Just $ SqliteLoadingError err
        Right toolbox -> do
            -- Register all tools from the toolbox
            -- Activation is extracted from sqliteToolboxActivation in registerSqliteTool
            regResult <- ToolReg.registerSqliteTools toolbox

            case regResult of
                Left err -> pure $ Just $ SqliteLoadingError err
                Right registrations -> do
                    atomically $ modifyTVar' toolsTVar (\existing -> existing ++ registrations)
                    pure Nothing

-------------------------------------------------------------------------------
-- System Toolbox Loading
-------------------------------------------------------------------------------

-- | Load a System toolbox and register its tools.
loadSystemToolbox ::
    Tracer IO SystemToolbox.Trace ->
    TVar [ToolRegistration] ->
    SystemToolboxDescription ->
    IO (Maybe LoadingError)
loadSystemToolbox tracer toolsTVar desc = do
    -- Initialize the toolbox
    initResult <- SystemToolbox.initializeToolbox tracer desc

    case initResult of
        Left err -> pure $ Just $ SystemLoadingError err
        Right toolbox -> do
            -- Register all tools from the toolbox
            -- Activation is extracted from systemToolboxActivation in registerSystemTool
            regResult <- ToolReg.registerSystemTools toolbox

            case regResult of
                Left err -> pure $ Just $ SystemLoadingError err
                Right registrations -> do
                    atomically $ modifyTVar' toolsTVar (\existing -> existing ++ registrations)
                    pure Nothing

-------------------------------------------------------------------------------
-- Developer Toolbox Loading
-------------------------------------------------------------------------------

-- | Load a Developer toolbox and register its tools.
loadDeveloperToolbox ::
    Tracer IO DeveloperToolbox.Trace ->
    TVar [ToolRegistration] ->
    DeveloperToolboxDescription ->
    IO (Maybe LoadingError)
loadDeveloperToolbox tracer toolsTVar desc = do
    -- Initialize the toolbox
    initResult <- DeveloperToolbox.initializeToolbox tracer desc

    case initResult of
        Left err -> pure $ Just $ DeveloperLoadingError err
        Right toolbox -> do
            -- Register all tools from the toolbox
            -- Activation is extracted from developerToolboxActivation in registerDeveloperTool
            regResult <- ToolReg.registerDeveloperTools toolbox

            case regResult of
                Left err -> pure $ Just $ DeveloperLoadingError err
                Right registrations -> do
                    atomically $ modifyTVar' toolsTVar (\existing -> existing ++ registrations)
                    pure Nothing

-------------------------------------------------------------------------------
-- Lua Toolbox Loading
-------------------------------------------------------------------------------

-- | Load a Lua toolbox and register its tools.
loadLuaToolbox ::
    Tracer IO LuaToolbox.Trace ->
    TVar [ToolRegistration] ->
    LuaToolboxDescription ->
    IO (Maybe LoadingError)
loadLuaToolbox tracer toolsTVar desc = do
    -- Initialize the toolbox with a dummy portal
    -- The actual portal is passed at execution time
    initResult <- LuaToolbox.initializeToolbox tracer desc

    case initResult of
        Left err -> pure $ Just $ LuaLoadingError err
        Right toolbox -> do
            -- Register all tools from the toolbox
            -- Activation is extracted from luaToolboxActivation in registerLuaTool
            regResult <- ToolReg.registerLuaTools toolbox

            case regResult of
                Left err -> pure $ Just $ LuaLoadingError err
                Right registrations -> do
                    atomically $ modifyTVar' toolsTVar (\existing -> existing ++ registrations)
                    pure Nothing

-------------------------------------------------------------------------------
-- Skills Toolbox Loading
-------------------------------------------------------------------------------

{- | Load skills toolboxes from the agent configuration.

Skills provide procedural knowledge and executable capabilities via
progressive disclosure. This function:
1. Loads skills from configured sources (directories, git repos)
2. Registers metadata tools for all skills (skill_list, skill_describe_*)
3. Auto-enables skills configured in autoEnableSkills

Note: Script tools are computed dynamically based on session state via
computeSkillTools in System.Agents.Tools.Skills.Toolbox.

Note: SkillDirectory paths are resolved relative to the execution's current
working directory.
-}
loadSkillsTools ::
    Agent ->
    TVar [ToolRegistration] ->
    IO (Maybe LoadingError)
loadSkillsTools agent toolsTVar = do
    let sources = fromMaybe [] (skillSources agent)
    let autoEnable = fromMaybe [] (autoEnableSkills agent)

    if null sources
        then pure Nothing
        else do
            -- Resolve relative paths in skill sources to current working directory
            resolvedSources <- mapM resolveSkillSource sources

            -- Load skills from all sources
            result <- SkillsSource.loadSkillsFromSources resolvedSources

            case result of
                Left err -> pure $ Just $ SkillsLoadingError (Text.unpack err)
                Right skillsStore -> do
                    -- Register skill metadata tools (always available)
                    let metaTools =
                            concatMap
                                (SkillsToolbox.makeMetaTools skillsStore)
                                (allSkills skillsStore)

                    -- Register skill_list tool if there are skills
                    let listTool = SkillsToolbox.makeListSkillsTool skillsStore

                    -- Register auto-enable tools for skills in autoEnableSkills
                    autoTools <- loadAutoEnableSkills skillsStore autoEnable

                    -- Combine all tools
                    let allTools = metaTools ++ maybeToList listTool ++ autoTools

                    -- Add to tools TVar
                    atomically $ modifyTVar' toolsTVar (\existing -> existing ++ allTools)

                    pure Nothing

{- | Resolve relative paths in skill sources to absolute paths.
Note: SkillDirectory paths are resolved relative to the execution's current
working directory.
-}
resolveSkillSource :: SkillSource -> IO SkillSource
resolveSkillSource (SkillDirectory path) = do
    cwd <- getCurrentDirectory
    pure $ SkillDirectory $ if FilePath.isRelative path then cwd </> path else path
resolveSkillSource gitRepo@(SkillGitRepo _ _) = pure gitRepo

{- | Load tools for auto-enabled skills.
For now, this is empty - script tools will be computed dynamically
based on session state via computeSkillTools.
-}
loadAutoEnableSkills :: SkillsStore -> [SkillName] -> IO [ToolRegistration]
loadAutoEnableSkills _skillsStore _autoEnable = do
    -- Note: Script tools for auto-enabled skills are computed dynamically
    -- via computeSkillTools based on session state. The enable/disable
    -- state is tracked by the session folding mechanism.
    -- For now, we just return empty - the actual script tools will be
    -- computed when the session is available.
    pure []

-- | Helper to convert Maybe to list.
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

-------------------------------------------------------------------------------
-- Utility Functions
-------------------------------------------------------------------------------

-- | Collect the first error from a list of Maybe errors.
collectFirstError :: [Maybe LoadingError] -> Maybe LoadingError
collectFirstError = foldl go Nothing
  where
    go acc@(Just _) _ = acc
    go Nothing (Just err) = Just err
    go Nothing Nothing = Nothing

