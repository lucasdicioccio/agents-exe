{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- | OS-native agent tree management.

This module provides agent discovery, configuration loading, and tree
building using the OS-native ECS architecture. It replaces the legacy
Runtime-per-agent model with pure OS-native operations.
-}
module System.Agents.AgentTree (
    -- * Registry types and operations
    AgentRegistry,
    newAgentRegistry,
    registerAgent,
    lookupAgent,
    lookupAgentBySlug,

    -- * Props and configuration
    Props (..),
    LoadedApiKeys,

    -- * Agent tree types
    OSAgentTree (..),
    OSAgentNode (..),
    AgentConfigTree (..),
    agentRootDir,

    -- * Configuration graph types
    AgentConfigGraph (..),
    AgentConfigNode (..),
    ReferenceError (..),
    formatReferenceError,
    ReferenceValidationTrace (..),

    -- * Loading and initialization
    loadAgentTreeConfig,
    loadAgentTree,
    loadAgentTreeFromConfig,
    LoadAgentResult (..),
    withAgentTree,
    LoadingError (..),
    formatLoadingError,

    -- * Two-phase loading algorithm
    discoverAgentConfigs,
    validateReferences,
    createAgents,
    wireToolReferences,
    buildAgentTree,
    detectCycles,

    -- * Tracing
    TreeTrace (..),

    -- * Utility functions
    readOpenApiKeysFile,

    -- * Re-export Agent type for field access (without constructor to avoid name shadowing)
    Agent,
    -- Re-export field accessors individually (avoiding those that conflict with ApiKey)
    slug,
    announce,
    systemPrompt,
    flavor,
    modelUrl,
    modelName,
    -- apiKeyId is excluded due to conflict with ApiKey.apiKeyId - use .apiKeyId instead
    toolDirectory,
    bashToolboxes,
    mcpServers,
    openApiToolboxes,
    postgrestToolboxes,
    builtinToolboxes,
    extraAgents,
    skillSources,
    autoEnableSkills,
) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Monad (unless)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as CByteString
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Either as Either
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Proxy (Proxy (..))
import Data.Semigroup (sconcat)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Prod.Tracer (Tracer (..), contramap, runTracer)
import System.FilePath ((</>))
import qualified System.FilePath as FilePath

-- Import ToolLoader qualified to avoid name collisions with LoadingError
import qualified System.Agents.AgentTree.ToolLoader as ToolLoader
import System.Agents.AgentTree.Trace
import qualified System.Agents.Tools.Bindings as Bindings
import System.Agents.Tools.Bindings.Types (AgentAddress (..), BindingValue, ScopedBinding (..))
import System.Agents.Tools.Context (ToolExecutionContext (..))
import qualified System.Agents.Tools.IO as IOTools
import System.Agents.Tools.Params.Types (ParamName, ParamScope (..), ParameterDecl (..), Params, ProcessParams)
import System.Agents.ToolSchema (ParamProperty (..), ParamType (..), ToolDescription (..), ToolName (..), toJsonSchemaPair)
import System.Agents.ApiKeys (
    LoadedApiKeys,
    readOpenApiKeysFile,
 )
import System.Agents.Base (
    AgentDescription (..),
    AgentId (..),
    AgentSlug,
    ExtraAgentRef (..),
    FileSystemDirectoryDescription (..),
 )
import qualified System.Agents.Base as AgentsBase
import qualified System.Agents.FileLoader as FileLoader
import System.Agents.SessionStore (SessionCatalog)

-- OS Core imports

import qualified System.Agents.OS.Core as OS
import System.Agents.OS.Core.Types (EntityId (..))
import qualified System.Agents.OS.Core.Types as OSTypes
import qualified System.Agents.OS.Core.World as OSWorld

import System.Agents.ToolRegistration

-------------------------------------------------------------------------------
-- Agent Registry (OS-Native)
-------------------------------------------------------------------------------

{- | Agent registry using the OS ECS model.

This registry uses the Entity-Component-System architecture for
agent management, providing thread-safe STM operations and efficient lookups.
-}
data AgentRegistry = AgentRegistry
    { registryWorld :: TVar OS.World
    -- ^ The ECS world containing agent components
    , registrySlugIndex :: TVar (HashMap AgentSlug OSTypes.EntityId)
    -- ^ Index mapping slugs to entity IDs for O(1) lookups
    }

{- | Create a new empty OS-based agent registry.

This initializes a new ECS world with the required component stores
registered for agent configuration and state.
-}
newAgentRegistry :: IO AgentRegistry
newAgentRegistry = do
    world <- atomically $ do
        w <- OS.newWorld
        w' <- OSWorld.registerComponentStore w (Proxy @OS.AgentConfig)
        OSWorld.registerComponentStore w' (Proxy @OS.AgentState)
    worldTVar <- newTVarIO world
    slugIndex <- newTVarIO HashMap.empty
    pure $ AgentRegistry worldTVar slugIndex

{- | Register an agent in the OS registry.

Creates a new entity for the agent, attaches the AgentConfig component,
and updates the slug index.
-}
registerAgent :: AgentRegistry -> AgentSlug -> OS.AgentConfig -> IO AgentId
registerAgent registry agentSlug config = do
    -- Generate a new agent ID
    entityId <- OSTypes.newEntityId
    let theNewAgentId = AgentId (unEntityId entityId)

    -- Update the world with the agent's config
    atomically $ do
        world <- readTVar (registryWorld registry)
        OSWorld.setComponent world entityId config
        writeTVar (registryWorld registry) world

        -- Update the slug index
        modifyTVar' (registrySlugIndex registry) (HashMap.insert agentSlug entityId)

    pure theNewAgentId

-- | Result of the discovery phase - represents the agent reference graph
data AgentConfigGraph = AgentConfigGraph
    { graphNodes :: Map.Map AgentSlug AgentConfigNode
    -- ^ All agents by slug, with their file paths and configs
    , graphEdges :: Map.Map AgentSlug [AgentSlug]
    -- ^ References from each agent to others (children + extra-agents)
    , graphRootSlug :: AgentSlug
    -- ^ The slug of the root agent (from rootAgentFile)
    }
    deriving (Show)

lookupAgent :: AgentRegistry -> AgentId -> IO (Maybe OS.AgentConfig)
lookupAgent registry (AgentId uuid) = do
    let entityId = EntityId uuid
    world <- readTVarIO (registryWorld registry)
    atomically $ OSWorld.getComponent world entityId

{- | Lookup an agent by its slug in the OS registry.

This uses the slug index for O(1) lookup.
-}
lookupAgentBySlug :: AgentRegistry -> AgentSlug -> IO (Maybe (AgentId, OS.AgentConfig))
lookupAgentBySlug registry agentSlug = do
    mEntityId <- HashMap.lookup agentSlug <$> readTVarIO (registrySlugIndex registry)
    case mEntityId of
        Nothing -> pure Nothing
        Just entityId -> do
            mConfig <- lookupAgent registry (AgentId (unEntityId entityId))
            case mConfig of
                Nothing -> pure Nothing
                Just config -> pure $ Just (AgentId (unEntityId entityId), config)

-------------------------------------------------------------------------------
-- OS Agent Tree Types
-------------------------------------------------------------------------------

{- | An agent node in the OS-native tree.
Contains references to OS entities and their associated data.
-}
data OSAgentNode = OSAgentNode
    { osNodeFile :: FilePath
    , osNodeConfig :: Agent
    , osNodeAgentId :: AgentId
    , osNodeChildren :: [OSAgentNode]
    , osNodeTools :: TVar [ToolRegistration]
    -- ^ Mutable tools for this agent (OS-native via STM)
    , osNodeParams :: TVar Params
    {- ^ This agent's parameters, resolved against process-level values and
    declared defaults (see @todos/tool-partial-application.md@). Written
    once by 'loadAgentToolboxes'; empty until then.
    -}
    , osNodeExposeBindings :: TVar [Bindings.Binding]
    {- ^ This node's own @whenUnbound: "expose"@ bindings (toolbox-level and
    agent-level, §7): the ones whose argument reappears in the schema when
    the parameter is unbound for the current session. Written once by
    'loadAgentToolboxes', alongside 'osNodeParams'; empty until then.
    -}
    }

{- | The OS-native agent tree.
Contains the world, registry, and tree structure for OS-native agent management.
-}
data OSAgentTree = OSAgentTree
    { osTreeWorld :: OS.World
    , osTreeRegistry :: AgentRegistry
    , osTreeNodes :: Map AgentSlug OSAgentNode
    , osTreeRoot :: OSAgentNode
    , osTreeRootDir :: FilePath
    }

-------------------------------------------------------------------------------
-- Configuration Graph Types
-------------------------------------------------------------------------------

-- | A node in the configuration graph
data AgentConfigNode = AgentConfigNode
    { nodeFile :: FilePath
    , nodeConfig :: Agent
    , nodeChildren :: [AgentSlug]
    -- ^ Child agents from toolDirectory (slugs only)
    , nodeExtraRefs :: [AgentSlug]
    -- ^ Extra agents referenced via extra-agents config
    , nodeExtraWith :: Map AgentSlug (Map ParamName BindingValue)
    {- ^ The @with@ of each extra-agents entry, keyed by the referenced
    agent's slug (@todos/tool-partial-application.md@, §7, Phase 5). A
    child reached only through 'nodeChildren' (a toolDirectory) has no
    entry here: it gets process values and defaults only.
    -}
    , nodeExtraNarrowable :: Map AgentSlug Bool
    {- ^ Whether each extra-agents entry may be narrowed by an ancestor
    above this node (@todos/tool-partial-application.md@, §8, Phase 6),
    keyed by the referenced agent's slug. Absent (and any child reached
    through 'nodeChildren') means 'True'.
    -}
    }
    deriving (Show)

-- | Validation error types
data ReferenceError
    = {- | Agent in file references missing slug
      (referrer slug, referrer file, missing slug)
      -}
      MissingAgentReference AgentSlug FilePath AgentSlug
    | -- | Same slug defined in multiple files
      DuplicateAgentSlug AgentSlug [FilePath]
    deriving (Show)

-- | Format a ReferenceError into a helpful user-facing message
formatReferenceError :: ReferenceError -> Map.Map AgentSlug AgentConfigNode -> Text
formatReferenceError (DuplicateAgentSlug dupSlug files) _ =
    Text.unlines
        [ "ReferenceError: Duplicate agent slug '" <> dupSlug <> "'"
        , "  This slug is defined in multiple files:"
        , Text.unlines ["    - " <> Text.pack f | f <- files]
        , "  Solution: Use unique slugs for each agent configuration."
        ]
formatReferenceError (MissingAgentReference referrerSlug referrerFile missingSlug) allNodes =
    let
        -- Get all available slugs and their files for suggestions
        availableAgents = Map.toList allNodes

        -- Find agents defined in the same directory as the referrer
        referrerDir = FilePath.takeDirectory referrerFile
        sameDirAgents =
            [ (s, node.nodeFile)
            | (s, node) <- availableAgents
            , FilePath.takeDirectory node.nodeFile == referrerDir
            , s /= referrerSlug
            ]

        -- Build suggestion message
        suggestionMsg =
            if null sameDirAgents
                then ""
                else
                    Text.unlines
                        [ ""
                        , "  Did you mean to reference one of these agents?"
                        , Text.unlines
                            [ "    - '" <> s <> "' (defined in " <> Text.pack f <> ")"
                            | (s, f) <- sameDirAgents
                            ]
                        ]

        -- Find agents with similar slugs (simple case-insensitive contains check)
        similarSlugs =
            [ s
            | (s, _) <- availableAgents
            , Text.toLower missingSlug `Text.isInfixOf` Text.toLower s
                || Text.toLower s `Text.isInfixOf` Text.toLower missingSlug
            , s /= missingSlug
            ]

        similarMsg =
            if null similarSlugs
                then ""
                else
                    Text.unlines
                        [ ""
                        , "  Similar slugs found:"
                        , Text.unlines ["    - '" <> s <> "'" | s <- similarSlugs]
                        ]
     in
        Text.unlines
            [ "ReferenceError: Missing agent reference '" <> missingSlug <> "'"
            , "  Referrer: '" <> referrerSlug <> "' in file: " <> Text.pack referrerFile
            , ""
            , "  Hint: The slug '" <> missingSlug <> "' was not found in any loaded agent configuration."
            , suggestionMsg
            , similarMsg
            , "  Remember: The 'slug' in extraAgents must match the target agent's actual slug,"
            , "  not an arbitrary name. Check that your extraAgents configuration uses the"
            , "  correct slug from the target file."
            , ""
            , "  For more information, see: docs/advanced-configuration.md"
            ]

data LoadingError
    = AgentLoadingError FileLoader.InvalidAgentError
    | RefLoadingError ReferenceError
    | -- | Description and error message
      OpenAPIInitError Text String
    | -- | Description and error message
      PostgRESTInitError Text String
    | -- | Path and error message
      ConfigFileError FilePath String
    | ToolLoaderError ToolLoader.LoadingError
    | -- | A @whenUnbound: "expose"@ binding (§7) referencing a @secret@
      -- parameter (argument, parameter name): exposing it back to the model
      -- would let it simply retype the secret in plain text.
      ExposedSecretParameter Text ParamName
    | OtherError String
    deriving (Show)

-- | Convert ToolLoader error to LoadingError
convertToolLoaderError :: ToolLoader.LoadingError -> LoadingError
convertToolLoaderError = ToolLoaderError

-- | Format a LoadingError into a user-facing message
formatLoadingError :: LoadingError -> Map.Map AgentSlug AgentConfigNode -> Text
formatLoadingError (AgentLoadingError err) _ = Text.pack $ show err
formatLoadingError (RefLoadingError err) allNodes = formatReferenceError err allNodes
formatLoadingError (OpenAPIInitError desc msg) _ =
    "OpenAPI Initialization Error for " <> desc <> ": " <> Text.pack msg
formatLoadingError (PostgRESTInitError desc msg) _ =
    "PostgREST Initialization Error for " <> desc <> ": " <> Text.pack msg
formatLoadingError (ConfigFileError path msg) _ =
    "Configuration File Error in " <> Text.pack path <> ": " <> Text.pack msg
formatLoadingError (ToolLoaderError toolErr) _ = formatToolLoaderError toolErr
formatLoadingError (ExposedSecretParameter arg pName) _ =
    "argument '" <> arg <> "' is bound with whenUnbound: \"expose\" to secret parameter '" <> pName <> "': a secret cannot be exposed back to the model"
formatLoadingError (OtherError msg) _ = Text.pack msg

-- | Format ToolLoader errors
formatToolLoaderError :: ToolLoader.LoadingError -> Text
formatToolLoaderError (ToolLoader.OtherError msg) = Text.pack msg
formatToolLoaderError (ToolLoader.BashLoadingError msg) = "Bash Tool Loading Error: " <> Text.pack msg
formatToolLoaderError (ToolLoader.McpLoadingError msg) = "MCP Server Loading Error: " <> Text.pack msg
formatToolLoaderError (ToolLoader.OpenAPILoadingError msg) = "OpenAPI Toolbox Loading Error: " <> Text.pack msg
formatToolLoaderError (ToolLoader.PostgRESTLoadingError msg) = "PostgREST Toolbox Loading Error: " <> Text.pack msg
formatToolLoaderError (ToolLoader.SqliteLoadingError msg) = "SQLite Toolbox Loading Error: " <> Text.pack msg
formatToolLoaderError (ToolLoader.SystemLoadingError msg) = "System Toolbox Loading Error: " <> Text.pack msg
formatToolLoaderError (ToolLoader.DeveloperLoadingError msg) = "Developer Toolbox Loading Error: " <> Text.pack msg
formatToolLoaderError (ToolLoader.LuaLoadingError msg) = "Lua Toolbox Loading Error: " <> Text.pack msg
formatToolLoaderError (ToolLoader.SkillsLoadingError msg) = "Skills Toolbox Loading Error: " <> Text.pack msg
formatToolLoaderError (ToolLoader.ParameterLoadingError msg) = "Parameter Error: " <> Text.pack msg

data LoadAgentResult
    = Errors (NonEmpty.NonEmpty LoadingError)
    | Initialized OSAgentTree

-------------------------------------------------------------------------------
-- Props Configuration
-------------------------------------------------------------------------------

data Props = Props
    { apiKeys :: LoadedApiKeys
    , apiKeysFile :: FilePath
    -- ^ Path to API keys file (for resolving ApiKey secret sources)
    , rootAgentFile :: FilePath
    , interactiveTracer :: Tracer IO TreeTrace
    , agentToTool :: OSAgentNode -> AgentSlug -> AgentId -> Maybe (Map ParamName BindingValue) -> Bool -> ToolRegistration
    {- ^ Function to create a tool registration from an agent node. The
    'with' map, when present, is this reference's @with@
    (@todos/tool-partial-application.md@, §7): 'Nothing' for a child
    reached through a toolDirectory, which gets no reference site to hang
    one on. The 'Bool' is whether this reference is narrowable (§8, Phase
    6): 'True' for a child reached through a toolDirectory.
    -}
    , sessionCatalog :: SessionCatalog
    -- ^ Sessions visible to session introspection capabilities (e.g., @list-sessions@)
    , processParams :: ProcessParams
    -- ^ Operator-supplied parameter values (@--set@/@--set-json@/@--pin@/@--pin-json@/
    -- @--params-file@), resolved against each agent's declared parameters
    -- (see @todos/tool-partial-application.md@). Defaults to empty.
    }

-------------------------------------------------------------------------------
-- Phase 0: Configuration Discovery
-------------------------------------------------------------------------------

{- | Discover all agents starting from root using BFS traversal.

This function collects all agent configurations without creating any runtimes.
It follows both toolDirectory (children) and extra-agents references.

Returns a graph of all discovered agents, or errors if loading fails.
-}
discoverAgentConfigs ::
    Props ->
    IO (Either (NonEmpty.NonEmpty LoadingError) AgentConfigGraph)
discoverAgentConfigs props = do
    -- Normalize the root file path for consistent comparison
    let normalizedRoot = FilePath.normalise props.rootAgentFile
    -- Use BFS to discover all agents starting from root
    result <- bfsDiscovery props [(normalizedRoot, Nothing)] Map.empty
    case result of
        Left errs -> pure $ Left errs
        Right nodes -> do
            -- Build edges map from nodes
            let edges = Map.map (\n -> n.nodeExtraRefs) nodes
            -- Find the root slug by matching file path
            case findNodeByFile normalizedRoot nodes of
                Nothing ->
                    pure $
                        Left $
                            NonEmpty.singleton $
                                OtherError $
                                    "Root agent file not found in discovered nodes: " ++ props.rootAgentFile
                Just (rootSlug, _) ->
                    pure $ Right $ AgentConfigGraph nodes edges rootSlug

{- | BFS discovery state
FilePath: path to agent config
Maybe AgentSlug: optional parent slug (for cycle detection context)
-}
type DiscoveryQueue = [(FilePath, Maybe AgentSlug)]

{- | BFS traversal to discover all agents.

We track visited files to avoid reloading the same config.
We track loaded slugs to detect duplicates.
-}
bfsDiscovery ::
    Props ->
    DiscoveryQueue ->
    Map.Map AgentSlug AgentConfigNode ->
    IO (Either (NonEmpty.NonEmpty LoadingError) (Map.Map AgentSlug AgentConfigNode))
bfsDiscovery _ [] visited = pure $ Right visited
bfsDiscovery props ((filePath, mParent) : queue) visited = do
    -- Normalize the file path for consistent comparison
    let normalizedFilePath = FilePath.normalise filePath
    -- Check if we've already loaded this file
    case findNodeByFile normalizedFilePath visited of
        Just (foundSlug, _) -> do
            -- File already loaded, check for parent reference
            let visited' = case mParent of
                    Just parentSlug ->
                        -- Add this slug to parent's children if not already there
                        Map.adjust
                            (\n -> n{nodeChildren = List.nub (foundSlug : n.nodeChildren)})
                            parentSlug
                            visited
                    Nothing -> visited
            bfsDiscovery props queue visited'
        Nothing -> do
            -- Load the agent config
            loadResult <- FileLoader.loadJsonFile (contramap DataLoadingTrace tracer) filePath
            case loadResult of
                Left err -> pure $ Left (NonEmpty.singleton $ AgentLoadingError err)
                Right (AgentDescription agent) -> do
                    let agentSlug = slug agent

                    -- Check for duplicate slug
                    case Map.lookup agentSlug visited of
                        Just existingNode ->
                            -- Duplicate slug detected
                            let err =
                                    DuplicateAgentSlug
                                        agentSlug
                                        [existingNode.nodeFile, filePath]
                             in pure $ Left (NonEmpty.singleton $ RefLoadingError err)
                        Nothing -> do
                            -- Discover children from toolDirectory (if specified)
                            let rootDir = FilePath.takeDirectory filePath
                            childFiles <- discoverChildFiles rootDir agent

                            -- Get extra agent refs
                            let refs = maybe [] id (extraAgents agent)
                            let extraRefs = map extraAgentSlug refs
                            let extraPaths = map extraAgentPath refs
                            let extraWith = Map.fromList [(r.extraAgentSlug, w) | r <- refs, Just w <- [r.extraAgentWith]]
                            let extraNarrowable = Map.fromList [(r.extraAgentSlug, n) | r <- refs, Just n <- [r.extraAgentNarrowable]]

                            -- Resolve extra agent paths to absolute/normalized paths
                            let resolvedExtraPaths = map (\p -> FilePath.normalise (rootDir </> p)) extraPaths

                            -- Create node
                            let node =
                                    AgentConfigNode
                                        { nodeFile = filePath
                                        , nodeConfig = agent
                                        , nodeChildren = [] -- Will be populated when children are processed
                                        , nodeExtraRefs = extraRefs
                                        , nodeExtraWith = extraWith
                                        , nodeExtraNarrowable = extraNarrowable
                                        }

                            let visited' = Map.insert agentSlug node visited

                            -- Add children to queue (with this slug as parent)
                            let childQueue = [(f, Just agentSlug) | f <- childFiles]

                            -- Add extra agents to queue (with this slug as parent for tracking)
                            -- Pass the resolved absolute paths for proper file matching
                            let extraQueue = [(f, Just agentSlug) | f <- resolvedExtraPaths]

                            bfsDiscovery props (queue ++ childQueue ++ extraQueue) visited'
  where
    tracer = props.interactiveTracer

{- | Discover child agent files from an agent configuration.
This looks at the legacy 'toolDirectory' field and any 'FileSystemDirectory'
entries in 'bashToolboxes' to find sub-agent JSON files.
-}
discoverChildFiles :: FilePath -> Agent -> IO [FilePath]
discoverChildFiles rootDir agent = do
    -- Collect all directories to scan for sub-agents
    let legacyDir = fmap (rootDir </>) (toolDirectory agent)
    let bashToolboxDirs =
            [ rootDir </> desc.fsDirPath
            | AgentsBase.FileSystemDirectory desc <- Maybe.fromMaybe [] (bashToolboxes agent)
            ]

    -- Also look in bashToolboxes directories for sub-agents
    let allDirs = maybeToList legacyDir ++ bashToolboxDirs

    -- Scan all directories for JSON files
    allFiles <- mapM FileLoader.listJsonDirectory allDirs
    pure $ concat allFiles

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

{- | Find a node by its file path using normalized comparison.
Both the search path and stored paths are normalized before comparison.
-}
findNodeByFile :: FilePath -> Map.Map AgentSlug AgentConfigNode -> Maybe (AgentSlug, AgentConfigNode)
findNodeByFile file nodes =
    let normalizedSearchPath = FilePath.normalise file
     in List.find (\(_, n) -> FilePath.normalise n.nodeFile == normalizedSearchPath) (Map.toList nodes)

-------------------------------------------------------------------------------
-- Phase 1: Reference Validation
-------------------------------------------------------------------------------

{- | Validate the configuration graph.

Checks that:
1. All edges point to existing nodes (no missing references)
2. No duplicate slugs exist

Note: Cycles are allowed but detected and logged separately via 'detectCycles'.
-}
validateReferences ::
    AgentConfigGraph ->
    Either (NonEmpty.NonEmpty LoadingError) ()
validateReferences graph =
    let allEdges = Map.toList graph.graphEdges

        -- Check for missing references
        missingErrors =
            concatMap
                (\(from, tos) -> checkMissingReferences graph from tos)
                allEdges
     in case NonEmpty.nonEmpty missingErrors of
            Just errs -> Left errs
            Nothing -> Right ()

-- | Check if any references from a node are missing
checkMissingReferences ::
    AgentConfigGraph ->
    AgentSlug ->
    [AgentSlug] ->
    [LoadingError]
checkMissingReferences graph fromSlug refs =
    case Map.lookup fromSlug graph.graphNodes of
        Nothing -> [] -- Should not happen if graph is consistent
        Just node ->
            Maybe.catMaybes $
                map
                    ( \ref ->
                        if Map.member ref graph.graphNodes
                            then Nothing
                            else
                                Just $
                                    RefLoadingError $
                                        MissingAgentReference fromSlug node.nodeFile ref
                    )
                    refs

-------------------------------------------------------------------------------
-- Phase 2: Agent Creation
-------------------------------------------------------------------------------

{- | Create OS agents for all configurations.

For each agent in the graph:
1. Create OS agent entity
2. Register in AgentRegistry
3. Store in result map
-}
createAgents ::
    Props ->
    AgentConfigGraph ->
    AgentRegistry ->
    IO (Either (NonEmpty.NonEmpty LoadingError) (Map.Map AgentSlug OSAgentNode))
createAgents props graph registry = do
    -- Process nodes in dependency order
    results <- mapM (createSingleAgent props graph registry) (Map.toList graph.graphNodes)

    let (errs, nodes) = Either.partitionEithers results
    case NonEmpty.nonEmpty errs of
        Just someErrs -> pure $ Left someErrs
        Nothing -> do
            -- Build slug -> node map
            let nodeMap = Map.fromList [(slug (osNodeConfig node), node) | node <- nodes]
            pure $ Right nodeMap

-- | Create a single OS agent from a config node
createSingleAgent ::
    Props ->
    AgentConfigGraph ->
    AgentRegistry ->
    (AgentSlug, AgentConfigNode) ->
    IO (Either LoadingError OSAgentNode)
createSingleAgent _props _graph registry (_agentSlug, node) = do
    let agent = node.nodeConfig
    let _rootDir = FilePath.takeDirectory node.nodeFile

    -- Create OS AgentConfig
    let osConfig =
            OS.AgentConfig
                { OS.agentName = slug agent
                , OS.agentModel =
                    OS.ModelConfig
                        { OS.modelFlavor = flavor agent
                        , OS.modelUrl = modelUrl agent
                        , OS.modelName = modelName agent
                        , OS.modelApiKeyId = agent.apiKeyId -- use record dot syntax to avoid ambiguity
                        }
                , OS.agentSystemPrompt = Text.unlines (systemPrompt agent)
                , OS.agentToolboxBindings = [] -- To be set up separately
                }

    -- Create OS agent entity and register
    agentId <- registerAgent registry (slug agent) osConfig

    -- Create mutable tools TVar for this agent
    toolsTVar <- newTVarIO []
    paramsTVar <- newTVarIO Map.empty
    exposeBindingsTVar <- newTVarIO []

    -- Create node
    let osNode =
            OSAgentNode
                { osNodeFile = node.nodeFile
                , osNodeConfig = agent
                , osNodeAgentId = agentId
                , osNodeChildren = [] -- Populated separately
                , osNodeTools = toolsTVar
                , osNodeParams = paramsTVar
                , osNodeExposeBindings = exposeBindingsTVar
                }

    pure $ Right osNode

-------------------------------------------------------------------------------
-- Phase 3: Tool Wiring
-------------------------------------------------------------------------------

{- | Wire up tool references for all agents.

This function:
1. Loads configured toolboxes (bash, builtin, MCP, OpenAPI, PostgREST, Skills) for all agents
2. Wires helper agent tools (child agents and extra-agents references)

Returns a list of loading errors from toolbox loading (empty if all succeeded).
-}
wireToolReferences ::
    Props ->
    AgentConfigGraph ->
    Map.Map AgentSlug OSAgentNode ->
    IO [LoadingError]
wireToolReferences props graph nodeMap = do
    -- First load configured toolboxes for all agents
    toolboxErrors <- mapM (loadAgentToolboxes props nodeMap) (Map.toList graph.graphNodes)
    -- Then wire helper agent tools (needs every node's osNodeParams already
    -- resolved above, to check a 'with' against the child's own defaults)
    wiringErrors <- mapM (wireAgentTools props graph nodeMap) (Map.toList graph.graphNodes)
    pure (concat toolboxErrors ++ concat wiringErrors)

{- | Load toolboxes for a single agent.

Loads bash tools, builtin toolboxes, MCP servers, OpenAPI, PostgREST,
and Skills toolboxes configured in the agent's configuration.
-}
loadAgentToolboxes ::
    Props ->
    Map.Map AgentSlug OSAgentNode ->
    (AgentSlug, AgentConfigNode) ->
    IO [LoadingError]
loadAgentToolboxes props nodeMap (nodeSlug, node) =
    case Map.lookup nodeSlug nodeMap of
        Nothing -> pure []
        Just osNode -> do
            let baseDir = FilePath.takeDirectory node.nodeFile
            let agent = node.nodeConfig
            (resolvedParams, toolboxExposeBindings, toolLoaderErrors) <-
                ToolLoader.loadAgentTools
                    (contramap ToolLoaderTrace props.interactiveTracer)
                    baseDir
                    props.apiKeysFile -- Pass the API keys file path for secret resolution
                    props.sessionCatalog -- Sessions for session introspection
                    props.processParams
                    agent
                    (osNodeTools osNode)
            atomically $ writeTVar (osNodeParams osNode) resolvedParams
            -- Agent-level bindings (§8.1, Phase 6): applied after every
            -- toolbox has registered its tools, since @tool@ here matches
            -- the LLM-visible name directly, across toolboxes.
            let agentBindings = Maybe.fromMaybe [] (AgentsBase.bindings agent)
            case agentBindings of
                [] -> pure ()
                _ -> do
                    let specialized = Bindings.specializeProcessParams resolvedParams agentBindings
                    atomically $ modifyTVar' (osNodeTools osNode) (map (Bindings.applyBindings specialized))
            -- Every whenUnbound: "expose" binding for this node (§7), toolbox-
            -- and agent-level: kept aside (not baked into the shared schema,
            -- see 'Bindings.narrowExposedSchema') for a per-session re-read.
            let agentExposeBindings = filter ((== Bindings.Expose) . Bindings.bindWhenUnbound) agentBindings
                exposeBindings = toolboxExposeBindings ++ agentExposeBindings
            atomically $ writeTVar (osNodeExposeBindings osNode) exposeBindings
            let secretParamNames = Set.fromList [p.paramName | p <- Maybe.fromMaybe [] (AgentsBase.parameters agent), p.paramSecret]
                exposedSecretErrors =
                    [ ExposedSecretParameter b.bindArg p
                    | b <- Bindings.exposedSecretBindings secretParamNames exposeBindings
                    , Bindings.Param p <- [b.bindValue]
                    ]
            pure $ exposedSecretErrors ++ map convertToolLoaderError toolLoaderErrors

{- | Wire tools for a single agent by appending sub-agent tools to the TVar.

This function partitions extra agents to prevent infinite loops during tool
wiring. Specifically, if an agent references itself in extraAgents (self-reference),
we skip wiring it as a tool to avoid redundant registration.
-}
wireAgentTools ::
    Props ->
    AgentConfigGraph ->
    Map.Map AgentSlug OSAgentNode ->
    (AgentSlug, AgentConfigNode) ->
    IO [LoadingError]
wireAgentTools props graph nodeMap (nodeSlug, node) =
    case Map.lookup nodeSlug nodeMap of
        Nothing -> pure [] -- Should not happen
        Just osNode -> do
            -- Look up child nodes (no reference site, so no 'with', always narrowable)
            let validChildren = [(childNode, Nothing, True) | childSlug <- node.nodeChildren, Just childNode <- [Map.lookup childSlug nodeMap]]

            -- Look up extra agent nodes
            -- Partition extra refs to skip self-references during wiring
            let (selfRefs, otherExtraRefs) = List.partition (== nodeSlug) node.nodeExtraRefs

            -- Log self-references for debugging
            unless (null selfRefs) $
                runTracer props.interactiveTracer $
                    ReferenceValidationTrace $
                        SelfReferenceDetected nodeSlug node.nodeFile nodeSlug

            let validExtras =
                    [ (extraNode, Map.lookup refSlug node.nodeExtraWith, Map.findWithDefault True refSlug node.nodeExtraNarrowable)
                    | refSlug <- otherExtraRefs
                    , Just extraNode <- [Map.lookup refSlug nodeMap]
                    ]

            -- Combine all helper nodes, each with its 'with' (if any) and narrowable flag
            let allHelpers = validChildren ++ validExtras

            withErrors <- concat <$> mapM (\(n, mw, _) -> checkWith node n mw) validExtras

            -- Create tool registrations for helper agents
            let helperTools =
                    [ props.agentToTool helperNode (slug (osNodeConfig helperNode)) (osNodeAgentId helperNode) mWith narrowable
                    | (helperNode, mWith, narrowable) <- allHelpers
                    ]

            -- describe_agent (§8.2, Phase 6): one tool per agent that has
            -- helpers, letting its own model discover what is still open
            -- below any one of them.
            let directHelpers =
                    Map.fromList
                        [ (slug (osNodeConfig helperNode), (narrowable, mWith))
                        | (helperNode, mWith, narrowable) <- allHelpers
                        ]
            let describeTools =
                    [ describeAgentTool graph.graphNodes nodeMap directHelpers
                    | not (Map.null directHelpers)
                    ]

            -- Atomically append helper tools to the node's tools TVar
            atomically $ modifyTVar' (osNodeTools osNode) (\existingTools -> existingTools ++ helperTools ++ describeTools)

            pure withErrors

{- | Checks one extra-agents reference's @with@ (§7, Phase 5): every key
must be a parameter the child declares, and every required session-scope
parameter of the child must be covered, either by @with@ or by the child's
own process value/default (already resolved into its 'osNodeParams' by the
time this runs, see 'wireToolReferences').
-}
checkWith :: AgentConfigNode -> OSAgentNode -> Maybe (Map ParamName BindingValue) -> IO [LoadingError]
checkWith _ _ Nothing = pure []
checkWith parentNode childNode (Just withMap) = do
    resolved <- readTVarIO (osNodeParams childNode)
    let childSlug = slug (osNodeConfig childNode)
        decls = Maybe.fromMaybe [] (AgentsBase.parameters (osNodeConfig childNode))
        declNames = Set.fromList (map paramName decls)
        unknown = [n | n <- Map.keys withMap, not (Set.member n declNames)]
        uncovered =
            [ paramName d
            | d <- decls
            , paramScope d == ScopeSession
            , paramRequired d
            , not (Map.member (paramName d) withMap)
            , not (Map.member (paramName d) resolved)
            ]
        errs =
            [ "'with' for extra agent '" <> childSlug <> "' in " <> Text.pack parentNode.nodeFile <> " names unknown parameter(s): " <> Text.intercalate ", " unknown
            | not (null unknown)
            ]
                ++ [ "'with' for extra agent '" <> childSlug <> "' in " <> Text.pack parentNode.nodeFile <> " does not cover required parameter(s): " <> Text.intercalate ", " uncovered
                   | not (null uncovered)
                   ]
    pure [OtherError (Text.unpack e) | e <- errs]

-------------------------------------------------------------------------------
-- Narrowing helpers, down the call chain (§8, Phase 6)
-------------------------------------------------------------------------------

-- | Recursion cutoff for 'describeAgentTool', mirroring the call-stack depth guard.
maxDescribeDepth :: Int
maxDescribeDepth = 8

-- | Argument to the @describe_agent@ tool.
newtype DescribeAgentArg = DescribeAgentArg AgentSlug

instance Aeson.FromJSON DescribeAgentArg where
    parseJSON = Aeson.withObject "DescribeAgentArg" $ \o -> DescribeAgentArg <$> o Aeson..: "slug"

{- | Builds the @describe_agent@ tool for one agent (§8.2, Phase 6): given
one of this agent's own helper slugs, describes that helper's open
parameters and tool arguments, and its own helpers, recursively, as seen
from this agent's position in the call chain (any binding this agent's
ancestors placed on that helper or below it is already accounted for, via
the caller's 'ctxInheritedBindings').
-}
describeAgentTool ::
    Map.Map AgentSlug AgentConfigNode ->
    Map.Map AgentSlug OSAgentNode ->
    -- | This agent's own direct helpers: slug, narrowable, and the @with@ of the reference (if any)
    Map.Map AgentSlug (Bool, Maybe (Map ParamName BindingValue)) ->
    ToolRegistration
describeAgentTool configNodes osNodes directHelpers =
    registerIOScriptInLLM io props
  where
    props =
        [ ParamProperty
            { propertyKey = "slug"
            , propertyType = StringParamType
            , propertyDescription = "the slug of one of this agent's own helpers, as named by its prompt_agent_<slug> tool"
            , propertyRequired = True
            }
        ]

    io =
        IOTools.IOScript
            ( IOTools.IOScriptDescription
                "describe_agent"
                "describes a helper's open (not yet bound) parameters and tool arguments, and its own helpers below it, recursively"
            )
            runDescribe

    runDescribe :: ToolExecutionContext -> DescribeAgentArg -> IO CByteString.ByteString
    runDescribe ctx (DescribeAgentArg targetSlug) =
        case Map.lookup targetSlug directHelpers of
            Nothing ->
                pure $
                    Text.encodeUtf8 $
                        "unknown helper '" <> targetSlug <> "'; this agent's helpers are: "
                            <> Text.intercalate ", " (Map.keys directHelpers)
            Just (narrowable, mWith)
                | not narrowable ->
                    pure $ render $ notNarrowableJSON targetSlug
                | otherwise -> do
                    json <- describeNode Set.empty 0 targetSlug mWith (Bindings.reRootBindings targetSlug (ctxInheritedBindings ctx))
                    pure $ render json

    render :: Aeson.Value -> CByteString.ByteString
    render = LBS.toStrict . Aeson.encode

    osConfigOf :: AgentSlug -> Maybe Agent
    osConfigOf s = osNodeConfig <$> Map.lookup s osNodes

    notNarrowableJSON :: AgentSlug -> Aeson.Value
    notNarrowableJSON s =
        Aeson.object
            [ "slug" .= s
            , "announce" .= maybe "" announce (osConfigOf s)
            , "narrowable" .= False
            ]

    describeNode :: Set.Set AgentSlug -> Int -> AgentSlug -> Maybe (Map ParamName BindingValue) -> [ScopedBinding] -> IO Aeson.Value
    describeNode visited depth nodeSlug mWith inherited
        | depth > maxDescribeDepth || Set.member nodeSlug visited =
            pure $ Aeson.object ["slug" .= nodeSlug, "cut" .= True]
        | otherwise = case (Map.lookup nodeSlug configNodes, Map.lookup nodeSlug osNodes) of
            (Just cfgNode, Just osNode) -> do
                resolvedParams <- readTVarIO (osNodeParams osNode)
                toolRegs <- readTVarIO (osNodeTools osNode)
                let agentCfg = osNodeConfig osNode
                    hereBindings = [sb | sb <- inherited, sbAddress sb == AgentHere]
                    declaredParams = Maybe.fromMaybe [] (AgentsBase.parameters agentCfg)
                    isBound p = Map.member (paramName p) resolvedParams || maybe False (Map.member (paramName p)) mWith
                    paramsJSON =
                        [ Aeson.object $
                            ["name" .= paramName p, "bound" .= isBound p]
                                ++ maybe [] (\d -> ["description" .= d]) (paramDescription p)
                        | p <- declaredParams
                        ]
                    isHidden toolNameText argKey =
                        any (\sb -> maybe True (`Bindings.globMatches` toolNameText) (sbTool sb) && sbArg sb == argKey) hereBindings
                    toolsJSON =
                        [ Aeson.object
                            [ "name" .= toolNameText
                            , "description" .= toolDescriptionText td
                            , "open" .=
                                [ Aeson.object ["arg" .= propertyKey p, "schema" .= snd (toJsonSchemaPair p)]
                                | p <- toolDescriptionParamProperties td
                                , not (isHidden toolNameText (propertyKey p))
                                ]
                            ]
                        | reg <- toolRegs
                        , let td = declareTool reg
                              ToolName toolNameText = toolDescriptionName td
                        , not ("io_prompt_agent_" `Text.isPrefixOf` toolNameText || toolNameText == "io_describe_agent")
                        ]
                    childSlugs = List.nub (cfgNode.nodeChildren ++ filter (/= nodeSlug) cfgNode.nodeExtraRefs)
                helpersJSON <-
                    mapM
                        ( \childSlug ->
                            let childNarrowable = Map.findWithDefault True childSlug cfgNode.nodeExtraNarrowable
                                childWith = Map.lookup childSlug cfgNode.nodeExtraWith
                             in if not childNarrowable
                                    then pure $ notNarrowableJSON childSlug
                                    else
                                        describeNode
                                            (Set.insert nodeSlug visited)
                                            (depth + 1)
                                            childSlug
                                            childWith
                                            (Bindings.reRootBindings childSlug inherited)
                        )
                        childSlugs
                pure $
                    Aeson.object
                        [ "slug" .= nodeSlug
                        , "announce" .= announce agentCfg
                        , "parameters" .= paramsJSON
                        , "tools" .= toolsJSON
                        , "helpers" .= helpersJSON
                        ]
            _ -> pure $ Aeson.object ["slug" .= nodeSlug, "error" .= ("not loaded" :: Text)]

-------------------------------------------------------------------------------
-- Phase 4: Build Agent Tree
-------------------------------------------------------------------------------

{- | Build the tree from the graph and initialized nodes.

Finds the root agent and recursively builds the tree structure using graphEdges.
-}
buildAgentTree ::
    AgentConfigGraph ->
    Map.Map AgentSlug OSAgentNode ->
    Either (NonEmpty.NonEmpty LoadingError) OSAgentTree
buildAgentTree graph nodeMap =
    -- Find root agent using the stored root slug from graph discovery
    case Map.lookup graph.graphRootSlug graph.graphNodes of
        Nothing ->
            Left $
                NonEmpty.singleton $
                    OtherError $
                        "Root agent not found in graph: " ++ Text.unpack graph.graphRootSlug
        Just rootNode ->
            case buildSubtree graph nodeMap Set.empty graph.graphRootSlug rootNode of
                Left errs -> Left errs
                Right (rootOSNode, _) ->
                    let rootDir = FilePath.takeDirectory rootNode.nodeFile
                     in Right $
                            OSAgentTree
                                { osTreeWorld = error "World not yet initialized" -- Will be populated from registry
                                , osTreeRegistry = error "Registry not yet stored" -- Caller must set this
                                , osTreeNodes = nodeMap
                                , osTreeRoot = rootOSNode
                                , osTreeRootDir = rootDir
                                }

{- | Recursively build a subtree with cycle detection.

The 'visited' set tracks slugs in the current path.
-}
buildSubtree ::
    AgentConfigGraph ->
    Map.Map AgentSlug OSAgentNode ->
    -- | Slugs in the current path (for cycle detection)
    Set.Set AgentSlug ->
    AgentSlug ->
    AgentConfigNode ->
    Either (NonEmpty.NonEmpty LoadingError) (OSAgentNode, Set.Set AgentSlug)
buildSubtree graph nodeMap visited nodeSlug node =
    if Set.member nodeSlug visited
        then -- Cycle detected - return node without children
            case Map.lookup nodeSlug nodeMap of
                Nothing -> Left $ NonEmpty.singleton $ OtherError $ "Node not found for slug: " ++ Text.unpack nodeSlug
                Just osNode -> Right (osNode{osNodeChildren = []}, visited)
        else case Map.lookup nodeSlug nodeMap of
            Nothing -> Left $ NonEmpty.singleton $ OtherError $ "Node not found for slug: " ++ Text.unpack nodeSlug
            Just osNode -> do
                let newVisited = Set.insert nodeSlug visited
                -- Build children
                let childSlugs = node.nodeChildren ++ node.nodeExtraRefs
                (children, finalVisited) <-
                    foldlM
                        ( \(acc, v) childSlug ->
                            case Map.lookup childSlug graph.graphNodes of
                                Nothing -> pure (acc, v) -- Skip missing children
                                Just childNode ->
                                    case buildSubtree graph nodeMap v childSlug childNode of
                                        Left errs -> Left errs
                                        Right (childOSNode, v') -> pure (childOSNode : acc, v')
                        )
                        ([], newVisited)
                        childSlugs
                Right (osNode{osNodeChildren = reverse children}, finalVisited)

-- Helper for Either with accumulator
foldlM :: (b -> a -> Either e b) -> b -> [a] -> Either e b
foldlM _ acc [] = Right acc
foldlM f acc (x : xs) = case f acc x of
    Left e -> Left e
    Right acc' -> foldlM f acc' xs

-------------------------------------------------------------------------------
-- Cycle Detection
-------------------------------------------------------------------------------

{- | Detect cycles for warning purposes (doesn't prevent loading).

Uses DFS with three-color marking to detect all cycles in the graph.
Returns a list of all cycles found (each cycle is a list of slugs).
-}
detectCycles :: AgentConfigGraph -> [[AgentSlug]]
detectCycles graph =
    let allSlugs = Map.keys graph.graphNodes
        -- DFS from each unvisited node
        (_, cycles) = foldl (dfsFromNode graph) (Set.empty, []) allSlugs
     in cycles

-- | DFS state: (visited, recStack, cycles)
type DFSState = (Set.Set AgentSlug, Set.Set AgentSlug, [[AgentSlug]])

-- | DFS from a specific node
dfsFromNode ::
    AgentConfigGraph ->
    (Set.Set AgentSlug, [[AgentSlug]]) ->
    AgentSlug ->
    (Set.Set AgentSlug, [[AgentSlug]])
dfsFromNode graph (visited, cycles) nodeSlug
    | Set.member nodeSlug visited = (visited, cycles)
    | otherwise =
        let (_, _, newCycles) = dfsVisit graph (visited, Set.empty, cycles) nodeSlug
         in (Set.insert nodeSlug visited, newCycles)

-- | DFS visit a node
dfsVisit ::
    AgentConfigGraph ->
    DFSState ->
    AgentSlug ->
    DFSState
dfsVisit graph (visited, recStack, cycles) nodeSlug
    | Set.member nodeSlug recStack =
        -- Cycle detected! Extract the cycle
        let cyclePath = extractCycle nodeSlug recStack
         in (visited, recStack, cyclePath : cycles)
    | Set.member nodeSlug visited = (visited, recStack, cycles)
    | otherwise =
        case Map.lookup nodeSlug graph.graphEdges of
            Nothing -> (Set.insert nodeSlug visited, recStack, cycles)
            Just neighbors ->
                let newRecStack = Set.insert nodeSlug recStack
                    (visited', recStack', cycles') =
                        foldl
                            (dfsVisit graph)
                            (visited, newRecStack, cycles)
                            neighbors
                 in (Set.insert nodeSlug visited', Set.delete nodeSlug recStack', cycles')

-- | Extract the cycle from the recursion stack
extractCycle :: AgentSlug -> Set.Set AgentSlug -> [AgentSlug]
extractCycle nodeSlug recStack =
    -- The cycle contains the slug and all nodes in the recursion stack
    nodeSlug : Set.toList recStack

-------------------------------------------------------------------------------
-- Loading Functions
-------------------------------------------------------------------------------

{- | Load agent configuration tree from file.
This is a legacy compatibility function that loads the tree structure
without creating OS entities.
-}
loadAgentTreeConfig ::
    Props ->
    IO (Either (NonEmpty.NonEmpty LoadingError) AgentConfigTree)
loadAgentTreeConfig props = do
    let tracer = props.interactiveTracer
    boss <- FileLoader.loadJsonFile (contramap DataLoadingTrace tracer) props.rootAgentFile
    case boss of
        Left err ->
            pure $ Left (NonEmpty.singleton (AgentLoadingError err))
        Right (AgentDescription agent) -> do
            let rootdir = FilePath.takeDirectory props.rootAgentFile
            let legacyToolDirs = maybeToList $ fmap (rootdir </>) (toolDirectory agent)
            let bashToolboxDirs =
                    [ rootdir </> desc.fsDirPath
                    | AgentsBase.FileSystemDirectory desc <- Maybe.fromMaybe [] (bashToolboxes agent)
                    ]
            let allDirs = legacyToolDirs ++ bashToolboxDirs

            -- Collect child configs from all directories
            subConfigs <- concat <$> mapM FileLoader.listJsonDirectory allDirs

            let propz = [props{rootAgentFile = c} | c <- subConfigs]
            (kos, oks) <- fmap Either.partitionEithers $ traverse loadAgentTreeConfig propz
            case NonEmpty.nonEmpty kos of
                Just errs -> do
                    pure $ Left $ sconcat errs
                Nothing -> do
                    pure $ Right $ AgentConfigTree props.rootAgentFile agent oks

{- | Main entry point for loading agent trees (OS-native).

Phase 0: Discover all configs via BFS traversal
Phase 1: Validate all references
Phase 2: Create OS agents
Phase 3: Wire tool references (includes loading toolboxes + helper agents)
Phase 4: Build final tree
-}
loadAgentTree :: Props -> IO LoadAgentResult
loadAgentTree props = do
    -- Create the agent registry
    registry <- newAgentRegistry

    -- Phase 0: Discover all configs
    graphResult <- discoverAgentConfigs props
    case graphResult of
        Left errs -> pure $ Errors errs
        Right graph -> do
            -- Log discovery
            runTracer props.interactiveTracer $
                ReferenceValidationTrace $
                    ValidationStarted (Map.keysSet graph.graphNodes)

            -- Phase 1: Validate references
            case validateReferences graph of
                Left errs -> pure $ Errors errs
                Right () -> do
                    runTracer props.interactiveTracer (ReferenceValidationTrace ValidationComplete)

                    -- Detect and warn about cycles (cycles are allowed)
                    let cycles = detectCycles graph
                    unless (null cycles) $
                        runTracer props.interactiveTracer (CyclicReferencesWarning cycles)

                    -- Phase 2: Create OS agents
                    agentsResult <- createAgents props graph registry
                    case agentsResult of
                        Left errs -> pure $ Errors errs
                        Right nodeMap -> do
                            -- Phase 3: Wire tool references (now includes loading toolboxes)
                            toolErrors <- wireToolReferences props graph nodeMap
                            case NonEmpty.nonEmpty toolErrors of
                                Just errs -> pure $ Errors errs
                                Nothing -> do
                                    -- Phase 4: Build final tree
                                    case buildAgentTree graph nodeMap of
                                        Left errs -> pure $ Errors errs
                                        Right tree -> do
                                            -- Store registry in tree
                                            let finalTree = tree{osTreeRegistry = registry}
                                            pure $ Initialized finalTree

{- | Load a tree of one agent from a configuration held in memory (e.g. read
from a database) instead of a file. Nothing is discovered on disk: the agent
has no sub-agents, and any file-based tools it names resolve against the
given directory. 'props.rootAgentFile' is not read.
-}
loadAgentTreeFromConfig :: Props -> FilePath -> Agent -> IO LoadAgentResult
loadAgentTreeFromConfig props baseDir agent = do
    registry <- newAgentRegistry
    let agentSlug = AgentsBase.slug agent
        node =
            AgentConfigNode
                { nodeFile = baseDir </> (Text.unpack agentSlug <> ".json")
                , nodeConfig = agent
                , nodeChildren = []
                , nodeExtraRefs = []
                , nodeExtraWith = Map.empty
                , nodeExtraNarrowable = Map.empty
                }
        graph = AgentConfigGraph (Map.singleton agentSlug node) (Map.singleton agentSlug []) agentSlug
    agentsResult <- createAgents props graph registry
    case agentsResult of
        Left errs -> pure $ Errors errs
        Right nodeMap -> do
            toolErrors <- wireToolReferences props graph nodeMap
            case NonEmpty.nonEmpty toolErrors of
                Just errs -> pure $ Errors errs
                Nothing -> case buildAgentTree graph nodeMap of
                    Left errs -> pure $ Errors errs
                    Right tree -> pure $ Initialized tree{osTreeRegistry = registry}

-- | Run an action with a loaded agent tree.
withAgentTree :: Props -> (LoadAgentResult -> IO a) -> IO a
withAgentTree props continue = do
    loadAgentTree props >>= continue
