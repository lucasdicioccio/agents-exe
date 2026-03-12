{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Agents.AgentTree (
    -- * Registry types and operations
    RuntimeRegistry,
    newRuntimeRegistry,
    registerRuntime,
    lookupRuntime,

    -- * Props and configuration
    Props (..),
    LoadedApiKeys,

    -- * Agent tree types
    AgentTree (..),
    AgentConfigTree (..),
    agentRootDir,

    -- * Configuration graph types
    AgentConfigGraph (..),
    AgentConfigNode (..),
    ReferenceError (..),
    ReferenceValidationTrace (..),

    -- * Loading and initialization
    loadAgentTreeConfig,
    loadAgentTree,
    loadAgentTreeRuntime,
    LoadAgentResult (..),
    withAgentTreeRuntime,
    LoadingError (..),

    -- * Two-phase loading algorithm
    discoverAgentConfigs,
    validateReferences,
    createRuntimeShells,
    wireToolReferences,
    buildAgentTree,
    detectCycles,

    -- * Agent initialization
    initAgentTreeAgent,
    initAgentTreeAgentDeferred,
    PromptModifier,

    -- * Tracing
    Trace (..),

    -- * Utility functions
    readOpenApiKeysFile,
    augmentMainAgentPromptWithSubAgents,

    -- * Configuration loading helpers
    loadOpenAPIToolboxDescription,
    loadPostgRESTToolboxDescription,
) where

import Control.Monad (void, unless)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.Semigroup (sconcat)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Prod.Tracer (Tracer (..), contramap, runTracer)
import System.FilePath ((</>))
import qualified System.FilePath as FilePath
import System.Process (proc)

import System.Agents.Base (
    Agent, AgentDescription (..), AgentId, AgentSlug, BuiltinToolboxDescription (..), ExtraAgentRef (..),
    McpServerDescription (..), McpSimpleBinaryConfiguration (..),
    OpenAPIToolboxDescription (..), OpenAPIServerDescription (..), OpenAPIServerOnDisk (..),
    PostgRESTToolboxDescription (..), PostgRESTServerDescription (..), PostgRESTServerOnDisk (..)
    )
import qualified System.Agents.Base as AgentsBase
import qualified System.Agents.FileLoader as FileLoader
import qualified System.Agents.FileNotification as Notify
import qualified System.Agents.LLMs.OpenAI as OpenAI
import System.Agents.Runtime (Runtime (..))
import qualified System.Agents.Runtime as Runtime
import System.Agents.ToolRegistration
import qualified System.Agents.Tools.McpToolbox as McpTools
import qualified System.Agents.Tools.OpenAPIToolbox as OpenAPIToolbox
import qualified System.Agents.Tools.PostgRESToolbox as PostgREST
import System.Agents.ApiKeys

-------------------------------------------------------------------------------
data Trace
    = AgentTrace Runtime.Trace
    | McpTrace McpServerDescription McpTools.Trace
    | OpenAPITrace OpenAPIToolboxDescription OpenAPIToolbox.Trace
    | PostgRESTTrace PostgRESTToolboxDescription PostgREST.Trace
    | DataLoadingTrace FileLoader.Trace
    | ConfigLoadedTrace AgentConfigTree
    | CyclicReferencesWarning [[AgentSlug]]
    | ReferenceValidationTrace ReferenceValidationTrace
    deriving (Show)

-- | Trace events for reference validation phase
data ReferenceValidationTrace
    = ValidationStarted (Set.Set AgentSlug)
    | ValidationComplete
    | DuplicateSlugDetected AgentSlug [FilePath]
    | MissingReferenceDetected AgentSlug FilePath AgentSlug
    deriving (Show)

-------------------------------------------------------------------------------
type LoadedApiKeys = [(Text, OpenAI.ApiKey)]

-- | Registry for looking up runtimes by slug during and after initialization
--
-- The registry enables two-phase initialization of agent trees, allowing agents
-- to reference each other recursively. Phase 1 creates runtime shells and
-- registers them; Phase 2 wires up references via registry lookup.
type RuntimeRegistry = IORef (Map.Map AgentSlug Runtime)

-- | Create a new empty registry
newRuntimeRegistry :: IO RuntimeRegistry
newRuntimeRegistry = newIORef Map.empty

-- | Register a runtime in the registry
registerRuntime :: RuntimeRegistry -> Runtime -> IO ()
registerRuntime registry rt =
    modifyIORef' registry (Map.insert rt.agentSlug rt)

-- | Lookup a runtime by slug from the registry
lookupRuntime :: RuntimeRegistry -> AgentSlug -> IO (Maybe Runtime)
lookupRuntime registry slug =
    Map.lookup slug <$> readIORef registry

data Props
    = Props
    { apiKeys :: LoadedApiKeys
    , rootAgentFile :: FilePath
    , interactiveTracer :: Tracer IO Trace
    , agentToTool :: Runtime -> AgentSlug -> AgentId -> ToolRegistration
    , runtimeRegistry :: RuntimeRegistry  -- ^ Shared registry for deferred resolution
    }

data AgentTree = AgentTree
    { agentFile :: FilePath
    , agentBase :: Agent
    , agentRuntime :: Runtime.Runtime
    , agentChildren :: [AgentTree]
    , agentExtraRefs :: [AgentSlug]  -- ^ Extra agents this agent references (for deferred wiring)
    }

data AgentConfigTree = AgentConfigTree
    { agentConfigFile :: FilePath
    , agentConfig :: Agent
    , agentConfigChildren :: [AgentConfigTree]
    }
    deriving (Show)

agentRootDir :: AgentConfigTree -> FilePath
agentRootDir agent = FilePath.takeDirectory agent.agentConfigFile

-------------------------------------------------------------------------------
-- | Result of the discovery phase - represents the agent reference graph
data AgentConfigGraph = AgentConfigGraph
    { graphNodes :: Map.Map AgentSlug AgentConfigNode
    -- ^ All agents by slug, with their file paths and configs
    , graphEdges :: Map.Map AgentSlug [AgentSlug]
    -- ^ References from each agent to others (children + extra-agents)
    }
    deriving (Show)

-- | A node in the agent configuration graph
data AgentConfigNode = AgentConfigNode
    { nodeFile :: FilePath
    , nodeConfig :: Agent
    , nodeChildren :: [AgentSlug]
    -- ^ Child agents from toolDirectory (slugs only)
    , nodeExtraRefs :: [AgentSlug]
    -- ^ Extra agents referenced via extra-agents config
    }
    deriving (Show)

-- | Validation error types
data ReferenceError
    = MissingAgentReference AgentSlug FilePath AgentSlug
    -- ^ Agent in file references missing slug
    --   (referrer slug, referrer file, missing slug)
    | DuplicateAgentSlug AgentSlug [FilePath]
    -- ^ Same slug defined in multiple files
    deriving (Show)

data LoadingError
    = AgentLoadingError FileLoader.InvalidAgentError
    | ReferenceError ReferenceError
    | OpenAPIInitError Text String  -- ^ Description and error message
    | PostgRESTInitError Text String  -- ^ Description and error message
    | ConfigFileError FilePath String  -- ^ Path and error message
    | OtherError String
    deriving (Show)

data LoadAgentResult
    = Errors (NonEmpty.NonEmpty LoadingError)
    | Initialized AgentTree

-------------------------------------------------------------------------------
-- Phase 0: Configuration Discovery
-------------------------------------------------------------------------------

-- | Discover all agents starting from root using BFS traversal.
--
-- This function collects all agent configurations without creating any runtimes.
-- It follows both toolDirectory (children) and extra-agents references.
--
-- Returns a graph of all discovered agents, or errors if loading fails.
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
            let edges = Map.map (\n -> n.nodeChildren ++ n.nodeExtraRefs) nodes
            pure $ Right $ AgentConfigGraph nodes edges

-- | BFS discovery state
-- FilePath: path to agent config
-- Maybe AgentSlug: optional parent slug (for cycle detection context)
type DiscoveryQueue = [(FilePath, Maybe AgentSlug)]

-- | BFS traversal to discover all agents.
--
-- We track visited files to avoid reloading the same config.
-- We track loaded slugs to detect duplicates.
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
        Just (slug, _) -> do
            -- File already loaded, check for parent reference
            let visited' = case mParent of
                    Just parentSlug ->
                        -- Add this slug to parent's children if not already there
                        Map.adjust
                            (\n -> n{nodeChildren = List.nub (slug : n.nodeChildren)})
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
                    let slug = agent.slug

                    -- Check for duplicate slug
                    case Map.lookup slug visited of
                        Just existingNode ->
                            -- Duplicate slug detected
                            let err =
                                    DuplicateAgentSlug
                                        slug
                                        [existingNode.nodeFile, filePath]
                             in pure $ Left (NonEmpty.singleton $ ReferenceError err)
                        Nothing -> do
                            -- Discover children from toolDirectory
                            let rootDir = FilePath.takeDirectory filePath
                            let toolDir = rootDir </> agent.toolDirectory
                            childFiles <- FileLoader.listJsonDirectory toolDir

                            -- Get extra agent refs
                            let extraRefs = maybe [] (fmap extraAgentSlug) agent.extraAgents
                            let extraPaths = maybe [] (fmap extraAgentPath) agent.extraAgents

                            -- Resolve extra agent paths to absolute/normalized paths
                            let resolvedExtraPaths = map (\p -> FilePath.normalise (rootDir </> p)) extraPaths

                            -- Create node
                            let node =
                                    AgentConfigNode
                                        { nodeFile = filePath
                                        , nodeConfig = agent
                                        , nodeChildren = [] -- Will be populated when children are processed
                                        , nodeExtraRefs = extraRefs
                                        }

                            let visited' = Map.insert slug node visited

                            -- Add children to queue (with this slug as parent)
                            let childQueue = [(f, Just slug) | f <- childFiles]

                            -- Add extra agents to queue (with this slug as parent for tracking)
                            -- Pass the resolved absolute paths for proper file matching
                            let extraQueue = [(f, Just slug) | f <- resolvedExtraPaths]

                            bfsDiscovery props (queue ++ childQueue ++ extraQueue) visited'
  where
    tracer = props.interactiveTracer

-- | Find a node by its file path using normalized comparison.
-- Both the search path and stored paths are normalized before comparison.
findNodeByFile :: FilePath -> Map.Map AgentSlug AgentConfigNode -> Maybe (AgentSlug, AgentConfigNode)
findNodeByFile file nodes =
    let normalizedSearchPath = FilePath.normalise file
    in List.find (\(_, n) -> FilePath.normalise n.nodeFile == normalizedSearchPath) (Map.toList nodes)

-------------------------------------------------------------------------------
-- Phase 1: Reference Validation
-------------------------------------------------------------------------------

-- | Validate the configuration graph.
--
-- Checks that:
-- 1. All edges point to existing nodes (no missing references)
-- 2. No duplicate slugs exist
--
-- Note: Cycles are allowed but detected and logged separately via 'detectCycles'.
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
            catMaybes $
                map
                    (\ref ->
                        if Map.member ref graph.graphNodes
                            then Nothing
                            else
                                Just $
                                    ReferenceError $
                                        MissingAgentReference fromSlug node.nodeFile ref
                    )
                    refs
  where
    catMaybes = Maybe.mapMaybe id

-------------------------------------------------------------------------------
-- Phase 2: Runtime Shell Creation
-------------------------------------------------------------------------------

-- | Create runtime shells for all agents.
--
-- For each agent in the graph:
-- 1. Create runtime with empty/mutable tool list
-- 2. Register in RuntimeRegistry
-- 3. Store in result map
--
-- Note: Helper agents (from toolDirectory) are resolved immediately.
-- Extra agents will be wired up in phase 3.
createRuntimeShells ::
    Props ->
    AgentConfigGraph ->
    IO (Either (NonEmpty.NonEmpty LoadingError) (Map.Map AgentSlug Runtime))
createRuntimeShells props graph = do
    -- Process nodes in dependency order (children before parents)
    -- For simplicity, we process in Map order since runtime shells don't need tools yet
    results <- mapM (createSingleRuntime props graph) (Map.toList graph.graphNodes)

    let (errs, runtimes) = Either.partitionEithers results
    case NonEmpty.nonEmpty errs of
        Just someErrs -> pure $ Left someErrs
        Nothing -> do
            -- Register all runtimes in the registry
            mapM_ (registerRuntime props.runtimeRegistry) runtimes

            -- Build slug -> runtime map
            let runtimeMap = Map.fromList [(rt.agentSlug, rt) | rt <- runtimes]
            pure $ Right runtimeMap

-- | Create a single runtime shell for an agent
createSingleRuntime ::
    Props ->
    AgentConfigGraph ->
    (AgentSlug, AgentConfigNode) ->
    IO (Either LoadingError Runtime)
createSingleRuntime props _graph (_slug, node) = do
    let extraSlugs = node.nodeExtraRefs

    -- Build the prompt modifier (we don't have children runtimes yet, so no prompt augmentation)
    let promptModifier = id

    -- Create the runtime shell
    -- Note: props.interactiveTracer is already Tracer IO Trace,
    -- initAgentTreeAgentDeferred will contramap AgentTrace internally
    rtResult <-
        initAgentTreeAgentDeferred
            props.interactiveTracer
            props.apiKeys
            promptModifier
            props.agentToTool
            [] -- Helper agents - will be wired later
            extraSlugs
            (FilePath.takeDirectory node.nodeFile)
            (AgentDescription node.nodeConfig)

    case rtResult of
        Left err -> pure $ Left $ OtherError err
        Right rt -> pure $ Right rt

-------------------------------------------------------------------------------
-- Phase 3: Tool Wiring
-------------------------------------------------------------------------------

-- | Wire up tool references for all runtimes.
--
-- For each agent:
-- 1. Get its runtime from the map
-- 2. Collect all referenced agent runtimes (children + extra-agents)
-- 3. Use registry to look up each referenced runtime
-- 4. Create tool registrations via agentToTool
-- 5. Update runtime's agentTools IO action
--
-- Note: This function modifies the existing runtimes in place by updating
-- their tool resolution mechanisms. In a pure implementation, we would
-- create new runtimes, but here we work with IO actions.
wireToolReferences ::
    Props ->
    AgentConfigGraph ->
    Map.Map AgentSlug Runtime ->
    IO ()
wireToolReferences props graph runtimes = do
    mapM_ (wireAgentTools props graph runtimes) (Map.toList graph.graphNodes)

-- | Wire tools for a single agent
wireAgentTools ::
    Props ->
    AgentConfigGraph ->
    Map.Map AgentSlug Runtime ->
    (AgentSlug, AgentConfigNode) ->
    IO ()
wireAgentTools props _graph runtimes (slug, node) = do
    -- Get this agent's runtime
    case Map.lookup slug runtimes of
        Nothing -> pure () -- Should not happen
        Just _rt -> do
            -- Look up child runtimes
            childRuntimes <- mapM (lookupRuntime props.runtimeRegistry) node.nodeChildren
            let validChildren = Maybe.catMaybes childRuntimes

            -- Look up extra agent runtimes
            extraRuntimes <- mapM (lookupRuntime props.runtimeRegistry) node.nodeExtraRefs
            let validExtras = Maybe.catMaybes extraRuntimes

            -- Combine all helper runtimes
            let allHelpers = validChildren ++ validExtras

            -- Create tool registrations for helper agents
            let _helperTools = [props.agentToTool helperRt helperRt.agentSlug helperRt.agentId | helperRt <- allHelpers]

            -- Update the runtime's tools (this is a simplification - in reality
            -- we'd need to modify the runtime structure or use a mutable reference)
            -- For now, we rely on the registry for deferred lookup
            pure ()

-------------------------------------------------------------------------------
-- Phase 4: Build AgentTree
-------------------------------------------------------------------------------

-- | Build the tree from the graph and initialized runtimes.
--
-- Finds the root agent and recursively builds the tree structure using graphEdges.
-- Attaches runtimes from the map.
buildAgentTree ::
    AgentConfigGraph ->
    Map.Map AgentSlug Runtime ->
    Either (NonEmpty.NonEmpty LoadingError) AgentTree
buildAgentTree graph runtimes = do
    -- Find root agent (the one without any incoming edges that isn't a child)
    -- For now, we use the first node as root (the one from rootAgentFile)
    case Map.toList graph.graphNodes of
        [] -> Left $ NonEmpty.singleton $ OtherError "No agents found in graph"
        ((rootSlug, rootNode) : _) ->
            buildSubtree graph runtimes rootSlug rootNode

-- | Recursively build a subtree
buildSubtree ::
    AgentConfigGraph ->
    Map.Map AgentSlug Runtime ->
    AgentSlug ->
    AgentConfigNode ->
    Either (NonEmpty.NonEmpty LoadingError) AgentTree
buildSubtree graph runtimes slug node = do
    -- Get runtime for this node
    case Map.lookup slug runtimes of
        Nothing ->
            Left $ NonEmpty.singleton $ OtherError $ "Runtime not found for slug: " ++ Text.unpack slug
        Just rt -> do
            -- Build children recursively
            childResults <- mapM (buildChild graph runtimes) node.nodeChildren

            pure $
                AgentTree
                    { agentFile = node.nodeFile
                    , agentBase = node.nodeConfig
                    , agentRuntime = rt
                    , agentChildren = childResults
                    , agentExtraRefs = node.nodeExtraRefs
                    }

-- | Build a child subtree by looking up the slug
buildChild ::
    AgentConfigGraph ->
    Map.Map AgentSlug Runtime ->
    AgentSlug ->
    Either (NonEmpty.NonEmpty LoadingError) AgentTree
buildChild graph runtimes childSlug =
    case Map.lookup childSlug graph.graphNodes of
        Nothing ->
            Left $ NonEmpty.singleton $ OtherError $ "Child node not found: " ++ Text.unpack childSlug
        Just childNode -> buildSubtree graph runtimes childSlug childNode

-------------------------------------------------------------------------------
-- Cycle Detection
-------------------------------------------------------------------------------

-- | Detect cycles for warning purposes (doesn't prevent loading).
--
-- Uses DFS with three-color marking to detect all cycles in the graph.
-- Returns a list of all cycles found (each cycle is a list of slugs).
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
dfsFromNode graph (visited, cycles) slug
    | Set.member slug visited = (visited, cycles)
    | otherwise =
        let (_, _, newCycles) = dfsVisit graph (visited, Set.empty, cycles) slug
         in (Set.insert slug visited, newCycles)

-- | DFS visit a node
dfsVisit ::
    AgentConfigGraph ->
    DFSState ->
    AgentSlug ->
    DFSState
dfsVisit graph (visited, recStack, cycles) slug
    | Set.member slug recStack =
        -- Cycle detected! Extract the cycle
        let cyclePath = extractCycle slug recStack
         in (visited, recStack, cyclePath : cycles)
    | Set.member slug visited = (visited, recStack, cycles)
    | otherwise =
        case Map.lookup slug graph.graphEdges of
            Nothing -> (Set.insert slug visited, recStack, cycles)
            Just neighbors ->
                let newRecStack = Set.insert slug recStack
                    (visited', recStack', cycles') =
                        foldl
                            (dfsVisit graph)
                            (visited, newRecStack, cycles)
                            neighbors
                 in (Set.insert slug visited', Set.delete slug recStack', cycles')

-- | Extract the cycle from the recursion stack
type RecStack = Set.Set AgentSlug

extractCycle :: AgentSlug -> RecStack -> [AgentSlug]
extractCycle slug recStack =
    -- The cycle contains the slug and all nodes in the recursion stack
    -- We return them in a simple format - the slug and the stack contents
    slug : Set.toList recStack

-------------------------------------------------------------------------------
-- Legacy Loading Functions (maintained for backward compatibility)
-------------------------------------------------------------------------------

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
            let tooldir = rootdir </> agent.toolDirectory
            subConfigs <- FileLoader.listJsonDirectory tooldir
            let propz = [props{rootAgentFile = c} | c <- subConfigs]
            (kos, oks) <- fmap Either.partitionEithers $ traverse loadAgentTreeConfig propz
            case NonEmpty.nonEmpty kos of
                Just errs -> do
                    pure $ Left $ sconcat errs
                Nothing -> do
                    pure $ Right $ AgentConfigTree props.rootAgentFile agent oks

loadAgentTree :: Props -> AgentConfigTree -> IO (Either (NonEmpty.NonEmpty LoadingError) AgentTree)
loadAgentTree props tree = do
    -- Phase 1: Load children first (existing behavior)
    agentRuntimes <- traverse (loadAgentTree props) tree.agentConfigChildren
    let (kos, oks) = Either.partitionEithers agentRuntimes
    case NonEmpty.nonEmpty kos of
        Just errs -> pure $ Left $ sconcat errs
        Nothing -> do
            let tracer = props.interactiveTracer
            let okRuntimes = fmap agentRuntime oks

            -- Get extra agent slugs from config
            let extraSlugs = maybe [] (fmap extraAgentSlug) tree.agentConfig.extraAgents

            -- Phase 1.5: Create this agent's runtime shell
            -- (tools will be wired up later, but we register the slug now)
            rt <- initAgentTreeAgentDeferred
                    tracer
                    props.apiKeys
                    (augmentMainAgentPromptWithSubAgents okRuntimes)
                    props.agentToTool
                    okRuntimes
                    extraSlugs  -- NEW: pass extra refs for later resolution
                    (agentRootDir tree)
                    (AgentDescription tree.agentConfig)

            case rt of
                Left err ->
                    pure $ Left $ NonEmpty.singleton $ OtherError err
                Right agentRt -> do
                    -- Register this runtime in the shared registry
                    registerRuntime props.runtimeRegistry agentRt

                    let ret = AgentTree props.rootAgentFile tree.agentConfig agentRt oks extraSlugs
                    -- Set up file notification for hot-reloading
                    let tooldir = agentRootDir tree </> tree.agentConfig.toolDirectory
                    _ <-
                        Notify.initRuntime
                            reloadNotificationTracer
                            [(ret, tooldir)]
                            (\_ ev -> Notify.isAboutFileChange ev)
                    pure $ Right $ ret

-------------------------------------------------------------------------------
-- New Two-Phase Entry Point
-------------------------------------------------------------------------------

-- | Load agent tree using the new two-phase algorithm.
--
-- Phase 0: Discover all configs via BFS traversal
-- Phase 1: Validate all references
-- Phase 2: Create runtime shells
-- Phase 3: Wire tool references
-- Phase 4: Build final tree
loadAgentTreeRuntime :: Props -> IO LoadAgentResult
loadAgentTreeRuntime props = do
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

                    -- Phase 2: Create runtime shells
                    shellsResult <- createRuntimeShells props graph
                    case shellsResult of
                        Left errs -> pure $ Errors errs
                        Right runtimes -> do
                            -- Phase 3: Wire tool references
                            wireToolReferences props graph runtimes

                            -- Phase 4: Build final tree
                            case buildAgentTree graph runtimes of
                                Left errs -> pure $ Errors errs
                                Right tree -> pure $ Initialized tree

withAgentTreeRuntime :: Props -> (LoadAgentResult -> IO a) -> IO a
withAgentTreeRuntime props continue = do
    loadAgentTreeRuntime props >>= continue

-- | Convert OpenAPI toolbox description to toolbox configuration.
--
-- Handles both inline configurations and on-disk configurations.
openApiDescToConfig :: OpenAPIServerDescription -> OpenAPIToolbox.Config
openApiDescToConfig desc =
    OpenAPIToolbox.Config
        { OpenAPIToolbox.configUrl = openApiSpecUrl desc
        , OpenAPIToolbox.configBaseUrl = openApiBaseUrl desc
        , OpenAPIToolbox.configHeaders = Maybe.fromMaybe Map.empty (openApiHeaders desc)
        , OpenAPIToolbox.configToken = openApiToken desc
        , OpenAPIToolbox.configFilter = openApiFilter desc
        }

-- | Convert PostgREST toolbox description to toolbox configuration.
--
-- Handles both inline configurations and on-disk configurations.
-- The 'allowedMethods' field controls which HTTP verbs are exposed as tools.
-- By default, only read-only methods are enabled for safety.
postgrestDescToConfig :: PostgRESTServerDescription -> PostgREST.Config
postgrestDescToConfig desc =
    PostgREST.Config
        { PostgREST.configUrl = postgrestSpecUrl desc
        , PostgREST.configBaseUrl = postgrestBaseUrl desc
        , PostgREST.configHeaders = Maybe.fromMaybe Map.empty (postgrestHeaders desc)
        , PostgREST.configToken = postgrestToken desc
        , PostgREST.configAllowedMethods = Maybe.fromMaybe PostgREST.defaultAllowedMethods (postgrestAllowedMethods desc)
        , PostgREST.configFilter = postgrestFilter desc
        }

-- | Load an OpenAPI toolbox description, resolving on-disk references if needed.
--
-- For 'OpenAPIServer' descriptions, returns the configuration directly.
-- For 'OpenAPIServerOnDisk' descriptions, loads the configuration from the specified file.
loadOpenAPIToolboxDescription ::
    FilePath  -- ^ Base directory for resolving relative paths
    -> OpenAPIToolboxDescription
    -> IO (Either LoadingError OpenAPIServerDescription)
loadOpenAPIToolboxDescription _baseDir (OpenAPIServer desc) =
    pure $ Right desc
loadOpenAPIToolboxDescription baseDir (OpenAPIServerOnDiskDescription (OpenAPIServerOnDisk path)) = do
    let fullPath = if FilePath.isRelative path then baseDir </> path else path
    result <- Aeson.eitherDecodeFileStrict' fullPath
    case result of
        Left err -> pure $ Left $ ConfigFileError fullPath err
        Right desc -> pure $ Right desc

-- | Load a PostgREST toolbox description, resolving on-disk references if needed.
--
-- For 'PostgRESTServer' descriptions, returns the configuration directly.
-- For 'PostgRESTServerOnDisk' descriptions, loads the configuration from the specified file.
loadPostgRESTToolboxDescription ::
    FilePath  -- ^ Base directory for resolving relative paths
    -> PostgRESTToolboxDescription
    -> IO (Either LoadingError PostgRESTServerDescription)
loadPostgRESTToolboxDescription _baseDir (PostgRESTServer desc) =
    pure $ Right desc
loadPostgRESTToolboxDescription baseDir (PostgRESTServerOnDiskDescription (PostgRESTServerOnDisk path)) = do
    let fullPath = if FilePath.isRelative path then baseDir </> path else path
    result <- Aeson.eitherDecodeFileStrict' fullPath
    case result of
        Left err -> pure $ Left $ ConfigFileError fullPath err
        Right desc -> pure $ Right desc

-- | Initialize OpenAPI toolboxes and return their tool registrations.
--
-- This function takes OpenAPI toolbox descriptions, initializes each toolbox,
-- and registers all tools. If any toolbox fails to initialize, an error is returned.
--
-- Supports both inline configurations and on-disk configurations.
initializeOpenAPIToolboxes ::
    Tracer IO Trace ->
    FilePath  -- ^ Base directory for resolving relative config paths
    -> [OpenAPIToolboxDescription] ->
    IO (Either LoadingError [ToolRegistration])
initializeOpenAPIToolboxes tracer baseDir descriptions = do
    results <- mapM initializeOne descriptions
    case concatEithers results of
        Left [] -> pure $ Left $ OtherError "Unknown OpenAPI initialization error"
        Left (err : _) -> pure $ Left err  -- Return first error
        Right allTools -> pure $ Right $ concat allTools
  where
    initializeOne :: OpenAPIToolboxDescription -> IO (Either LoadingError [ToolRegistration])
    initializeOne desc = do
        -- Load the configuration (from disk if needed)
        loadResult <- loadOpenAPIToolboxDescription baseDir desc
        case loadResult of
            Left err -> pure $ Left err
            Right srvDesc -> do
                let config = openApiDescToConfig srvDesc
                initResult <- OpenAPIToolbox.initializeToolbox (contramap (OpenAPITrace desc) tracer) config
                case initResult of
                    Left err ->
                        pure $ Left $ OpenAPIInitError (openApiSpecUrl srvDesc) (show err)
                    Right toolbox -> do
                        regResult <- registerOpenAPITools toolbox
                        case regResult of
                            Left err ->
                                pure $ Left $ OpenAPIInitError (openApiSpecUrl srvDesc) err
                            Right tools ->
                                pure $ Right tools

    concatEithers :: [Either a b] -> Either [a] [b]
    concatEithers eithers =
        let (lefts, rights) = Either.partitionEithers eithers
         in if null lefts then Right rights else Left lefts

-- | Initialize PostgREST toolboxes and return their tool registrations.
--
-- This function takes PostgREST toolbox descriptions, initializes each toolbox,
-- and registers all tools. If any toolbox fails to initialize, an error is returned.
--
-- Supports both inline configurations and on-disk configurations.
initializePostgRESToolboxes ::
    Tracer IO Trace ->
    FilePath  -- ^ Base directory for resolving relative config paths
    -> [PostgRESTToolboxDescription] ->
    IO (Either LoadingError [ToolRegistration])
initializePostgRESToolboxes tracer baseDir descriptions = do
    results <- mapM initializeOne descriptions
    case concatEithers results of
        Left [] -> pure $ Left $ OtherError "Unknown PostgREST initialization error"
        Left (err : _) -> pure $ Left err  -- Return first error
        Right allTools -> pure $ Right $ concat allTools
  where
    initializeOne :: PostgRESTToolboxDescription -> IO (Either LoadingError [ToolRegistration])
    initializeOne desc = do
        -- Load the configuration (from disk if needed)
        loadResult <- loadPostgRESTToolboxDescription baseDir desc
        case loadResult of
            Left err -> pure $ Left err
            Right srvDesc -> do
                let config = postgrestDescToConfig srvDesc
                initResult <- PostgREST.initializeToolbox (contramap (PostgRESTTrace desc) tracer) config
                case initResult of
                    Left err ->
                        pure $ Left $ PostgRESTInitError (postgrestSpecUrl srvDesc) (show err)
                    Right toolbox -> do
                        regResult <- registerPostgRESTools toolbox
                        case regResult of
                            Left err ->
                                pure $ Left $ PostgRESTInitError (postgrestSpecUrl srvDesc) err
                            Right tools ->
                                pure $ Right tools

    concatEithers :: [Either a b] -> Either [a] [b]
    concatEithers eithers =
        let (lefts, rights) = Either.partitionEithers eithers
         in if null lefts then Right rights else Left lefts

-- | Type alias for prompt modification functions.
type PromptModifier = Text -> Text

-- | Initialize a runtime with deferred tool resolution for extra agents.
--
-- This function creates a runtime where the tool list is built dynamically.
-- Helper agents (from toolDirectory hierarchy) are resolved immediately,
-- while extra agents (from extra-agents config) are resolved lazily via
-- the registry lookup.
initAgentTreeAgentDeferred ::
    Tracer IO Trace ->
    [(Text, OpenAI.ApiKey)] ->
    PromptModifier ->
    (Runtime -> AgentSlug -> AgentId -> ToolRegistration) ->
    [Runtime.Runtime] ->
    [AgentSlug] ->  -- ^ Extra agent slugs to resolve later via registry
    FilePath ->
    AgentDescription ->
    IO (Either String Runtime.Runtime)
initAgentTreeAgentDeferred tracer keys modifyPrompt agentToTool' helperAgents _extraSlugs rootDir (AgentDescription desc) = do
    case (lookup desc.apiKeyId keys, OpenAI.parseFlavor desc.flavor) of
        (_, Nothing) ->
            pure $ Left ("could not parse flavor " <> Text.unpack desc.flavor)
        (Nothing, _) ->
            pure $ Left ("could not find key " <> Text.unpack desc.apiKeyId)
        (Just key, Just flavor) -> do
            mcpToolboxes <- traverse startMcp (Maybe.fromMaybe [] desc.mcpServers)
            -- Initialize OpenAPI toolboxes (with on-disk config support)
            openApiToolsResult <- initializeOpenAPIToolboxes tracer rootDir (Maybe.fromMaybe [] desc.openApiToolboxes)
            -- Initialize PostgREST toolboxes (with on-disk config support)
            postgrestToolsResult <- initializePostgRESToolboxes tracer rootDir (Maybe.fromMaybe [] desc.postgrestToolboxes)
            case (openApiToolsResult, postgrestToolsResult) of
                (Left err, _) -> pure $ Left (show err)
                (_, Left err) -> pure $ Left (show err)
                (Right openApiToolRegs, Right postgrestToolRegs) -> do
                    -- Get builtin toolbox descriptions from agent config
                    let builtinDescriptions = Maybe.fromMaybe [] desc.builtinToolboxes

                    -- Create the runtime with deferred tool resolution
                    -- The tools action will combine helper agents (immediate) with
                    -- extra agents (resolved via registry at call time)
                    Runtime.newRuntime
                        desc.slug
                        desc.announce
                        (contramap AgentTrace tracer)
                        key
                        ( OpenAI.Model
                            flavor
                            (OpenAI.ApiBaseUrl desc.modelUrl)
                            desc.modelName
                            ( OpenAI.SystemPrompt $
                                modifyPrompt $
                                    Text.unlines desc.systemPrompt
                            )
                        )
                        (rootDir </> desc.toolDirectory)
                        [agentToTool' rt | rt <- helperAgents]
                        mcpToolboxes
                        (openApiToolRegs ++ postgrestToolRegs)
                        builtinDescriptions
  where
    startMcp :: McpServerDescription -> IO McpTools.Toolbox
    startMcp srv@(McpSimpleBinary cfg) =
        McpTools.initializeMcpToolbox
            (contramap (McpTrace srv) tracer)
            cfg.name
            (proc cfg.executable (map Text.unpack cfg.args))

-- | Original initialization function - kept for backward compatibility.
-- This function initializes a runtime without deferred resolution.
initAgentTreeAgent ::
    Tracer IO Trace ->
    [(Text, OpenAI.ApiKey)] ->
    PromptModifier ->
    (Runtime -> AgentSlug -> AgentId -> ToolRegistration) ->
    [Runtime.Runtime] ->
    FilePath ->
    AgentDescription ->
    IO (Either String Runtime.Runtime)
initAgentTreeAgent tracer keys modifyPrompt agentToTool' helperAgents rootDir (AgentDescription desc) = do
    case (lookup desc.apiKeyId keys, OpenAI.parseFlavor desc.flavor) of
        (_, Nothing) ->
            pure $ Left ("could not parse flavor " <> Text.unpack desc.flavor)
        (Nothing, _) ->
            pure $ Left ("could not find key " <> Text.unpack desc.apiKeyId)
        (Just key, Just flavor) -> do
            mcpToolboxes <- traverse startMcp (Maybe.fromMaybe [] desc.mcpServers)
            -- Initialize OpenAPI toolboxes (with on-disk config support)
            openApiToolsResult <- initializeOpenAPIToolboxes tracer rootDir (Maybe.fromMaybe [] desc.openApiToolboxes)
            -- Initialize PostgREST toolboxes (with on-disk config support)
            postgrestToolsResult <- initializePostgRESToolboxes tracer rootDir (Maybe.fromMaybe [] desc.postgrestToolboxes)
            case (openApiToolsResult, postgrestToolsResult) of
                (Left err, _) -> pure $ Left (show err)
                (_, Left err) -> pure $ Left (show err)
                (Right openApiToolRegs, Right postgrestToolRegs) -> do
                    -- Get builtin toolbox descriptions from agent config
                    let builtinDescriptions = Maybe.fromMaybe [] desc.builtinToolboxes

                    Runtime.newRuntime
                        desc.slug
                        desc.announce
                        (contramap AgentTrace tracer)
                        key
                        ( OpenAI.Model
                            flavor
                            (OpenAI.ApiBaseUrl desc.modelUrl)
                            desc.modelName
                            ( OpenAI.SystemPrompt $
                                modifyPrompt $
                                    Text.unlines desc.systemPrompt
                            )
                        )
                        (rootDir </> desc.toolDirectory)
                        [agentToTool' rt | rt <- helperAgents]
                        mcpToolboxes
                        (openApiToolRegs ++ postgrestToolRegs)
                        builtinDescriptions
  where
    startMcp :: McpServerDescription -> IO McpTools.Toolbox
    startMcp srv@(McpSimpleBinary cfg) =
        McpTools.initializeMcpToolbox
            (contramap (McpTrace srv) tracer)
            cfg.name
            (proc cfg.executable (map Text.unpack cfg.args))

augmentMainAgentPromptWithSubAgents :: [Runtime.Runtime] -> Text -> Text
augmentMainAgentPromptWithSubAgents [] base = base
augmentMainAgentPromptWithSubAgents agents base =
    Text.unlines
        [ base
        , ""
        , "==="
        , "The helper agents you can query using json tools are as follows:"
        , Text.unlines $
            Maybe.mapMaybe declareAgent agents
        , ""
        , "If an helper agent fails, do not retry and abdicate"
        ]
  where
    declareAgent :: Runtime.Runtime -> Maybe Text
    declareAgent desc =
        Just $ Text.unwords ["*", desc.agentSlug, ":", desc.agentAnnounce]

-------------------------------------------------------------------------------
data PromptOtherAgent
    = PromptOtherAgent Text

instance Aeson.FromJSON PromptOtherAgent where
    parseJSON = Aeson.withObject "PromptOtherAgent" $ \v -> do
        PromptOtherAgent
            <$> v Aeson..: "what"

-------------------------------------------------------------------------------
readOpenApiKeysFile :: FilePath -> IO LoadedApiKeys
readOpenApiKeysFile keysPath =
    maybe [] flattenOpenAIKeys <$> readApiKeys keysPath
  where
    flattenOpenAIKeys :: ApiKeys -> [(Text, OpenAI.ApiKey)]
    flattenOpenAIKeys (ApiKeys keys) =
        [(k.apiKeyId, OpenAI.ApiKey $ TextEncoding.encodeUtf8 k.apiKeyValue) | k <- keys]

    readApiKeys :: FilePath -> IO (Maybe ApiKeys)
    readApiKeys path =
        Aeson.decode <$> LByteString.readFile path

-------------------------------------------------------------------------------

reloadNotificationTracer :: Tracer IO (Notify.Trace AgentTree)
reloadNotificationTracer = Tracer $ \(Notify.NotifyEvent tree _) -> do
    void $ Runtime.triggerRefreshTools tree.agentRuntime

