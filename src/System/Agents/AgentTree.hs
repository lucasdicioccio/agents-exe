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
    
    -- * Loading and initialization
    loadAgentTreeConfig,
    loadAgentTree,
    loadAgentTreeRuntime,
    LoadAgentResult (..),
    withAgentTreeRuntime,
    LoadingError (..),
    
    -- * Agent initialization
    initAgentTreeAgent,
    initAgentTreeAgentDeferred,
    PromptModifier,
    
    -- * Tracing
    Trace (..),
    
    -- * Utility functions
    readOpenApiKeysFile,
    augmentMainAgentPromptWithSubAgents,
) where

import Control.Monad (void)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Either as Either
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Semigroup (sconcat)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Prod.Tracer (Tracer (..), contramap)
import System.FilePath ((</>))
import qualified System.FilePath as FilePath
import System.Process (proc)

import System.Agents.Base (Agent, AgentDescription (..), AgentId, AgentSlug, ExtraAgentRef (..), McpServerDescription (..))
import qualified System.Agents.Base as AgentsBase
import qualified System.Agents.FileLoader as FileLoader
import qualified System.Agents.FileNotification as Notify
import qualified System.Agents.LLMs.OpenAI as OpenAI
import System.Agents.Runtime (Runtime (..))
import qualified System.Agents.Runtime as Runtime
import System.Agents.ToolRegistration
import qualified System.Agents.Tools.McpToolbox as McpTools

import System.Agents.ApiKeys

-------------------------------------------------------------------------------
data Trace
    = AgentTrace Runtime.Trace
    | McpTrace McpServerDescription McpTools.Trace
    | DataLoadingTrace FileLoader.Trace
    | ConfigLoadedTrace AgentConfigTree
    deriving
        ( -- | AgentInitialized AgentConfigTree Runtime
          Show
        )

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
data LoadingError
    = AgentLoadingError FileLoader.InvalidAgentError
    | OtherError String
    deriving (Show)

data LoadAgentResult
    = Errors (NonEmpty.NonEmpty LoadingError)
    | Initialized AgentTree

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

loadAgentTreeRuntime :: Props -> IO LoadAgentResult
loadAgentTreeRuntime props = do
    cfgs <- loadAgentTreeConfig props
    case cfgs of
        Left errs -> pure $ Errors errs
        Right cfg -> do
            runTracer props.interactiveTracer (ConfigLoadedTrace cfg)
            tree <- loadAgentTree props cfg
            case tree of
                Left errs -> pure $ Errors errs
                Right ok -> pure $ Initialized ok

withAgentTreeRuntime :: Props -> (LoadAgentResult -> IO a) -> IO a
withAgentTreeRuntime props continue = do
    loadAgentTreeRuntime props >>= continue

-------------------------------------------------------------------------------

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
initAgentTreeAgentDeferred tracer keys modifyPrompt agentToTool' helperAgents extraSlugs rootDir (AgentDescription desc) = do
    case (lookup desc.apiKeyId keys, OpenAI.parseFlavor desc.flavor) of
        (_, Nothing) ->
            pure $ Left ("could not parse flavor " <> Text.unpack desc.flavor)
        (Nothing, _) ->
            pure $ Left ("could not find key " <> Text.unpack desc.apiKeyId)
        (Just key, Just flavor) -> do
            mcpToolboxes <- traverse startMcp (Maybe.fromMaybe [] desc.mcpServers)
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
                -- Build tool registrations: helper agents are immediate,
                -- extra agents will be resolved lazily via the registry
                [agentToTool' rt | rt <- helperAgents]
                mcpToolboxes
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
        [(k.apiKeyId, OpenAI.ApiKey $ Text.encodeUtf8 k.apiKeyValue) | k <- keys]

    readApiKeys :: FilePath -> IO (Maybe ApiKeys)
    readApiKeys path =
        Aeson.decode <$> LByteString.readFile path

-------------------------------------------------------------------------------

reloadNotificationTracer :: Tracer IO (Notify.Trace AgentTree)
reloadNotificationTracer = Tracer $ \(Notify.NotifyEvent tree _) -> do
    void $ Runtime.triggerRefreshTools tree.agentRuntime

