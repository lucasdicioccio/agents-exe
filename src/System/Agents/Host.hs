{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Everything a long-lived process needs to run agents without the TUI:
the loaded agents, the database, and the stores built on it.

'withHost' loads each root agent file, opens (and migrates) one SQLite
database for sessions and continuations, and wires sub-agent tools so that
sub-agents store their sessions in that database, labelled with their
parent. Root agents are built with 'hostDeps', which stores nothing: the
session runner ("System.Agents.Host.Runner") stores their sessions itself,
with versioned writes.
-}
module System.Agents.Host (
    Host (..),
    HostConfig (..),
    defaultHostConfig,
    TemplateLibraries,
    standardLibraries,
    HostTrace (..),
    HostError (..),
    withHost,
    HostStores (..),
    withHostStores,

    -- * Agents
    StoredAgents,
    noStoredAgents,
    AgentSource (..),
    hostAllAgents,
    lookupAgent,
    AgentEditError (..),
    formatHelperError,
    putStoredAgent,
    putStoredAgentWithFiles,
    deleteStoredAgent,
    setStoredNodeRetirement,
) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO, writeTVar)
import Control.Exception (Exception, IOException, throwIO, try)
import Control.Monad (forM_, zipWithM)
import Data.Foldable (toList)
import Data.IORef (atomicModifyIORef', newIORef)
import Data.List (partition)
import Data.Maybe (isNothing)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (NominalDiffTime, getCurrentTime)
import Database.SQLite.Simple (Only (..), query_, withConnection)
import Prod.Tracer (Tracer, contramap, runTracer)
import System.Directory (removePathForcibly)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import System.Agents.AgentFactory (AgentDeps (..), Completion, SessionSink (..), defaultAgentDeps)
import qualified System.Agents.AgentFactory as AgentFactory
import System.Agents.AgentStore (AgentStore (..), HelperError (..), StoredAgent (..), ToolFiles, fileBasedFields, materializeToolFiles, mkSqliteAgentStore, referencedSlugs, resolveHelpers, storedPathErrors)
import System.Agents.AgentTree (LoadAgentResult (..), OSAgentNode (..), OSAgentTree (..), Props (..), formatLoadingError, loadAgentTreeFromConfigs, readOpenApiKeysFile, releaseAgentNode, withAgentTree)
import System.Agents.ApiKeys (LoadedApiKeys, readOpenApiKeysFileStrict)
import qualified System.Agents.AgentTree.OneShotTool as OneShotTool
import System.Agents.FileLoader (TemplateLibraries, standardLibraries)
import System.Agents.AgentTree.Trace (TreeTrace)
import qualified System.Agents.Base as Base
import System.Agents.Host.Coordination (Coordination, noCoordination)
import System.Agents.Session.Async (ContinuationStore, mkSqliteContinuationStore)
import System.Agents.Session.Mailbox (MailStore)
import System.Agents.Session.MailStore (mkSqliteMailStore)
import System.Agents.Session.Types (IsolationSpec, SessionId)
import System.Agents.Session.WatchStore (WatchStore, mkSqliteWatchStore)
import System.Agents.SessionStore (FileSessionStore (..), SessionBackend, backendCatalog, fileSessionBackend, mkCompositeSessionStore, mkSqliteSessionStore)
import System.Agents.Tools.Isolated (toolIsolationFromSpec)
import System.Agents.Tools.Params.Types (ProcessParams)

-- | Loaded agents and the stores sessions live in.
data Host = Host
    { hostAgents :: Map Text OSAgentNode
    -- ^ Root agents from files, by slug. See 'hostAllAgents' for all of them.
    , hostStoredAgents :: StoredAgents
    -- ^ Root agents from the database, which can change while the host runs.
    , hostDeps :: AgentDeps
    -- ^ For root agents: no session sink, the runner stores their sessions.
    , hostOwnerApiKeys :: Map Text LoadedApiKeys
    {- ^ API keys of the owners that have their own ('hcOwnerApiKeysFiles').
    The runner builds the agents of such an owner's sessions, sub-sessions
    included, with these keys instead of the ones in 'hostDeps'.
    -}
    , hostSubAgentDeps :: AgentDeps
    -- ^ For sub-agents: they store their own sessions in 'hostBackend'.
    , hostBackend :: SessionBackend
    , hostContinuations :: ContinuationStore
    , hostMail :: MailStore
    -- ^ Durable mail (@todos/session-mailbox.md@, Phase 3), one row per
    -- envelope ever accepted by any session this host runs.
    , hostWatches :: WatchStore
    -- ^ Durable @watch-session@ registrations (@todos/os-as-standalone-server.md@
    -- G11), so 'System.Agents.Host.Runner.recoverOnStartup' can re-register
    -- the ones still active after a restart.
    , hostCoordination :: Coordination
    {- ^ Run leases and signals shared with the other processes on the same
    database, if any ("System.Agents.Host.Coordination").
    -}
    , hostTracer :: Tracer IO HostTrace
    , hostStreamTokens :: Bool
    -- ^ Whether the runner streams LLM answers as text deltas.
    , hostLiveSessionTtl :: NominalDiffTime
    {- ^ How long an idle session keeps its in-memory agent (and the
    background calls it runs) before the runner evicts it.
    -}
    , hostProcessParams :: ProcessParams
    {- ^ Operator-supplied parameter values (@--set@ / @--pin@ / ...), shared
    by every agent this host loaded. A 'pinned' one cannot be overridden by
    a session or message value (@todos/tool-partial-application.md@, Phase 4).
    -}
    }

data HostConfig = HostConfig
    { hcAgentFiles :: [FilePath]
    -- ^ Root agent files; each root is addressable by its slug.
    , hcApiKeysFile :: FilePath
    , hcDatabasePath :: FilePath
    -- ^ SQLite database, for 'withHost'; 'withHostStores' ignores it.
    , hcCompletion :: Maybe (OSAgentNode -> Completion)
    -- ^ Replaces every LLM call, e.g. with a mock in tests.
    , hcLiveSessionTtl :: NominalDiffTime
    , hcStreamTokens :: Bool
    -- ^ Stream root agents' LLM answers (see 'hostStreamTokens').
    , hcProcessParams :: ProcessParams
    -- ^ See 'hostProcessParams'.
    , hcTemplateLibraries :: TemplateLibraries
    -- ^ Libraries @.tramaj@ agent files may import; 'standardLibraries' by default.
    , hcOwnerApiKeysFiles :: [(Text, FilePath)]
    {- ^ Owners that call the LLM with their own API keys: an owner and a
    keys file in the format of 'hcApiKeysFile'. That file replaces
    'hcApiKeysFile' for the owner's sessions, it is not merged with it: a
    key id it does not hold is not looked up in 'hcApiKeysFile'. Owners not
    listed here, and sessions without an owner, use 'hcApiKeysFile'.
    A file that cannot be read or parsed, or an owner listed twice, fails
    the start ('OwnerApiKeysFailed').
    -}
    , hcToolIsolation :: Maybe IsolationSpec
    {- ^ Run the bash and MCP tool calls of every agent this host builds
    outside the process ("System.Agents.Tools.Isolated"). 'Nothing' (the
    default) runs them in-process, as before.
    -}
    , hcLegacySessionDirs :: [FilePath]
    {- ^ Read-only fallback locations for pre-existing @conv.<uuid>.json@
    session history (@todos/os-as-standalone-server.md@ Design §6). When
    non-empty, 'withHost' and 'withHostStores' composite the primary
    backend with a file-backed read fallback over these directories
    ('mkCompositeSessionStore'), so old file-store history stays visible
    once the primary backend (SQLite or Postgres) is the one being written
    to. Both the TUI and @agents-exe serve@ go through this: passing a
    config's resolved session-store read prefixes here is what "serve gets
    the legacy fallback for free" means.
    -}
    }

-- | A configuration with the default idle time of 15 minutes.
defaultHostConfig :: [FilePath] -> FilePath -> FilePath -> HostConfig
defaultHostConfig files keysFile dbPath =
    HostConfig
        { hcAgentFiles = files
        , hcApiKeysFile = keysFile
        , hcDatabasePath = dbPath
        , hcCompletion = Nothing
        , hcLiveSessionTtl = 15 * 60
        , hcStreamTokens = False
        , hcProcessParams = mempty
        , hcTemplateLibraries = standardLibraries
        , hcOwnerApiKeysFiles = []
        , hcToolIsolation = Nothing
        , hcLegacySessionDirs = []
        }

data HostTrace
    = HostAgentTrace !AgentFactory.Trace
    | HostSubAgentTrace !OneShotTool.Trace
    | HostTreeTrace !TreeTrace
    | HostRecoveredSessions ![SessionId]
    | -- | Recovered sessions that were given a run at boot, to start again
      -- the background calls marked as safe to re-run (see
      -- 'Runner.recoverOnStartup').
      HostResumedForReruns ![SessionId]
    | -- | A recovered session whose running calls were failed because these
      -- required parameters are no longer bound (see 'Runner.recoverOnStartup').
      HostRecoveredParamsRequired !SessionId ![Text]
    | -- | @watch-session@ registrations re-registered at boot, by watch id
      -- (G11, @todos/os-as-standalone-server.md@; see 'Runner.recoverWatches').
      HostRecoveredWatches ![Text]
    | -- | Sessions whose run another process stopped renewing, taken over
      -- by this one (see 'Runner.takeOverExpired').
      HostTookOverSessions ![SessionId]
    | -- | A run stopped here because another process took the session over.
      HostRunLeaseLost !SessionId
    | -- | A heartbeat that failed (e.g. the database is unreachable), and why.
      HostCoordinationFailed !Text
    | -- | A runner event, by kind (e.g. @run.started@), for a session.
      HostRunnerTrace !Text !SessionId
    | -- | A stored agent that was not loaded, and why.
      HostStoredAgentSkipped !Text !Text
    deriving (Show)

data HostError
    = AgentLoadFailed FilePath String
    | DuplicateAgentSlug Text
    | -- | An owner's API keys file ('hcOwnerApiKeysFiles') and what is wrong with it.
      OwnerApiKeysFailed Text FilePath String
    | -- | 'hcToolIsolation' names something that cannot run tool calls.
      ToolIsolationRefused Text
    deriving (Show)

instance Exception HostError

-- | Where a host keeps sessions, continuations, and stored agents.
data HostStores = HostStores
    { hsSessions :: SessionBackend
    , hsContinuations :: ContinuationStore
    , hsMail :: MailStore
    , hsWatches :: WatchStore
    , hsAgents :: Maybe AgentStore
    -- ^ 'Nothing': agents come from files only.
    , hsCoordination :: Coordination
    -- ^ 'noCoordination' unless other processes share the stores.
    }

{- | Load the agents, open and migrate the SQLite database, and run the action.

The database gets WAL journaling and a busy timeout, so that the CLI (or a
second process) can read it while the host writes. Agent trees, and the MCP
servers they start, live as long as the action.
-}
withHost :: HostConfig -> Tracer IO HostTrace -> (Host -> IO a) -> IO a
withHost cfg tracer action =
    withConnection cfg.hcDatabasePath $ \conn -> do
        _ <- query_ conn "PRAGMA journal_mode = WAL" :: IO [Only Text]
        _ <- query_ conn "PRAGMA busy_timeout = 5000" :: IO [Only Int]
        backend <- mkSqliteSessionStore conn
        store <- mkSqliteContinuationStore conn
        mail <- mkSqliteMailStore conn
        watches <- mkSqliteWatchStore conn
        agents <- mkSqliteAgentStore conn
        withHostStores cfg (HostStores backend store mail watches (Just agents) noCoordination) tracer action

{- | Like 'withHost', with stores the caller opened (e.g. on Postgres, with
@agents-postgres@).
-}
withHostStores :: HostConfig -> HostStores -> Tracer IO HostTrace -> (Host -> IO a) -> IO a
withHostStores cfg stores tracer action = do
    let backend = case cfg.hcLegacySessionDirs of
            [] -> stores.hsSessions
            dirs -> mkCompositeSessionStore (stores.hsSessions : map (fileSessionBackend . FileSessionStore) dirs)
        store = stores.hsContinuations
    keys <- readOpenApiKeysFile cfg.hcApiKeysFile
    ownerKeys <- loadOwnerApiKeys cfg.hcOwnerApiKeysFiles
    isolation <- case traverse toolIsolationFromSpec cfg.hcToolIsolation of
        Left err -> throwIO $ ToolIsolationRefused err
        Right iso -> pure iso
    let rootDeps =
            (defaultAgentDeps keys)
                { adContinuationStore = Just store
                , adCompletion = cfg.hcCompletion
                , adToolIsolation = isolation
                }
        -- These dependencies are fixed when the agents load, before any
        -- owner is known: they only serve a helper that could not be run
        -- as a session of its own (the runner builds those per owner).
        -- With per-owner keys, such a call gets no key rather than the
        -- shared ones.
        subDeps =
            rootDeps
                { adSessionSink = SinkBackend backend
                , adApiKeys = if Map.null ownerKeys then keys else []
                }
        props file =
            Props
                { apiKeys = keys
                , apiKeysFile = cfg.hcApiKeysFile
                , rootAgentFile = file
                , interactiveTracer = contramap HostTreeTrace tracer
                , agentToTool = OneShotTool.turnAgentRuntimeIntoIOTool (contramap HostSubAgentTrace tracer) subDeps
                , sessionCatalog = backendCatalog backend
                , processParams = cfg.hcProcessParams
                , templateLibraries = cfg.hcTemplateLibraries
                }
        loadAll [] k = k []
        loadAll (file : rest) k =
            withAgentTree (props file) $ \case
                Errors errs -> throwIO $ AgentLoadFailed file (show errs)
                Initialized tree -> loadAll rest $ \roots -> k (tree.osTreeRoot : roots)
        -- Each load of a stored agent gets a directory of its own under
        -- 'filesRoot', holding the tool files of the agent and its helpers;
        -- it goes away with the node ('releaseAgentNode').
        loadStored filesRoot counter sa helpers = do
            n <- atomicModifyIORef' counter (\i -> (i + 1, i)) :: IO Int
            let dir = filesRoot </> show n
                discard = removePathForcibly dir
                place :: Int -> StoredAgent -> IO (FilePath, Base.Agent)
                place i agent = let agentDir = dir </> show i in (,) agentDir <$> materializeToolFiles agentDir agent
                file = "<database>" </> Text.unpack (Base.slug sa.saConfig) <> ".json"
            result <- try $ do
                root <- place 0 sa
                others <- zipWithM place [1 ..] helpers
                loadAgentTreeFromConfigs (props file) root others
            case result of
                Left err -> discard >> pure (Left $ Text.pack $ show (err :: IOException))
                Right (Errors errs) -> discard >> pure (Left $ Text.intercalate "; " (map (`formatLoadingError` Map.empty) (toList errs)))
                Right (Initialized tree) -> do
                    atomically $ modifyTVar' tree.osTreeRoot.osNodeRelease (++ [discard])
                    pure $ Right tree.osTreeRoot
    loadAll cfg.hcAgentFiles $ \roots -> withSystemTempDirectory "agents-stored" $ \filesRoot -> do
        agents <- indexBySlug roots
        loaded <- newTVarIO Map.empty
        lock <- newMVar ()
        retire <- newTVarIO releaseAgentNode
        counter <- newIORef 0
        let storedAgents = StoredAgents stores.hsAgents loaded (loadStored filesRoot counter) lock retire
        forM_ stores.hsAgents $ \agentStore -> do
            saved <- agentStore.asList
            let (hidden, usable) = partition (\sa -> Map.member (Base.slug sa.saConfig) agents) saved
                available = Map.fromList [(Base.slug sa.saConfig, sa) | sa <- usable]
                skip sa = runTracer tracer . HostStoredAgentSkipped (storedSlug sa)
            forM_ hidden $ \sa -> skip sa "an agent file has the same slug"
            forM_ usable $ \sa ->
                case resolveHelpers available sa.saConfig of
                    Left err -> skip sa (formatHelperError err)
                    Right helpers ->
                        loadStored filesRoot counter sa helpers >>= \case
                            Left err -> skip sa err
                            Right node -> atomically $ modifyTVar' loaded (Map.insert (Base.slug sa.saConfig) (sa, node))
        action
            Host
                { hostAgents = agents
                , hostStoredAgents = storedAgents
                , hostDeps = rootDeps
                , hostOwnerApiKeys = ownerKeys
                , hostSubAgentDeps = subDeps
                , hostBackend = backend
                , hostContinuations = store
                , hostMail = stores.hsMail
                , hostWatches = stores.hsWatches
                , hostCoordination = stores.hsCoordination
                , hostTracer = tracer
                , hostStreamTokens = cfg.hcStreamTokens
                , hostLiveSessionTtl = cfg.hcLiveSessionTtl
                , hostProcessParams = cfg.hcProcessParams
                }
  where
    loadOwnerApiKeys :: [(Text, FilePath)] -> IO (Map Text LoadedApiKeys)
    loadOwnerApiKeys = go Map.empty
      where
        go acc [] = pure acc
        go acc ((owner, file) : rest)
            | Text.null owner = throwIO $ OwnerApiKeysFailed owner file "empty owner"
            | Map.member owner acc = throwIO $ OwnerApiKeysFailed owner file "owner listed twice"
            | otherwise =
                try (readOpenApiKeysFileStrict file) >>= \case
                    Left (e :: IOException) -> throwIO $ OwnerApiKeysFailed owner file (show e)
                    Right (Left err) -> throwIO $ OwnerApiKeysFailed owner file err
                    Right (Right loaded) -> go (Map.insert owner loaded acc) rest

    indexBySlug :: [OSAgentNode] -> IO (Map Text OSAgentNode)
    indexBySlug = go Map.empty
      where
        go :: Map Text OSAgentNode -> [OSAgentNode] -> IO (Map Text OSAgentNode)
        go acc [] = pure acc
        go acc (node : rest) =
            let slug = Base.slug node.osNodeConfig
             in if Map.member slug acc
                    then throwIO $ DuplicateAgentSlug slug
                    else go (Map.insert slug node acc) rest

-------------------------------------------------------------------------------
-- Stored agents
-------------------------------------------------------------------------------

-- | Agents kept in the database, loaded next to the agents from files.
data StoredAgents = StoredAgents
    { stStore :: Maybe AgentStore
    , stLoaded :: TVar (Map Text (StoredAgent, OSAgentNode))
    , stLoad :: StoredAgent -> [StoredAgent] -> IO (Either Text OSAgentNode)
    -- ^ Load an agent with the stored agents it reaches ('resolveHelpers').
    , stLock :: MVar ()
    -- ^ Serialises edits.
    , stRetire :: TVar (OSAgentNode -> IO ())
    {- ^ What to do with a node an edit took out of service. Stops its MCP
    servers right away, until a session runner replaces it with one that
    waits for the sessions still using the node ('retireStoredNode').
    -}
    }

-- | No stored agents, and no way to store any (e.g. for tests).
noStoredAgents :: IO StoredAgents
noStoredAgents = do
    loaded <- newTVarIO Map.empty
    lock <- newMVar ()
    retire <- newTVarIO releaseAgentNode
    pure $ StoredAgents Nothing loaded (\_ _ -> pure (Left "this host stores no agents")) lock retire

data AgentSource = FromFile | FromDatabase StoredAgent

-- | Every root agent, by slug. A file agent hides a stored one with its slug.
hostAllAgents :: Host -> IO (Map Text (AgentSource, OSAgentNode))
hostAllAgents host = do
    stored <- readTVarIO host.hostStoredAgents.stLoaded
    pure $
        Map.union
            (Map.map (\node -> (FromFile, node)) host.hostAgents)
            (Map.map (\(sa, node) -> (FromDatabase sa, node)) stored)

lookupAgent :: Host -> Text -> IO (Maybe OSAgentNode)
lookupAgent host slug = fmap snd . Map.lookup slug <$> hostAllAgents host

data AgentEditError
    = -- | The host has no agent store.
      EditsUnsupported
    | AgentDefinedByFile Text
    | -- | The configuration uses these file-based fields.
      AgentUsesFiles [Text]
    | -- | A tool path or a file path leaves the agent's directory, or a
      -- helper is named with a path: one message per problem.
      AgentInvalidPaths [Text]
    | -- | An @extraAgents@ entry names no stored agent, or closes a cycle.
      AgentHelperError HelperError
    | AgentFailedToLoad Text
    | NoStoredAgent Text
    | -- | The agent cannot be deleted: these stored agents name it as a helper.
      AgentInUse Text [Text]
    deriving (Show, Eq)

formatHelperError :: HelperError -> Text
formatHelperError = \case
    UnknownHelper from slug -> from <> " names " <> slug <> " in extraAgents, which is not a stored agent"
    HelperCycle slugs -> "extraAgents of stored agents form a cycle: " <> Text.intercalate " -> " slugs

{- | Choose what happens to a stored agent's node once an edit replaces or
deletes it (see 'stRetire'). A session runner uses this to wait for the
sessions that still use the node.
-}
setStoredNodeRetirement :: Host -> (OSAgentNode -> IO ()) -> IO ()
setStoredNodeRetirement host retire = atomically $ writeTVar host.hostStoredAgents.stRetire retire

-- | Hand a node an edit took out of service to 'stRetire'.
retireStoredNode :: StoredAgents -> OSAgentNode -> IO ()
retireStoredNode st node = readTVarIO st.stRetire >>= \retire -> retire node

{- | Store an agent and load it, replacing a stored agent with its slug.
Sessions that already built the previous version keep it until the runner
drops them from memory; its MCP servers are stopped then, and not before
('stRetire'). Returns whether the agent is new.
-}
putStoredAgent :: Host -> Maybe Text -> Base.Agent -> IO (Either AgentEditError (StoredAgent, Bool))
putStoredAgent host by agent = putStoredAgentWithFiles host by agent Map.empty

{- | 'putStoredAgent', with the files of the agent's bash tools: its
@toolDirectory@ and @bashToolboxes@ paths are relative to the directory
these files are written to when the agent is loaded.

The agent's @extraAgents@ name stored agents that exist already, and may
not lead back to it. Stored agents that reach this one as a helper are
loaded again, so that they call the new version.
-}
putStoredAgentWithFiles :: Host -> Maybe Text -> Base.Agent -> ToolFiles -> IO (Either AgentEditError (StoredAgent, Bool))
putStoredAgentWithFiles host by agent files = case st.stStore of
    Nothing -> pure $ Left EditsUnsupported
    Just agentStore -> withMVar st.stLock $ \_ -> do
        now <- getCurrentTime
        loaded <- readTVarIO st.stLoaded
        let slug = Base.slug agent
            candidate = StoredAgent agent files now by
            available = Map.insert slug candidate (Map.map fst loaded)
            checked
                | Map.member slug host.hostAgents = Left $ AgentDefinedByFile slug
                | fields@(_ : _) <- fileBasedFields agent = Left $ AgentUsesFiles fields
                | errs@(_ : _) <- storedPathErrors agent files = Left $ AgentInvalidPaths errs
                | otherwise = either (Left . AgentHelperError) Right (resolveHelpers available agent)
        case checked of
            Left err -> pure $ Left err
            Right helpers ->
                st.stLoad candidate helpers >>= \case
                    Left err -> pure $ Left $ AgentFailedToLoad err
                    Right node -> do
                        sa <- agentStore.asPut by agent files
                        let previous = Map.lookup slug loaded
                        atomically $ modifyTVar' st.stLoaded (Map.insert slug (sa, node))
                        mapM_ (retireStoredNode st . snd) previous
                        reloadDependents (Map.insert slug sa (Map.map fst loaded)) slug
                        pure $ Right (sa, isNothing previous)
  where
    st = host.hostStoredAgents
    -- A dependent that does not load again keeps its previous version.
    reloadDependents available slug =
        forM_ (Map.toList available) $ \(other, sa) ->
            case resolveHelpers available sa.saConfig of
                Right helpers | other /= slug, slug `elem` map storedSlug helpers -> do
                    st.stLoad sa helpers >>= \case
                        Left err -> runTracer host.hostTracer $ HostStoredAgentSkipped other ("not reloaded after " <> slug <> " changed: " <> err)
                        Right node -> do
                            previous <- Map.lookup other <$> readTVarIO st.stLoaded
                            atomically $ modifyTVar' st.stLoaded (Map.insert other (sa, node))
                            mapM_ (retireStoredNode st . snd) previous
                _ -> pure ()

{- | Remove a stored agent. Its sessions stay; runs on them fail until an
agent with the slug exists again. An agent that other stored agents name as
a helper is not removed.
-}
deleteStoredAgent :: Host -> Text -> IO (Either AgentEditError ())
deleteStoredAgent host slug = case st.stStore of
    Nothing -> pure $ Left EditsUnsupported
    Just agentStore -> withMVar st.stLock $ \_ -> do
        loaded <- readTVarIO st.stLoaded
        let referrers = [other | (other, (sa, _)) <- Map.toList loaded, other /= slug, slug `elem` referencedSlugs sa.saConfig]
        if Map.member slug host.hostAgents
            then pure $ Left $ AgentDefinedByFile slug
            else
                if not (null referrers)
                    then pure $ Left $ AgentInUse slug referrers
                    else do
                        removed <- agentStore.asDelete slug
                        atomically $ modifyTVar' st.stLoaded (Map.delete slug)
                        mapM_ (retireStoredNode st . snd) (Map.lookup slug loaded)
                        pure $ if removed then Right () else Left (NoStoredAgent slug)
  where
    st = host.hostStoredAgents

storedSlug :: StoredAgent -> Text
storedSlug sa = Base.slug sa.saConfig
