{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

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
    putStoredAgent,
    deleteStoredAgent,
) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO)
import Control.Exception (Exception, throwIO)
import Control.Monad (forM_)
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (NominalDiffTime)
import Database.SQLite.Simple (Only (..), query_, withConnection)
import Prod.Tracer (Tracer, contramap, runTracer)
import System.FilePath ((</>))

import System.Agents.AgentFactory (AgentDeps (..), Completion, SessionSink (..), defaultAgentDeps)
import qualified System.Agents.AgentFactory as AgentFactory
import System.Agents.AgentStore (AgentStore (..), StoredAgent (..), fileBasedFields, mkSqliteAgentStore)
import System.Agents.AgentTree (LoadAgentResult (..), OSAgentNode (..), OSAgentTree (..), Props (..), formatLoadingError, loadAgentTreeFromConfig, readOpenApiKeysFile, withAgentTree)
import qualified System.Agents.AgentTree.OneShotTool as OneShotTool
import System.Agents.AgentTree.Trace (TreeTrace)
import qualified System.Agents.Base as Base
import System.Agents.Session.Async (ContinuationStore, mkSqliteContinuationStore)
import System.Agents.Session.Mailbox (MailStore)
import System.Agents.Session.MailStore (mkSqliteMailStore)
import System.Agents.Session.Types (SessionId)
import System.Agents.SessionStore (FileSessionStore (..), SessionBackend, backendCatalog, fileSessionBackend, mkCompositeSessionStore, mkSqliteSessionStore)
import System.Agents.Tools.Params.Types (ProcessParams)

-- | Loaded agents and the stores sessions live in.
data Host = Host
    { hostAgents :: Map Text OSAgentNode
    -- ^ Root agents from files, by slug. See 'hostAllAgents' for all of them.
    , hostStoredAgents :: StoredAgents
    -- ^ Root agents from the database, which can change while the host runs.
    , hostDeps :: AgentDeps
    -- ^ For root agents: no session sink, the runner stores their sessions.
    , hostSubAgentDeps :: AgentDeps
    -- ^ For sub-agents: they store their own sessions in 'hostBackend'.
    , hostBackend :: SessionBackend
    , hostContinuations :: ContinuationStore
    , hostMail :: MailStore
    -- ^ Durable mail (@todos/session-mailbox.md@, Phase 3), one row per
    -- envelope ever accepted by any session this host runs.
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
        , hcLegacySessionDirs = []
        }

data HostTrace
    = HostAgentTrace !AgentFactory.Trace
    | HostSubAgentTrace !OneShotTool.Trace
    | HostTreeTrace !TreeTrace
    | HostRecoveredSessions ![SessionId]
    | -- | A recovered session whose running calls were failed because these
      -- required parameters are no longer bound (see 'Runner.recoverOnStartup').
      HostRecoveredParamsRequired !SessionId ![Text]
    | -- | A runner event, by kind (e.g. @run.started@), for a session.
      HostRunnerTrace !Text !SessionId
    | -- | A stored agent that was not loaded, and why.
      HostStoredAgentSkipped !Text !Text
    deriving (Show)

data HostError
    = AgentLoadFailed FilePath String
    | DuplicateAgentSlug Text
    deriving (Show)

instance Exception HostError

-- | Where a host keeps sessions, continuations, and stored agents.
data HostStores = HostStores
    { hsSessions :: SessionBackend
    , hsContinuations :: ContinuationStore
    , hsMail :: MailStore
    , hsAgents :: Maybe AgentStore
    -- ^ 'Nothing': agents come from files only.
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
        agents <- mkSqliteAgentStore conn
        withHostStores cfg (HostStores backend store mail (Just agents)) tracer action

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
    let rootDeps =
            (defaultAgentDeps keys)
                { adContinuationStore = Just store
                , adCompletion = cfg.hcCompletion
                }
        subDeps = rootDeps{adSessionSink = SinkBackend backend}
        props file =
            Props
                { apiKeys = keys
                , apiKeysFile = cfg.hcApiKeysFile
                , rootAgentFile = file
                , interactiveTracer = contramap HostTreeTrace tracer
                , agentToTool = OneShotTool.turnAgentRuntimeIntoIOTool (contramap HostSubAgentTrace tracer) subDeps
                , sessionCatalog = backendCatalog backend
                , processParams = cfg.hcProcessParams
                }
        loadAll [] k = k []
        loadAll (file : rest) k =
            withAgentTree (props file) $ \case
                Errors errs -> throwIO $ AgentLoadFailed file (show errs)
                Initialized tree -> loadAll rest $ \roots -> k (tree.osTreeRoot : roots)
        loadStored agent =
            loadAgentTreeFromConfig (props ("<database>" </> Text.unpack (Base.slug agent) <> ".json")) "." agent >>= \case
                Errors errs -> pure $ Left $ Text.intercalate "; " (map (`formatLoadingError` Map.empty) (toList errs))
                Initialized tree -> pure $ Right tree.osTreeRoot
    loadAll cfg.hcAgentFiles $ \roots -> do
        agents <- indexBySlug roots
        loaded <- newTVarIO Map.empty
        lock <- newMVar ()
        let storedAgents = StoredAgents stores.hsAgents loaded loadStored lock
        forM_ stores.hsAgents $ \agentStore -> do
            saved <- agentStore.asList
            forM_ saved $ \sa -> do
                let slug = Base.slug sa.saConfig
                    skip = runTracer tracer . HostStoredAgentSkipped slug
                if Map.member slug agents
                    then skip "an agent file has the same slug"
                    else
                        loadStored sa.saConfig >>= \case
                            Left err -> skip err
                            Right node -> atomically $ modifyTVar' loaded (Map.insert slug (sa, node))
        action
            Host
                { hostAgents = agents
                , hostStoredAgents = storedAgents
                , hostDeps = rootDeps
                , hostSubAgentDeps = subDeps
                , hostBackend = backend
                , hostContinuations = store
                , hostMail = stores.hsMail
                , hostTracer = tracer
                , hostStreamTokens = cfg.hcStreamTokens
                , hostLiveSessionTtl = cfg.hcLiveSessionTtl
                , hostProcessParams = cfg.hcProcessParams
                }
  where
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
    , stLoad :: Base.Agent -> IO (Either Text OSAgentNode)
    , stLock :: MVar ()
    -- ^ Serialises edits.
    }

-- | No stored agents, and no way to store any (e.g. for tests).
noStoredAgents :: IO StoredAgents
noStoredAgents = do
    loaded <- newTVarIO Map.empty
    lock <- newMVar ()
    pure $ StoredAgents Nothing loaded (\_ -> pure (Left "this host stores no agents")) lock

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
    | AgentFailedToLoad Text
    | NoStoredAgent Text
    deriving (Show, Eq)

{- | Store an agent and load it, replacing a stored agent with its slug.
Sessions that already built the previous version keep it until the runner
drops them from memory. Returns whether the agent is new.
-}
putStoredAgent :: Host -> Maybe Text -> Base.Agent -> IO (Either AgentEditError (StoredAgent, Bool))
putStoredAgent host by agent = case st.stStore of
    Nothing -> pure $ Left EditsUnsupported
    Just agentStore -> withMVar st.stLock $ \_ -> do
        let slug = Base.slug agent
        if Map.member slug host.hostAgents
            then pure $ Left $ AgentDefinedByFile slug
            else case fileBasedFields agent of
                fields@(_ : _) -> pure $ Left $ AgentUsesFiles fields
                [] ->
                    st.stLoad agent >>= \case
                        Left err -> pure $ Left $ AgentFailedToLoad err
                        Right node -> do
                            sa <- agentStore.asPut by agent
                            existed <- Map.member slug <$> readTVarIO st.stLoaded
                            atomically $ modifyTVar' st.stLoaded (Map.insert slug (sa, node))
                            pure $ Right (sa, not existed)
  where
    st = host.hostStoredAgents

{- | Remove a stored agent. Its sessions stay; runs on them fail until an
agent with the slug exists again.
-}
deleteStoredAgent :: Host -> Text -> IO (Either AgentEditError ())
deleteStoredAgent host slug = case st.stStore of
    Nothing -> pure $ Left EditsUnsupported
    Just agentStore -> withMVar st.stLock $ \_ ->
        if Map.member slug host.hostAgents
            then pure $ Left $ AgentDefinedByFile slug
            else do
                removed <- agentStore.asDelete slug
                atomically $ modifyTVar' st.stLoaded (Map.delete slug)
                pure $ if removed then Right () else Left (NoStoredAgent slug)
  where
    st = host.hostStoredAgents
