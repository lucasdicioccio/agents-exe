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
) where

import Control.Exception (Exception, throwIO)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import Database.SQLite.Simple (Only (..), query_, withConnection)
import Prod.Tracer (Tracer, contramap)

import System.Agents.AgentFactory (AgentDeps (..), Completion, SessionSink (..), defaultAgentDeps)
import qualified System.Agents.AgentFactory as AgentFactory
import System.Agents.AgentTree (LoadAgentResult (..), OSAgentNode (..), OSAgentTree (..), Props (..), readOpenApiKeysFile, withAgentTree)
import qualified System.Agents.AgentTree.OneShotTool as OneShotTool
import System.Agents.AgentTree.Trace (TreeTrace)
import qualified System.Agents.Base as Base
import System.Agents.Session.Async (ContinuationStore, mkSqliteContinuationStore)
import System.Agents.Session.Types (SessionId)
import System.Agents.SessionStore (SessionBackend, backendCatalog, mkSqliteSessionStore)

-- | Loaded agents and the stores sessions live in.
data Host = Host
    { hostAgents :: Map Text OSAgentNode
    -- ^ Root agents, by slug.
    , hostDeps :: AgentDeps
    -- ^ For root agents: no session sink, the runner stores their sessions.
    , hostSubAgentDeps :: AgentDeps
    -- ^ For sub-agents: they store their own sessions in 'hostBackend'.
    , hostBackend :: SessionBackend
    , hostContinuations :: ContinuationStore
    , hostTracer :: Tracer IO HostTrace
    , hostLiveSessionTtl :: NominalDiffTime
    {- ^ How long an idle session keeps its in-memory agent (and the
    background calls it runs) before the runner evicts it.
    -}
    }

data HostConfig = HostConfig
    { hcAgentFiles :: [FilePath]
    -- ^ Root agent files; each root is addressable by its slug.
    , hcApiKeysFile :: FilePath
    , hcDatabasePath :: FilePath
    , hcCompletion :: Maybe (OSAgentNode -> Completion)
    -- ^ Replaces every LLM call, e.g. with a mock in tests.
    , hcLiveSessionTtl :: NominalDiffTime
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
        }

data HostTrace
    = HostAgentTrace !AgentFactory.Trace
    | HostSubAgentTrace !OneShotTool.Trace
    | HostTreeTrace !TreeTrace
    | HostRecoveredSessions ![SessionId]
    | HostRunnerTrace !Text !SessionId
    -- ^ A runner event, by kind (e.g. @run.started@), for a session.
    deriving (Show)

data HostError
    = AgentLoadFailed FilePath String
    | DuplicateAgentSlug Text
    deriving (Show)

instance Exception HostError

{- | Load the agents, open and migrate the database, and run the action.

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
                    }
            loadAll [] k = k []
            loadAll (file : rest) k =
                withAgentTree (props file) $ \case
                    Errors errs -> throwIO $ AgentLoadFailed file (show errs)
                    Initialized tree -> loadAll rest $ \roots -> k (tree.osTreeRoot : roots)
        loadAll cfg.hcAgentFiles $ \roots -> do
            agents <- indexBySlug roots
            action
                Host
                    { hostAgents = agents
                    , hostDeps = rootDeps
                    , hostSubAgentDeps = subDeps
                    , hostBackend = backend
                    , hostContinuations = store
                    , hostTracer = tracer
                    , hostLiveSessionTtl = cfg.hcLiveSessionTtl
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
