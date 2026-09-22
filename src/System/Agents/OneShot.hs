{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | One-shot execution of agents with OS compatibility layer.

This module provides single-conversation execution (batch mode) with support
for both the legacy Runtime interface and the new OS model via RuntimeBridge.

The primary functions ('mainOneShotText', 'runtimeToAgent') have been updated
to use OS-native types. 'mainOneShotText' now works directly with 'OSAgentTree'.
-}
module System.Agents.OneShot (
    -- * Types
    Trace (..),
    ThinkingOutput (..),

    -- * Main functions (OS-native)
    nodeToAgent,
    fileStoringCallback,
    mainPrintAgent,
    mainOneShotText,
    mainOneShotTextWithThinking,

    -- * Re-export from StoreSessionProgress
    agentStoreSession,
    agentWithSessionProgress,

    -- * Utility functions
    mapProgressiveDisclosureTrace,
    parseModelFlavor,
) where

import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO)
import Control.Exception (Exception, finally)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LByteString
import Data.Foldable (traverse_)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import Prod.Tracer (Tracer (..))
import System.IO (stderr)

import System.Agents.AgentFactory (
    AgentRole (..),
    Trace (..),
    buildAgent,
    fileAgentDeps,
    mapProgressiveDisclosureTrace,
 )
import System.Agents.AgentTree (
    LoadAgentResult (..),
    LoadedApiKeys,
    OSAgentNode (..),
    OSAgentTree (..),
    Props (..),
    withAgentTree,
 )
import qualified System.Agents.Base as Base
import System.Agents.Base (ConversationId, newConversationId)
import qualified System.Agents.LLMs.OpenAI as OpenAI
import System.Agents.Media.Types (MediaAttachment)
import System.Agents.Session.Base
import System.Agents.Session.Loop
import System.Agents.SessionStore (SessionStore)
import qualified System.Agents.SessionStore as SessionStore

-- Re-export session storage combinators
import System.Agents.Combinators.StoreSessionProgress (
    agentStoreSession,
    agentWithSessionProgress,
    filepathStoreCallback,
 )

-- | Controls where thinking content should be output.
data ThinkingOutput
    = -- | Suppress thinking output (default)
      ThinkingNone
    | -- | Output thinking to stdout
      ThinkingStdout
    | -- | Output thinking to stderr
      ThinkingStderr
    deriving (Show, Eq, Ord)

mainPrintAgent :: Props -> IO ()
mainPrintAgent props = do
    withAgentTree props $ \x -> do
        case x of
            Errors errs -> traverse_ print errs
            Initialized _ -> pure ()

-- | Configuration for one-shot execution with optional session persistence.
data OneShotConfig = OneShotConfig
    { onSessionProgress :: ConversationId -> OnSessionProgress
    -- ^ Callback for session progress updates (defaults to 'ignoreSessionProgress')
    , initialSession :: Maybe Session
    -- ^ Optional initial session to resume from
    , extraSavePath :: Maybe FilePath
    -- ^ Optional final session store path
    , thinkingOutput :: ThinkingOutput
    -- ^ Where to output thinking content (defaults to 'ThinkingNone')
    , mediaAttachments :: [MediaAttachment]
    -- ^ Optional media attachments for multi-modal LLM queries
    }

-- | Creates a configuration that optionally persists sessions to a file on top of the SessionStore.
fileStoringConfig :: SessionStore -> Maybe Session -> Maybe FilePath -> OneShotConfig
fileStoringConfig store mSession mPath =
    OneShotConfig
        { onSessionProgress = fileStoringCallback store
        , initialSession = mSession
        , extraSavePath = mPath
        , thinkingOutput = ThinkingNone
        , mediaAttachments = []
        }

-- | Run a one-shot agent with the given configuration.
runOneShotWithConfig ::
    SessionStore ->
    OneShotConfig ->
    ConversationId ->
    -- | The tracer for logging
    Tracer IO Trace ->
    -- | API keys for creating HTTP runtime
    LoadedApiKeys ->
    -- | The agent node to execute
    OSAgentNode ->
    Text ->
    IO OneShotResult
runOneShotWithConfig store config convId tracer loadedApiKeys node query = do
    agent1 <- nodeToAgentWithThinking store config.extraSavePath config.thinkingOutput config.mediaAttachments convId tracer loadedApiKeys node

    -- Create or use initial session with media support (version 1)
    session0 <- case config.initialSession of
        Just s -> pure s
        Nothing -> Session [] <$> newSessionId <*> pure Nothing <*> newTurnId <*> pure (Just 1) <*> pure Nothing <*> pure 0

    -- Phase 4 (@todos/session-mailbox.md@, D12): one 'MailRouter' for this
    -- run, so a spawned session (and, through it, further descendants) can
    -- be addressed by 'send-message'/'watch-session'. Spawned sessions run
    -- as threads with their own in-memory mailbox, tracked here so they can
    -- be cancelled together with the root, the same way background calls
    -- already are via 'withEngineShutdown' inside 'runUntilBlocked'.
    router <- newMailRouter
    spawnedVar <- newTVarIO []

    agentWithQuery <-
        agentSetQuery (UserQuery query []) $
            agentWithSessionProgress (config.onSessionProgress convId) $
                agent1
    let agent =
            withSpawnSession (oneShotSpawnSession store tracer loadedApiKeys router spawnedVar session0.sessionId node) $
                withMailRouter router $
                    agentWithQuery
    registerOwnMailbox router session0.sessionId Nothing agent

    config.onSessionProgress convId (SessionStarted session0)
    -- The agent returns the final session as part of its stop result.
    result <- runUntilBlocked convId agent session0 `finally` cancelSpawnedSessions spawnedVar
    case result of
        Left (llmTurn, finalSession) -> do
            config.onSessionProgress convId (SessionCompleted finalSession)
            pure $ OneShotResult $ extractResponseText llmTurn.llmResponse
        Right pausedSession -> do
            config.onSessionProgress convId (SessionUpdated pausedSession)
            -- Also store under the session id, which the durable 'session'
            -- commands use to find it.
            SessionStore.storeSession store (SessionStore.sessionIdToConversationId pausedSession.sessionId) pausedSession
            pure $ OneShotResult $ pausedReport pausedSession

{- | JSON report printed when a one-shot run stops on deferred tool calls.

Lists the session id and each deferred call with its continuation token, so
the calls can be completed with @session complete@ and the run continued with
@session resume@.
-}
pausedReport :: Session -> Text
pausedReport sess =
    Text.decodeUtf8 $
        LByteString.toStrict $
            Aeson.encode $
                Aeson.object
                    [ "status" .= ("paused" :: Text)
                    , "reason" .= ("waiting for deferred tool calls" :: Text)
                    , "session_id" .= sess.sessionId
                    , "deferred_calls"
                        .= [ Aeson.object
                                    [ "tool" .= llmToolCallName tc.tcCall
                                    , "tool_call_id" .= providerToolCallId tc.tcCall
                                    , "continuation_token" .= tc.tcContinuation
                                    ]
                                 | PartialUserTurn partial _ <- take 1 sess.turns
                                 , tc <- partial.pTrackedToolCalls
                                 , tc.tcState == Deferred
                                 ]
                    ]

{- | Run a one-shot agent with optional file-based session storage.

This function uses OS-native types for agent execution.
-}
mainOneShotText ::
    Tracer IO Trace ->
    SessionStore ->
    Maybe FilePath ->
    Maybe Session ->
    Props ->
    Text ->
    IO ()
mainOneShotText tracer store mPath mSession props query = do
    mainOneShotTextWithThinking tracer store mPath mSession ThinkingNone [] props query

-- | Run a one-shot agent with configurable thinking output and media attachments.
mainOneShotTextWithThinking ::
    Tracer IO Trace ->
    SessionStore ->
    Maybe FilePath ->
    Maybe Session ->
    ThinkingOutput ->
    -- | Media attachments for multi-modal queries
    [MediaAttachment] ->
    Props ->
    Text ->
    IO ()
mainOneShotTextWithThinking tracer store mPath mSession thinkingOut mediaAttachs props query = do
    convId <- newConversationId
    withAgentTree props $ \x -> do
        case x of
            Errors errs -> traverse_ print errs
            Initialized tree -> do
                let config =
                        (fileStoringConfig store mSession mPath)
                            { thinkingOutput = thinkingOut
                            , mediaAttachments = mediaAttachs
                            }
                let node = osTreeRoot tree
                OneShotResult result <- runOneShotWithConfig store config convId tracer (apiKeys props) node query
                Text.putStrLn result

data SessionLoadingFailed = SessionLoadingFailed FilePath
    deriving (Show)
instance Exception SessionLoadingFailed

-- | Stopping result type that carries the final response text.
newtype OneShotResult = OneShotResult Text

-- | Extract text from an LLM response.
extractResponseText :: LlmResponse -> Text
extractResponseText (LlmResponse txt _thinking _ _) = Maybe.fromMaybe "" txt

-- | Parse flavor from text, defaulting to OpenAIv1 if not recognized.
parseModelFlavor :: Text -> OpenAI.ModelFlavor
parseModelFlavor txt = Maybe.fromMaybe OpenAI.OpenAIv1 $ OpenAI.parseFlavor txt

{- | Converts an OSAgentNode into an Agent that stops when no tool calls are present.

The agent is built by 'buildAgent' with file-based storage keyed by the given
conversation ID; the optional path receives an extra copy of the session.
-}
nodeToAgent ::
    SessionStore ->
    Maybe FilePath ->
    ConversationId ->
    -- | The tracer for logging
    Tracer IO Trace ->
    -- | API keys for creating HTTP runtime
    LoadedApiKeys ->
    OSAgentNode ->
    IO (Agent (LlmTurnContent, Session))
nodeToAgent store mPath convId tracer loadedApiKeys node =
    nodeToAgentWithThinking store mPath ThinkingNone [] convId tracer loadedApiKeys node

-- | Converts an OSAgentNode into an Agent with configurable thinking output and media support.
nodeToAgentWithThinking ::
    SessionStore ->
    Maybe FilePath ->
    ThinkingOutput ->
    -- | Media attachments for multi-modal queries
    [MediaAttachment] ->
    ConversationId ->
    -- | The tracer for logging
    Tracer IO Trace ->
    -- | API keys for creating HTTP runtime
    LoadedApiKeys ->
    OSAgentNode ->
    IO (Agent (LlmTurnContent, Session))
nodeToAgentWithThinking store mPath thinkingOut mediaAttachs convId tracer loadedApiKeys node = do
    agent <- buildAgent tracer (fileAgentDeps store loadedApiKeys) RootAgent convId node
    pure $
        agentWithSessionProgress (filepathStoreCallback mPath) $
            agent
                { step = \sess -> do
                    action <- agent.step sess
                    -- Output thinking if present and configured
                    case action of
                        Stop (llmTurn, _) ->
                            case (thinkingOut, llmTurn.llmResponse.responseThinking) of
                                (ThinkingStdout, Just t) -> Text.putStrLn t
                                (ThinkingStderr, Just t) -> Text.hPutStrLn stderr t
                                _ -> pure ()
                        _ -> pure ()
                    pure action
                , complete = \completion ->
                    -- Inject media attachments into the completion
                    agent.complete completion{completeMedia = mediaAttachs}
                }

{- | Creates a callback that stores session progress to a file.
This is useful for creating an 'OnSessionProgress' handler that persists to disk.
-}
fileStoringCallback :: SessionStore -> ConversationId -> OnSessionProgress
fileStoringCallback store convId progress =
    case progress of
        SessionUpdated sess -> SessionStore.storeSession store convId sess
        SessionCompleted sess -> SessionStore.storeSession store convId sess
        SessionStarted sess -> SessionStore.storeSession store convId sess
        SessionFailed sess _ -> SessionStore.storeSession store convId sess

{- | Set a one-shot agent's query.

Per @todos/session-mailbox.md@ (Phase 1, "TUI and one-shot post
'UserMessage'"), the query is pre-loaded as one 'UserMessage' envelope on a
fresh in-memory mailbox rather than answered through 'usrQuery': the first
receive point (R1) folds it into the run's first user turn. 'usrQuery' is
left untouched for embedders that still rely on it with
@ctxMailbox = Nothing@.
-}
agentSetQuery :: forall r. UserQuery -> Agent r -> IO (Agent r)
agentSetQuery query agent = do
    mb <- newInMemoryMailbox
    _ <- mb.mbSend Outgoing{outId = Nothing, outFrom = FromUser Nothing, outPriority = Normal, outHops = 0, outBody = UserMessage query}
    pure agent{ctxMailbox = Just mb}

{- | Register an agent's own mailbox on the router under its session id
(Phase 4, D12), so a spawned descendant -- or a sibling, if one is ever
addressable in this process -- can 'send-message' back to it. A no-op if
the agent has no mailbox (e.g. 'ctxInterruptCompletions'-style embedding
without one).
-}
registerOwnMailbox :: MailRouter -> SessionId -> Maybe SessionId -> Agent r -> IO ()
registerOwnMailbox router sid mParent agent = case agent.ctxMailbox of
    Nothing -> pure ()
    Just mb -> do
        _ <- router.mrRegister sid (MailboxInfo Nothing mParent "running" MailScopeSubtree MailScopeChildren) mb
        pure ()

-- | Cancel every session thread started by 'oneShotSpawnSession' for this run.
cancelSpawnedSessions :: TVar [Async ()] -> IO ()
cancelSpawnedSessions spawnedVar = readTVarIO spawnedVar >>= mapM_ cancel

{- | The @run@ front-end's @spawn-session@ hook (@todos/session-mailbox.md@,
Phase 4, §5): @<slug>@ is resolved among the caller's own declared helpers
(@'osNodeChildren'@), the same universe @prompt_agent_\<slug\>@ draws from,
narrowed the same way a fresh call/return sub-agent is built (via
'nodeToAgent') -- unlike the server's @spawn-session@, which resolves
against every agent the host knows about, since @run@ has no such registry.

The child runs as its own thread with its own in-memory mailbox, tracked in
'spawnedVar' so 'runOneShotWithConfig' can cancel it alongside the root
(the spec's "a thread ... cancelled with the root like background calls
are"). It is not call\/return: this returns as soon as the child session
exists, without waiting for (or ever returning) its answer, which arrives
by mail instead.
-}
oneShotSpawnSession ::
    SessionStore ->
    Tracer IO Trace ->
    LoadedApiKeys ->
    MailRouter ->
    TVar [Async ()] ->
    SessionId ->
    OSAgentNode ->
    Text ->
    Text ->
    IO (Either Text SessionId)
oneShotSpawnSession store tracer loadedApiKeys router spawnedVar ownSid node slug text =
    case List.find (\child -> Base.slug child.osNodeConfig == slug) node.osNodeChildren of
        Nothing -> pure $ Left ("no such helper: " <> slug)
        Just childNode -> do
            childConvId <- newConversationId
            childSession <- Session [] <$> newSessionId <*> pure Nothing <*> newTurnId <*> pure (Just 1) <*> pure Nothing <*> pure 0
            childAgent0 <- nodeToAgent store Nothing childConvId tracer loadedApiKeys childNode
            childAgentWithQuery <- agentSetQuery (UserQuery text []) childAgent0
            let childAgent =
                    withSpawnSession (oneShotSpawnSession store tracer loadedApiKeys router spawnedVar childSession.sessionId childNode) $
                        withMailRouter router $
                            childAgentWithQuery
            registerOwnMailbox router childSession.sessionId (Just ownSid) childAgent
            a <- async $ () <$ runUntilBlocked childConvId childAgent childSession
            atomically $ modifyTVar' spawnedVar (a :)
            pure $ Right childSession.sessionId
