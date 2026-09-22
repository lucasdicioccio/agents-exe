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

import Control.Exception (Exception)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LByteString
import Data.Foldable (traverse_)
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

    agent <-
        agentSetQuery (UserQuery query []) $
            agentWithSessionProgress (config.onSessionProgress convId) $
                agent1

    -- Create or use initial session with media support (version 1)
    session0 <- case config.initialSession of
        Just s -> pure s
        Nothing -> Session [] <$> newSessionId <*> pure Nothing <*> newTurnId <*> pure (Just 1) <*> pure Nothing <*> pure 0

    config.onSessionProgress convId (SessionStarted session0)
    -- The agent returns the final session as part of its stop result.
    result <- runUntilBlocked convId agent session0
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
