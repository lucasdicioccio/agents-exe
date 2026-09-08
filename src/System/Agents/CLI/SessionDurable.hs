{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Module for the 'session' durable-workflow command handlers.

These commands operate on stored sessions, supporting pause/resume of
asynchronous execution, inspection of pending deferred calls, injection
of external results, and execution of isolated calls.

The commands load sessions through the configured file-based
'SessionStore' (the same store used by the rest of the CLI). When an
agent is required to advance execution, the first supplied agent file is
loaded exactly like the one-shot command.
-}
module System.Agents.CLI.SessionDurable (
    -- * Types
    SessionDurableOptions (..),
    SessionDurableCommand (..),

    -- * Handler
    handleSessionDurable,

    -- * Pure helpers (exported for testing)
    formatContinuationToken,
    parseContinuationToken,
    parseResultFile,
    extractDeferredCalls,
    extractIsolatedCalls,
    buildToolCallPolicy,
    applyAgentDurableConfig,
) where

import Control.Monad (forM_)
import Data.Map (Map)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LByteString
import Data.List (find)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import qualified Data.Text.IO as Text
import qualified Data.UUID as UUID
import Prod.Tracer (silent)
import System.Exit (exitFailure)
import System.IO (stderr)

import qualified System.Agents.AgentTree as AgentTree
import qualified System.Agents.AgentTree.OneShotTool as OneShotTool
import qualified System.Agents.Base as Base
import System.Agents.CLI.Aliases (AliasDefinition)
import System.Agents.CLI.OneShot (PromptScriptOptions (..), loadPromptScriptOptions)
import System.Agents.Media.Types (MediaAttachment (..))
import System.Agents.OneShot (nodeToAgent)
import System.Agents.Session.Base
import System.Agents.Session.Step (getPartialTurn, runStepM)
import System.Agents.Session.Wake (resumeSession, wakeSession)
import qualified System.Agents.SessionStore as SessionStore
import qualified System.Agents.Tools.Context as Ctx

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Options for the 'session' durable-workflow command.
data SessionDurableOptions = SessionDurableOptions
    { sdCommand :: SessionDurableCommand
    }
    deriving (Show)

-- | Subcommands for durable session operations.
data SessionDurableCommand
    = SessionStart PromptScriptOptions Bool
    | SessionStep SessionId
    | SessionPause SessionId
    | SessionResume SessionId
    | SessionPending SessionId
    | SessionComplete ContinuationToken FilePath
    | SessionRunIsolated SessionId
    deriving (Show)

-------------------------------------------------------------------------------
-- Pure helpers
-------------------------------------------------------------------------------

{- | Render a continuation token as text.

This is the canonical string representation used by the CLI and by
external workers that complete yielded calls.
-}
formatContinuationToken :: ContinuationToken -> Text
formatContinuationToken (ContinuationToken uuid) = UUID.toText uuid

{- | Parse a continuation token from text.

Accepts the canonical UUID text form produced by 'formatContinuationToken'.
-}
parseContinuationToken :: Text -> Maybe ContinuationToken
parseContinuationToken txt =
    ContinuationToken <$> UUID.fromText txt

{- | Read a result file and convert it to a 'UserToolResponse'.

If the file contents decode as the JSON representation of a
'UserToolResponse', that value is used. Otherwise the raw contents are
treated as a plain text response.
-}
parseResultFile :: LByteString.ByteString -> Either Text UserToolResponse
parseResultFile bs
    | LByteString.null bs = Left "result file is empty"
    | otherwise =
        case Aeson.eitherDecode bs of
            Right response -> Right response
            Left _ -> Right $ TextResponse $ Text.strip $ TextEnc.decodeUtf8 $ LByteString.toStrict bs

-- | Extract all deferred calls from the latest partial turn of a session.
extractDeferredCalls :: Session -> [(ToolCallId, Maybe ContinuationToken, Text, ToolCallDisposition)]
extractDeferredCalls session =
    case getPartialTurn session of
        Nothing -> []
        Just partial ->
            [ (tc.tcId, tc.tcContinuation, callName tc.tcCall, disp)
            | tc <- partial.pTrackedToolCalls
            , tc.tcState == Deferred
            , let disp = appliedDisposition tc.tcPolicy
            ]

-- | Extract deferred calls whose base disposition is 'RunIsolated'.
extractIsolatedCalls :: Session -> [(ToolCallId, ContinuationToken, LlmToolCall, ToolCallDisposition)]
extractIsolatedCalls session =
    catMaybes $ map isolateInfo $ extractDeferredCalls session
  where
    isolateInfo (cid, mToken, _name, disp) =
        case (mToken, snd (flattenDisposition disp)) of
            (Just token, RunIsolated _) ->
                case findTrackedCall session cid of
                    Just tc -> Just (cid, token, tc.tcCall, disp)
                    Nothing -> Nothing
            _ -> Nothing

-- | Look up a tracked call by id in the latest partial turn.
findTrackedCall :: Session -> ToolCallId -> Maybe TrackedToolCall
findTrackedCall session cid =
    case getPartialTurn session of
        Nothing -> Nothing
        Just partial -> listToMaybe [tc | tc <- partial.pTrackedToolCalls, tc.tcId == cid]

-- | Extract the disposition stored in an applied policy.
appliedDisposition :: AppliedPolicy -> ToolCallDisposition
appliedDisposition policy = policy.apDisposition

-- | Extract the function name from an LLM tool call.
callName :: LlmToolCall -> Text
callName (LlmToolCall val) =
    case val of
        Aeson.Object obj ->
            case KeyMap.lookup "function" obj of
                Just (Aeson.Object func) ->
                    case KeyMap.lookup "name" func of
                        Just (Aeson.String n) -> n
                        _ -> "unknown"
                _ -> "unknown"
        _ -> "unknown"

-------------------------------------------------------------------------------
-- Session loading / storing
-------------------------------------------------------------------------------

-- | Load a session by its id from the file-based session store.
loadSessionById :: SessionStore.SessionStore -> SessionId -> IO (Maybe Session)
loadSessionById store sid =
    SessionStore.readSession store (SessionStore.sessionIdToConversationId sid)

-- | Persist a session to the file-based session store.
storeSessionById :: SessionStore.SessionStore -> SessionId -> Session -> IO ()
storeSessionById store sid sess =
    SessionStore.storeSession store (SessionStore.sessionIdToConversationId sid) sess

-- | Load a session or fail with a helpful message.
requireSession :: SessionStore.SessionStore -> SessionId -> Text -> IO Session
requireSession store sid label = do
    mSession <- loadSessionById store sid
    case mSession of
        Nothing -> do
            Text.hPutStrLn stderr $ label <> ": session not found: " <> formatSessionId sid
            exitFailure
        Just sess -> pure sess

formatSessionId :: SessionId -> Text
formatSessionId (SessionId uuid) = UUID.toText uuid

-------------------------------------------------------------------------------
-- Agent construction
-------------------------------------------------------------------------------

{- | Build an execution agent from the first supplied agent file.

The agent is configured for the session's conversation id so that resumed
execution carries the same lineage. The caller can further customise the
agent (e.g. set asynchronous mode) before running steps.
-}
-- | Load the JSON agent configuration from a file.
loadJsonAgentFile :: FilePath -> IO Base.Agent
loadJsonAgentFile path = do
    result <- Aeson.eitherDecodeFileStrict' path
    case result of
        Left err -> do
            Text.hPutStrLn stderr $ "Error parsing agent file " <> Text.pack path <> ": " <> Text.pack err
            exitFailure
        Right (Base.AgentDescription agent) -> pure agent

-- | Build a runtime 'ToolCallPolicy' from a declarative policy config.
buildToolCallPolicy :: Base.ToolCallPolicyConfig -> ToolCallPolicy
buildToolCallPolicy cfg _ctx call =
    maybe (Base.tpcDefaultDisposition cfg) Base.tprDisposition $
        find (\rule -> Base.tprToolName rule == callName call) (Base.tpcRules cfg)

-- | Apply durable settings from the JSON agent config to a runtime agent.
applyAgentDurableConfig :: Base.Agent -> Agent r -> Agent r
applyAgentDurableConfig jsonAgent agent =
    let agent' = maybe agent (\mode -> withExecutionMode mode agent) (Base.executionMode jsonAgent)
     in maybe agent' (\cfg -> withToolCallPolicy (buildToolCallPolicy cfg) agent') (Base.toolCallPolicyConfig jsonAgent)
-- | Build an execution agent from the first supplied agent file for a given
-- conversation id. Durable settings from the JSON agent config are applied.
buildAgentForFile ::
    SessionStore.SessionStore ->
    FilePath ->
    [FilePath] ->
    Base.ConversationId ->
    (Agent (LlmTurnContent, Session) -> IO a) ->
    IO a
buildAgentForFile _ _ [] _ _ = do
    Text.hPutStrLn stderr "Error: session commands that advance execution require an agent file (use --agent-file)"
    exitFailure
buildAgentForFile store apiKeysFile (agentFile : _) convId action = do
    jsonAgent <- loadJsonAgentFile agentFile
    apiKeys <- AgentTree.readOpenApiKeysFile apiKeysFile
    let props =
            AgentTree.Props
                { AgentTree.apiKeys = apiKeys
                , AgentTree.apiKeysFile = apiKeysFile
                , AgentTree.rootAgentFile = agentFile
                , AgentTree.interactiveTracer = silent
                , AgentTree.agentToTool = OneShotTool.turnAgentRuntimeIntoIOTool silent store apiKeys
                , AgentTree.sessionStore = store
                }
    AgentTree.withAgentTree props $ \case
        AgentTree.Errors errs -> do
            Text.hPutStrLn stderr $ "Error loading agent tree: " <> Text.pack (show errs)
            exitFailure
        AgentTree.Initialized tree -> do
            agent0 <- nodeToAgent store Nothing convId silent apiKeys (AgentTree.osTreeRoot tree)
            let agent = applyAgentDurableConfig jsonAgent agent0
            action agent

-- | Build an execution agent from the first supplied agent file.
--
-- The agent is configured for the session's conversation id so that resumed
-- execution carries the same lineage. The caller can further customise the
-- agent (e.g. set asynchronous mode) before running steps.
withAgentForSession ::
    SessionStore.SessionStore ->
    FilePath ->
    [FilePath] ->
    Session ->
    (Agent (LlmTurnContent, Session) -> IO a) ->
    IO a
withAgentForSession store apiKeysFile agentFiles sess action = do
    let convId = SessionStore.sessionIdToConversationId (sessionId sess)
    buildAgentForFile store apiKeysFile agentFiles convId action

-------------------------------------------------------------------------------
-- Command handlers
-------------------------------------------------------------------------------

-- | Dispatch a durable session command.
handleSessionDurable ::
    SessionStore.SessionStore ->
    FilePath ->
    [FilePath] ->
    Map Text AliasDefinition ->
    SessionDurableOptions ->
    IO ()
handleSessionDurable store apiKeysFile agentFiles aliases opts =
    case opts.sdCommand of
        SessionStart promptOpts runStep -> handleStart store apiKeysFile agentFiles aliases promptOpts runStep
        SessionStep sid -> handleStep store apiKeysFile agentFiles sid
        SessionPause sid -> handlePause store apiKeysFile agentFiles sid
        SessionResume sid -> handleResume store apiKeysFile agentFiles sid
        SessionPending sid -> handlePending store sid
        SessionComplete token path -> handleComplete store token path
        SessionRunIsolated sid -> handleRunIsolated store apiKeysFile agentFiles sid

-- | Start a new durable session from a prompt.
handleStart ::
    SessionStore.SessionStore ->
    FilePath ->
    [FilePath] ->
    Map Text AliasDefinition ->
    PromptScriptOptions ->
    Bool ->
    IO ()
handleStart _ _ [] _ _ _ = do
    Text.hPutStrLn stderr "Error: session start requires an agent file (use --agent-file)"
    exitFailure
handleStart store apiKeysFile agentFiles aliases opts runStep = do
    (promptText, mediaAttachments) <- loadPromptScriptOptions aliases Nothing opts
    convId <- Base.newConversationId
    sid <- newSessionId
    tid <- newTurnId
    buildAgentForFile store apiKeysFile agentFiles convId $ \agent0 -> do
        let agent = agent0{ctxExecutionMode = Asynchronous}
        sPrompt <- sysPrompt agent
        sTools <- sysTools agent
        let uQuery = Just (UserQuery promptText mediaAttachments)
        let initialTurn = UserTurn (UserTurnContent sPrompt sTools uQuery []) Nothing
        let sess = Session [initialTurn] sid Nothing tid (Just 2) (Just Asynchronous)
        if runStep
            then do
                (_agent, result) <- runStepM convId agent sess
                case result of
                    Left (llmTurn, finalSess) -> do
                        storeSessionById store sid finalSess
                        Text.putStrLn $ "session-id: " <> formatSessionId sid
                        Text.putStrLn "Session completed."
                        forM_ llmTurn.llmResponse.responseText Text.putStrLn
                    Right finalSess -> do
                        storeSessionById store sid finalSess
                        Text.putStrLn $ "session-id: " <> formatSessionId sid
                        printSessionState finalSess
            else do
                storeSessionById store sid sess
                Text.putStrLn $ "session-id: " <> formatSessionId sid
                printSessionState sess

-- | Run exactly one scheduling step on an existing session.
handleStep ::
    SessionStore.SessionStore ->
    FilePath ->
    [FilePath] ->
    SessionId ->
    IO ()
handleStep store apiKeysFile agentFiles sid = do
    sess <- requireSession store sid "step"
    withAgentForSession store apiKeysFile agentFiles sess $ \agent0 -> do
        let agent = agent0{ctxExecutionMode = Asynchronous}
        let convId = SessionStore.sessionIdToConversationId sid
        (_agent, result) <- runStepM convId agent sess
        case result of
            Left (llmTurn, finalSess) -> do
                storeSessionById store sid finalSess
                Text.putStrLn "Session completed."
                forM_ llmTurn.llmResponse.responseText Text.putStrLn
            Right finalSess -> do
                storeSessionById store sid finalSess
                printSessionState finalSess

-- | Pause after one async step of the agent.
handlePause ::
    SessionStore.SessionStore ->
    FilePath ->
    [FilePath] ->
    SessionId ->
    IO ()
handlePause store apiKeysFile agentFiles sid = do
    sess <- requireSession store sid "pause"
    withAgentForSession store apiKeysFile agentFiles sess $ \agent0 -> do
        let agent = agent0{ctxExecutionMode = Asynchronous}
        let convId = SessionStore.sessionIdToConversationId sid
        (_agent, result) <- runStepM convId agent sess
        case result of
            Left (llmTurn, finalSess) -> do
                storeSessionById store sid finalSess
                Text.putStrLn "Session completed."
                forM_ llmTurn.llmResponse.responseText Text.putStrLn
            Right sess' -> do
                storeSessionById store sid sess'
                printYieldedState sess'

-- | Resume execution until completion or the next yield.
handleResume ::
    SessionStore.SessionStore ->
    FilePath ->
    [FilePath] ->
    SessionId ->
    IO ()
handleResume store apiKeysFile agentFiles sid = do
    sess <- requireSession store sid "resume"
    withAgentForSession store apiKeysFile agentFiles sess $ \agent0 -> do
        let agent = agent0{ctxExecutionMode = Asynchronous}
        let convId = SessionStore.sessionIdToConversationId sid
        final <- resumeSession convId agent sess
        case final of
            Left (llmTurn, finalSess) -> do
                storeSessionById store sid finalSess
                Text.putStrLn "Session completed."
                forM_ llmTurn.llmResponse.responseText Text.putStrLn
            Right sess' -> do
                storeSessionById store sid sess'
                printYieldedState sess'

-- | List pending deferred calls for a session.
handlePending :: SessionStore.SessionStore -> SessionId -> IO ()
handlePending store sid = do
    sess <- requireSession store sid "pending"
    let deferred = extractDeferredCalls sess
    if null deferred
        then Text.putStrLn "No pending deferred calls."
        else do
            forM_ deferred $ \(cid, mToken, toolName, disp) -> do
                let tokenTxt = maybe "<no token>" formatContinuationToken mToken
                Text.putStrLn $ "  call " <> formatToolCallId cid <> ": " <> toolName
                Text.putStrLn $ "    token: " <> tokenTxt
                Text.putStrLn $ "    disposition: " <> Text.pack (show disp)

-- | Complete a deferred call by injecting a result from a file.
handleComplete :: SessionStore.SessionStore -> ContinuationToken -> FilePath -> IO ()
handleComplete store token path = do
    bs <- LByteString.readFile path
    response <- case parseResultFile bs of
        Left err -> do
            Text.hPutStrLn stderr $ "Error reading result file: " <> err
            exitFailure
        Right resp -> pure resp
    (sid, sess) <- findSessionForToken store token
    updated <- wakeSession sess [(token, response)]
    storeSessionById store sid updated
    case getPartialTurn updated of
        Nothing -> Text.putStrLn "Turn is now complete."
        Just _ -> Text.putStrLn "Turn is still partial."

-- | Execute deferred isolated calls through the configured deployment runner.
handleRunIsolated ::
    SessionStore.SessionStore ->
    FilePath ->
    [FilePath] ->
    SessionId ->
    IO ()
handleRunIsolated store apiKeysFile agentFiles sid = do
    sess <- requireSession store sid "run-isolated"
    withAgentForSession store apiKeysFile agentFiles sess $ \agent -> do
        case agent.ctxDeploymentRunner of
            Nothing -> do
                let isolated = extractIsolatedCalls sess
                Text.putStrLn "No deployment runner configured on the agent."
                Text.putStrLn $ "Deferred isolated calls found: " <> Text.pack (show $ length isolated)
            Just runner -> do
                let isolated = extractIsolatedCalls sess
                if null isolated
                    then Text.putStrLn "No deferred isolated calls to execute."
                    else do
                        results <- mapM (runIsolated sess runner) isolated
                        let pairs = [(tok, res) | (_, tok, _, res) <- results]
                        updated <- wakeSession sess pairs
                        storeSessionById store sid updated
                        forM_ results $ \(_cid, tok, toolName, res) ->
                            Text.putStrLn $ "  " <> toolName <> " (" <> formatContinuationToken tok <> "): " <> responseSummary res
  where
    runIsolated session runner (cid, token, call, disp) = do
        result <- runner.drExecute $ mkIsolationEnvelope token call (contextSnapshotForCall session cid) disp Nothing
        let userRes = either (TextResponse . ("isolation error: " <>) . (\(IsolationError e) -> e)) id result
        pure (cid, token, callName call, userRes)
    -- Build a minimal snapshot from session ids.
    contextSnapshotForCall session _cid =
        let convId = SessionStore.sessionIdToConversationId (sessionId session)
         in Ctx.ToolExecutionContextSnapshot
                { Ctx.tecsSessionId = sessionId session
                , Ctx.tecsConversationId = convId
                , Ctx.tecsTurnId = session.turnId
                , Ctx.tecsCallStack = []
                , Ctx.tecsAllowedTools = []
                , Ctx.tecsParentConversation = Nothing
                }

-- | Short human-readable summary of a tool response.
responseSummary :: UserToolResponse -> Text
responseSummary (TextResponse txt) = Text.take 80 txt
responseSummary (JsonResponse val) = Text.take 80 $ Text.pack $ show val
responseSummary (MediaResponse (MediaAttachment mime _ _)) = "<media " <> mime <> ">"
responseSummary (MixedResponse parts) = "<mixed " <> Text.pack (show $ length parts) <> " parts>"
-------------------------------------------------------------------------------
-- Output helpers
-------------------------------------------------------------------------------

-- | Print the yielded state of a session (partial turn info).
printYieldedState :: Session -> IO ()
printYieldedState sess =
    case getPartialTurn sess of
        Nothing -> Text.putStrLn "No partial turn; session is ready for the next step."
        Just partial -> do
            let completed = [tc | tc <- partial.pTrackedToolCalls, tc.tcState == Completed]
            let deferred = [tc | tc <- partial.pTrackedToolCalls, tc.tcState == Deferred]
            let ready = [tc | tc <- partial.pTrackedToolCalls, tc.tcState == Ready]
            Text.putStrLn "Yielded partial turn:"
            Text.putStrLn $ "  completed: " <> Text.pack (show $ length completed)
            Text.putStrLn $ "  deferred:  " <> Text.pack (show $ length deferred)
            Text.putStrLn $ "  ready:     " <> Text.pack (show $ length ready)
            forM_ deferred $ \tc ->
                forM_ tc.tcContinuation $ \token ->
                    Text.putStrLn $ "  token: " <> formatContinuationToken token <> " (" <> callName tc.tcCall <> ")"

-- | Print a human-readable summary of the current session state.
printSessionState :: Session -> IO ()
printSessionState sess =
    case sess.turns of
        [] -> Text.putStrLn "State: empty session"
        (turn : _) -> case turn of
            UserTurn _ _ -> Text.putStrLn "State: UserTurn (ready for LLM)"
            LlmTurn llmTurn _ ->
                if null llmTurn.llmToolCalls
                    then do
                        Text.putStrLn "State: LlmTurn (completed)"
                        forM_ llmTurn.llmResponse.responseText Text.putStrLn
                    else do
                        Text.putStrLn "State: LlmTurn"
                        forM_ llmTurn.llmToolCalls $ \call ->
                            Text.putStrLn $ "  tool call: " <> callName call
            PartialUserTurn _ _ -> printYieldedState sess

formatToolCallId :: ToolCallId -> Text
formatToolCallId (ToolCallId uuid) = UUID.toText uuid

-------------------------------------------------------------------------------
-- Token / session lookup
-------------------------------------------------------------------------------

-- | Find the session that contains a deferred call with the given token.
findSessionForToken :: SessionStore.SessionStore -> ContinuationToken -> IO (SessionId, Session)
findSessionForToken store token = do
    sessions <- SessionStore.listSessions store
    matches <- catMaybes <$> mapM loadAndCheck sessions
    case matches of
        (sid, sess) : _ -> pure (sid, sess)
        [] -> do
            Text.hPutStrLn stderr $ "No session found containing token: " <> formatContinuationToken token
            exitFailure
  where
    loadAndCheck (_path, mSess, convId) =
        case mSess of
            Nothing -> pure Nothing
            Just sess ->
                let sid = SessionStore.conversationIdToSessionId convId
                 in if any (\(_, mTok, _, _) -> mTok == Just token) (extractDeferredCalls sess)
                        then pure $ Just (sid, sess)
                        else pure Nothing

