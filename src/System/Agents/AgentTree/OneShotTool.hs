{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- | Provides a version of turnAgentRuntimeIntoIOTool based on OneShot.hs
implementation of LLM session calls.

This module has been updated to work with OS-native structures and includes
TUI visibility support for subcall conversations.

Type conversion strategy:
- Base.ConversationId is used throughout most logic (from System.Agents.Base)
- OS.Core.Types.ConversationId/EntityId are only used when interacting with OS World
- Conversion: Base.ConversationId (UUID) -> EntityId (UUID) -> OS ConversationId
-}
module System.Agents.AgentTree.OneShotTool (
    Trace (..),
    turnAgentRuntimeIntoIOTool,
) where

import Control.Concurrent.STM (TQueue, atomically, newTVarIO, readTVarIO, writeTQueue)
import Control.Exception (SomeAsyncException, SomeException, catch, displayException, fromException, throwIO)
import Control.Monad (forM_)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as CByteString
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time (getCurrentTime)
import Prod.Tracer (Tracer (..), contramap)

import System.Agents.AgentFactory (AgentDeps, AgentRole (..), buildAgent)
import qualified System.Agents.AgentFactory as AgentFactory
import System.Agents.AgentTree (OSAgentNode (..))
import qualified System.Agents.Base as Base
import System.Agents.OS.Conversation (
    ConversationConfig (..),
    ConversationState (..),
    ConversationStatus (..),
    Lineage (..),
    emptyLineage,
    pushLineage,
 )
import qualified System.Agents.OS.Conversation as OSConv
import System.Agents.OS.Core.Types (
    AgentId (..),
    ConversationId (..),
    EntityId (..),
 )
import System.Agents.OS.Core.World (World, setComponent)
import qualified System.Agents.OS.Core.World as OSWorld
import System.Agents.OS.Events (OSEvent (..))
import System.Agents.Session.Base (
    Agent (..),
    LlmResponse (..),
    LlmTurnContent (..),
    MailBody (UserMessage),
    Outgoing (..),
    PartialUserTurnContent (..),
    Priority (Normal),
    Sender (FromUser),
    Session (..),
    Turn (..),
    UserQuery (..),
    newInMemoryMailbox,
    newTurnId,
 )
import qualified System.Agents.Session.Base as SessionBase
import System.Agents.Session.Loop (run)
import qualified System.Agents.SessionStore as SessionStore
import System.Agents.ToolRegistration (
    ToolRegistration,
    registerIOScriptInLLM,
 )
import System.Agents.ToolSchema (ParamProperty (..), ParamType (..))

-- Import ToolExecutionContext with qualified access to avoid ambiguity with Agent fields.
-- DuplicateRecordFields allows both Agent and ToolExecutionContext to have the same field names.
import qualified System.Agents.Tools.Bindings as Bindings
import System.Agents.Tools.Bindings.Types (
    AgentAddress (..),
    AgentBinding (..),
    Binding (..),
    BindingValue (..),
    DerivedNarrowing (..),
    ScopedBinding (..),
    WhenUnbound (..),
    reRootBindings,
 )
import System.Agents.Tools.Context (CallStackEntry (..), ToolExecutionContext (..))
import qualified System.Agents.Tools.Context as Ctx
import qualified System.Agents.Tools.IO as IOTools
import System.Agents.Tools.Params.Types (ParamName, ParamValue (..), Params)

-------------------------------------------------------------------------------
-- Trace Types
-------------------------------------------------------------------------------

newtype Trace
    = OneShotTrace AgentFactory.Trace
    deriving (Show)

-------------------------------------------------------------------------------

-- | Data type for the prompt argument to the sub-agent.
data PromptOtherAgent = PromptOtherAgent
    { what :: Text
    , poaBindings :: Maybe [AgentBinding]
    {- ^ Narrows one of the helper's own tool arguments, or one of its
    helpers' (@todos/tool-partial-application.md@, §8.3). Resolved against
    the caller's 'ctxParams' when this call is made.
    -}
    , poaWith :: Maybe (Map.Map ParamName BindingValue)
    {- ^ Call-time @with@ (§8.3): fills the helper's own parameters for
    this call, same as the reference's static @with@ (§7), but chosen by
    the calling model. Cannot refill a parameter the reference's @with@
    already fills.
    -}
    , poaAs :: Maybe Text
    {- ^ Reuses a narrowing @derive_agent@ saved under this name for this
    helper, earlier in the session (§8.4). Its @bindings@ apply before this
    call's own 'poaBindings'; its @with@ fills what this call's own
    'poaWith' and the reference's static @with@ do not.
    -}
    }
    deriving (Show)

instance Aeson.FromJSON PromptOtherAgent where
    parseJSON = Aeson.withObject "PromptOtherAgent" $ \v ->
        PromptOtherAgent
            <$> v Aeson..: "what"
            <*> v Aeson..:? "bindings"
            <*> v Aeson..:? "with"
            <*> v Aeson..:? "as"

-------------------------------------------------------------------------------
-- Type Conversions
-------------------------------------------------------------------------------

{- | Convert a Base.AgentId to an OS.Core.Types.AgentId
Base.AgentId wraps UUID, OS.AgentId wraps EntityId which wraps UUID
-}
baseAgentIdToOS :: Base.AgentId -> AgentId
baseAgentIdToOS (Base.AgentId uuid) = AgentId (EntityId uuid)

{- | Convert a Base.ConversationId to an OS.Core.Types.ConversationId
Base.ConversationId wraps UUID, OS.ConversationId wraps EntityId which wraps UUID
-}
baseConversationIdToOS :: Base.ConversationId -> ConversationId
baseConversationIdToOS (Base.ConversationId uuid) = ConversationId (EntityId uuid)

-- | Convert an OS.Core.Types.ConversationId to an EntityId (for World operations)
osConversationIdToEntityId :: ConversationId -> EntityId
osConversationIdToEntityId = unConversationId

-- | Convert a Base.ConversationId directly to an EntityId
baseConversationIdToEntityId :: Base.ConversationId -> EntityId
baseConversationIdToEntityId (Base.ConversationId uuid) = EntityId uuid

-- | Create a new Base.ConversationId
newBaseConversationId :: IO Base.ConversationId
newBaseConversationId = Base.newConversationId

-------------------------------------------------------------------------------

{- | Converts an OSAgentNode into an IO Tool using the OneShot session-based approach.

This version uses the LLM session calls from OneShot.hs. It creates an Agent from
the OSAgentNode, runs it with a session, and returns the result.

When the ToolExecutionContext includes a World and EventQueue, this function:
1. Inserts the subcall conversation into the OS World as a first-class entity
2. Emits OSEvent_SubcallStarted at the beginning
3. Emits OSEvent_SubcallProgress during execution (after each step)
4. Emits OSEvent_SubcallCompleted or OSEvent_SubcallFailed at the end

This enables TUI visibility for subcall conversations, showing parent/child
relationships and tracking subcall lifecycle.

Type handling:
- ctx.ctxConversationId is Base.ConversationId (from Tools.Context)
- OSEvent types use Base.ConversationId (from OS.Events)
- OS World operations use OS.Core.Types.ConversationId/EntityId
- Session.run uses Base.ConversationId
-}
turnAgentRuntimeIntoIOTool ::
    Tracer IO Trace ->
    -- | Dependencies shared with the calling agent (API keys, session storage)
    AgentDeps ->
    -- | The OS agent node to convert into a tool
    OSAgentNode ->
    -- | The slug of the calling agent (for tracing)
    Base.AgentSlug ->
    -- | The ID of the calling agent (for tracing)
    Base.AgentId ->
    {- | This reference's @with@ (@todos/tool-partial-application.md@, §7):
    fills the child's parameters from the caller's 'ctxParams' at call
    time. 'Nothing' for a child reached through a toolDirectory.
    -}
    Maybe (Map.Map ParamName BindingValue) ->
    {- | Whether this reference is narrowable (@todos/tool-partial-application.md@,
    §8, Phase 6): 'True' for a child reached through a toolDirectory. When
    'False', any @bindings@/@with@ on this call, and any inherited binding
    addressed at this agent or below, are refused.
    -}
    Bool ->
    -- | The resulting tool registration
    ToolRegistration
turnAgentRuntimeIntoIOTool tracer deps node callerSlug _callerId mWith narrowable =
    registerIOScriptInLLM io props
  where
    agent = node.osNodeConfig

    -- Define the parameter properties for the LLM tool schema
    props =
        [ ParamProperty
            { propertyKey = "what"
            , propertyType = StringParamType
            , propertyDescription = "the prompt to call the specialized-agent with"
            , propertyRequired = True
            }
        , ParamProperty
            { propertyKey = "bindings"
            , propertyType = OpaqueParamType "array"
            , propertyDescription =
                "narrows some of this helper's own open tool arguments, or one of its "
                    <> "own helpers' (see describe_agent): [{\"agent\": \"<path below this helper, "
                    <> "or omitted for the helper itself, or \\\"**\\\" for it and everything below>\", "
                    <> "\"tool\": \"<glob, optional>\", \"arg\": \"<name>\", "
                    <> "\"value\": {\"tag\": \"Literal\", \"contents\": <json>} | {\"tag\": \"Param\", \"contents\": \"<your own parameter name>\"}}]"
            , propertyRequired = False
            }
        , ParamProperty
            { propertyKey = "with"
            , propertyType = OpaqueParamType "object"
            , propertyDescription =
                "fills some of this helper's own parameters for this call only: "
                    <> "{\"<helper's parameter name>\": {\"tag\": \"Literal\", \"contents\": <json>} | "
                    <> "{\"tag\": \"Param\", \"contents\": \"<your own parameter name>\"}}"
            , propertyRequired = False
            }
        , ParamProperty
            { propertyKey = "as"
            , propertyType = StringParamType
            , propertyDescription =
                "reuses a narrowing saved earlier in this session with derive_agent {\"from\": \""
                    <> Base.slug agent
                    <> "\", \"slug\": \"<name>\", ...}, by that name; its bindings/with apply before this call's own"
            , propertyRequired = False
            }
        ]

    -- Create the IO script that wraps the agent
    io =
        IOTools.IOScript
            ( IOTools.IOScriptDescription
                ("prompt_agent_" <> Base.slug agent)
                ("asks a prompt to the expert agent: " <> Base.slug agent)
            )
            runSubAgent

    -- Run the sub-agent with the given prompt and execution context
    runSubAgent :: ToolExecutionContext -> PromptOtherAgent -> IO CByteString.ByteString
    runSubAgent ctx (PromptOtherAgent query mBindingsArg mCallWith mAs)
        | not narrowable, Just _ <- mBindingsArg =
            pure $ Text.encodeUtf8 $ "agent '" <> Base.slug agent <> "' is not narrowable: 'bindings' on this call is refused"
        | not narrowable, Just _ <- mCallWith =
            pure $ Text.encodeUtf8 $ "agent '" <> Base.slug agent <> "' is not narrowable: 'with' on this call is refused"
        | not narrowable, Just _ <- mAs =
            pure $ Text.encodeUtf8 $ "agent '" <> Base.slug agent <> "' is not narrowable: 'as' on this call is refused"
        | otherwise = case mAs of
            Nothing -> proceed (fromMaybe [] mBindingsArg) (fromMaybe Map.empty mCallWith)
            Just asSlug -> case Map.lookup (Base.slug agent, asSlug) ctx.ctxDerivedNarrowings of
                Nothing ->
                    pure $
                        Text.encodeUtf8 $
                            "no narrowing named '" <> asSlug <> "' has been derived for '" <> Base.slug agent
                                <> "'; call derive_agent {\"from\": \"" <> Base.slug agent <> "\", \"slug\": \"" <> asSlug <> "\", ...} first"
                Just dn ->
                    proceed
                        (dn.dnBindings ++ fromMaybe [] mBindingsArg)
                        (Map.union (fromMaybe Map.empty mCallWith) dn.dnWith)
      where
        proceed :: [AgentBinding] -> Map.Map ParamName BindingValue -> IO CByteString.ByteString
        proceed bindingsArg callWith = case resolveCallBindings ctx.ctxParams bindingsArg of
            Left err -> pure $ Text.encodeUtf8 err
            Right ownScoped -> runSubAgentWithBindings ctx query ownScoped callWith

    runSubAgentWithBindings :: ToolExecutionContext -> Text -> [ScopedBinding] -> Map.Map ParamName BindingValue -> IO CByteString.ByteString
    runSubAgentWithBindings ctx query ownScoped callWith = do
        -- Extract the conversation ID from the execution context for tracing
        -- ctx.ctxConversationId is Base.ConversationId
        let parentBaseConvId = ctx.ctxConversationId

        -- Calculate new call stack from parent context for arbitrarily deep nesting
        -- Use qualified access to avoid ambiguity with Agent fields
        let parentCallStack = Ctx.ctxCallStack ctx
        subcallBaseConvId <- newBaseConversationId
        let newEntry = CallStackEntry (Base.slug agent) subcallBaseConvId (length parentCallStack)
        let subcallCallStack = newEntry : parentCallStack

        -- Extract OS integration fields from context
        let mWorld = Ctx.ctxWorld ctx
        let mEventQueue = Ctx.ctxEventQueue ctx
        let mParentBaseConv = Ctx.ctxParentConversation ctx

        -- Narrowing helpers, down the call chain (§8.3, Phase 6): the
        -- caller's inherited bindings, re-rooted at this helper, plus this
        -- call's own bindings. Those addressed at the helper itself are
        -- applied now, wrapping its tools for this call only (the shared,
        -- loaded toolboxes are never mutated); the rest travel in the
        -- sub-agent's own 'ctxInheritedBindings' for it to apply when it
        -- prompts its own helpers.
        let inheritedHere = if narrowable then reRootBindings (Base.slug agent) ctx.ctxInheritedBindings else []
            allScoped = ownScoped ++ inheritedHere
            hereBindings = [sb | sb <- allScoped, sbAddress sb == AgentHere]
            restBindings = [sb | sb <- allScoped, sbAddress sb /= AgentHere]
        nodeForCall <-
            if null hereBindings
                then pure node
                else do
                    rawTools <- readTVarIO node.osNodeTools
                    let toolBindings = [Binding (sbTool sb) (sbArg sb) (Literal (sbValue sb)) Omit | sb <- hereBindings]
                        wrappedTools = map (Bindings.applyBindings toolBindings) rawTools
                    wrappedToolsTVar <- newTVarIO wrappedTools
                    pure node{osNodeTools = wrappedToolsTVar}

        -- Build the sub-agent with the caller's dependencies, then attach the
        -- OS integration fields: the World and EventQueue are essential for
        -- nested subcalls to be visible in the TUI
        sessionAgent0 <-
            buildAgent
                (contramap OneShotTrace tracer)
                deps
                (SubAgent parentBaseConvId subcallCallStack)
                subcallBaseConvId
                nodeForCall
        -- A call-time 'with' (§8.3) cannot refill a parameter the
        -- reference's own static 'with' (§7) already fills.
        let refWith = fromMaybe Map.empty mWith
            effectiveCallWith = Map.difference callWith refWith
            withOverlay = resolveWith ctx.ctxParams (Map.union refWith effectiveCallWith)
        let sessionAgent =
                sessionAgent0
                    { SessionBase.ctxWorld = mWorld
                    , SessionBase.ctxEventQueue = mEventQueue
                    , SessionBase.ctxParams = Map.union withOverlay sessionAgent0.ctxParams
                    , SessionBase.ctxInheritedBindings = restBindings
                    , -- Phase 4 (@todos/session-mailbox.md@ §5, D12): handed
                      -- down the same way as ctxWorld/ctxEventQueue, so a
                      -- deeply-nested helper can still send-message /
                      -- spawn-session.
                      SessionBase.ctxMailRouter = Ctx.ctxMailRouter ctx
                    , SessionBase.ctxSpawnSession = Ctx.ctxSpawnSession ctx
                    , -- Phase 6 (@todos/session-mailbox.md@ §7): handed down the same way.
                      SessionBase.ctxWatchSession = Ctx.ctxWatchSession ctx
                    , SessionBase.ctxUnwatchSession = Ctx.ctxUnwatchSession ctx
                    }

        -- Set the query on the agent
        agentWithQuery <- agentSetQuery (UserQuery query []) sessionAgent

        -- Create a fresh session with media support (version 1), identified
        -- like its conversation so that its own sub-agents can name it as parent
        let subcallSessionId = SessionStore.conversationIdToSessionId subcallBaseConvId
        session0 <- Session [] subcallSessionId Nothing <$> newTurnId <*> pure (Just 1) <*> pure Nothing <*> pure 0

        -- Get current time for timestamps
        now <- getCurrentTime

        let depth = length parentCallStack

        -- Insert into OS World and emit start event if OS integration is available
        -- Convert Base.ConversationId to OS types for World operations
        let subcallOSConvId = baseConversationIdToOS subcallBaseConvId
        let subcallEntityId = osConversationIdToEntityId subcallOSConvId

        -- Insert conversation into OS World if world is available
        case mWorld of
            Just world -> do
                -- Insert ConversationConfig using OS ConversationId
                let convConfig =
                        ConversationConfig
                            { conversationTitle = Just $ "↳ " <> Base.slug agent
                            , conversationMetadata =
                                Map.fromList
                                    [ ("agentSlug", Aeson.String $ Base.slug agent)
                                    , ("callerSlug", Aeson.String callerSlug)
                                    , ("isSubcall", Aeson.Bool True)
                                    ]
                            }

                -- Create TVar for last activity
                lastActivityVar <- newTVarIO now

                -- Convert Base.AgentId to OS.AgentId for conversation state
                let osAgentId = baseAgentIdToOS node.osNodeAgentId

                -- Insert ConversationState using OS AgentId
                let convState =
                        ConversationState
                            { conversationAgentId = osAgentId
                            , conversationStatus = ConversationActive
                            , conversationStartedAt = now
                            , conversationLastActivity = lastActivityVar
                            }

                -- Build lineage with parent link if available
                -- Convert parent Base.ConversationId to EntityId for lineage
                lineage <- case mParentBaseConv of
                    Just parentConvId -> do
                        let parentEntityId = baseConversationIdToEntityId parentConvId
                        let baseLineage = pushLineage OSConv.ConversationFrame parentEntityId now emptyLineage
                        pure $ pushLineage OSConv.ConversationFrame subcallEntityId now baseLineage
                    Nothing -> pure $ pushLineage OSConv.ConversationFrame subcallEntityId now emptyLineage

                -- Perform all insertions atomically
                atomically $ do
                    setComponent world subcallEntityId convConfig
                    setComponent world subcallEntityId convState
                    setComponent world subcallEntityId (Lineage $ OSConv.lineageStack lineage)
            Nothing -> pure ()

        -- Emit SubcallStarted event if event queue is available
        -- OSEvent uses Base.ConversationId directly
        case mEventQueue of
            Just eventQueue -> do
                let event =
                        OSEvent_SubcallStarted
                            { subcallParentConversationId = parentBaseConvId
                            , subcallConversationId = subcallBaseConvId
                            , subcallAgentSlug = Base.slug agent
                            , subcallDepth = depth
                            }
                atomically $ writeTQueue eventQueue event
            Nothing -> pure ()

        -- Run the agent and handle result
        -- Session.run uses Base.ConversationId
        result <-
            runSubAgentWithEventEmission
                subcallBaseConvId
                session0
                agentWithQuery
                mWorld
                mEventQueue
                (Ctx.ctxProgressCallback ctx)

        -- Return the result
        pure $ Text.encodeUtf8 result

{- | Run the sub-agent with event emission for TUI visibility.

This function wraps the agent's step function to emit progress events after each
step, enabling the TUI to show the conversation as it loads. Without this, the TUI
would only see the start and end events, not the intermediate progress.

Uses Base.ConversationId throughout, converting to OS types only for World operations.
-}
runSubAgentWithEventEmission ::
    Base.ConversationId ->
    Session ->
    Agent (LlmTurnContent, Session) ->
    Maybe World ->
    Maybe (TQueue OSEvent) ->
    -- | Progress callback of the calling tool call, if it runs in the background
    Maybe (Aeson.Value -> IO ()) ->
    IO Text
runSubAgentWithEventEmission baseConvId session0 agent mWorld mEventQueue mReportProgress = do
    -- Create a progress emitter function that sends SubcallProgress events
    let emitProgress sess = do
            forM_ mReportProgress ($ subAgentProgress sess)
            case mEventQueue of
                Just eventQueue -> do
                    let event =
                            OSEvent_SubcallProgress
                                { subcallProgressConversationId = baseConvId
                                , subcallProgressSession = sess
                                }
                    atomically $ writeTQueue eventQueue event
                Nothing -> pure ()

    -- Wrap the agent's step function to emit progress after each step
    let agentWithProgress = wrapAgentWithProgress emitProgress agent

    result <-
        catch
            ( do
                (finalTurnContent, finalSession) <- run baseConvId agentWithProgress session0
                -- Emit final progress with the completed session
                emitProgress finalSession
                -- Extract and return the response text
                let resultText = extractResponseText finalTurnContent.llmResponse
                pure $ Right resultText
            )
            ( \e -> do
                -- Cancellation (e.g. cancel-tool-call) must stop the sub-agent,
                -- not be reported as a failure.
                case fromException e of
                    Just (_ :: SomeAsyncException) -> throwIO e
                    Nothing -> pure ()
                let errMsg = Text.pack $ displayException (e :: SomeException)
                pure $ Left errMsg
            )

    -- Emit completion or failure event
    -- OSEvent types use Base.ConversationId
    case mEventQueue of
        Just eventQueue -> do
            case result of
                Right resultText -> do
                    let event =
                            OSEvent_SubcallCompleted
                                { subcallCompletedConversationId = baseConvId
                                , subcallCompletedResult = resultText
                                }
                    atomically $ writeTQueue eventQueue event
                    -- Update OS World status if available
                    -- Convert Base.ConversationId to OS types for World update
                    case mWorld of
                        Just world -> do
                            let osConvId = baseConversationIdToOS baseConvId
                            updateConversationStatus world osConvId ConversationArchived
                        Nothing -> pure ()
                Left errMsg -> do
                    let event =
                            OSEvent_SubcallFailed
                                { subcallFailedConversationId = baseConvId
                                , subcallFailedError = errMsg
                                }
                    atomically $ writeTQueue eventQueue event
                    -- Update OS World status if available
                    -- Convert Base.ConversationId to OS types for World update
                    case mWorld of
                        Just world -> do
                            let osConvId = baseConversationIdToOS baseConvId
                            updateConversationStatus world osConvId (ConversationError errMsg)
                        Nothing -> pure ()
        Nothing -> pure ()

    -- Return result or re-throw error
    case result of
        Right resultText -> pure resultText
        Left errMsg -> error $ Text.unpack errMsg

{- | Progress payload describing where a sub-agent is, reported to the
calling tool call.
-}
subAgentProgress :: Session -> Aeson.Value
subAgentProgress sess =
    Aeson.object
        [ "message" .= describe sess.turns
        , "turns" .= length sess.turns
        ]
  where
    describe :: [Turn] -> Text
    describe = \case
        [] -> "sub-agent starting"
        (LlmTurn llm _ : _)
            | null llm.llmToolCalls -> "sub-agent answered"
            | otherwise -> "sub-agent calling " <> Text.intercalate ", " (map SessionBase.llmToolCallName llm.llmToolCalls)
        (UserTurn _ _ : _) -> "sub-agent waiting for its LLM"
        (PartialUserTurn partial _ : _) ->
            "sub-agent has " <> Text.pack (show (length partial.pTrackedToolCalls)) <> " tool calls in progress"

{- | Wrap an agent's step function to emit progress events after each step.

This allows the TUI to track the conversation as it progresses, showing
intermediate states rather than just the final result.
-}
wrapAgentWithProgress ::
    (Session -> IO ()) ->
    Agent (LlmTurnContent, Session) ->
    Agent (LlmTurnContent, Session)
wrapAgentWithProgress emitProgress agent =
    agent
        { step = \sess -> do
            -- Emit progress before executing the step
            emitProgress sess
            -- Execute the original step
            result <- agent.step sess
            pure result
        }

{- | Update conversation status in OS World.
Takes OS.Core.Types.ConversationId since this is a World operation.
-}
updateConversationStatus :: World -> ConversationId -> ConversationStatus -> IO ()
updateConversationStatus world osConvId newStatus = do
    now <- getCurrentTime
    let entityId = unConversationId osConvId
    mState <- atomically $ OSWorld.getComponent @ConversationState world entityId
    case mState of
        Just state -> do
            -- Update last activity
            newLastActivity <- newTVarIO now
            atomically $ do
                let updatedState =
                        state
                            { conversationStatus = newStatus
                            , conversationLastActivity = newLastActivity
                            }
                setComponent world entityId updatedState
        Nothing -> pure ()

{- | Resolves a reference's @with@ against the caller's 'ctxParams'
(@todos/tool-partial-application.md@, §7). A 'Param' that the caller has no
value for is left out: the child falls back to its own process value or
default, and a still-required, still-missing parameter fails the call the
same way an unbound binding does. A 'Literal' is never secret, since it is
the agent author's own text, not something routed through the model.
-}
resolveWith :: Params -> Map.Map ParamName BindingValue -> Params
resolveWith callerParams = Map.mapMaybe resolveOne
  where
    resolveOne (Literal v) = Just (ParamValue v False)
    resolveOne (Param p) = Map.lookup p callerParams

{- | Resolves a @prompt_agent_*@ call's own @bindings@ argument
(@todos/tool-partial-application.md@, §8.3) against the caller's
'ctxParams'. A 'Literal' value is never secret (the caller's own model
wrote it); a 'Param' inherits secrecy from the caller's value. An
unresolvable 'Param' fails the whole call when its policy is 'Fail' (told
to the caller's model, so it can retry, per D8's "errors the model can fix
are told to it"), and is dropped silently when it is not: the target
argument is simply left unbound, as if the binding had not been given.
-}
resolveCallBindings :: Params -> [AgentBinding] -> Either Text [ScopedBinding]
resolveCallBindings callerParams = go []
  where
    go :: [ScopedBinding] -> [AgentBinding] -> Either Text [ScopedBinding]
    go acc [] = Right (reverse acc)
    go acc (b : bs) = case abValue b of
        Literal v -> go (ScopedBinding (abAgent b) (abTool b) (abArg b) v False : acc) bs
        Param p -> case Map.lookup p callerParams of
            Just pv -> go (ScopedBinding (abAgent b) (abTool b) (abArg b) (pvValue pv) (pvSecret pv) : acc) bs
            Nothing -> case abWhenUnbound b of
                Fail -> Left $ "binding for argument '" <> abArg b <> "' has no value for parameter '" <> p <> "'"
                _ -> go acc bs

{- | Set the user query on an agent.

Pre-loads it as one 'UserMessage' envelope on a fresh in-memory mailbox
rather than answering 'usrQuery' (@todos/session-mailbox.md@, Phase 1); see
'System.Agents.OneShot.agentSetQuery'.
-}
agentSetQuery :: UserQuery -> Agent r -> IO (Agent r)
agentSetQuery query agent = do
    mb <- newInMemoryMailbox
    _ <- mb.mbSend Outgoing{outId = Nothing, outFrom = FromUser Nothing, outPriority = Normal, outHops = 0, outBody = UserMessage query}
    pure agent{ctxMailbox = Just mb}

extractResponseText :: LlmResponse -> Text
extractResponseText (LlmResponse txt _thinking _ _) =
    fromMaybe "" txt
