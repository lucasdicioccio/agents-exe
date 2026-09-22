{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Helpers for managing tool-call entities in the OS ECS world.

This module provides operations to create, update, and query the
'ToolCallConfig' and 'ToolCallState' components that make every tool
call a first-class entity in the OS world.

Design notes:

* The ECS 'EntityId' for a tool call is derived from the session-layer
  'Session.ToolCallId' UUID. This lets 'findToolCallEntityBySessionId'
  locate the OS entity for a session call by comparing UUIDs.
* All operations are IO wrappers around STM transactions, matching the
  style of other OS world consumers.
-}
module System.Agents.OS.Conversation.ToolCalls (
    -- * Component registration
    registerToolCallComponents,
    ensureToolCallComponents,
    ensureToolCallComponentsIO,

    -- * Entity lifecycle
    createToolCallEntity,
    createToolCallEntityWithProviderId,
    startToolCall,
    completeToolCall,
    failToolCall,
    cancelToolCall,
    addToolCallProgress,
    recordChildSession,

    -- * Queries
    findToolCallEntityBySessionId,
    findToolCallEntityByToolCallId,
    findToolCallEntityByProviderCallId,
    listToolCallsBySessionAndConversation,
    isToolCallCompletedIO,
) where

import Control.Concurrent.STM (STM, atomically)
import Data.Aeson (Value)
import Data.List (sortOn)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Ord (Down (..))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Data.UUID (UUID)

import qualified System.Agents.Base as Base
import System.Agents.OS.Conversation.Types
import System.Agents.OS.Core.Types (EntityId (..), ToolCallId (..), TurnId (..))
import System.Agents.OS.Core.World (World)
import qualified System.Agents.OS.Core.World as World
import qualified System.Agents.Session.Types as Session

-------------------------------------------------------------------------------
-- Component registration
-------------------------------------------------------------------------------

{- | Register the component stores required for tool-call entities.

Creates empty stores for 'ToolCallConfig' and 'ToolCallState' if they
do not already exist in the world.

/Warning:/ This always allocates fresh stores and returns a new 'World'
value. If stores already exist and contain data, calling this function
will replace them with empty stores. Prefer 'ensureToolCallComponents' for
idempotent registration.
-}
registerToolCallComponents :: World -> STM World
registerToolCallComponents world = do
    world1 <- World.registerComponentStore world (Proxy @ToolCallConfig)
    World.registerComponentStore world1 (Proxy @ToolCallState)

{- | Idempotently register tool-call component stores.

If both the 'ToolCallConfig' and 'ToolCallState' stores already exist,
the existing world is returned unchanged. Otherwise the missing stores
are created.
-}
ensureToolCallComponents :: World -> STM World
ensureToolCallComponents world = do
    mConfigStore <- World.getComponentStore world (Proxy @ToolCallConfig)
    mStateStore <- World.getComponentStore world (Proxy @ToolCallState)
    case (mConfigStore, mStateStore) of
        (Just _, Just _) -> pure world
        _ -> registerToolCallComponents world

{- | 'ensureToolCallComponents' wrapped in 'atomically' for convenience.
-}
ensureToolCallComponentsIO :: World -> IO World
ensureToolCallComponentsIO = atomically . ensureToolCallComponents

-------------------------------------------------------------------------------
-- Entity lifecycle
-------------------------------------------------------------------------------

{- | Create a new tool-call entity in the 'TcPending' state.

The ECS 'EntityId' is derived from the session-layer 'Session.ToolCallId'
UUID so that the call can later be looked up by its session id.

The caller should ensure that the tool-call component stores are
registered in the world (see 'ensureToolCallComponents'). If the stores
are missing, 'World.setComponent' will silently do nothing.
-}
createToolCallEntity ::
    World ->
    Session.SessionId ->
    Base.ConversationId ->
    Session.TurnId ->
    Maybe EntityId ->
    Text ->
    Value ->
    Session.ToolCallId ->
    IO EntityId
createToolCallEntity world sessId convId turnId mParentEntityId toolName toolInput sessToolCallId =
    createToolCallEntityWithProviderId world sessId convId turnId mParentEntityId toolName toolInput sessToolCallId Nothing

{- | Like 'createToolCallEntity', additionally recording the tool-call id
assigned by the LLM provider so capabilities can resolve it.
-}
createToolCallEntityWithProviderId ::
    World ->
    Session.SessionId ->
    Base.ConversationId ->
    Session.TurnId ->
    Maybe EntityId ->
    Text ->
    Value ->
    Session.ToolCallId ->
    Maybe Text ->
    IO EntityId
createToolCallEntityWithProviderId world sessId convId turnId mParentEntityId toolName toolInput sessToolCallId mProviderId = do
    let eid = sessionToolCallIdToEntityId sessToolCallId
        cfg =
            ToolCallConfig
                { tcTurnId = sessionTurnIdToOs turnId
                , tcToolName = toolName
                , tcToolInput = toolInput
                , tcParentCallId = fmap ToolCallId mParentEntityId
                , tcSessionId = sessId
                , tcConversationId = convId
                , tcProviderCallId = mProviderId
                , tcChildSessionId = Nothing
                }
        st =
            ToolCallState
                { tcStatus = TcPending
                , tcStartedAt = Nothing
                , tcCompletedAt = Nothing
                , tcResult = Nothing
                , tcProgress = []
                }
    atomically $ do
        World.setComponent world eid cfg
        World.setComponent world eid st
    pure eid

-- | Mark a tool-call entity as executing and record its start time.
startToolCall :: World -> EntityId -> IO ()
startToolCall world eid = do
    now <- getCurrentTime
    atomically $ World.modifyComponent @ToolCallState world eid $ \st ->
        st
            { tcStatus = TcExecuting
            , tcStartedAt = Just now
            }

-- | Mark a tool-call entity as completed with a result.
--
-- If the entity has already been cancelled, the state is left unchanged
-- so that cancellation is not overwritten by a late engine completion.
completeToolCall :: World -> EntityId -> Value -> IO ()
completeToolCall world eid result = do
    now <- getCurrentTime
    atomically $ World.modifyComponent @ToolCallState world eid $ \st ->
        case st.tcStatus of
            TcCancelled -> st
            _ ->
                st
                    { tcStatus = TcCompleted result
                    , tcCompletedAt = Just now
                    , tcResult = Just result
                    }

-- | Mark a tool-call entity as failed with an error message.
--
-- If the entity has already been cancelled, the state is left unchanged
-- so that cancellation is not overwritten by a late engine failure.
failToolCall :: World -> EntityId -> Text -> IO ()
failToolCall world eid err = do
    now <- getCurrentTime
    atomically $ World.modifyComponent @ToolCallState world eid $ \st ->
        case st.tcStatus of
            TcCancelled -> st
            _ ->
                st
                    { tcStatus = TcFailed err
                    , tcCompletedAt = Just now
                    }

-- | Mark a tool-call entity as cancelled.
--
-- Calls that already reached a final state are left unchanged so that a
-- late cancellation does not overwrite a result.
cancelToolCall :: World -> EntityId -> IO ()
cancelToolCall world eid = do
    now <- getCurrentTime
    atomically $ World.modifyComponent @ToolCallState world eid $ \st ->
        if isToolCallCompleted st
            then st
            else
                st
                    { tcStatus = TcCancelled
                    , tcCompletedAt = Just now
                    }

-- | Add a progress update to a tool-call entity.
addToolCallProgress :: World -> EntityId -> ToolCallProgress -> IO ()
addToolCallProgress world eid progress =
    atomically $ World.modifyComponent @ToolCallState world eid $ \st ->
        st{tcProgress = take maxToolCallProgressEntries (progress : tcProgress st)}

{- | Record the child session a @prompt_agent_\<slug\>@ call started, once it
exists (@todos/session-mailbox.md@ Phase 4, §5). A no-op if the entity has
no 'ToolCallConfig' (e.g. it was never registered).
-}
recordChildSession :: World -> EntityId -> Session.SessionId -> IO ()
recordChildSession world eid childSessionId =
    atomically $ World.modifyComponent @ToolCallConfig world eid $ \cfg ->
        cfg{tcChildSessionId = Just childSessionId}

{- | Number of progress entries kept per tool call (newest first), so chatty
tools cannot grow the entity without bound.
-}
maxToolCallProgressEntries :: Int
maxToolCallProgressEntries = 50

-------------------------------------------------------------------------------
-- Queries
-------------------------------------------------------------------------------

{- | Find the ECS entity id for a session-layer tool call.

The entity id is located by comparing the session call's UUID with the
'EntityId' UUID of tool-call entities that belong to the given session.
-}
findToolCallEntityBySessionId ::
    World ->
    Session.SessionId ->
    Session.ToolCallId ->
    STM (Maybe EntityId)
findToolCallEntityBySessionId world sessId sessToolCallId = do
    let targetUuid = sessionToolCallIdToUuid sessToolCallId
    eids <- World.allEntitiesWithComponent @ToolCallConfig world
    matches <- mapM (check targetUuid) eids
    pure $ listToMaybe (catMaybes matches)
  where
    check :: UUID -> EntityId -> STM (Maybe EntityId)
    check targetUuid eid
        | unEntityId eid /= targetUuid = pure Nothing
        | otherwise = do
            mConfig <- World.getComponent @ToolCallConfig world eid
            pure $ case mConfig of
                Just cfg | cfg.tcSessionId == sessId -> Just eid
                _ -> Nothing

{- | Find the ECS entity id for a session-layer tool call by its id.

The ECS 'EntityId' is derived directly from the session-layer 'ToolCallId'
UUID, so this function checks whether a tool-call config exists at that
entity id. The session id is not required because the 'ToolCallId' is
already globally unique.
-}
findToolCallEntityByToolCallId :: World -> Session.ToolCallId -> IO (Maybe EntityId)
findToolCallEntityByToolCallId world sessToolCallId = do
    let targetUuid = sessionToolCallIdToUuid sessToolCallId
        targetEid = EntityId targetUuid
    atomically $ do
        mConfig <- World.getComponent @ToolCallConfig world targetEid
        pure $ case mConfig of
            Just _ -> Just targetEid
            Nothing -> Nothing

{- | Find a tool-call entity by the id the LLM provider assigned to it.

Provider ids are only unique within a conversation (and some providers
reuse ids such as @functions.bash:0@ across turns), so the lookup is scoped
to the session and conversation. When several entities match, calls that
are still in flight win, then the most recently started one.
-}
findToolCallEntityByProviderCallId ::
    World ->
    Session.SessionId ->
    Base.ConversationId ->
    Text ->
    STM (Maybe EntityId)
findToolCallEntityByProviderCallId world sessId convId providerId = do
    triples <- listToolCallsBySessionAndConversation world sessId convId
    let matches =
            [ (eid, st)
            | (eid, cfg, st) <- triples
            , cfg.tcProviderCallId == Just providerId
            ]
    pure $
        fmap fst $
            listToMaybe $
                sortOn (\(_, st) -> (isToolCallCompleted st, Down st.tcStartedAt)) matches

{- | Return whether a tool-call entity is in a final state.

Final states are 'TcCompleted', 'TcFailed', and 'TcCancelled'.
Returns 'False' if the entity has no 'ToolCallState'.
-}
isToolCallCompletedIO :: World -> EntityId -> IO Bool
isToolCallCompletedIO world eid = do
    atomically $ do
        mState <- World.getComponent @ToolCallState world eid
        pure $ maybe False isToolCallCompleted mState

{- | List all tool-call entities belonging to a session and conversation.

Returns triples of @(entityId, config, state)@ for every tool-call entity
that has both components and whose config matches the given session and
conversation ids.
-}
listToolCallsBySessionAndConversation ::
    World ->
    Session.SessionId ->
    Base.ConversationId ->
    STM [(EntityId, ToolCallConfig, ToolCallState)]
listToolCallsBySessionAndConversation world sessId convId = do
    eids <- World.allEntitiesWithComponent @ToolCallConfig world
    fmap catMaybes $ mapM loadTriple eids
  where
    loadTriple :: EntityId -> STM (Maybe (EntityId, ToolCallConfig, ToolCallState))
    loadTriple eid = do
        mConfig <- World.getComponent @ToolCallConfig world eid
        case mConfig of
            Just cfg | cfg.tcSessionId == sessId && cfg.tcConversationId == convId -> do
                mState <- World.getComponent @ToolCallState world eid
                pure $ case mState of
                    Just st -> Just (eid, cfg, st)
                    Nothing -> Nothing
            _ -> pure Nothing

-------------------------------------------------------------------------------
-- Conversion helpers
-------------------------------------------------------------------------------

-- | Convert a session-layer 'Session.TurnId' to an OS 'TurnId'.
sessionTurnIdToOs :: Session.TurnId -> TurnId
sessionTurnIdToOs (Session.TurnId uuid) = TurnId (EntityId uuid)

-- | Convert a session-layer 'Session.ToolCallId' to an 'EntityId'.
sessionToolCallIdToEntityId :: Session.ToolCallId -> EntityId
sessionToolCallIdToEntityId (Session.ToolCallId uuid) = EntityId uuid

-- | Extract the UUID from a session-layer 'Session.ToolCallId'.
sessionToolCallIdToUuid :: Session.ToolCallId -> UUID
sessionToolCallIdToUuid (Session.ToolCallId uuid) = uuid

