{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- | System-toolbox capabilities for inspecting async tool calls.

This module implements the @get-tool-call-status@, @list-running-tool-calls@,
and @cancel-tool-call@ capabilities.  They look up the corresponding
ECS tool-call entity in the OS world and translate its state into
LLM-friendly JSON results.

Tool calls are addressed by the id the LLM provider assigned to them (e.g.
@call_abc123@), which is the only id the model knows. The internal
'Session.ToolCallId' UUID is accepted as well.
-}
module System.Agents.Tools.SystemToolbox.ToolCallStatus (
    -- * Capability handlers
    getToolCallStatus,
    listRunningToolCalls,
    cancelToolCallById,
) where

import Control.Concurrent.STM (atomically, retry)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (parseMaybe)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.UUID as UUID
import System.Timeout (timeout)

import System.Agents.OS.Conversation.ToolCalls (
    cancelToolCall,
    findToolCallEntityByProviderCallId,
    findToolCallEntityByToolCallId,
    listToolCallsBySessionAndConversation,
 )
import System.Agents.OS.Conversation.Types (
    ToolCallConfig,
    ToolCallState,
    ToolCallStatus (..),
    isToolCallCompleted,
    tcCompletedAt,
    tcProgress,
    tcProviderCallId,
    tcResult,
    tcStartedAt,
    tcStatus,
    tcToolName,
 )
import System.Agents.OS.Core.Types (EntityId (..), unEntityId)
import System.Agents.OS.Core.World (World, getComponent)
import qualified System.Agents.Session.Types as Session
import System.Agents.Tools.Context (ToolCall (..), ToolExecutionContext (..))
import System.Agents.Tools.SystemToolbox.Types (
    CancelToolCallParams (..),
    CancelToolCallResult (..),
    GetToolCallStatusParams (..),
    ListRunningToolCallsResult (..),
    QueryError (..),
    RunningToolCallInfo (..),
    ToolCallStatusResult (..),
 )

-------------------------------------------------------------------------------
-- get-tool-call-status
-------------------------------------------------------------------------------

{- | Query the status of a single tool call.

The call is looked up first in the OS world.  If it is not present there,
the context's full session history is searched for a tracked call with the
same id: a final call is reported from the session, a running call is
reported as @orphaned@ (which is final, since it can no longer complete), and
a deferred call as @pending@.

When 'waitForCompletion' is enabled, the function blocks until the entity
becomes final or the timeout expires.
-}
getToolCallStatus ::
    ToolExecutionContext ->
    GetToolCallStatusParams ->
    IO (Either QueryError ToolCallStatusResult)
getToolCallStatus ctx params = do
    mResolved <- resolveEntity ctx (toolCallId params)
    case (ctxWorld ctx, mResolved) of
        (Just world, Just eid) -> buildStatusResult params world eid
        _ -> findInSessionOrFail ctx params

-------------------------------------------------------------------------------
-- list-running-tool-calls
-------------------------------------------------------------------------------

{- | List all tool-call entities for the current session/conversation that
have not yet reached a final state.
-}
listRunningToolCalls ::
    ToolExecutionContext ->
    IO (Either QueryError ListRunningToolCallsResult)
listRunningToolCalls ctx =
    case ctxWorld ctx of
        Nothing -> pure $ Left $ SystemInfoError "OS world not available"
        Just world -> do
            entities <-
                atomically $
                    listToolCallsBySessionAndConversation
                        world
                        (ctxSessionId ctx)
                        (ctxConversationId ctx)
            let running =
                    [ mkRunningInfo eid cfg st
                    | (eid, cfg, st) <- entities
                    , not (isToolCallCompleted st)
                    ]
            pure $ Right $ ListRunningToolCallsResult running
  where
    mkRunningInfo :: EntityId -> ToolCallConfig -> ToolCallState -> RunningToolCallInfo
    mkRunningInfo eid cfg st =
        RunningToolCallInfo
            { rtciToolCallId = fromMaybe (UUID.toText (unEntityId eid)) (tcProviderCallId cfg)
            , rtciToolName = tcToolName cfg
            , rtciStatus = statusText (tcStatus st)
            , rtciStartedAt = tcStartedAt st
            }

-------------------------------------------------------------------------------
-- cancel-tool-call
-------------------------------------------------------------------------------

{- | Cancel a running tool call by id.

If the async engine owns the call, its background thread is interrupted
and the entity is marked cancelled. Otherwise (e.g. the call is still
pending, or its engine is gone) the entity is marked cancelled on a
best-effort basis. Returns @cancelled: false@ with the previous status when
the call was already final.
-}
cancelToolCallById ::
    ToolExecutionContext ->
    CancelToolCallParams ->
    IO (Either QueryError CancelToolCallResult)
cancelToolCallById ctx params =
    case ctxWorld ctx of
        Nothing -> pure $ Left $ SystemInfoError "OS world not available"
        Just world -> do
            mEid <- resolveEntity ctx (ctpToolCallId params)
            case mEid of
                Nothing -> pure $ Left $ SystemInfoError "tool call not found"
                Just eid -> do
                    mSt <- atomically $ getComponent @ToolCallState world eid
                    case mSt of
                        Nothing -> pure $ Left $ SystemInfoError "tool call not found"
                        Just st
                            | isToolCallCompleted st -> pure $ Right $ result False st
                            | otherwise -> do
                                killed <- case ctxCancelToolCall ctx of
                                    Just cancelHook -> cancelHook (Session.ToolCallId (unEntityId eid))
                                    Nothing -> pure False
                                if killed
                                    then pure $ Right $ result True st
                                    else do
                                        cancelToolCall world eid
                                        mSt' <- atomically $ getComponent @ToolCallState world eid
                                        let cancelled = fmap tcStatus mSt' == Just TcCancelled
                                        pure $ Right $ result cancelled st
  where
    result cancelled st =
        CancelToolCallResult
            { ccrToolCallId = ctpToolCallId params
            , ccrCancelled = cancelled
            , ccrPreviousStatus = statusText (tcStatus st)
            }

-------------------------------------------------------------------------------
-- Internal helpers
-------------------------------------------------------------------------------

{- | Resolve a textual tool-call id to its OS entity.

An internal UUID is tried first; otherwise the text is treated as a
provider id scoped to the context's session and conversation.
-}
resolveEntity :: ToolExecutionContext -> Text -> IO (Maybe EntityId)
resolveEntity ctx txt =
    case ctxWorld ctx of
        Nothing -> pure Nothing
        Just world -> do
            byUuid <- case UUID.fromText txt of
                Just uuid -> findToolCallEntityByToolCallId world (Session.ToolCallId uuid)
                Nothing -> pure Nothing
            case byUuid of
                Just eid -> pure (Just eid)
                Nothing ->
                    atomically $
                        findToolCallEntityByProviderCallId
                            world
                            (ctxSessionId ctx)
                            (ctxConversationId ctx)
                            txt

-- | Translate an OS tool-call status to an LLM-facing string.
statusText :: ToolCallStatus -> Text
statusText TcPending = "pending"
statusText TcExecuting = "running"
statusText (TcCompleted _) = "completed"
statusText (TcFailed _) = "failed"
statusText TcCancelled = "cancelled"

{- | Look for the call id in the partial turns of the full session.

A final call is reported from the session; any other call is @orphaned@.
Returns a "not found" error when the call is not in the session either.
-}
findInSessionOrFail ::
    ToolExecutionContext ->
    GetToolCallStatusParams ->
    IO (Either QueryError ToolCallStatusResult)
findInSessionOrFail ctx params =
    case findKnownCall ctx (toolCallId params) of
        Nothing -> pure $ Left $ SystemInfoError "tool call not found"
        Just tc -> pure $ Right $ sessionResult params tc

{- | Look up a call among the tracked calls carried by the context, falling
back to the full session when the agent includes it.
-}
findKnownCall :: ToolExecutionContext -> Text -> Maybe Session.TrackedToolCall
findKnownCall ctx txt =
    case listToMaybe (filter (matchesToolCallId txt) (ctxSessionToolCalls ctx)) of
        Just tc -> Just tc
        Nothing -> ctxFullSession ctx >>= \session -> findTrackedCall session txt

{- | Search the session turns for a tracked call whose internal or provider
id matches. Turns are stored newest first, so the most recent match wins.
-}
findTrackedCall :: Session.Session -> Text -> Maybe Session.TrackedToolCall
findTrackedCall session txt =
    listToMaybe
        [ tc
        | turn <- Session.turns session
        , Session.PartialUserTurn content _ <- [turn]
        , tc <- Session.pTrackedToolCalls content
        , matchesToolCallId txt tc
        ]

-- | Whether a tracked call is addressed by the given internal or provider id.
matchesToolCallId :: Text -> Session.TrackedToolCall -> Bool
matchesToolCallId txt tc =
    fmap Session.ToolCallId (UUID.fromText txt) == Just (Session.tcId tc)
        || Session.providerToolCallId (Session.tcCall tc) == Just txt

-- | Build a status result for a call known only from the session history.
sessionResult :: GetToolCallStatusParams -> Session.TrackedToolCall -> ToolCallStatusResult
sessionResult params tc =
    ToolCallStatusResult
        { tcsrToolCallId = toolCallId params
        , tcsrStatus = status
        , tcsrToolName = extractToolName (Session.tcCall tc)
        , tcsrStartedAt = Nothing
        , tcsrCompletedAt = Nothing
        , tcsrResult = if final then fmap Aeson.toJSON (Session.tcResult tc) else Nothing
        , tcsrProgress = []
        , tcsrIsFinal = final || orphaned
        }
  where
    final = Session.isFinalToolCallState (Session.tcState tc)
    -- A running call without an entity can never finish.
    orphaned = Session.tcState tc == Session.Running
    status = case Session.tcState tc of
        Session.Completed -> "completed"
        Session.Failed -> "failed"
        Session.Running -> "orphaned"
        Session.Deferred -> "pending"
        Session.Ready -> "pending"

-- | Try to extract a tool name from the LLM-issued call value.
extractToolName :: Session.LlmToolCall -> Maybe Text
extractToolName (Session.LlmToolCall val) =
    case parseMaybe Aeson.parseJSON val of
        Just (ToolCall name _) -> Just name
        Nothing ->
            case val of
                Aeson.Object obj ->
                    case KeyMap.lookup "function" obj of
                        Just (Aeson.Object func) ->
                            case KeyMap.lookup "name" func of
                                Just (Aeson.String n) -> Just n
                                _ -> Nothing
                        _ -> Nothing
                _ -> Nothing

-- | Build a status result from an OS tool-call entity.
buildStatusResult ::
    GetToolCallStatusParams ->
    World ->
    EntityId ->
    IO (Either QueryError ToolCallStatusResult)
buildStatusResult params world eid = do
    mComponents <-
        atomically $ do
            mCfg <- getComponent @ToolCallConfig world eid
            mSt <- getComponent @ToolCallState world eid
            pure (mCfg, mSt)
    case mComponents of
        (Just cfg, Just st) -> do
            st' <-
                if waitForCompletion params && not (isToolCallCompleted st)
                    then waitForFinal world eid st (timeoutSeconds params)
                    else pure st
            pure $ Right $ mkStatusResult params cfg st'
        _ -> pure $ Left $ SystemInfoError "tool call not found"

-- | Block until a tool-call entity reaches a final state or the timeout
-- (in seconds) expires, returning its latest state.
waitForFinal :: World -> EntityId -> ToolCallState -> Int -> IO ToolCallState
waitForFinal world eid initial seconds = do
    mFinal <- timeout (max 0 seconds * 1000000) $ atomically $ do
        mSt <- getComponent @ToolCallState world eid
        case mSt of
            Just st | isToolCallCompleted st -> pure st
            _ -> retry
    case mFinal of
        Just st -> pure st
        Nothing -> fromMaybe initial <$> atomically (getComponent @ToolCallState world eid)

-- | Assemble the final 'ToolCallStatusResult' from a config and state.
mkStatusResult ::
    GetToolCallStatusParams ->
    ToolCallConfig ->
    ToolCallState ->
    ToolCallStatusResult
mkStatusResult params cfg st =
    ToolCallStatusResult
        { tcsrToolCallId = toolCallId params
        , tcsrStatus = statusText (tcStatus st)
        , tcsrToolName = Just (tcToolName cfg)
        , tcsrStartedAt = tcStartedAt st
        , tcsrCompletedAt = tcCompletedAt st
        , tcsrResult = tcResult st
        , tcsrProgress = if includeProgress params then tcProgress st else []
        , tcsrIsFinal = isToolCallCompleted st
        }
