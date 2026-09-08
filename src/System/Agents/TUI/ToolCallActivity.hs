{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Live view of background (async) tool calls for the TUI.

The async engine publishes 'ToolCallActivity' events while calls run. Session
snapshots only change at step boundaries, so the TUI keeps the latest
activity per call to show what is running, its latest progress, and calls
that finished but whose result has not reached the session yet.
-}
module System.Agents.TUI.ToolCallActivity (
    ToolCallView (..),
    ToolCallViews,
    applyToolCallActivity,
    pruneToolCallViews,
    sessionToolCallViews,
    runningToolCallViews,
    describeToolCallView,
    summarizeProgress,
) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LByteString
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time (UTCTime)

import System.Agents.OS.Events (ToolCallActivity (..), ToolCallPhase (..), isFinalToolCallPhase)
import System.Agents.Session.Base (
    PartialUserTurnContent (..),
    Session (..),
    SessionId,
    ToolCallId,
    ToolCallState (..),
    TrackedToolCall (..),
    Turn (..),
 )

-- | What the TUI knows about one background call.
data ToolCallView = ToolCallView
    { tcvLatest :: ToolCallActivity
    -- ^ Most recent event for the call
    , tcvStartedAt :: UTCTime
    -- ^ Time of the first event seen for the call
    , tcvLastProgress :: Maybe Aeson.Value
    -- ^ Most recent progress payload, if the tool reported any
    }
    deriving (Show, Eq)

-- | Views per session, then per call.
type ToolCallViews = Map SessionId (Map ToolCallId ToolCallView)

-- | Record an activity event.
applyToolCallActivity :: ToolCallActivity -> ToolCallViews -> ToolCallViews
applyToolCallActivity act =
    Map.alter (Just . Map.alter (Just . update) act.tcaToolCallId . fromMaybe Map.empty) act.tcaSessionId
  where
    update :: Maybe ToolCallView -> ToolCallView
    update = \case
        Nothing -> ToolCallView act act.tcaAt (progressOf act.tcaPhase)
        Just prev ->
            ToolCallView
                { tcvLatest = act
                , tcvStartedAt = prev.tcvStartedAt
                , tcvLastProgress = maybe prev.tcvLastProgress Just (progressOf act.tcaPhase)
                }
    progressOf :: ToolCallPhase -> Maybe Aeson.Value
    progressOf = \case
        ToolCallProgressed v -> Just v
        _ -> Nothing

{- | Drop views the session snapshot has caught up with.

A finished call is kept while the session still shows it running, so the
TUI can tell that its result is waiting to be delivered. Calls the session
no longer shows as running are dropped once finished.
-}
pruneToolCallViews :: Session -> ToolCallViews -> ToolCallViews
pruneToolCallViews sess =
    Map.update keepSession sess.sessionId
  where
    running = [tc.tcId | PartialUserTurn p _ <- sess.turns, tc <- p.pTrackedToolCalls, tc.tcState == Running]
    keepSession :: Map ToolCallId ToolCallView -> Maybe (Map ToolCallId ToolCallView)
    keepSession views =
        let kept = Map.filterWithKey keep views
         in if Map.null kept then Nothing else Just kept
    keep :: ToolCallId -> ToolCallView -> Bool
    keep cid view = not (isFinalToolCallPhase view.tcvLatest.tcaPhase) || cid `elem` running

-- | Views for one session.
sessionToolCallViews :: SessionId -> ToolCallViews -> Map ToolCallId ToolCallView
sessionToolCallViews sid = Map.findWithDefault Map.empty sid

-- | Views of calls that are still running.
runningToolCallViews :: Map ToolCallId ToolCallView -> [ToolCallView]
runningToolCallViews = filter (not . isFinalToolCallPhase . (.tcvLatest.tcaPhase)) . Map.elems

-- | One-line status such as @running: 3 lines of output@.
describeToolCallView :: ToolCallView -> Text
describeToolCallView view =
    case view.tcvLatest.tcaPhase of
        ToolCallStarted -> "running"
        ToolCallProgressed v -> "running: " <> summarizeProgress v
        ToolCallCompleted -> "finished, result pending delivery"
        ToolCallFailed err -> "failed: " <> oneLine err
        ToolCallCancelled -> "cancelled"

{- | Short text for a progress payload: a string as-is, a common text field
of an object (@message@, @line@, @text@, @status@), else compact JSON.
-}
summarizeProgress :: Aeson.Value -> Text
summarizeProgress = oneLine . \case
    Aeson.String t -> t
    Aeson.Object obj
        | (t : _) <- [t | k <- ["message", "line", "text", "status"], Just (Aeson.String t) <- [KeyMap.lookup k obj]] ->
            t
    v -> Text.decodeUtf8 (LByteString.toStrict (Aeson.encode v))

-- | Last non-empty line, truncated for a status line.
oneLine :: Text -> Text
oneLine t =
    let line = case filter (not . Text.null . Text.strip) (Text.lines t) of
            [] -> ""
            ls -> Text.strip (last ls)
     in if Text.length line > 100 then Text.take 97 line <> "..." else line
