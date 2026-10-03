{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | The terminal program behind @agents-exe spectate@: a fixed, read-only
dashboard over a 'RunnerClient'.

Everything it shows comes from "System.Agents.Spectate", the pure model
this module only paints: the session tree on the top left, tool calls
below it, and on the right the text of one session -- the one the model
is writing in, unless the spectator picked another in the tree.

It sends no command after its initial 'Client.listSessions': the keys only
move the selection, scroll the text, or quit.
-}
module System.Agents.TUI.Spectate (
    -- * Running
    runSpectate,

    -- * State, exposed for tests
    SpectateState (..),
    SpectateEvent (..),
    initSpectateState,
    focusedSession,
    moveSelection,
    textWindow,
    rowWindow,

    -- * Drawing
    drawSpectate,
) where

import Brick
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Widgets.Border (borderWithLabel)
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, displayException, try)
import Control.Lens ((^.))
import Control.Monad (forever, void)
import Data.List (findIndex)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime, getCurrentTime)
import qualified Graphics.Vty as Vty

import System.Agents.Host.Client (RunnerClient)
import qualified System.Agents.Host.Client as Client
import System.Agents.Host.Runner (ReplayUnavailable (..))
import qualified System.Agents.Protocol as Protocol
import System.Agents.Session.Base (SessionId, SessionStatus (..))
import System.Agents.SessionStore (SessionQuery (..), allSessionsQuery)
import System.Agents.Spectate

-------------------------------------------------------------------------------
-- State
-------------------------------------------------------------------------------

data SpectateState = SpectateState
    { ssModel :: Spectate
    , ssNow :: UTCTime
    -- ^ The clock durations are shown against, moved by 'SpectateTick'.
    , ssPinned :: Maybe SessionId
    -- ^ The session the spectator selected; 'Nothing' follows the action.
    , ssScroll :: Int
    -- ^ Lines the text pane is scrolled back from its end.
    , ssSource :: Text
    -- ^ What is being watched, for the status line.
    , ssStreamEnded :: Bool
    }

data SpectateEvent
    = -- | An event of the runner, and when it was received.
      SpectateRunnerEvent UTCTime Protocol.Event
    | SpectateTick UTCTime
    | -- | The event stream ended for good.
      SpectateStreamEnded

initSpectateState :: Text -> UTCTime -> Spectate -> SpectateState
initSpectateState source now model = SpectateState model now Nothing 0 source False

-- | The session whose text is shown: the selected one while it exists.
focusedSession :: SpectateState -> Maybe SessionId
focusedSession st = case st.ssPinned of
    Just sid | Map.member sid st.ssModel.spNodes -> Just sid
    _ -> followTarget st.ssModel

-- | Select the tree row that many lines away from the focused one.
moveSelection :: Int -> SpectateState -> SpectateState
moveSelection delta st = case rows of
    [] -> st
    _ ->
        let current = maybe 0 id (focusedSession st >>= \sid -> findIndex ((== sid) . (.rowNode.nSession)) rows)
            target = max 0 (min (length rows - 1) (current + delta))
         in st{ssPinned = Just (rows !! target).rowNode.nSession, ssScroll = 0}
  where
    rows = treeRows st.ssModel

-------------------------------------------------------------------------------
-- Running
-------------------------------------------------------------------------------

{- | Watch a runner until the spectator quits. The sessions running right
now are listed once, so that a run already in progress shows up; from then
on the display only follows the event stream.
-}
runSpectate :: Text -> RunnerClient -> IO (Either Text ())
runSpectate source client = do
    subscribed <- try (Client.subscribeAll client)
    case subscribed of
        Left (e :: SomeException) -> pure (Left (Text.pack (displayException e)))
        Right (Left ReplayUnavailable) -> pure (Left "the event stream is not available")
        Right (Right sub) -> do
            running <- either (const []) id <$> Client.listSessions client allSessionsQuery{sqStatuses = Just [StatusRunning]}
            now <- getCurrentTime
            chan <- newBChan 256
            void $ forkIO (bridge chan sub)
            void $ forkIO $ forever $ do
                threadDelay 1000000
                writeBChan chan . SpectateTick =<< getCurrentTime
            let st = initSpectateState source now (seedSessions now running emptySpectate)
            void $ customMainWithDefaultVty (Just chan) app st
            Client.subClose sub
            pure (Right ())

-- | Forward the runner's events, stamped on receipt, until the stream fails.
bridge :: BChan SpectateEvent -> Client.Subscription -> IO ()
bridge chan sub = loop
  where
    loop =
        (try (Client.subNext sub) :: IO (Either SomeException Protocol.Event)) >>= \case
            Left _ -> writeBChan chan SpectateStreamEnded
            Right ev -> do
                now <- getCurrentTime
                writeBChan chan (SpectateRunnerEvent now ev)
                loop

app :: App SpectateState SpectateEvent ()
app =
    App
        { appDraw = drawSpectate
        , appChooseCursor = neverShowCursor
        , appHandleEvent = handleEvent
        , appStartEvent = pure ()
        , appAttrMap = const spectateAttrMap
        }

handleEvent :: BrickEvent () SpectateEvent -> EventM () SpectateState ()
handleEvent = \case
    AppEvent (SpectateRunnerEvent now ev) ->
        modify (\st -> st{ssModel = applyEvent now ev st.ssModel, ssNow = max now st.ssNow})
    AppEvent (SpectateTick now) -> modify (\st -> st{ssNow = now})
    AppEvent SpectateStreamEnded -> modify (\st -> st{ssStreamEnded = True})
    VtyEvent (Vty.EvKey key mods) -> case (key, mods) of
        (Vty.KChar 'q', []) -> halt
        (Vty.KEsc, []) -> halt
        (Vty.KChar 'c', [Vty.MCtrl]) -> halt
        (Vty.KUp, []) -> modify (moveSelection (-1))
        (Vty.KChar 'k', []) -> modify (moveSelection (-1))
        (Vty.KDown, []) -> modify (moveSelection 1)
        (Vty.KChar 'j', []) -> modify (moveSelection 1)
        (Vty.KChar 'f', []) -> modify (\st -> st{ssPinned = Nothing, ssScroll = 0})
        (Vty.KPageUp, []) -> modify (\st -> st{ssScroll = st.ssScroll + scrollStep})
        (Vty.KPageDown, []) -> modify (\st -> st{ssScroll = max 0 (st.ssScroll - scrollStep)})
        _ -> pure ()
    _ -> pure ()

scrollStep :: Int
scrollStep = 10

-------------------------------------------------------------------------------
-- Drawing
-------------------------------------------------------------------------------

runningAttr, failedAttr, quietAttr, selectedAttr, statusAttr :: AttrName
runningAttr = attrName "spectateRunning"
failedAttr = attrName "spectateFailed"
quietAttr = attrName "spectateQuiet"
selectedAttr = attrName "spectateSelected"
statusAttr = attrName "spectateStatus"

spectateAttrMap :: AttrMap
spectateAttrMap =
    attrMap
        Vty.defAttr
        [ (runningAttr, fg Vty.green)
        , (failedAttr, fg Vty.red)
        , (quietAttr, Vty.defAttr `Vty.withStyle` Vty.dim)
        , (selectedAttr, Vty.defAttr `Vty.withStyle` Vty.reverseVideo)
        , (statusAttr, Vty.defAttr `Vty.withStyle` Vty.reverseVideo)
        ]

-- | The whole screen; polymorphic in the widget name, it uses none.
drawSpectate :: SpectateState -> [Widget n]
drawSpectate st =
    [ vBox
        [ hBox
            [ hLimitPercent 45 $
                vBox
                    [ borderWithLabel (txt " agents ") (rowsPane selected treeLines)
                    , vLimitPercent 40 $ borderWithLabel (txt " tool calls ") (rowsPane Nothing toolLines)
                    ]
            , borderWithLabel (txt textTitle) (textPane st.ssScroll text)
            ]
        , withAttr statusAttr (padRight Max (txt (statusLine st)))
        ]
    ]
  where
    model = st.ssModel
    focus = focusedSession st
    rows = treeRows model
    selected = focus >>= \sid -> findIndex ((== sid) . (.rowNode.nSession)) rows
    treeLines = [(nodeAttr row.rowNode, renderTreeRow st.ssNow row) | row <- rows]
    toolLines = [(toolAttr row, renderToolRow st.ssNow model row) | row <- toolRows model]
    text = maybe "" (`sessionText` model) focus
    textTitle = case focus >>= (`Map.lookup` model.spNodes) of
        Nothing -> " text "
        Just node ->
            " text: " <> nodeLabel node <> (if st.ssPinned == Nothing then " (following) " else " (selected) ")

nodeAttr :: Node -> AttrName
nodeAttr node = case node.nState of
    NodeRunning -> runningAttr
    NodeFailed _ -> failedAttr
    _ -> quietAttr

toolAttr :: ToolRow -> AttrName
toolAttr row = case row.trOutcome of
    ToolRunning -> runningAttr
    ToolFailed _ -> failedAttr
    _ -> quietAttr

statusLine :: SpectateState -> Text
statusLine st =
    Text.intercalate
        " | "
        [ " " <> st.ssSource <> (if st.ssStreamEnded then " (stream ended)" else "")
        , showT (length (filter isActive nodes)) <> "/" <> count (length nodes) "session" <> " running"
        , count (length (filter ((== ToolRunning) . (.trOutcome)) (Map.elems st.ssModel.spTools))) "tool call"
        , count st.ssModel.spEvents "event"
        , "up/down select, f follow, pgup/pgdn scroll, q quit "
        ]
  where
    nodes = Map.elems st.ssModel.spNodes
    showT = Text.pack . show
    count n what = showT n <> " " <> what <> (if n == 1 then "" else "s")

{- | The lines of a list that fit a height, keeping the selected one in
view (roughly centred) when there is one.
-}
rowWindow :: Maybe Int -> Int -> [a] -> [a]
rowWindow selected height rows = take height (drop start rows)
  where
    start = case selected of
        Nothing -> 0
        Just i -> max 0 (min (i - height `div` 2) (length rows - height))

{- | The lines of a text that fit a width and a height: its end, or
earlier lines when scrolled back.
-}
textWindow :: Int -> Int -> Int -> Text -> [Text]
textWindow width height scroll text = take height (drop start ls)
  where
    ls = wrapText width (Text.stripEnd text)
    start = max 0 (length ls - height - max 0 scroll)

rowsPane :: Maybe Int -> [(AttrName, Text)] -> Widget n
rowsPane selected rows = Widget Greedy Greedy $ do
    ctx <- getContext
    let height = ctx ^. availHeightL
        shown = rowWindow selected height (zip [0 :: Int ..] rows)
        line (i, (attr, t))
            | Just i == selected = withAttr selectedAttr (padRight Max (textLine t))
            | otherwise = withAttr attr (textLine t)
    render (padBottom Max (padRight Max (vBox (map line shown))))

textPane :: Int -> Text -> Widget n
textPane scroll text = Widget Greedy Greedy $ do
    ctx <- getContext
    let shown = textWindow (ctx ^. availWidthL) (ctx ^. availHeightL) scroll text
    render (padBottom Max (padRight Max (vBox (map textLine shown))))

-- | One line of text; an empty one still takes a row.
textLine :: Text -> Widget n
textLine t
    | Text.null t = txt " "
    | otherwise = txt t
