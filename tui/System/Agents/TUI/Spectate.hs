{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | The terminal program behind @agents-exe spectate@: a read-only
dashboard over a 'RunnerClient'.

Everything it shows comes from "System.Agents.Spectate", the pure model
this module only paints, in three panels: the session tree, the tool
calls, and the text of one session -- the one the model is writing in,
unless the spectator picked another in the tree.

Which panels show, where and how large, and how often the screen
refreshes, is a 'Layout' ("System.Agents.Spectate.Layout"): given at start
('SpectateConfig') and changed with keys while running, as @top@ does.

It sends no command after its initial 'Client.listSessions': the keys only
change what the spectator sees, save the layout to a local file, or quit.
-}
module System.Agents.TUI.Spectate (
    -- * Running
    runSpectate,
    SpectateConfig (..),
    defaultSpectateConfig,

    -- * State, exposed for tests
    SpectateState (..),
    SpectateEvent (..),
    initSpectateState,
    withLayout,
    focusedSession,
    moveSelection,
    applyRefresh,
    layoutKey,
    helpLines,
    textWindow,
    rowWindow,

    -- * Drawing
    drawSpectate,
) where

import Brick
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Widgets.Border (border, borderWithLabel)
import Brick.Widgets.Center (centerLayer)
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (IOException, SomeException, displayException, try)
import Control.Lens ((^.))
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (findIndex)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import qualified Graphics.Vty as Vty
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

import System.Agents.Host.Client (RunnerClient)
import qualified System.Agents.Host.Client as Client
import System.Agents.Host.Runner (ReplayUnavailable (..))
import qualified System.Agents.Protocol as Protocol
import System.Agents.Session.Base (SessionId, SessionStatus (..))
import System.Agents.SessionStore (SessionQuery (..), allSessionsQuery)
import System.Agents.Spectate
import System.Agents.Spectate.Layout

-------------------------------------------------------------------------------
-- State
-------------------------------------------------------------------------------

data SpectateState = SpectateState
    { ssModel :: Spectate
    , ssNow :: UTCTime
    -- ^ The clock durations are shown against, moved by every refresh.
    , ssPinned :: Maybe SessionId
    -- ^ The session the spectator selected; 'Nothing' follows the action.
    , ssScroll :: Int
    -- ^ Lines the text pane is scrolled back from its end.
    , ssSource :: Text
    -- ^ What is being watched, for the status line.
    , ssStreamEnded :: Bool
    , ssLayout :: Layout
    -- ^ The panels shown and the refresh interval.
    , ssStartLayout :: Layout
    -- ^ The layout the spectator started with, which @0@ goes back to.
    , ssActive :: Panel
    -- ^ The panel the layout keys move and resize.
    , ssHelp :: Bool
    -- ^ The key help is shown over the screen.
    , ssNotice :: Maybe Text
    -- ^ How the last save went, shown until the next key.
    }

data SpectateEvent
    = {- | One refresh of the screen: its time, and the events received
      since the last one, each with when it was received.
      -}
      SpectateRefresh UTCTime [(UTCTime, Protocol.Event)]
    | -- | The event stream ended for good.
      SpectateStreamEnded

-- | A state showing 'defaultLayout'; 'withLayout' starts from another.
initSpectateState :: Text -> UTCTime -> Spectate -> SpectateState
initSpectateState source now model =
    SpectateState
        { ssModel = model
        , ssNow = now
        , ssPinned = Nothing
        , ssScroll = 0
        , ssSource = source
        , ssStreamEnded = False
        , ssLayout = defaultLayout
        , ssStartLayout = defaultLayout
        , ssActive = nextPanel 0 PanelTree defaultLayout
        , ssHelp = False
        , ssNotice = Nothing
        }

-- | Start from a layout: it is shown, and is what @0@ goes back to.
withLayout :: Layout -> SpectateState -> SpectateState
withLayout layout st =
    st{ssLayout = layout, ssStartLayout = layout, ssActive = nextPanel 0 st.ssActive layout}

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

-- | Fold the events of one refresh, and move the clock to it.
applyRefresh :: UTCTime -> [(UTCTime, Protocol.Event)] -> SpectateState -> SpectateState
applyRefresh now events st =
    st
        { ssModel = foldl (\model (received, ev) -> applyEvent received ev model) st.ssModel events
        , ssNow = max now st.ssNow
        }

{- | What a key that tunes the display does to the state; 'Nothing' for any
other key. None of them sends anything anywhere.
-}
layoutKey :: Char -> SpectateState -> Maybe SpectateState
layoutKey key st = case key of
    '1' -> toggled PanelTree
    '2' -> toggled PanelTools
    '3' -> toggled PanelText
    '<' -> change (movePanel (-1) active)
    '>' -> change (movePanel 1 active)
    '[' -> change (resizeWidth (-1) active)
    ']' -> change (resizeWidth 1 active)
    '-' -> change (resizeHeight (-1) active)
    '+' -> change (resizeHeight 1 active)
    '=' -> change (resizeHeight 1 active)
    's' -> change (restackPanel active)
    'd' -> change (\l -> l{layRefreshMs = slowerRefresh l.layRefreshMs})
    'D' -> change (\l -> l{layRefreshMs = fasterRefresh l.layRefreshMs})
    '0' -> Just (withLayout st.ssStartLayout st)
    _ -> Nothing
  where
    active = st.ssActive
    change f = Just st{ssLayout = f st.ssLayout}
    -- A panel that comes back is the one the next keys act on; when the
    -- active one goes, the first visible one takes over.
    toggled panel =
        let layout = togglePanel panel st.ssLayout
            wasShown = panel `elem` visiblePanels st.ssLayout
         in Just st{ssLayout = layout, ssActive = if wasShown then nextPanel 0 active layout else panel}

-------------------------------------------------------------------------------
-- Running
-------------------------------------------------------------------------------

-- | How 'runSpectate' starts.
data SpectateConfig = SpectateConfig
    { scLayout :: Layout
    , scLayoutFile :: Maybe FilePath
    -- ^ Where @W@ saves the layout; with 'Nothing' there is nowhere to.
    }

defaultSpectateConfig :: SpectateConfig
defaultSpectateConfig = SpectateConfig defaultLayout Nothing

{- | Watch a runner until the spectator quits. The sessions running right
now are listed once, so that a run already in progress shows up; from then
on the display only follows the event stream.

Events are folded, and the screen redrawn, once per refresh interval; the
ones received in between wait.
-}
runSpectate :: SpectateConfig -> Text -> RunnerClient -> IO (Either Text ())
runSpectate config source client = do
    subscribed <- try (Client.subscribeAll client)
    case subscribed of
        Left (e :: SomeException) -> pure (Left (Text.pack (displayException e)))
        Right (Left ReplayUnavailable) -> pure (Left "the event stream is not available")
        Right (Right sub) -> do
            running <- either (const []) id <$> Client.listSessions client allSessionsQuery{sqStatuses = Just [StatusRunning]}
            now <- getCurrentTime
            chan <- newBChan 16
            pending <- newIORef []
            interval <- newIORef config.scLayout.layRefreshMs
            void $ forkIO (bridge chan pending sub)
            void $ forkIO (refresher chan pending interval now)
            let st = withLayout config.scLayout (initSpectateState source now (seedSessions now running emptySpectate))
            void $ customMainWithDefaultVty (Just chan) (app (SpectateEnv interval config.scLayoutFile)) st
            Client.subClose sub
            pure (Right ())

-- | Events received and not shown yet, most recent first.
type Pending = IORef [(UTCTime, Protocol.Event)]

takePending :: Pending -> IO [(UTCTime, Protocol.Event)]
takePending pending = reverse <$> atomicModifyIORef' pending (\evs -> ([], evs))

{- | Receive the runner's events, stamped on receipt, until the stream
fails; what was received by then is shown before the end is.
-}
bridge :: BChan SpectateEvent -> Pending -> Client.Subscription -> IO ()
bridge chan pending sub = loop
  where
    loop =
        (try (Client.subNext sub) :: IO (Either SomeException Protocol.Event)) >>= \case
            Left _ -> do
                now <- getCurrentTime
                writeBChan chan . SpectateRefresh now =<< takePending pending
                writeBChan chan SpectateStreamEnded
            Right ev -> do
                now <- getCurrentTime
                atomicModifyIORef' pending (\evs -> ((now, ev) : evs, ()))
                loop

{- | Refresh the screen every interval. The interval is read again after
each short sleep, so that a change made with the keys applies at once
rather than after the interval it replaces.
-}
refresher :: BChan SpectateEvent -> Pending -> IORef Int -> UTCTime -> IO ()
refresher chan pending interval = loop
  where
    loop lastRefresh = do
        threadDelay (refreshPollMs * 1000)
        ms <- readIORef interval
        now <- getCurrentTime
        if diffUTCTime now lastRefresh * 1000 >= fromIntegral ms
            then do
                writeBChan chan . SpectateRefresh now =<< takePending pending
                loop now
            else loop lastRefresh

-- | How often 'refresher' looks at the clock; no interval is shorter.
refreshPollMs :: Int
refreshPollMs = 50

-- | What the key handler needs besides the state.
data SpectateEnv = SpectateEnv
    { envInterval :: IORef Int
    -- ^ The refresh interval 'refresher' reads, in milliseconds.
    , envLayoutFile :: Maybe FilePath
    }

app :: SpectateEnv -> App SpectateState SpectateEvent ()
app env =
    App
        { appDraw = drawSpectate
        , appChooseCursor = neverShowCursor
        , appHandleEvent = handleEvent env
        , appStartEvent = pure ()
        , appAttrMap = const spectateAttrMap
        }

handleEvent :: SpectateEnv -> BrickEvent () SpectateEvent -> EventM () SpectateState ()
handleEvent env = \case
    AppEvent (SpectateRefresh now events) -> modify (applyRefresh now events)
    AppEvent SpectateStreamEnded -> modify (\st -> st{ssStreamEnded = True})
    VtyEvent (Vty.EvKey key mods) -> do
        modify (\st -> st{ssNotice = Nothing})
        st <- get
        case (key, mods) of
            (Vty.KChar 'c', [Vty.MCtrl]) -> halt
            (Vty.KChar 'q', []) -> halt
            (Vty.KEsc, [])
                | st.ssHelp -> put st{ssHelp = False}
                | otherwise -> halt
            (Vty.KChar '?', []) -> put st{ssHelp = not st.ssHelp}
            (Vty.KChar 'h', []) -> put st{ssHelp = not st.ssHelp}
            (Vty.KUp, []) -> modify (moveSelection (-1))
            (Vty.KChar 'k', []) -> modify (moveSelection (-1))
            (Vty.KDown, []) -> modify (moveSelection 1)
            (Vty.KChar 'j', []) -> modify (moveSelection 1)
            (Vty.KChar 'f', []) -> put st{ssPinned = Nothing, ssScroll = 0}
            (Vty.KPageUp, []) -> put st{ssScroll = st.ssScroll + scrollStep}
            (Vty.KPageDown, []) -> put st{ssScroll = max 0 (st.ssScroll - scrollStep)}
            (Vty.KChar '\t', []) -> put st{ssActive = nextPanel 1 st.ssActive st.ssLayout}
            (Vty.KBackTab, []) -> put st{ssActive = nextPanel (-1) st.ssActive st.ssLayout}
            (Vty.KChar 'W', []) -> do
                notice <- liftIO (saveLayout env.envLayoutFile st.ssLayout)
                put st{ssNotice = Just notice}
            (Vty.KChar c, []) -> case layoutKey c st of
                Nothing -> pure ()
                Just changed -> do
                    when (changed.ssLayout.layRefreshMs /= st.ssLayout.layRefreshMs) $
                        liftIO (writeIORef env.envInterval changed.ssLayout.layRefreshMs)
                    put changed
            _ -> pure ()
    _ -> pure ()

-- | Write the layout to its file; the text says how it went.
saveLayout :: Maybe FilePath -> Layout -> IO Text
saveLayout Nothing _ = pure "no layout file to save to"
saveLayout (Just path) layout = do
    written <- try $ do
        createDirectoryIfMissing True (takeDirectory path)
        Text.writeFile path (renderLayoutFile layout)
    pure $ case written of
        Right () -> "layout saved to " <> Text.pack path
        Left (e :: IOException) -> "layout not saved: " <> Text.pack (displayException e)

scrollStep :: Int
scrollStep = 10

-------------------------------------------------------------------------------
-- Drawing
-------------------------------------------------------------------------------

runningAttr, failedAttr, quietAttr, selectedAttr, statusAttr, activeAttr :: AttrName
runningAttr = attrName "spectateRunning"
failedAttr = attrName "spectateFailed"
quietAttr = attrName "spectateQuiet"
selectedAttr = attrName "spectateSelected"
statusAttr = attrName "spectateStatus"
activeAttr = attrName "spectateActive"

spectateAttrMap :: AttrMap
spectateAttrMap =
    attrMap
        Vty.defAttr
        [ (runningAttr, fg Vty.green)
        , (failedAttr, fg Vty.red)
        , (quietAttr, Vty.defAttr `Vty.withStyle` Vty.dim)
        , (selectedAttr, Vty.defAttr `Vty.withStyle` Vty.reverseVideo)
        , (statusAttr, Vty.defAttr `Vty.withStyle` Vty.reverseVideo)
        , (activeAttr, Vty.defAttr `Vty.withStyle` Vty.bold)
        ]

-- | The whole screen; polymorphic in the widget name, it uses none.
drawSpectate :: SpectateState -> [Widget n]
drawSpectate st =
    [helpOverlay st | st.ssHelp]
        <> [ vBox
                [ columnsOf
                    [ (col.colWidth, stackOf [(slot.slotHeight, panel slot.slotPanel) | slot <- col.colSlots])
                    | col <- st.ssLayout.layColumns
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
        Nothing -> panelTitle PanelText
        Just node ->
            panelTitle PanelText <> ": " <> nodeLabel node <> (if st.ssPinned == Nothing then " (following)" else " (selected)")
    -- The active panel is only told apart when there is another one.
    several = length (visiblePanels st.ssLayout) > 1
    label p title
        | several && p == st.ssActive = withAttr activeAttr (txt ("[" <> title <> "]"))
        | otherwise = txt (" " <> title <> " ")
    panel p = case p of
        PanelTree -> borderWithLabel (label p (panelTitle p)) (rowsPane selected treeLines)
        PanelTools -> borderWithLabel (label p (panelTitle p)) (rowsPane Nothing toolLines)
        PanelText -> borderWithLabel (label p textTitle) (textPane st.ssScroll text)

{- | Widgets side by side, each as wide as its share of the width. Brick's
own percentages are of what the widgets before left, which only suits two.
-}
columnsOf :: [(Int, Widget n)] -> Widget n
columnsOf cols = Widget Greedy Greedy $ do
    ctx <- getContext
    let widths = shares (ctx ^. availWidthL) (map fst cols)
    render (hBox [hLimit w widget | (w, (_, widget)) <- zip widths cols, w > 0])

-- | Widgets on top of each other, each as tall as its share of the height.
stackOf :: [(Int, Widget n)] -> Widget n
stackOf slots = Widget Greedy Greedy $ do
    ctx <- getContext
    let heights = shares (ctx ^. availHeightL) (map fst slots)
    render (vBox [vLimit h widget | (h, (_, widget)) <- zip heights slots, h > 0])

helpOverlay :: SpectateState -> Widget n
helpOverlay st = centerLayer (border (padLeftRight 1 (vBox (map textLine (helpLines st)))))

{- | The flags that reproduce the layout on screen, first so that a short
terminal still shows them, then the keys.
-}
helpLines :: SpectateState -> [Text]
helpLines st =
    [ "This layout: " <> layoutFlags st.ssLayout
    , ""
    , "Sessions and text"
    , "  up/down, k/j    select a session; the text panel shows it"
    , "  f               follow the session that writes"
    , "  pgup/pgdn       scroll the text"
    , ""
    , "Panels"
    , "  1 2 3           show or hide agents, tool calls, text"
    , "  tab, shift-tab  choose the panel the next keys act on, now [" <> panelTitle st.ssActive <> "]"
    , "  < >             exchange it with the panel before or after"
    , "  [ ]             narrow or widen its column"
    , "  - +             shorten or heighten it in its column"
    , "  s               stack it under the column before, or give it its own"
    , "  0               back to the layout at start"
    , ""
    , "Refresh"
    , "  d D             longer or shorter interval, now " <> renderRefresh st.ssLayout.layRefreshMs <> "s"
    , ""
    , "  W               save this layout for the next runs"
    , "  ? or h          this help"
    , "  q, esc, ctrl-c  quit (the runs go on)"
    ]

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
    Text.intercalate " | " $
        maybe [] (\notice -> [" " <> notice]) st.ssNotice
            <> [ " " <> st.ssSource <> (if st.ssStreamEnded then " (stream ended)" else "")
               , showT (length (filter isActive nodes)) <> "/" <> count (length nodes) "session" <> " running"
               , count (length (filter ((== ToolRunning) . (.trOutcome)) (Map.elems st.ssModel.spTools))) "tool call"
               , count st.ssModel.spEvents "event"
               , "every " <> renderRefresh st.ssLayout.layRefreshMs <> "s"
               , "? keys, q quit "
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
    -- An empty list still fills its panel: padding has nothing to pad
    -- from no line at all.
    render (padBottom Max (padRight Max (vBox (if null shown then [textLine ""] else map line shown))))

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
