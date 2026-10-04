{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The @spectate@ dashboard: its pure model ("System.Agents.Spectate"),
its layout ("System.Agents.Spectate.Layout"), the selection, windowing and
layout keys of its screen ("System.Agents.TUI.Spectate"),
and the model folded over a real runner's event stream through
'inProcessClient' -- the client an embedded TUI drives, so what a spectator
shows does not depend on the events coming over HTTP.
-}
module SpectateTests (tests) where

import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time (UTCTime (..), addUTCTime, fromGregorian, getCurrentTime)
import qualified Data.UUID as UUID
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import AgentFactoryTests (mockCompletion, testNode)
import RunnerTests (expectRight, message, testHost)
import System.Agents.CLI.Spectate (resolveLayout)
import System.Agents.Host.Client
import System.Agents.Host.Runner (withSessionRunner)
import System.Agents.OS.Events (ToolCallActivity (..))
import qualified System.Agents.OS.Events as Activity
import System.Agents.Protocol
import System.Agents.Session.Base (
    LlmResponse (..),
    LlmTurnContent (..),
    SessionId (..),
    SessionStatus (..),
    ToolCallId (..),
    Turn (..),
 )
import System.Agents.SessionStore (SessionMeta (..), freshSessionMeta, sessionIdToConversationId)
import System.Agents.Spectate
import System.Agents.Spectate.Layout
import System.Agents.TUI.Spectate (
    SpectateState (..),
    applyRefresh,
    focusedSession,
    helpLines,
    initSpectateState,
    layoutKey,
    moveSelection,
    rowWindow,
    textWindow,
 )

tests :: TestTree
tests =
    testGroup
        "Spectate"
        [ testCase "sub-agent calls nest under their caller, by depth" treeTest
        , testCase "a sub-agent call ending marks an in-tool node, not a session that already stopped" subcallEndTest
        , testCase "durations are measured from when the spectator received the event" elapsedTest
        , testCase "tool calls: started, progress, first final state wins, running ones listed first" toolsTest
        , testCase "streamed text is not repeated by the stored turn" streamedTextTest
        , testCase "without streaming, the stored turn provides the text" storedTextTest
        , testCase "the followed session is the one that wrote last" followTest
        , testCase "sessions running at attach time are seeded" seedTest
        , testCase "a deleted session leaves the tree, with its tool calls" deleteTest
        , testCase "finished tool calls and quiet roots are bounded" pruneTest
        , testCase "formatElapsed" formatElapsedTest
        , testCase "wrapText breaks at spaces, and long words at the width" wrapTextTest
        , testCase "textWindow shows the end, or earlier lines when scrolled" textWindowTest
        , testCase "rowWindow keeps the selected row in view" rowWindowTest
        , testCase "moving the selection pins a session; a vanished one falls back to following" selectionTest
        , testCase "a run on a real runner, folded through inProcessClient" inProcessRunTest
        , testCase "--panels: columns, stacks and shares, read back from their rendering" panelsTest
        , testCase "--panels: what is refused, and why" panelsErrorTest
        , testCase "--refresh: seconds with decimals, within bounds" refreshTest
        , testCase "panels are hidden, shown, exchanged, restacked and resized" layoutChangesTest
        , testCase "shares add up to the cells there are" sharesTest
        , testCase "a saved layout is read back; flags go over it" savedLayoutTest
        , testCase "layout keys change the screen, and only the screen" layoutKeysTest
        , testCase "a refresh folds the events received since the last one" refreshFoldTest
        ]

-------------------------------------------------------------------------------
-- Fixtures
-------------------------------------------------------------------------------

t0 :: UTCTime
t0 = UTCTime (fromGregorian 2026 1 1) 0

-- | @t0@ plus that many seconds.
at :: Int -> UTCTime
at n = addUTCTime (fromIntegral n) t0

sid :: Int -> SessionId
sid n = SessionId (UUID.fromWords 0 0 0 (fromIntegral n))

call :: Int -> ToolCallId
call n = ToolCallId (UUID.fromWords 1 0 0 (fromIntegral n))

-- | Fold events, each received one second after the previous, from @t0@.
fold :: [(Maybe SessionId, EventBody)] -> Spectate
fold = foldFrom emptySpectate

foldFrom :: Spectate -> [(Maybe SessionId, EventBody)] -> Spectate
foldFrom sp0 evs =
    foldl
        (\sp (n, (s, body)) -> applyEvent (at n) (Event (EventSeq (fromIntegral n)) s Nothing body) sp)
        sp0
        (zip [0 ..] evs)

on :: Int -> EventBody -> (Maybe SessionId, EventBody)
on n body = (Just (sid n), body)

meta :: Int -> Maybe Text -> Maybe Int -> SessionStatus -> SessionMeta
meta n agent parent status =
    (freshSessionMeta (sid n) t0){smAgent = agent, smParent = sid <$> parent, smStatus = status}

llmTurn :: Text -> Turn
llmTurn answer = LlmTurn (LlmTurnContent (LlmResponse (Just answer) Nothing Aeson.Null Nothing) []) Nothing

activity :: Int -> Int -> Text -> Activity.ToolCallPhase -> EventBody
activity s c name phase =
    ToolCallProgressed (ToolCallActivity (sid s) (sessionIdToConversationId (sid s)) (call c) Nothing name phase t0)

node :: Int -> Spectate -> IO Node
node n sp = maybe (assertFailure ("no node " <> show n)) pure (Map.lookup (sid n) sp.spNodes)

-------------------------------------------------------------------------------
-- Model
-------------------------------------------------------------------------------

treeTest :: Assertion
treeTest = do
    let sp =
            fold
                [ (Nothing, SessionCreated (meta 1 (Just "root") Nothing StatusReady))
                , on 1 (RunStarted UntilBlocked)
                , on 1 (SubcallStarted (sid 1) (sid 2) "helper" 1)
                , on 2 (SubcallStarted (sid 2) (sid 3) "sub-helper" 2)
                , on 1 (SubcallStarted (sid 1) (sid 4) "other" 1)
                ]
    [(r.rowDepth, r.rowNode.nSession) | r <- treeRows sp]
        @?= [(0, sid 1), (1, sid 2), (2, sid 3), (1, sid 4)]
    map (\r -> r.rowNode.nAgent) (treeRows sp) @?= map Just ["root", "helper", "sub-helper", "other"]
    assertBool "all running" (all (isActive . (.rowNode)) (treeRows sp))
    -- A sub-agent call whose caller was never announced still hangs under it.
    let orphan = fold [on 7 (SubcallStarted (sid 7) (sid 8) "helper" 1)]
    [(r.rowDepth, r.rowNode.nSession) | r <- treeRows orphan] @?= [(0, sid 7), (1, sid 8)]

subcallEndTest :: Assertion
subcallEndTest = do
    -- In-tool call: no session events of its own, the subcall events are all there is.
    let inTool = fold [on 1 (SubcallStarted (sid 1) (sid 2) "helper" 1), on 1 (SubcallCompleted (sid 2) (Just "done"))]
    n2 <- node 2 inTool
    n2.nState @?= NodeReturned
    let failed = fold [on 1 (SubcallStarted (sid 1) (sid 2) "helper" 1), on 1 (SubcallFailed (sid 2) "boom\ntrace")]
    f2 <- node 2 failed
    f2.nState @?= NodeFailed "boom\ntrace"
    assertBool "failure shows its first line" ("failed 0s: boom" `Text.isSuffixOf` renderTreeRow (at 1) (TreeRow 1 f2))
    -- A real child session: created and run on its own stream, already stopped
    -- when the caller's subcall.completed arrives.
    let real =
            fold
                [ (Nothing, SessionCreated (meta 2 (Just "helper") (Just 1) StatusReady))
                , on 2 (RunStarted UntilBlocked)
                , on 1 (SubcallStarted (sid 1) (sid 2) "helper" 1)
                , on 2 (RunStopped StatusIdle)
                , on 1 (SubcallCompleted (sid 2) (Just "done"))
                ]
    r2 <- node 2 real
    r2.nState @?= NodeStopped StatusIdle
    r2.nParent @?= Just (sid 1)

elapsedTest :: Assertion
elapsedTest = do
    let sp = fold [(Nothing, SessionCreated (meta 1 (Just "root") Nothing StatusReady)), on 1 (RunStarted UntilBlocked)]
    n1 <- node 1 sp
    n1.nSince @?= at 1
    renderTreeRow (at 76) (TreeRow 0 n1) @?= "* root  00000000  running 1m15s"
    -- session.updated on every stored step does not restart the clock.
    let later = foldFrom sp [on 1 (TextDelta "x"), on 1 (SessionUpdated (meta 1 (Just "root") Nothing StatusRunning) Nothing)]
    n1' <- node 1 later
    n1'.nSince @?= at 1
    -- An unnamed session is labelled by its id.
    let anon = fold [on 9 (RunStarted StepOnce)]
    n9 <- node 9 anon
    renderTreeRow (at 3) (TreeRow 1 n9) @?= "  * session 00000000  running 3s"

toolsTest :: Assertion
toolsTest = do
    let sp =
            fold
                [ (Nothing, SessionCreated (meta 1 (Just "root") Nothing StatusRunning))
                , on 1 (ToolCallStarted (call 1) "bash")
                , on 1 (activity 1 2 "fetch" Activity.ToolCallStarted)
                , on 1 (activity 1 2 "fetch" (Activity.ToolCallProgressed (Aeson.object ["message" Aeson..= ("page 3" :: Text)])))
                , on 1 (ToolCallStarted (call 3) "grep")
                , on 1 (ToolCallCompleted (call 1) "bash" False)
                , on 1 (activity 1 3 "grep" Activity.ToolCallCompleted)
                , -- A later, different final state for the same call changes nothing.
                  on 1 (ToolCallCompleted (call 3) "grep" False)
                ]
    map (\r -> (r.trName, r.trOutcome)) (toolRows sp)
        @?= [("fetch", ToolRunning), ("grep", ToolSucceeded), ("bash", ToolFailed Nothing)]
    map (renderToolRow (at 10) sp) (toolRows sp)
        @?= [ "* fetch  root  8s  page 3"
            , "- grep  root  ok in 2s"
            , "! bash  root  failed in 4s"
            ]
    -- A completion for a call never announced still shows up, finished.
    let late = fold [on 1 (activity 1 5 "curl" (Activity.ToolCallFailed "timeout\nmore"))]
    map (renderToolRow (at 9) late) (toolRows late) @?= ["! curl  session 00000000  failed in 0s  timeout"]

streamedTextTest :: Assertion
streamedTextTest = do
    let sp =
            fold
                [ on 1 (TextDelta "Hello, ")
                , on 1 (TextDelta "world.")
                , on 1 (SessionUpdated (meta 1 Nothing Nothing StatusRunning) (Just (llmTurn "Hello, world.")))
                , on 1 (TextDelta "Next.")
                ]
    sessionText (sid 1) sp @?= "Hello, world.\n\nNext."

storedTextTest :: Assertion
storedTextTest = do
    let sp =
            fold
                [ on 1 (SessionUpdated (meta 1 Nothing Nothing StatusRunning) (Just (llmTurn "First answer.")))
                , on 1 (SessionUpdated (meta 1 Nothing Nothing StatusRunning) (Just (llmTurn "  ")))
                , on 1 (SessionUpdated (meta 1 Nothing Nothing StatusIdle) (Just (llmTurn "Second answer.")))
                ]
    sessionText (sid 1) sp @?= "First answer.\n\nSecond answer.\n\n"
    -- The text kept per session is bounded.
    let long = fold [on 1 (TextDelta (Text.replicate (maxTextChars + 10) "a")), on 1 (TextDelta "end")]
    Text.length (sessionText (sid 1) long) @?= maxTextChars
    assertBool "keeps the end" ("end" `Text.isSuffixOf` sessionText (sid 1) long)

followTest :: Assertion
followTest = do
    followTarget emptySpectate @?= Nothing
    let sp = fold [on 1 (RunStarted UntilBlocked), on 2 (RunStarted UntilBlocked), on 2 (RunStopped StatusIdle)]
    -- No text yet: the running session that appeared last.
    followTarget sp @?= Just (sid 1)
    followTarget (foldFrom sp [on 2 (TextDelta "hi")]) @?= Just (sid 2)
    followTarget (foldFrom sp [on 2 (TextDelta "hi"), on 1 (TextDelta "yo")]) @?= Just (sid 1)
    -- The writer was deleted: back to the running one.
    followTarget (foldFrom sp [on 2 (TextDelta "hi"), (Nothing, SessionDeleted (sid 2))]) @?= Just (sid 1)

seedTest :: Assertion
seedTest = do
    let sp =
            seedSessions
                t0
                [meta 2 (Just "helper") (Just 1) StatusRunning, meta 1 (Just "root") Nothing StatusRunning]
                emptySpectate
    [(r.rowDepth, r.rowNode.nAgent, r.rowNode.nState) | r <- treeRows sp]
        @?= [(0, Just "root", NodeRunning), (1, Just "helper", NodeRunning)]
    sp.spEvents @?= 0

deleteTest :: Assertion
deleteTest = do
    let sp =
            fold
                [ on 1 (RunStarted UntilBlocked)
                , on 1 (ToolCallStarted (call 1) "bash")
                , on 2 (ToolCallStarted (call 2) "grep")
                , (Nothing, SessionDeleted (sid 1))
                ]
    Map.keys sp.spNodes @?= [sid 2]
    map (.trName) (toolRows sp) @?= ["grep"]

pruneTest :: Assertion
pruneTest = do
    let n = maxFinishedTools + 20
        calls = concat [[on 1 (ToolCallStarted (call c) "bash"), on 1 (ToolCallCompleted (call c) "bash" True)] | c <- [1 .. n]]
        sp = fold (on 1 (RunStarted UntilBlocked) : on 1 (ToolCallStarted (call 0) "long") : calls)
    length (toolRows sp) @?= maxFinishedTools + 1
    -- The running call survives however old it is; the newest finished ones are kept.
    fmap (.trName) (Map.lookup (call 0) sp.spTools) @?= Just "long"
    assertBool "newest kept" (Map.member (call n) sp.spTools)
    assertBool "oldest dropped" (not (Map.member (call 1) sp.spTools))
    -- Quiet roots are bounded; a running root, and a quiet root above a
    -- running child, are never dropped.
    let m = maxQuietRoots + 10
        roots =
            [on 1 (RunStarted UntilBlocked), on 2 (RunStopped StatusIdle), on 2 (SubcallStarted (sid 2) (sid 3) "helper" 1)]
                <> [on k (RunStopped StatusIdle) | k <- [10 .. 10 + m - 1]]
        many = fold roots
    Map.size many.spNodes @?= 3 + maxQuietRoots
    assertBool "running root kept" (Map.member (sid 1) many.spNodes)
    assertBool "root above a running child kept" (Map.member (sid 2) many.spNodes)
    assertBool "oldest quiet root dropped" (not (Map.member (sid 10) many.spNodes))

formatElapsedTest :: Assertion
formatElapsedTest =
    map formatElapsed [-3, 0, 59.9, 60, 3599, 3600, 7384]
        @?= ["0s", "0s", "59s", "1m00s", "59m59s", "1h00m", "2h03m"]

wrapTextTest :: Assertion
wrapTextTest = do
    wrapText 10 "hello world, again" @?= ["hello", "world,", "again"]
    wrapText 5 "abcdefghijkl" @?= ["abcde", "fghij", "kl"]
    wrapText 10 "one\n\ntwo" @?= ["one", "", "two"]
    wrapText 0 "ab" @?= ["a", "b"]
    assertBool "never wider than asked" (all ((<= 12) . Text.length) (wrapText 12 (Text.replicate 9 "lorem ipsum, ")))

-------------------------------------------------------------------------------
-- Screen
-------------------------------------------------------------------------------

textWindowTest :: Assertion
textWindowTest = do
    let text = "l1\nl2\nl3\nl4\nl5\n\n"
    textWindow 20 2 0 text @?= ["l4", "l5"]
    textWindow 20 2 2 text @?= ["l2", "l3"]
    textWindow 20 2 99 text @?= ["l1", "l2"]
    textWindow 20 9 0 text @?= ["l1", "l2", "l3", "l4", "l5"]

rowWindowTest :: Assertion
rowWindowTest = do
    let rows = [0 .. 9 :: Int]
    rowWindow Nothing 4 rows @?= [0, 1, 2, 3]
    rowWindow (Just 0) 4 rows @?= [0, 1, 2, 3]
    rowWindow (Just 5) 4 rows @?= [3, 4, 5, 6]
    rowWindow (Just 9) 4 rows @?= [6, 7, 8, 9]
    rowWindow (Just 2) 20 rows @?= rows

selectionTest :: Assertion
selectionTest = do
    let sp =
            fold
                [ on 1 (RunStarted UntilBlocked)
                , on 1 (SubcallStarted (sid 1) (sid 2) "helper" 1)
                , on 1 (SubcallStarted (sid 1) (sid 3) "other" 1)
                , on 2 (TextDelta "hi")
                ]
        st0 = initSpectateState "test" t0 sp
    st0.ssPinned @?= Nothing
    focusedSession st0 @?= Just (sid 2)
    let down = moveSelection 1 st0{ssScroll = 30}
    down.ssPinned @?= Just (sid 3)
    down.ssScroll @?= 0
    focusedSession (moveSelection 5 down) @?= Just (sid 3)
    focusedSession (moveSelection (-9) down) @?= Just (sid 1)
    -- A pinned session keeps the focus while others write...
    let busy = down{ssModel = foldFrom sp [on 1 (TextDelta "yo")]}
    focusedSession busy @?= Just (sid 3)
    -- ...and gives it back once it is gone.
    let gone = down{ssModel = foldFrom sp [(Nothing, SessionDeleted (sid 3))]}
    focusedSession gone @?= Just (sid 2)

-------------------------------------------------------------------------------
-- Over a runner
-------------------------------------------------------------------------------

inProcessRunTest :: Assertion
inProcessRunTest = do
    agentNode <- testNode "{}"
    host <- testHost [agentNode] (const mockCompletion)
    withSessionRunner host $ \runner -> do
        let client = inProcessClient Nothing runner
        sub <- expectRight =<< subscribeAll client
        created <- expectRight =<< createSession client "test-agent" (Just (message "hi")) (Just UntilBlocked) Map.empty
        let target = created.smSessionId
            watch sp = do
                ev <- subNext sub
                now <- getCurrentTime
                let sp' = applyEvent now ev sp
                case ev.evBody of
                    RunStopped _ | ev.evSession == Just target -> pure sp'
                    _ -> watch sp'
        sp <- watch emptySpectate
        subClose sub
        watched <- maybe (assertFailure "the session is not in the tree") pure (Map.lookup target sp.spNodes)
        watched.nAgent @?= Just "test-agent"
        watched.nState @?= NodeStopped StatusIdle
        map (.rowNode.nSession) (treeRows sp) @?= [target]
        followTarget sp @?= Just target
        assertBool
            ("the user's query and the answer are in the text: " <> show watched.nText)
            ("> hi\n\n" `Text.isPrefixOf` watched.nText && Text.length watched.nText > Text.length "> hi\n\n")

-------------------------------------------------------------------------------
-- Layout
-------------------------------------------------------------------------------

panelsOf :: Text -> IO Layout
panelsOf spec = case parsePanels spec of
    Right cols -> pure defaultLayout{layColumns = cols}
    Left err -> assertFailure ("panels " <> Text.unpack spec <> ": " <> Text.unpack err)

panelsTest :: Assertion
panelsTest = do
    parsePanels "tree:60+tools:40/45,text/55" @?= Right defaultColumns
    renderPanels defaultColumns @?= "tree:60+tools:40/45,text/55"
    -- The example of the feature: three columns, equal shares.
    three <- panelsOf "tree,tools,text"
    map (.colWidth) three.layColumns @?= [50, 50, 50]
    visiblePanels three @?= [PanelTree, PanelTools, PanelText]
    renderPanels three.layColumns @?= "tree,tools,text"
    -- One stack, in another order; names are case-blind, spaces ignored.
    stack <- panelsOf " Text + agents:30 "
    stack.layColumns @?= [Column [Slot PanelText 50, Slot PanelTree 30] 50]
    -- A panel that is not named is hidden.
    only <- panelsOf "text"
    visiblePanels only @?= [PanelText]
    -- Every layout the keys can reach reads back from its rendering.
    let reachable = scanl (\l f -> f l) defaultLayout changes
        changes =
            [ togglePanel PanelTools
            , movePanel 1 PanelTree
            , togglePanel PanelTools
            , restackPanel PanelTools
            , resizeWidth 3 PanelText
            , resizeHeight (-2) PanelTools
            , restackPanel PanelTree
            ]
    mapM_ (\l -> parsePanels (renderPanels l.layColumns) @?= Right l.layColumns) reachable

panelsErrorTest :: Assertion
panelsErrorTest = do
    refused "" "panel name is missing"
    refused "tree,,text" "panel name is missing"
    refused "tree,logs" "unknown panel 'logs'"
    refused "tree,text+tree" "more than once"
    refused "tree:0" "not a number between 1 and 100"
    refused "tree/101" "not a number between 1 and 100"
    refused "tree:x" "not a number between 1 and 100"
    refused "tree:1:2" "more than one height"
    refused "tree/1/2" "more than one width"
  where
    refused spec why = case parsePanels spec of
        Left err -> assertBool (Text.unpack spec <> ": " <> Text.unpack err) (why `Text.isInfixOf` err)
        Right cols -> assertFailure (Text.unpack spec <> " was accepted: " <> show cols)

refreshTest :: Assertion
refreshTest = do
    parseRefresh "1" @?= Right 1000
    parseRefresh "0.5" @?= Right 500
    parseRefresh ".25" @?= Right 250
    parseRefresh "2.125" @?= Right 2125
    parseRefresh "60" @?= Right 60000
    mapM_ (\t -> assertBool (Text.unpack t) (either (const True) (const False) (parseRefresh t))) ["", "0", "0.05", "61", "1.2345", "1s", "-1", "1.", "1.2.3"]
    map renderRefresh [1000, 500, 250, 2125, 60000, 100] @?= ["1", "0.5", "0.25", "2.125", "60", "0.1"]
    mapM_ (\ms -> parseRefresh (renderRefresh ms) @?= Right ms) [100, 250, 1000, 2125, 60000]
    -- The keys step through fixed intervals, and stop at the bounds.
    slowerRefresh 1000 @?= 2000
    fasterRefresh 1000 @?= 500
    slowerRefresh 750 @?= 1000
    fasterRefresh 750 @?= 500
    slowerRefresh maxRefreshMs @?= maxRefreshMs
    fasterRefresh minRefreshMs @?= minRefreshMs

layoutChangesTest :: Assertion
layoutChangesTest = do
    let shown = renderPanels . (.layColumns)
    -- Hiding empties no column; the last panel stays; showing appends.
    let noTools = togglePanel PanelTools defaultLayout
    shown noTools @?= "tree:60/45,text/55"
    let onlyText = togglePanel PanelTree noTools
    shown onlyText @?= "text/55"
    togglePanel PanelText onlyText @?= onlyText
    shown (togglePanel PanelTools onlyText) @?= "text/55,tools"
    -- Exchanging keeps the sizes of the places.
    shown (movePanel 1 PanelTree defaultLayout) @?= "tools:60+tree:40/45,text/55"
    shown (movePanel 1 PanelTools defaultLayout) @?= "tree:60+text:40/45,tools/55"
    movePanel (-1) PanelTree defaultLayout @?= defaultLayout
    movePanel 1 PanelText defaultLayout @?= defaultLayout
    movePanel 1 PanelTools onlyText @?= onlyText
    -- Restacking: out of a shared column, then back under the one before.
    -- A panel keeps its height share, for when it is stacked again.
    let apart = restackPanel PanelTools defaultLayout
    shown apart @?= "tree:60/45,tools:40,text/55"
    shown (restackPanel PanelText apart) @?= "tree:60/45,tools:40+text"
    shown (restackPanel PanelTree defaultLayout) @?= "tools:40/45,tree:60,text/55"
    restackPanel PanelTools (restackPanel PanelTools defaultLayout) @?= defaultLayout
    restackPanel PanelTree apart @?= apart
    -- Sizes move by steps of five, within bounds.
    shown (resizeWidth 1 PanelTools defaultLayout) @?= "tree:60+tools:40,text/55"
    shown (resizeHeight (-1) PanelTools defaultLayout) @?= "tree:60+tools:35/45,text/55"
    shown (resizeHeight 99 PanelTree defaultLayout) @?= "tree:95+tools:40/45,text/55"
    shown (resizeWidth (-99) PanelText defaultLayout) @?= "tree:60+tools:40/45,text/5"
    -- The panel after the last is the first; a hidden one gives the first.
    nextPanel 1 PanelText defaultLayout @?= PanelTree
    nextPanel (-1) PanelTree defaultLayout @?= PanelText
    nextPanel 0 PanelTree onlyText @?= PanelText

sharesTest :: Assertion
sharesTest = do
    shares 100 [45, 55] @?= [45, 55]
    shares 80 [50, 50, 50] @?= [26, 26, 28]
    shares 7 [60, 40] @?= [4, 3]
    shares 10 [50] @?= [10]
    shares 0 [1, 2] @?= [0, 0]
    shares 10 [] @?= []
    mapM_ (\(n, ws) -> sum (shares n ws) @?= n) [(n, ws) | n <- [1, 2, 3, 79, 120, 211], ws <- [[5, 95], [33, 33, 33], [60, 40], [95, 5, 50]]]

savedLayoutTest :: Assertion
savedLayoutTest = do
    let layout = (restackPanel PanelTools defaultLayout){layRefreshMs = 250}
    parseLayoutFile defaultLayout (renderLayoutFile layout) @?= Right layout
    -- A line the file lacks leaves that part alone.
    parseLayoutFile layout "# only the interval\n\nrefresh 2\n" @?= Right layout{layRefreshMs = 2000}
    parseLayoutFile layout "" @?= Right layout
    parseLayoutFile layout "refresh 1\npanels tree,nope\n"
        @?= Left "line 2: unknown panel 'nope' (expected tree, tools or text)"
    parseLayoutFile layout "colour blue\n" @?= Left "line 1: unknown setting 'colour' (expected panels or refresh)"
    withSystemTempDirectory "spectate-layout" $ \dir -> do
        let path = dir </> "spectate-layout"
        -- No file: the default layout, with the flags over it.
        resolveLayout path Nothing Nothing >>= (@?= Right defaultLayout)
        resolveLayout path Nothing (Just 500) >>= (@?= Right defaultLayout{layRefreshMs = 500})
        -- A saved file: read, each flag replacing its part only.
        Text.writeFile path (renderLayoutFile layout)
        resolveLayout path Nothing Nothing >>= (@?= Right layout)
        resolveLayout path (Just defaultColumns) Nothing >>= (@?= Right layout{layColumns = defaultColumns})
        -- A broken file is said, not skipped.
        Text.writeFile path "panels tree+tree\n"
        broken <- resolveLayout path Nothing Nothing
        case broken of
            Left err -> assertBool (Text.unpack err) (Text.pack path `Text.isInfixOf` err && "more than once" `Text.isInfixOf` err)
            Right l -> assertFailure ("a broken layout file was read as " <> show l)

layoutKeysTest :: Assertion
layoutKeysTest = do
    let sp = fold [on 1 (RunStarted UntilBlocked), on 1 (TextDelta "hi")]
        st0 = (initSpectateState "test" t0 sp){ssPinned = Just (sid 1), ssScroll = 3}
        press :: String -> SpectateState -> SpectateState
        press keys st = foldl (\acc key -> maybe acc id (layoutKey key acc)) st keys
        shown :: SpectateState -> Text
        shown st = renderPanels st.ssLayout.layColumns
    st0.ssActive @?= PanelTree
    -- Hiding the active panel hands the keys to the first one left;
    -- a panel that comes back takes them.
    let hidden = press "1" st0
    shown hidden @?= "tools:40/45,text/55"
    hidden.ssActive @?= PanelTools
    let back = press "1" hidden
    shown back @?= "tools:40/45,text/55,tree"
    back.ssActive @?= PanelTree
    (press "3" st0).ssActive @?= PanelTree
    -- Moving, resizing and restacking act on the active panel.
    shown (press ">" st0) @?= "tools:60+tree:40/45,text/55"
    shown (press "]]-" st0) @?= "tree:55+tools:40/55,text/55"
    shown (press "s" st0) @?= "tools:40/45,tree:60,text/55"
    shown (press "+=" st0{ssActive = PanelTools}) @?= "tree:60+tools/45,text/55"
    -- The interval, and going back to the layout at start.
    (press "dd" st0).ssLayout.layRefreshMs @?= 5000
    (press "DDDDDD" st0).ssLayout.layRefreshMs @?= minRefreshMs
    let tuned = press "2>]dd" st0
    assertBool "the keys changed the layout" (tuned.ssLayout /= defaultLayout)
    (press "0" tuned).ssLayout @?= defaultLayout
    -- Nothing but the layout moves: same model, selection and scroll.
    tuned.ssModel @?= st0.ssModel
    tuned.ssPinned @?= st0.ssPinned
    tuned.ssScroll @?= st0.ssScroll
    -- Other keys are not layout keys.
    mapM_ (\key -> assertBool [key] (maybe True (const False) (layoutKey key st0))) ("qjkfWx?h" :: String)
    -- The help names the flags that reproduce what is on screen.
    take 1 (helpLines tuned) @?= pure ("This layout: " <> layoutFlags tuned.ssLayout)
    layoutFlags defaultLayout @?= "--panels tree:60+tools:40/45,text/55 --refresh 1"

refreshFoldTest :: Assertion
refreshFoldTest = do
    let event n s body = (at n, Event (EventSeq (fromIntegral n)) (Just (sid s)) Nothing body)
        events = [event 1 1 (RunStarted UntilBlocked), event 2 1 (TextDelta "he"), event 3 1 (TextDelta "llo")]
        st0 = initSpectateState "test" t0 emptySpectate
        st = applyRefresh (at 5) events st0
    -- Durations count from when each event was received, not from the refresh.
    n1 <- node 1 st.ssModel
    n1.nSince @?= at 1
    n1.nText @?= "hello"
    st.ssModel.spEvents @?= 3
    st.ssNow @?= at 5
    -- A refresh with nothing new only moves the clock, and never back.
    let idle = applyRefresh (at 9) [] st
    idle.ssModel @?= st.ssModel
    idle.ssNow @?= at 9
    (applyRefresh (at 2) [] idle).ssNow @?= at 9
