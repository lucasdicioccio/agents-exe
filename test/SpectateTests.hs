{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The @spectate@ dashboard: its pure model ("System.Agents.Spectate"),
the selection and windowing of its screen ("System.Agents.TUI.Spectate"),
and the model folded over a real runner's event stream through
'inProcessClient' -- the client an embedded TUI drives, so what a spectator
shows does not depend on the events coming over HTTP.
-}
module SpectateTests (tests) where

import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime (..), addUTCTime, fromGregorian, getCurrentTime)
import qualified Data.UUID as UUID
import Test.Tasty
import Test.Tasty.HUnit

import AgentFactoryTests (mockCompletion, testNode)
import RunnerTests (expectRight, message, testHost)
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
import System.Agents.TUI.Spectate (
    SpectateState (..),
    focusedSession,
    initSpectateState,
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
