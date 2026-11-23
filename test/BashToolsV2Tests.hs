{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Tests for bash tools v2, slice 1 (@todos/bash-tools-v2.md@ §1.1, §2.1):
@describe@ with @"protocol": 2@ and a @process@ object, and @oneshot@ tools
whose output is cut into events, validated, and rendered.
-}
module BashToolsV2Tests (tests) where

import Control.Concurrent.MVar (modifyMVar_, newMVar, readMVar)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Either (isLeft)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.UUID as UUID
import Prod.Tracer (Tracer (..))
import System.Directory (getPermissions, setOwnerExecutable, setPermissions)
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import System.Agents.Base (ConversationId (..))
import System.Agents.Session.Base (SessionId (..), TurnId (..))
import qualified System.Agents.ToolRegistration as ToolRegistration
import qualified System.Agents.Tools.Bash as Bash
import System.Agents.Tools.Context (ToolExecutionContext (..), mkMinimalContext)
import System.Agents.Tools.ScriptProcess
import System.Agents.Tools.ScriptRuntime (defaultToolRuntimeConfig)
import qualified System.Agents.Tools.ScriptTypes as ScriptTypes

tests :: TestTree
tests =
    testGroup
        "Bash tools v2"
        [ testGroup "describe" describeTests
        , testGroup "event schemas" schemaTests
        , testGroup "framing" framingTests
        , testGroup "oneshot run" runTests
        , testGroup "stream results" ringTests
        , testGroup "stream run" streamTests
        ]

describeTests :: [TestTree]
describeTests =
    [ testCase "a tool without protocol is v1 and has no process" $
        withFixture "v1" (describeThen v1Describe "echo hi") $ \path -> do
            desc <- loadOk path
            desc.scriptInfo.scriptProtocol @?= 1
            assertBool "no process" (isNothing (ScriptTypes.scriptProcessOf desc.scriptInfo))
    , testCase "v2 fields are ignored on a v1 tool (D1)" $
        withFixture "v1p" (describeThen v1WithProcess "echo hi") $ \path -> do
            desc <- loadOk path
            desc.scriptInfo.scriptProcess @?= Nothing
    , testCase "a v2 tool without process is a jsonl oneshot" $
        withFixture "v2" (describeThen (v2Describe Nothing) "echo hi") $ \path -> do
            desc <- loadOk path
            desc.scriptInfo.scriptProtocol @?= 2
            ScriptTypes.scriptProcessOf desc.scriptInfo @?= Just defaultScriptProcess
    , testCase "process fields are parsed" $
        withFixture "v2p" (describeThen (v2Describe (Just linesProcess)) "echo hi") $ \path -> do
            desc <- loadOk path
            fmap (.processOutput.outputFraming) desc.scriptInfo.scriptProcess @?= Just FramingLines
            fmap (.processReady) desc.scriptInfo.scriptProcess @?= Just (Just ReadyExit)
    , testCase "describe v2 round-trips through JSON" $
        withFixture "v2p" (describeThen (v2Describe (Just eventProcess)) "echo hi") $ \path -> do
            desc <- loadOk path
            Aeson.decode (Aeson.encode desc.scriptInfo) @?= Just desc.scriptInfo
    , testCase "an unknown protocol is refused" $
        refused "{\"protocol\": 3, \"slug\": \"x\", \"description\": \"d\", \"args\": []}"
    , testCase "stream mode loads" $
        withFixture "st" (describeThen (v2Describe (Just "{\"mode\": \"stream\", \"ready\": {\"kind\": \"line\", \"match\": \"^ready$\"}}")) "true") $ \path -> do
            desc <- loadOk path
            fmap (.processMode) desc.scriptInfo.scriptProcess @?= Just ModeStream
    , testCase "process.background is parsed (true, object, false) and round-trips" $ do
        let bgOf process =
                withFixture "bg" (describeThen (v2Describe (Just process)) "true") $ \path -> do
                    desc <- loadOk path
                    Aeson.decode (Aeson.encode desc.scriptInfo) @?= Just desc.scriptInfo
                    pure (ScriptTypes.scriptBackgroundOf desc.scriptInfo)
        bgOf "{\"mode\": \"stream\", \"background\": true}" >>= (@?= Just (ProcessBackground Nothing))
        bgOf "{\"mode\": \"stream\", \"background\": {\"attachSeconds\": 3}}" >>= (@?= Just (ProcessBackground (Just 3)))
        bgOf "{\"mode\": \"stream\", \"background\": false}" >>= (@?= Nothing)
        bgOf "{\"mode\": \"stream\"}" >>= (@?= Nothing)
    , testCase "process.background is ignored on a v1 tool (D1)" $
        withFixture "bg1" (describeThen "{\"slug\": \"v1\", \"description\": \"d\", \"args\": [], \"process\": {\"background\": true}}" "true") $ \path -> do
            desc <- loadOk path
            ScriptTypes.scriptBackgroundOf desc.scriptInfo @?= Nothing
    , testCase "a stream tool's background declaration reaches its registration" $
        withFixture "bgr" (describeThen (v2Describe (Just "{\"mode\": \"stream\", \"background\": {\"attachSeconds\": 0}}")) "true") $ \path -> do
            desc <- loadOk path
            fmap snd (ToolRegistration.toolDeclaredBackground (ToolRegistration.registerBashToolInLLM Nothing desc))
                @?= Just (ProcessBackground (Just 0))
            fmap fst (ToolRegistration.toolDeclaredBackground (ToolRegistration.registerBashToolInLLM Nothing desc))
                @?= Just "bash_v2"
    , testCase "an invalid process.background is refused" $ do
        refused (v2Describe (Just "{\"mode\": \"stream\", \"background\": \"yes\"}"))
        refused (v2Describe (Just "{\"mode\": \"stream\", \"background\": {\"attachSeconds\": -1}}"))
    , testCase "an invalid ready pattern is refused" $
        refused (v2Describe (Just "{\"mode\": \"stream\", \"ready\": {\"kind\": \"line\", \"match\": \"(\"}}"))
    , testCase "a ready line needs line-based framing" $
        refused (v2Describe (Just "{\"mode\": \"stream\", \"output\": {\"framing\": \"chunks\"}, \"ready\": {\"kind\": \"line\", \"match\": \"x\"}}"))
    , testCase "timeout.start on a stream needs a ready line or exit" $
        refused "{\"protocol\": 2, \"slug\": \"x\", \"description\": \"d\", \"args\": [], \"process\": {\"mode\": \"stream\"}, \"runtime\": {\"timeout\": {\"start\": 1}}}"
    , testCase "output-media-type does not apply to a stream" $
        refused "{\"protocol\": 2, \"slug\": \"x\", \"description\": \"d\", \"args\": [], \"output-media-type\": \"image/png\", \"process\": {\"mode\": \"stream\", \"output\": {\"framing\": \"chunks\"}}}"
    , testCase "service mode is refused (not implemented yet)" $
        refused (v2Describe (Just "{\"mode\": \"service\"}"))
    , testCase "process.input is refused (not implemented yet)" $
        refused (v2Describe (Just "{\"input\": {\"framing\": \"jsonl\"}}"))
    , testCase "a runtime envdir is refused without --envdir-root" $
        refused "{\"protocol\": 2, \"slug\": \"x\", \"description\": \"d\", \"args\": [], \"runtime\": {\"envdir\": \"x\"}}"
    , testCase "a ready line on a oneshot is refused" $
        refused (v2Describe (Just "{\"ready\": {\"kind\": \"line\", \"match\": \"^ready$\"}}"))
    , testCase "an invalid framing is refused" $
        refused (v2Describe (Just "{\"output\": {\"framing\": \"xml\"}}"))
    , testCase "output-media-type needs chunks framing" $
        refused "{\"protocol\": 2, \"slug\": \"x\", \"description\": \"d\", \"args\": [], \"output-media-type\": \"image/png\"}"
    , testCase "subcommands inherit the v2 declaration" $
        withFixture "v2c" (describeThen v2Commands "echo '{}'") $ \path -> do
            r <- Bash.loadScriptTools nullTracer path
            case r of
                Left err -> assertFailure (show err)
                Right tools -> do
                    fmap (.scriptInfo.scriptProtocol) tools @?= [2, 2]
                    fmap (isJust . ScriptTypes.scriptProcessOf . (.scriptInfo)) tools @?= [True, True]
    ]

schemaTests :: [TestTree]
schemaTests =
    [ testCase "a matching object has no problems" $
        validateAgainstSchema lineSchema (obj [("line", "x"), ("level", "error")]) @?= []
    , testCase "a missing required property is reported" $
        assertBool "problems" (not (null (validateAgainstSchema lineSchema (obj [("level", "error")]))))
    , testCase "a wrong type is reported" $
        assertBool "problems" (not (null (validateAgainstSchema lineSchema (Aeson.object ["line" Aeson..= (3 :: Int)]))))
    , testCase "enum is checked" $ do
        let s = schema "{\"enum\": [\"a\", \"b\"]}"
        validateAgainstSchema s (Aeson.String "a") @?= []
        assertBool "c refused" (not (null (validateAgainstSchema s (Aeson.String "c"))))
    , testCase "additionalProperties false is checked" $ do
        let s = schema "{\"type\": \"object\", \"properties\": {\"a\": {}}, \"additionalProperties\": false}"
        validateAgainstSchema s (Aeson.object ["a" Aeson..= True]) @?= []
        assertBool "b refused" (not (null (validateAgainstSchema s (Aeson.object ["b" Aeson..= True]))))
    , testCase "integer excludes fractions" $ do
        let s = schema "{\"type\": \"integer\"}"
        validateAgainstSchema s (Aeson.Number 3) @?= []
        assertBool "3.5 refused" (not (null (validateAgainstSchema s (Aeson.Number 3.5))))
    , testCase "items and anyOf are checked" $ do
        let s = schema "{\"type\": \"array\", \"items\": {\"anyOf\": [{\"type\": \"string\"}, {\"type\": \"null\"}]}}"
        validateAgainstSchema s (Aeson.toJSON [Aeson.String "a", Aeson.Null]) @?= []
        assertBool "number refused" (not (null (validateAgainstSchema s (Aeson.toJSON [Aeson.Number 1]))))
    , testCase "boolean schemas" $ do
        validateAgainstSchema (Aeson.Bool True) (Aeson.Number 1) @?= []
        assertBool "false refuses" (not (null (validateAgainstSchema (Aeson.Bool False) (Aeson.Number 1))))
    ]

framingTests :: [TestTree]
framingTests =
    [ testCase "jsonl renders a JSON array of events, invalid ones kept" $ do
        let out = ProcessOutput FramingJsonl (Just lineSchema)
            rendered = renderOutput out "{\"line\": \"a\"}\n\nnot json\n{\"level\": \"x\"}\r\n"
        case Aeson.decodeStrict rendered of
            Just (Aeson.Array evs) -> do
                length evs @?= 3
                case foldr (:) [] evs of
                    [first, second, third] -> do
                        first @?= obj [("line", "a")]
                        assertBool "second invalid" (isInvalid second)
                        assertBool "third invalid" (isInvalid third)
                    _ -> assertFailure "expected three events"
            _ -> assertFailure ("not a JSON array: " <> show rendered)
    , testCase "jsonl without trailing newline keeps the last event" $
        renderOutput defaultProcessOutput "{\"a\":1}\n{\"b\":2}" @?= "[{\"a\":1},{\"b\":2}]"
    , testCase "jsonl of an empty output is an empty array" $
        renderOutput defaultProcessOutput "" @?= "[]"
    , testCase "lines joins the non-empty lines" $
        renderOutput (ProcessOutput FramingLines Nothing) "a\n\nb\n" @?= "a\nb"
    , testCase "chunks keeps the output unchanged" $
        renderOutput (ProcessOutput FramingChunks Nothing) "a\n\nb\n" @?= "a\n\nb\n"
    ]

runTests :: [TestTree]
runTests =
    [ testCase "a v2 oneshot's result is its framed events" $
        withFixture "ev" (describeThen (v2Describe (Just eventProcess)) eventBody) $ \path -> do
            desc <- loadOk path
            out <- runOk desc Nothing
            case Aeson.decodeStrict (Text.encodeUtf8 out) of
                Just (Aeson.Array evs) -> do
                    length evs @?= 2
                    assertBool "second invalid" (any isInvalid evs)
                _ -> assertFailure ("not a JSON array: " <> Text.unpack out)
    , testCase "a v1 tool's output is unchanged" $
        withFixture "v1" (describeThen v1Describe eventBody) $ \path -> do
            desc <- loadOk path
            out <- runOk desc Nothing
            out @?= "{\"line\": \"started\"}\nnot an event\n"
    , testCase "empty-result applies to an empty v2 output" $
        withFixture "empty" (describeThen v2Empty "true") $ \path -> do
            desc <- loadOk path
            out <- runOk desc Nothing
            out @?= "nothing"
    , testCase "each event is reported as progress as it arrives" $
        withFixture "ev" (describeThen (v2Describe (Just eventProcess)) eventBody) $ \path -> do
            desc <- loadOk path
            reports <- newMVar []
            ctx <- testContext (\v -> modifyMVar_ reports (pure . (v :)))
            _ <- runOk desc (Just ctx)
            rs <- reverse <$> readMVar reports
            let events = [e | Aeson.Object o <- rs, Just e <- [KeyMap.lookup "event" o]]
            length events @?= 2
            case events of
                (first : _) -> first @?= obj [("line", "started")]
                _ -> pure ()
    ]

ringTests :: [TestTree]
ringTests =
    [ testCase "the ring keeps the latest events and counts the others" $ do
        let r = foldl (flip pushEvent) (emptyEventRing 2) [OutputEvent (Aeson.String t) | t <- ["a", "b", "c"]]
        ringEvents r @?= [OutputEvent (Aeson.String "b"), OutputEvent (Aeson.String "c")]
        ringElided r @?= 1
    , testCase "jsonl renders the events and the elided count" $ do
        let r = foldl (flip pushEvent) (emptyEventRing 1) [OutputEvent (obj [("line", "x")]), OutputEvent (obj [("line", "y")])]
        Aeson.decodeStrict (renderStreamResult (ProcessOutput FramingJsonl Nothing) r)
            @?= Just (Aeson.object ["events" Aeson..= [obj [("line", "y")]], "elided" Aeson..= (1 :: Int)])
    , testCase "lines are preceded by an elided note" $ do
        let r = foldl (flip pushEvent) (emptyEventRing 2) [OutputEvent (Aeson.String t) | t <- ["a", "b", "c"]]
        renderStreamResult (ProcessOutput FramingLines Nothing) r @?= "[1 earlier lines elided]\nb\nc"
    , testCase "nothing elided, no note" $ do
        let r = foldl (flip pushEvent) (emptyEventRing 5) [OutputEvent (Aeson.String t) | t <- ["a", "b"]]
        renderStreamResult (ProcessOutput FramingLines Nothing) r @?= "a\nb"
    ]

streamTests :: [TestTree]
streamTests =
    [ testCase "a stream's result is its latest events, with the elided count" $
        withFixture "st" (describeThen streamDescribe countBody) $ \path -> do
            desc <- loadOk path
            r <- Bash.runValueWithOptions (Bash.RunOptions 2) defaultToolRuntimeConfig nullTracer desc Nothing (Aeson.object [])
            case r of
                Right bytes ->
                    Aeson.decodeStrict bytes
                        @?= Just (Aeson.object ["events" Aeson..= [num 4, num 5], "elided" Aeson..= (3 :: Int)])
                Left err -> assertFailure (show err)
    , testCase "each event of a stream is reported as progress" $
        withFixture "st" (describeThen streamDescribe countBody) $ \path -> do
            desc <- loadOk path
            reports <- newMVar []
            ctx <- testContext (\v -> modifyMVar_ reports (pure . (v :)))
            _ <- runOk desc (Just ctx)
            rs <- reverse <$> readMVar reports
            [e | Aeson.Object o <- rs, Just e <- [KeyMap.lookup "event" o]] @?= fmap num [1 .. 5]
    , testCase "a failed stream reports its events so far" $
        withFixture "st" (describeThen streamDescribe (countBody <> "echo oops >&2\nexit 3\n")) $ \path -> do
            desc <- loadOk path
            r <- Bash.runValue nullTracer desc Nothing (Aeson.object [])
            case r of
                Left (Bash.ScriptStreamExitError _ _ err rendered) -> do
                    assertBool "stderr kept" (Text.isInfixOf "oops" (Text.decodeUtf8 err))
                    assertBool "events kept" (Text.isInfixOf "\"elided\":0" (Text.decodeUtf8 rendered))
                other -> assertFailure ("expected a stream exit error, got " <> show (fmap (const ()) other))
    , testCase "an empty stream gets the empty-result message" $
        withFixture "st" (describeThen streamEmpty "true") $ \path -> do
            desc <- loadOk path
            out <- runOk desc Nothing
            out @?= "nothing"
    , testCase "chunks framing streams raw chunks" $
        withFixture "st" (describeThen (v2Describe (Just "{\"mode\": \"stream\", \"output\": {\"framing\": \"chunks\"}}")) "printf 'abc'") $ \path -> do
            desc <- loadOk path
            out <- runOk desc Nothing
            out @?= "abc"
    , testCase "timeout.start bounds reaching the ready line, not the whole run" $
        withFixture "st" (describeThen (streamReady 1) "echo ready\nsleep 1.5\necho done\n") $ \path -> do
            desc <- loadOk path
            out <- runOk desc Nothing
            out @?= "ready\ndone"
    , testCase "a stream that never gets ready times out" $
        withFixture "st" (describeThen (streamReady 0.3) "sleep 5\necho ready\n") $ \path -> do
            desc <- loadOk path
            r <- Bash.runValue nullTracer desc Nothing (Aeson.object [])
            case r of
                Left (Bash.ScriptTimeoutError _ _) -> pure ()
                other -> assertFailure ("expected a timeout, got " <> show (fmap (const ()) other))
    ]
  where
    num :: Int -> Aeson.Value
    num n = Aeson.object ["n" Aeson..= n]

streamDescribe :: String
streamDescribe = v2Describe (Just "{\"mode\": \"stream\"}")

streamEmpty :: String
streamEmpty = "{\"protocol\": 2, \"slug\": \"e\", \"description\": \"d\", \"args\": [], \"empty-result\": {\"tag\": \"AddMessage\", \"contents\": \"nothing\"}, \"process\": {\"mode\": \"stream\"}}"

streamReady :: Double -> String
streamReady secs =
    "{\"protocol\": 2, \"slug\": \"r\", \"description\": \"d\", \"args\": [], \"process\": {\"mode\": \"stream\", \"output\": {\"framing\": \"lines\"}, \"ready\": {\"kind\": \"line\", \"match\": \"^ready$\"}}, \"runtime\": {\"timeout\": {\"start\": "
        <> show secs
        <> "}}}"

countBody :: String
countBody = "for i in 1 2 3 4 5; do echo \"{\\\"n\\\": $i}\"; done\n"

-------------------------------------------------------------------------------
-- Fixtures
-------------------------------------------------------------------------------

nullTracer :: Tracer IO a
nullTracer = Tracer (\_ -> pure ())

testContext :: (Aeson.Value -> IO ()) -> IO ToolExecutionContext
testContext report = do
    let portal _ _ = error "no portal in tests"
        ctx = mkMinimalContext (SessionId UUID.nil) (ConversationId UUID.nil) (TurnId UUID.nil) portal
    pure ctx{ctxProgressCallback = Just report}

loadOk :: FilePath -> IO Bash.ScriptDescription
loadOk path = do
    r <- Bash.loadScript nullTracer path
    case r of
        Left err -> assertFailure ("failed to load fixture: " <> show err)
        Right desc -> pure desc

refused :: String -> Assertion
refused json =
    withFixture "refused" (describeThen json "true") $ \path -> do
        r <- Bash.loadScript nullTracer path
        assertBool "expected a load error" (isLeft r)

runOk :: Bash.ScriptDescription -> Maybe ToolExecutionContext -> IO Text
runOk tool mCtx = do
    r <- Bash.runValue nullTracer tool mCtx (Aeson.object [])
    case r of
        Left err -> assertFailure ("run failed: " <> show err)
        Right bytes -> pure (Text.decodeUtf8With lenientDecode bytes)

withFixture :: String -> String -> (FilePath -> IO a) -> IO a
withFixture name content k =
    withSystemTempDirectory "bash-tools-v2" $ \dir -> do
        let path = dir <> "/" <> name
        writeFile path content
        perms <- getPermissions path
        setPermissions path (setOwnerExecutable True perms)
        k path

describeThen :: String -> String -> String
describeThen json body =
    unlines
        [ "#!/usr/bin/env bash"
        , "if [[ \"$1\" == \"describe\" ]]; then"
        , "  cat << 'EOF'"
        , json
        , "EOF"
        , "  exit 0"
        , "fi"
        , body
        ]

v1Describe :: String
v1Describe = "{\"slug\": \"v1\", \"description\": \"d\", \"args\": []}"

v1WithProcess :: String
v1WithProcess = "{\"slug\": \"v1\", \"description\": \"d\", \"args\": [], \"process\": {\"mode\": \"service\"}}"

v2Describe :: Maybe String -> String
v2Describe mProcess =
    "{\"protocol\": 2, \"slug\": \"v2\", \"description\": \"d\", \"args\": []"
        <> maybe "" (\p -> ", \"process\": " <> p) mProcess
        <> "}"

v2Empty :: String
v2Empty = "{\"protocol\": 2, \"slug\": \"e\", \"description\": \"d\", \"args\": [], \"empty-result\": {\"tag\": \"AddMessage\", \"contents\": \"nothing\"}}"

v2Commands :: String
v2Commands =
    "{\"protocol\": 2, \"slug\": \"c\", \"description\": \"d\", \"args\": [], \"commands\": [{\"name\": \"a\", \"description\": \"x\"}, {\"name\": \"b\", \"description\": \"y\"}]}"

linesProcess :: String
linesProcess = "{\"mode\": \"oneshot\", \"output\": {\"framing\": \"lines\"}, \"ready\": {\"kind\": \"exit\"}}"

eventProcess :: String
eventProcess =
    "{\"output\": {\"framing\": \"jsonl\", \"event\": {\"type\": \"object\", \"properties\": {\"line\": {\"type\": \"string\"}}, \"required\": [\"line\"]}}}"

eventBody :: String
eventBody = unlines ["echo '{\"line\": \"started\"}'", "echo 'not an event'"]

lineSchema :: Aeson.Value
lineSchema =
    schema "{\"type\": \"object\", \"properties\": {\"line\": {\"type\": \"string\"}, \"level\": {\"type\": \"string\"}}, \"required\": [\"line\"]}"

schema :: Text -> Aeson.Value
schema t = case Aeson.decodeStrict (Text.encodeUtf8 t) of
    Just v -> v
    Nothing -> error ("bad test schema: " <> Text.unpack t)

obj :: [(Text, Text)] -> Aeson.Value
obj kvs = Aeson.object [(fromText k, Aeson.String v) | (k, v) <- kvs]
  where
    fromText = Key.fromText

isInvalid :: Aeson.Value -> Bool
isInvalid (Aeson.Object o) = KeyMap.member "invalid" o
isInvalid _ = False
