{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Tests for bash tools v2, the @check@ verb and @check --probe@
(@todos/bash-tools-v2.md@ §1.5, D9).
-}
module BashToolsCheckTests (tests) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Prod.Tracer (Tracer (..))
import System.Directory (createDirectory, getPermissions, setOwnerExecutable, setPermissions)
import System.Exit (ExitCode (..))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import qualified System.Agents.Tools.Bash as Bash
import System.Agents.Tools.ScriptCheck
import System.Agents.Tools.ScriptRuntime

tests :: TestTree
tests =
    testGroup
        "Bash tools v2 check"
        [ testGroup "interpret" interpretTests
        , testGroup "probe" probeTests
        , testGroup "report" reportTests
        ]

interpretTests :: [TestTree]
interpretTests =
    [ testCase "exit 0 with ok true is ok" $
        interpretCheckOutput ExitSuccess "{\"ok\": true, \"detail\": \"fine\"}\n" "" @?= (ProbeOk, Just "fine", Nothing)
    , testCase "ok false is failed, with detail and fix" $
        interpretCheckOutput ExitSuccess "{\"ok\": false, \"detail\": \"429\", \"fix\": \"set KEY\"}" "" @?= (ProbeFailed, Just "429", Just "set KEY")
    , testCase "exit 0 without a document is failed" $ do
        let (s, d, _) = interpretCheckOutput ExitSuccess "hello" ""
        s @?= ProbeFailed
        assertMentions "JSON" d
    , testCase "a non-zero exit is failed even with ok true" $ do
        let (s, _, _) = interpretCheckOutput (ExitFailure 3) "{\"ok\": true}" ""
        s @?= ProbeFailed
    , testCase "a non-zero exit without a document reports stderr" $ do
        let (s, d, _) = interpretCheckOutput (ExitFailure 1) "" "curl: not found\n"
        s @?= ProbeFailed
        assertMentions "curl: not found" d
    ]

probeTests :: [TestTree]
probeTests =
    [ testCase "a tool whose check passes is ok, with its time" $
        withTool (checkTool "" "echo '{\"ok\": true}'") $ \path -> do
            r <- probe defaultToolRuntimeConfig path
            r.probeStatus @?= ProbeOk
            assertBool "has a time" (r.probeMillis /= Nothing)
    , testCase "a tool whose check fails reports detail and fix" $
        withTool (checkTool "" "echo '{\"ok\": false, \"detail\": \"down\", \"fix\": \"restart it\"}'; exit 1") $ \path -> do
            r <- probe defaultToolRuntimeConfig path
            r.probeStatus @?= ProbeFailed
            r.probeDetail @?= Just "down"
            r.probeFix @?= Just "restart it"
    , testCase "a v2 tool without check is unverified and not run" $
        withTool (describeThen v2NoCheck "if [[ \"$1\" == check ]]; then echo '{\"ok\": true}'; fi") $ \path -> do
            r <- probe defaultToolRuntimeConfig path
            r.probeStatus @?= ProbeUnverified
            r.probeMillis @?= Nothing
    , testCase "a v1 tool is unverified" $
        withTool (describeThen "{\"slug\": \"v1\", \"description\": \"d\", \"args\": []}" "true") $ \path -> do
            r <- probe defaultToolRuntimeConfig path
            r.probeStatus @?= ProbeUnverified
    , testCase "check runs under the runtime: environment and envdir reach it" $
        withSystemTempDirectory "check-envdir" $ \root -> do
            createDirectory (root <> "/tool")
            writeFile (root <> "/tool/KEY") "secret\n"
            let cfg = defaultToolRuntimeConfig{envdirRoot = Just root}
                rt = "{\"envdir\": \"tool\", \"environment\": {\"MODE\": \"probe\"}}"
                body = "if [[ \"$KEY\" == secret && \"$MODE\" == probe ]]; then echo '{\"ok\": true}'; else echo '{\"ok\": false}'; fi"
            withTool (checkTool rt body) $ \path -> do
                r <- probe cfg path
                r.probeStatus @?= ProbeOk
    , testCase "check is bounded by timeout.start" $
        withTool (checkTool "{\"timeout\": {\"start\": 0.3}}" "sleep 5; echo '{\"ok\": true}'") $ \path -> do
            r <- probe defaultToolRuntimeConfig path
            r.probeStatus @?= ProbeFailed
            assertMentions "within" r.probeDetail
    , testCase "a subcommand tool is checked once, naming all its tools" $
        withTool (describeThen subcommands "echo '{\"ok\": true}'") $ \path -> do
            loaded <- Bash.loadScriptTools nullTracer path
            descs <- either (\e -> assertFailure (show e)) pure loaded
            case toolBinaries descs of
                [(d, names)] -> do
                    names @?= ["multi_a", "multi_b"]
                    r <- probeScriptWith defaultToolRuntimeConfig names d
                    r.probeName @?= "multi"
                    r.probeStatus @?= ProbeOk
                other -> assertFailure ("expected one binary, got " <> show (length other))
    ]

reportTests :: [TestTree]
reportTests =
    [ testCase "the active backend is the first ok, else the first unverified" $ do
        (capabilityReport "c" [result "a" ProbeFailed, result "b" ProbeUnverified, result "c" ProbeOk]).capabilityActive @?= Just "c"
        (capabilityReport "c" [result "a" ProbeFailed, result "b" ProbeUnverified]).capabilityActive @?= Just "b"
        (capabilityReport "c" [result "a" ProbeFailed, result "b" ProbeSkipped]).capabilityActive @?= Nothing
    , testCase "the JSON report has the §1.5 shape" $ do
        let r = (result "read-jina" ProbeOk){probeMillis = Just 310, probeSandbox = Just (SandboxReport "auto" (Right "landlock"))}
        case Aeson.toJSON (capabilityReport "web_read" [r]) of
            Aeson.Object o -> do
                KeyMap.lookup "active" o @?= Just (Aeson.String "read-jina")
                case KeyMap.lookup "backends" o of
                    Just (Aeson.Array _) -> pure ()
                    other -> assertFailure ("backends: " <> show other)
            other -> assertFailure (show other)
        let encoded = Aeson.encode (capabilityReport "web_read" [r])
        assertBool "status ok" ("\"status\":\"ok\"" `isInfixOfL` encoded)
        assertBool "sandbox resolved" ("\"mechanism\":\"landlock\"" `isInfixOfL` encoded)
    , testCase "the text line names the sandbox mechanism auto resolved to" $ do
        let r = (result "t" ProbeOk){probeSandbox = Just (SandboxReport "auto" (Right "bwrap"))}
        assertBool "line" ("auto -> bwrap" `Text.isInfixOf` renderProbeLine r)
    ]
  where
    result n s = ProbeResult n ("/t/" <> Text.unpack n) [n] s Nothing Nothing Nothing Nothing
    isInfixOfL needle hay = Text.pack needle `Text.isInfixOf` Text.decodeUtf8 (LBS.toStrict hay)

probe :: ToolRuntimeConfig -> FilePath -> IO ProbeResult
probe cfg path = do
    loaded <- Bash.loadScriptWith cfg nullTracer path
    desc <- either (\e -> assertFailure ("failed to load fixture: " <> show e)) pure loaded
    probeScriptWith cfg ["t"] desc

assertMentions :: Text.Text -> Maybe Text.Text -> Assertion
assertMentions needle mt = case mt of
    Just t -> assertBool (Text.unpack t) (needle `Text.isInfixOf` t)
    Nothing -> assertFailure ("expected a detail mentioning " <> Text.unpack needle)

v2NoCheck :: String
v2NoCheck = "{\"protocol\": 2, \"slug\": \"t\", \"description\": \"d\", \"args\": []}"

subcommands :: String
subcommands = "{\"protocol\": 2, \"check\": true, \"slug\": \"multi\", \"description\": \"d\", \"args\": [], \"commands\": [{\"name\": \"a\", \"description\": \"a\", \"args\": []}, {\"name\": \"b\", \"description\": \"b\", \"args\": []}]}"

-- | A v2 tool declaring @check@, with the given runtime (or none when empty), whose @check@ runs the body.
checkTool :: String -> String -> String
checkTool runtime body =
    describeThen
        ( "{\"protocol\": 2, \"check\": true, \"slug\": \"t\", \"description\": \"d\", \"args\": []"
            <> (if null runtime then "" else ", \"runtime\": " <> runtime)
            <> "}"
        )
        ("if [[ \"$1\" == check ]]; then\n" <> body <> "\nfi")

withTool :: String -> (FilePath -> IO a) -> IO a
withTool content k =
    withSystemTempDirectory "bash-tools-check" $ \dir -> do
        let path = dir <> "/tool"
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

nullTracer :: Tracer IO a
nullTracer = Tracer (const (pure ()))
