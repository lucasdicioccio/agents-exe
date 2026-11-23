{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Tests for bash tools v2, capabilities with ordered backends
(@todos/bash-tools-v2.md@ §1.5, D9).
-}
module BashToolsCapabilityTests (tests) where

import qualified Data.Aeson as Aeson
import Data.Either (isLeft, isRight)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Prod.Tracer (Tracer (..))
import System.Directory (doesFileExist, getPermissions, setOwnerExecutable, setPermissions)
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import qualified System.Agents.Base as Base
import System.Agents.ToolRegistration (tagBackendResult)
import System.Agents.Tools.Base (CallResult (..))
import qualified System.Agents.Tools.Bash as Bash
import System.Agents.Tools.Capability
import System.Agents.Tools.ScriptCheck
import System.Agents.Tools.ScriptRuntime (defaultToolRuntimeConfig)
import System.Agents.Tools.ScriptTypes (ScriptDescription (..), ScriptInfo (..))

tests :: TestTree
tests =
    testGroup
        "Bash tools v2 capabilities"
        [ testGroup "configuration" configTests
        , testGroup "order" orderTests
        , testGroup "validate" validateTests
        , testGroup "select" selectTests
        , testGroup "results" resultTests
        ]

configTests :: [TestTree]
configTests =
    [ testCase "a Capability toolbox parses, required by default" $ do
        let json = "{\"tag\": \"Capability\", \"contents\": {\"capability\": \"web_read\", \"backends\": [\"a/read-jina\", \"a/read-curl\"]}}"
        case Aeson.eitherDecodeStrict (Text.encodeUtf8 json) of
            Right (Base.Capability c) -> do
                c.capName @?= "web_read"
                c.capBackends @?= ["a/read-jina", "a/read-curl"]
                Base.capabilityIsRequired c @?= True
            other -> assertFailure (show other)
    , testCase "it round-trips through JSON" $ do
        let c = Base.Capability (Base.CapabilityDescription "web_read" ["x"] (Just False) Nothing Nothing)
        Aeson.eitherDecode (Aeson.encode c) @?= Right c
    ]

orderTests :: [TestTree]
orderTests =
    [ testCase "an override moves the named backend first" $
        names (orderBackends [("web_read", "c")] "web_read" (map stub ["a", "b", "c"])) @?= ["c", "a", "b"]
    , testCase "an unknown backend name is ignored" $
        names (orderBackends [("web_read", "zz")] "web_read" (map stub ["a", "b"])) @?= ["a", "b"]
    , testCase "an override for another capability does not apply" $
        names (orderBackends [("other", "b")] "web_read" (map stub ["a", "b"])) @?= ["a", "b"]
    , testCase "the last override given wins" $
        names (orderBackends [("c", "a"), ("c", "b")] "c" (map stub ["a", "b"])) @?= ["b", "a"]
    ]
  where
    stub n = Backend n ("/t/" <> Text.unpack n) (Left "not loaded")
    names = map (.backendName)

validateTests :: [TestTree]
validateTests =
    [ testCase "backends with the same args are valid" $
        withTools [("a", okTool "x" "string"), ("b", unverifiedTool "x" "string")] $ \paths -> do
            backends <- load paths
            assertBool "valid" (isRight (validateBackends "cap" backends))
    , testCase "backends whose args differ are refused" $
        withTools [("a", okTool "x" "string"), ("b", okTool "y" "string")] $ \paths -> do
            backends <- load paths
            assertLeftMentions "same args" (validateBackends "cap" backends)
    , testCase "a backend with subcommands is refused" $
        withTools [("a", describeThen subcommands "true")] $ \paths -> do
            backends <- load paths
            assertLeftMentions "subcommands" (validateBackends "cap" backends)
    , testCase "a service backend is refused" $
        withTools [("a", describeThen service "true")] $ \paths -> do
            backends <- load paths
            assertLeftMentions "service" (validateBackends "cap" backends)
    , testCase "a backend that does not load is not a configuration error" $
        withTools [("a", okTool "x" "string")] $ \paths -> do
            backends <- load (paths <> ["/nonexistent/read-gone"])
            assertBool "valid" (isRight (validateBackends "cap" backends))
    , testCase "an invalid name, no backends and duplicate names are refused" $ do
        assertBool "name" (isLeft (validateBackends "web read" []))
        assertBool "none" (isLeft (validateBackends "cap" []))
        let dup n = Backend n "/x" (Left "")
        assertLeftMentions "several backends named" (validateBackends "cap" [dup "a", dup "a"])
    ]

selectTests :: [TestTree]
selectTests =
    [ testCase "the first ok backend is active, after a failed one" $
        withTools [("a", failingTool), ("b", okTool "x" "string")] $ \paths -> do
            (active, probes) <- select paths
            fmap (.backendName) active @?= Just "b"
            map (.probeStatus) probes @?= [ProbeFailed, ProbeOk]
    , testCase "an ok backend wins over an earlier unverified one" $
        withTools [("a", unverifiedTool "x" "string"), ("b", okTool "x" "string")] $ \paths -> do
            (active, _) <- select paths
            fmap (.backendName) active @?= Just "b"
    , testCase "without an ok backend, the first unverified one is active" $
        withTools [("a", failingTool), ("b", unverifiedTool "x" "string"), ("c", unverifiedTool "x" "string")] $ \paths -> do
            (active, _) <- select paths
            fmap (.backendName) active @?= Just "b"
    , testCase "a backend that does not load is failed and skipped" $
        withTools [("b", unverifiedTool "x" "string")] $ \paths -> do
            (active, probes) <- select ("/nonexistent/a" : paths)
            fmap (.backendName) active @?= Just "b"
            map (.probeStatus) probes @?= [ProbeFailed, ProbeUnverified]
    , testCase "no backend works: none is active" $
        withTools [("a", failingTool)] $ \paths -> do
            (active, probes) <- select paths
            fmap (.backendName) active @?= Nothing
            assertBool "message names the backend" ("a" `Text.isInfixOf` Text.pack (noActiveBackendMessage "cap" probes))
    , testCase "probes stop at the first ok backend" $
        withSystemTempDirectory "cap-marker" $ \dir -> do
            let marker = dir <> "/probed"
            withTools [("a", okTool "x" "string"), ("b", checkWith "x" "string" ("touch " <> marker <> "; echo '{\"ok\": true}'"))] $ \paths -> do
                (active, probes) <- select paths
                fmap (.backendName) active @?= Just "a"
                length probes @?= 1
                doesFileExist marker >>= assertBool "second backend not probed" . not
    , testCase "check --probe probes every backend and names the active one" $
        withTools [("a", failingTool), ("b", okTool "x" "string"), ("c", unverifiedTool "x" "string")] $ \paths -> do
            backends <- load paths
            report <- probeCapability defaultToolRuntimeConfig "cap" backends
            report.capabilityName @?= "cap"
            report.capabilityActive @?= Just "b"
            map (.probeStatus) report.capabilityBackends @?= [ProbeFailed, ProbeOk, ProbeUnverified]
            map (.probeName) report.capabilityBackends @?= ["a", "b", "c"]
    , testCase "the model sees the active backend under the capability's name" $
        withTools [("read-curl", unverifiedTool "x" "string")] $ \paths -> do
            backends <- load paths
            (active, _) <- selectActiveBackend defaultToolRuntimeConfig "web_read" backends
            case active of
                Just (b, desc) -> do
                    b.backendName @?= "read-curl"
                    (capabilityTool "web_read" desc).scriptInfo.scriptSlug @?= "web_read"
                Nothing -> assertFailure "no active backend"
    ]
  where
    select paths = do
        backends <- load paths
        (active, probes) <- selectActiveBackend defaultToolRuntimeConfig "cap" backends
        pure (fmap fst active, probes)

resultTests :: [TestTree]
resultTests =
    [ testCase "a text result starts with the backend that served it" $
        case tagBackendResult "web_read" "read-jina" (BlobToolSuccess () "hello" Nothing) of
            BlobToolSuccess () bs Nothing -> bs @?= "[backend read-jina]\nhello"
            _ -> assertFailure "expected a blob"
    , testCase "an error names the capability and the backend" $
        case tagBackendResult "web_read" "read-jina" (BashToolError () (Bash.ScriptRuntimeError "/t" "boom")) of
            BashToolError () (Bash.ScriptBackendError c b _) -> (c, b) @?= ("web_read", "read-jina")
            _ -> assertFailure "expected a backend error"
    ]

load :: [FilePath] -> IO [Backend]
load = loadBackendsWith defaultToolRuntimeConfig nullTracer

assertLeftMentions :: String -> Either String () -> Assertion
assertLeftMentions needle r = case r of
    Left msg -> assertBool msg (Text.pack needle `Text.isInfixOf` Text.pack msg)
    Right () -> assertFailure ("expected an error mentioning " <> needle)

-- | Tools named by their file name, written in a temporary directory.
withTools :: [(String, String)] -> ([FilePath] -> IO a) -> IO a
withTools tools k =
    withSystemTempDirectory "bash-tools-capability" $ \dir -> do
        paths <- mapM (write dir) tools
        k paths
  where
    write dir (name, content) = do
        let path = dir <> "/" <> name
        writeFile path content
        perms <- getPermissions path
        setPermissions path (setOwnerExecutable True perms)
        pure path

v2 :: Bool -> String -> String -> String
v2 check arg ty =
    "{\"protocol\": 2, \"check\": "
        <> (if check then "true" else "false")
        <> ", \"slug\": \"t\", \"description\": \"d\", \"args\": [{\"name\": \""
        <> arg
        <> "\", \"description\": \"an arg\", \"type\": \""
        <> ty
        <> "\", \"backing_type\": \""
        <> ty
        <> "\", \"arity\": \"single\", \"mode\": \"positional\"}]}"

checkWith :: String -> String -> String -> String
checkWith arg ty body = describeThen (v2 True arg ty) ("if [[ \"$1\" == check ]]; then\n" <> body <> "\nfi")

okTool :: String -> String -> String
okTool arg ty = checkWith arg ty "echo '{\"ok\": true}'"

unverifiedTool :: String -> String -> String
unverifiedTool arg ty = describeThen (v2 False arg ty) "true"

failingTool :: String
failingTool = checkWith "x" "string" "echo '{\"ok\": false, \"detail\": \"down\"}'; exit 1"

subcommands :: String
subcommands = "{\"protocol\": 2, \"slug\": \"multi\", \"description\": \"d\", \"args\": [], \"commands\": [{\"name\": \"a\", \"description\": \"a\", \"args\": []}]}"

service :: String
service = "{\"protocol\": 2, \"slug\": \"svc\", \"description\": \"d\", \"args\": [], \"process\": {\"mode\": \"service\", \"ready\": {\"kind\": \"line\", \"match\": \"ready\"}}}"

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
