{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Tests for bash tools v2, slice 2 (@todos/bash-tools-v2.md@ §1.2, §1.4):
the @runtime@ of a @oneshot@ tool (envdir, environment, rundir, timeouts,
cancel, run_as, sandbox) and the operator configuration it needs.
-}
module BashToolsRuntimeTests (tests) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Either (isLeft, isRight)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Prod.Tracer (Tracer (..))
import System.Directory (createDirectory, doesDirectoryExist, findExecutable, getPermissions, setOwnerExecutable, setPermissions)
import System.Exit (ExitCode (..))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Tasty
import Test.Tasty.HUnit

import qualified System.Agents.Tools.Bash as Bash
import System.Agents.Tools.ScriptRuntime
import System.Agents.Tools.ScriptTypes (ScriptInfo (..))

tests :: TestTree
tests =
    testGroup
        "Bash tools v2 runtime"
        [ testGroup "load" loadTests
        , testGroup "launch" launchTests
        , testGroup "run" runTests
        ]

loadTests :: [TestTree]
loadTests =
    [ testCase "an empty runtime loads with no operator configuration" $
        withTool (runtimeTool "{}" "true") $ \_ path -> do
            r <- Bash.loadScriptWith defaultToolRuntimeConfig nullTracer path
            assertBool "loads" (isRight r)
    , testCase "runtime round-trips through JSON" $
        withTool (runtimeTool fullRuntime "true") $ \_ path -> do
            desc <- loadWith allConfigured path
            Aeson.decode (Aeson.encode desc.scriptInfo) @?= Just desc.scriptInfo
    , testCase "runtime is ignored on a v1 tool (D1)" $
        withTool (describeThen "{\"slug\": \"v1\", \"description\": \"d\", \"args\": [], \"runtime\": {\"envdir\": \"x\"}}" "true") $ \_ path -> do
            desc <- loadWith defaultToolRuntimeConfig path
            desc.scriptInfo.scriptRuntime @?= Nothing
    , testCase "envdir needs --envdir-root" $
        refusedWith defaultToolRuntimeConfig "{\"envdir\": \"tool\"}"
    , testCase "envdir must be a single name" $
        refusedWith allConfigured "{\"envdir\": \"../tool\"}"
    , testCase "run_as needs a mechanism" $
        refusedWith defaultToolRuntimeConfig "{\"run_as\": {\"user\": \"nobody\"}}"
    , testCase "run_as with a mechanism loads" $
        acceptedWith allConfigured "{\"run_as\": {\"user\": \"nobody\", \"group\": \"nogroup\"}}"
    , testCase "run_as refuses an option-looking user" $
        refusedWith allConfigured "{\"run_as\": {\"user\": \"-x\"}}"
    , testCase "sandbox needs a mechanism (never unconfined by silence)" $
        refusedWith defaultToolRuntimeConfig "{\"sandbox\": {\"net\": \"none\"}}"
    , testCase "sandbox under landlock is refused (not implemented yet)" $
        refusedWith defaultToolRuntimeConfig{sandboxMechanism = Just SandboxLandlock} "{\"sandbox\": {}}"
    , testCase "sandbox under auto loads" $
        acceptedWith defaultToolRuntimeConfig{sandboxMechanism = Just SandboxAuto} "{\"sandbox\": {\"fs\": {\"read\": [\"/srv\"]}}}"
    , testCase "an unknown runtime field is refused" $
        refusedWith allConfigured "{\"run-as\": {\"user\": \"nobody\"}}"
    , testCase "an unknown sandbox field is refused" $
        refusedWith allConfigured "{\"sandbox\": {\"network\": \"none\"}}"
    , testCase "rundir must stay inside the tool's directory" $ do
        refusedWith allConfigured "{\"rundir\": \"../elsewhere\"}"
        refusedWith allConfigured "{\"rundir\": \"/abs\"}"
    , testCase "an invalid environment name is refused" $
        refusedWith allConfigured "{\"environment\": {\"A=B\": \"x\"}}"
    , testCase "timeout.idle and on_exit only apply to a service" $ do
        refusedWith allConfigured "{\"timeout\": {\"idle\": 10}}"
        refusedWith allConfigured "{\"on_exit\": \"restart\"}"
    , testCase "a non-positive timeout is refused" $
        refusedWith allConfigured "{\"timeout\": {\"stop\": 0}}"
    , testCase "cancel by send needs an open stdin" $
        refusedWith allConfigured "{\"cancel\": {\"send\": {\"op\": \"stop\"}}}"
    , testCase "cancel by signal loads" $
        acceptedWith allConfigured "{\"cancel\": \"SIGINT\", \"timeout\": {\"stop\": 2}}"
    ]

launchTests :: [TestTree]
launchTests =
    [ testCase "later overrides win and Nothing removes" $
        overrideEnvironment [("A", "1"), ("B", "2"), ("C", "3")] [("A", Just "x"), ("B", Nothing), ("A", Just "y"), ("D", Just "4")]
            @?= [("C", "3"), ("A", "y"), ("D", "4")]
    , testCase "sudo and runuser prefixes keep the environment" $ do
        sudo <- runAsPrefix RunAsSudo (RunAs "svc" (Just "grp"))
        sudo @?= Right ["sudo", "-n", "--preserve-env", "-u", "svc", "-g", "grp", "--"]
        runuser <- runAsPrefix RunAsRunuser (RunAs "svc" Nothing)
        runuser @?= Right ["runuser", "-p", "-u", "svc", "--"]
    , testCase "setpriv names the group explicitly" $ do
        r <- runAsPrefix RunAsSetpriv (RunAs "svc" (Just "grp"))
        r @?= Right ["setpriv", "--reuid=svc", "--regid=grp", "--init-groups", "--"]
    , testCase "bwrap: declared paths, net none, private tmp, rundir" $ do
        let args = bwrapArgs "/tools" (Just "/tools/state") (Sandbox ["data", "/srv/ro"] ["/var/out"] NetNone TmpPrivate)
        assertBool "tool dir read-only" (hasSeq ["--ro-bind", "/tools", "/tools"] args)
        assertBool "relative read resolved" (hasSeq ["--ro-bind", "/tools/data", "/tools/data"] args)
        assertBool "absolute read" (hasSeq ["--ro-bind", "/srv/ro", "/srv/ro"] args)
        assertBool "write" (hasSeq ["--bind", "/var/out", "/var/out"] args)
        assertBool "private tmp" (hasSeq ["--tmpfs", "/tmp"] args)
        assertBool "no network" ("--unshare-net" `elem` args)
        assertBool "rundir" (hasSeq ["--chdir", "/tools/state"] args)
    , testCase "bwrap: host network and shared tmp by default" $ do
        let args = bwrapArgs "/tools" Nothing (Sandbox [] [] NetHost TmpShared)
        assertBool "network kept" ("--unshare-net" `notElem` args)
        assertBool "shared tmp" (hasSeq ["--bind-try", "/tmp", "/tmp"] args)
    , testCase "run_as wraps the sandbox, which wraps the tool" $
        withSystemTempDirectory "runtime-launch" $ \dir -> do
            let rt = emptyScriptRuntime{runtimeRunAs = Just (RunAs "svc" (Just "grp")), runtimeSandbox = Just (Sandbox [] [] NetHost TmpShared)}
            r <- prepareLaunch allConfigured (dir <> "/tool") rt ["run"] [] []
            case r of
                Left err -> assertFailure err
                Right l -> do
                    l.launchCommand @?= "sudo"
                    assertBool "bwrap after sudo" (hasSeq ["--", "bwrap"] l.launchArgs)
                    assertBool "tool last" (hasSeq ["--", dir <> "/tool", "run"] l.launchArgs)
    ]

runTests :: [TestTree]
runTests =
    [ testCase "envdir, environment and rundir reach the tool" $
        withTool (runtimeTool "{\"envdir\": \"t\", \"environment\": {\"BAZ\": \"literal\"}, \"rundir\": \"state\"}" printEnv) $ \dir path ->
            withSystemTempDirectory "envdirs" $ \root -> do
                createDirectory (root <> "/t")
                writeFile (root <> "/t/FOO") "bar  \nignored\n"
                writeFile (root <> "/t/BAZ") "from-envdir"
                writeFile (root <> "/t/HOME") ""
                let cfg = defaultToolRuntimeConfig{envdirRoot = Just root}
                desc <- loadWith cfg path
                r <- Bash.runValueWith cfg nullTracer desc Nothing (Aeson.object [])
                out <- either (\e -> assertFailure (show e)) pure r
                case Aeson.decodeStrict out of
                    Just (Aeson.Array evs) | [Aeson.Object o] <- foldr (:) [] evs -> do
                        KeyMap.lookup "foo" o @?= Just "bar"
                        KeyMap.lookup "baz" o @?= Just "literal"
                        KeyMap.lookup "home" o @?= Just "unset"
                        KeyMap.lookup "pwd" o @?= Just (Aeson.String (Text.pack (dir <> "/state")))
                    _ -> assertFailure ("unexpected output: " <> show out)
                created <- doesDirectoryExist (dir <> "/state")
                assertBool "rundir created" created
    , testCase "a missing envdir fails the call, naming the field" $
        withTool (runtimeTool "{\"envdir\": \"absent\"}" "true") $ \_ path ->
            withSystemTempDirectory "envdirs" $ \root -> do
                let cfg = defaultToolRuntimeConfig{envdirRoot = Just root}
                desc <- loadWith cfg path
                r <- Bash.runValueWith cfg nullTracer desc Nothing (Aeson.object [])
                case r of
                    Left (Bash.ScriptRuntimeError _ msg) -> assertBool msg ("runtime.envdir" `Text.isInfixOf` Text.pack msg)
                    other -> assertFailure ("expected a runtime error, got " <> show other)
    , testCase "timeout.start stops a oneshot that does not complete" $
        withTool (runtimeTool "{\"timeout\": {\"start\": 0.3, \"stop\": 0.1}}" "sleep 10") $ \_ path -> do
            desc <- loadWith defaultToolRuntimeConfig path
            t0 <- getCurrentTime
            r <- Bash.runValueWith defaultToolRuntimeConfig nullTracer desc Nothing (Aeson.object [])
            t1 <- getCurrentTime
            case r of
                Left (Bash.ScriptTimeoutError _ secs) -> secs @?= 0.3
                other -> assertFailure ("expected a timeout, got " <> show other)
            assertBool "stopped promptly" (diffUTCTime t1 t0 < 5)
    , testCase "a sandboxed tool writes only where it declared (bwrap)" $
        withBwrap $
            withTool (runtimeTool "{\"sandbox\": {\"fs\": {\"write\": [\"out\"]}, \"net\": \"none\"}}" sandboxBody) $ \dir path -> do
                createDirectory (dir <> "/out")
                let cfg = defaultToolRuntimeConfig{sandboxMechanism = Just SandboxBwrap}
                desc <- loadWith cfg path
                r <- Bash.runValueWith cfg nullTracer desc Nothing (Aeson.object [])
                out <- either (\e -> assertFailure (show e)) pure r
                let txt = Text.decodeUtf8With lenientDecode out
                assertBool ("declared write allowed: " <> Text.unpack txt) ("out:ok" `Text.isInfixOf` txt)
                assertBool ("tool dir read-only: " <> Text.unpack txt) ("tooldir:denied" `Text.isInfixOf` txt)
    ]

-------------------------------------------------------------------------------
-- Fixtures
-------------------------------------------------------------------------------

nullTracer :: Tracer IO a
nullTracer = Tracer (\_ -> pure ())

allConfigured :: ToolRuntimeConfig
allConfigured = ToolRuntimeConfig (Just "/nonexistent-envdir-root") (Just RunAsSudo) (Just SandboxBwrap)

fullRuntime :: String
fullRuntime =
    "{\"run_as\": {\"user\": \"logs\", \"group\": \"logs\"}, \"sandbox\": {\"fs\": {\"read\": [\"/var/log\"], \"write\": [\"state\"]}, \"net\": \"none\", \"tmp\": \"private\"}, \"envdir\": \"env\", \"environment\": {\"LC_ALL\": \"C\"}, \"rundir\": \"state\", \"timeout\": {\"start\": 10, \"stop\": 5}, \"cancel\": \"SIGTERM\"}"

-- | A v2 lines-framed tool with the given runtime.
runtimeTool :: String -> String -> String
runtimeTool runtime =
    describeThen
        ( "{\"protocol\": 2, \"slug\": \"rt\", \"description\": \"d\", \"args\": [], \"runtime\": "
            <> runtime
            <> "}"
        )

printEnv :: String
printEnv = "printf '{\"foo\": \"%s\", \"baz\": \"%s\", \"home\": \"%s\", \"pwd\": \"%s\"}\\n' \"$FOO\" \"$BAZ\" \"${HOME-unset}\" \"$(pwd)\""

sandboxBody :: String
sandboxBody =
    unlines
        [ "dir=$(dirname \"$0\")"
        , "if echo x > \"$dir/out/f\" 2>/dev/null; then echo out:ok; else echo out:denied; fi"
        , "if echo x > \"$dir/g\" 2>/dev/null; then echo tooldir:ok; else echo tooldir:denied; fi"
        ]

loadWith :: ToolRuntimeConfig -> FilePath -> IO Bash.ScriptDescription
loadWith cfg path = do
    r <- Bash.loadScriptWith cfg nullTracer path
    either (\err -> assertFailure ("failed to load fixture: " <> show err)) pure r

refusedWith :: ToolRuntimeConfig -> String -> Assertion
refusedWith cfg runtime =
    withTool (runtimeTool runtime "true") $ \_ path -> do
        r <- Bash.loadScriptWith cfg nullTracer path
        assertBool ("expected a load error for " <> runtime) (isLeft r)

acceptedWith :: ToolRuntimeConfig -> String -> Assertion
acceptedWith cfg runtime =
    withTool (runtimeTool runtime "true") $ \_ path -> do
        r <- Bash.loadScriptWith cfg nullTracer path
        either (\err -> assertFailure ("expected to load: " <> show err)) (const (pure ())) r

withTool :: String -> (FilePath -> FilePath -> IO a) -> IO a
withTool content k =
    withSystemTempDirectory "bash-tools-runtime" $ \dir -> do
        let path = dir <> "/tool"
        writeFile path content
        perms <- getPermissions path
        setPermissions path (setOwnerExecutable True perms)
        k dir path

{- | Runs the test only where bubblewrap can build a sandbox (it needs
unprivileged user namespaces, which some systems restrict).
-}
withBwrap :: Assertion -> Assertion
withBwrap k = do
    mBwrap <- findExecutable "bwrap"
    usable <- case mBwrap of
        Nothing -> pure False
        Just _ -> do
            (code, _, _) <- readProcessWithExitCode "bwrap" ["--ro-bind", "/", "/", "true"] ""
            pure (code == ExitSuccess)
    if usable then k else pure ()

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

hasSeq :: [String] -> [String] -> Bool
hasSeq needle haystack = any (\i -> take (length needle) (drop i haystack) == needle) [0 .. length haystack]
