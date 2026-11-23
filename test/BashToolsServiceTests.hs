{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Tests for bash tools v2, slice 4 (@todos/bash-tools-v2.md@ §2.3, §2.4):
@service@ tools, their process handles, the typed @send@ companion, the
shared @processes@ tool, mail delivery, @on_exit@ and @timeout.idle@.
-}
module BashToolsServiceTests (tests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Either (isLeft)
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.UUID as UUID
import qualified Data.Vector as Vector
import Prod.Tracer (Tracer (..))
import System.Directory (getPermissions, setOwnerExecutable, setPermissions)
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import System.Agents.Base (ConversationId (..))
import System.Agents.Session.Base (SessionId (..), TurnId (..))
import System.Agents.Session.Mailbox (MailRouter (..), MailboxInfo (..), Mailbox (..), newInMemoryMailbox, newMailRouter)
import System.Agents.Session.Types (Envelope (..), MailBody (..), MailScope (..), Sender (..))
import System.Agents.ToolRegistration (ToolRegistration (..), registerBashToolsInLLMWith)
import System.Agents.ToolSchema (ParamProperty (..), ParamType (..), ToolDescription (..), ToolName (..))
import qualified System.Agents.Tools.Bash as Bash
import System.Agents.Tools.Context (ToolExecutionContext (..), mkMinimalContext)
import System.Agents.Tools.ScriptRuntime (defaultToolRuntimeConfig)
import System.Agents.Tools.ScriptService

tests :: TestTree
tests =
    testGroup
        "Bash tools v2: service"
        [ testGroup "describe" describeTests
        , testCase "a service with input gets a typed send companion" registrationTest
        , testCase "start, send, tail, list, stop" lifecycleTest
        , testCase "a process belongs to the session that started it" ownershipTest
        , testCase "an event that does not match the input schema is refused" invalidEventTest
        , testCase "an exit before ready fails the start" exitBeforeReadyTest
        , testCase "timeout.start bounds reaching ready" startTimeoutTest
        , testCase "output and the exit arrive as mail" mailTest
        , testCase "on_exit restart brings the process back" restartTest
        , testCase "timeout.idle stops a quiet process" idleTest
        , testCase "deleting a session stops its processes" sessionStopTest
        ]

describeTests :: [TestTree]
describeTests =
    [ testCase "a service loads" $
        withTool (serviceDescribe "") echoBody $ \_ -> pure ()
    , testCase "ready exit does not apply to a service" $
        refused (describeJson "{\"mode\": \"service\", \"ready\": {\"kind\": \"exit\"}}" "")
    , testCase "digest only applies to a service" $
        refused (describeJson "{\"mode\": \"stream\", \"digest\": {\"per\": \"round\"}}" "")
    , testCase "a digest count must be positive" $
        refused (describeJson "{\"mode\": \"service\", \"digest\": {\"per\": \"events\", \"n\": 0}}" "")
    , testCase "cancel by send needs process.input" $
        refused (describeJson "{\"mode\": \"service\"}" ", \"runtime\": {\"cancel\": {\"send\": {\"op\": \"quit\"}}}")
    , testCase "timeout.start on a service needs a ready line" $
        refused (describeJson "{\"mode\": \"service\"}" ", \"runtime\": {\"timeout\": {\"start\": 1}}")
    , testCase "on_exit and timeout.idle load on a service" $
        withTool (serviceDescribe ", \"runtime\": {\"on_exit\": \"restart\", \"timeout\": {\"idle\": 5}, \"cancel\": {\"send\": {\"op\": \"quit\"}}}") echoBody $ \_ -> pure ()
    ]

registrationTest :: Assertion
registrationTest =
    withTool (serviceDescribe "") echoBody $ \desc -> do
        let regs = registerBashToolsInLLMWith Bash.defaultRunOptions Nothing desc
        fmap (getToolName . (.declareTool.toolDescriptionName)) regs @?= ["bash_svc", "bash_svc_send"]
        case regs of
            [_, send] -> case [p | p <- send.declareTool.toolDescriptionParamProperties, p.propertyKey == "event"] of
                [p] -> case p.propertyType of
                    SchemaParamType s -> s @?= inputSchema
                    other -> assertFailure ("event is not a verbatim schema: " <> show other)
                _ -> assertFailure "no event parameter"
            _ -> assertFailure "expected two registrations"

lifecycleTest :: Assertion
lifecycleTest =
    withTool (serviceDescribe "") echoBody $ \desc -> do
        reg <- newProcessRegistry
        ctx <- sessionContext 1
        pid <- startOk reg desc ctx
        st <- statusOf reg ctx pid
        field "status" st @?= Just "running"
        r <- sendToService reg "bash_svc" desc ctx (Aeson.object ["process" Aeson..= pid, "event" Aeson..= ping])
        case r of
            Right v -> field' "events" v @?= Just (Aeson.toJSON [Aeson.object ["echo" Aeson..= ping]])
            Left err -> assertFailure ("send failed: " <> show err)
        Right tl <- runProcessesCommand reg ctx (ProcessesTail pid 10)
        case field' "events" tl of
            Just (Aeson.Array evs) -> Vector.length evs @?= 2
            other -> assertFailure ("unexpected tail: " <> show other)
        Right ls <- runProcessesCommand reg ctx ProcessesList
        case field' "processes" ls of
            Just (Aeson.Array ps) -> Vector.length ps @?= 1
            other -> assertFailure ("unexpected list: " <> show other)
        Right stopped <- runProcessesCommand reg ctx (ProcessesStop pid False)
        field "status" stopped @?= Just "exited"
        r2 <- sendToService reg "bash_svc" desc ctx (Aeson.object ["process" Aeson..= pid, "event" Aeson..= ping])
        assertBool "a stopped process takes no events" (isLeft r2)

ownershipTest :: Assertion
ownershipTest =
    withTool (serviceDescribe "") echoBody $ \desc -> do
        reg <- newProcessRegistry
        ctx <- sessionContext 1
        other <- sessionContext 2
        pid <- startOk reg desc ctx
        Right ls <- runProcessesCommand reg other ProcessesList
        field' "processes" ls @?= Just (Aeson.toJSON ([] :: [Aeson.Value]))
        st <- runProcessesCommand reg other (ProcessesStatus pid)
        assertBool "another session cannot see it" (isLeft st)
        sent <- sendToService reg "bash_svc" desc other (Aeson.object ["process" Aeson..= pid, "event" Aeson..= ping])
        assertBool "another session cannot send to it" (isLeft sent)
        _ <- runProcessesCommand reg ctx (ProcessesStop pid True)
        pure ()

invalidEventTest :: Assertion
invalidEventTest =
    withTool (serviceDescribe "") echoBody $ \desc -> do
        reg <- newProcessRegistry
        ctx <- sessionContext 1
        pid <- startOk reg desc ctx
        r <- sendToService reg "bash_svc" desc ctx (Aeson.object ["process" Aeson..= pid, "event" Aeson..= Aeson.object ["op" Aeson..= ("dance" :: Text)]])
        case r of
            Left (Bash.ScriptServiceError _ msg) -> assertBool msg ("does not match" `isInfixOf` msg)
            other -> assertFailure ("expected a refusal, got " <> show other)
        _ <- runProcessesCommand reg ctx (ProcessesStop pid True)
        pure ()

exitBeforeReadyTest :: Assertion
exitBeforeReadyTest =
    withTool (serviceDescribe "") "echo 'cannot open the log' >&2\nexit 2\n" $ \desc -> do
        reg <- newProcessRegistry
        ctx <- sessionContext 1
        r <- startService defaultServiceOptions defaultToolRuntimeConfig reg "bash_svc" desc ctx (Aeson.object [])
        case r of
            Left (Bash.ScriptServiceError _ msg) -> assertBool msg ("cannot open the log" `isInfixOf` msg)
            other -> assertFailure ("expected a failed start, got " <> show other)
        Right ls <- runProcessesCommand reg ctx ProcessesList
        field' "processes" ls @?= Just (Aeson.toJSON ([] :: [Aeson.Value]))

startTimeoutTest :: Assertion
startTimeoutTest =
    withTool (serviceDescribe ", \"runtime\": {\"timeout\": {\"start\": 0.3, \"stop\": 0.2}}") "sleep 10\n" $ \desc -> do
        reg <- newProcessRegistry
        ctx <- sessionContext 1
        r <- startService defaultServiceOptions defaultToolRuntimeConfig reg "bash_svc" desc ctx (Aeson.object [])
        case r of
            Left (Bash.ScriptTimeoutError _ _) -> pure ()
            other -> assertFailure ("expected a timeout, got " <> show other)

mailTest :: Assertion
mailTest =
    withTool (serviceDescribeWith "{\"per\": \"events\", \"n\": 1}" "") echoBody $ \desc -> do
        reg <- newProcessRegistry
        base <- sessionContext 1
        router <- newMailRouter
        mb <- newInMemoryMailbox
        _ <- router.mrRegister base.ctxSessionId (MailboxInfo Nothing Nothing "running" MailScopeSubtree MailScopeChildren) mb
        let ctx = base{ctxMailRouter = Just router}
        pid <- startOk reg desc ctx
        -- the ready line is an event: it is mailed
        first <- waitForMail mb (const True)
        fmap (.envFrom) first @?= Just (FromSystem ("process:" <> pid))
        -- quit: the process exits with 3, which on_exit report says in mail
        _ <- sendToService reg "bash_svc" desc ctx (Aeson.object ["process" Aeson..= pid, "event" Aeson..= Aeson.object ["op" Aeson..= ("quit" :: Text)]])
        exited <- waitForMail mb (\e -> case e.envBody of AgentMessage t _ _ -> "\"exited\"" `Text.isInfixOf` t; _ -> False)
        assertBool "an exit notice arrived" (exited /= Nothing)
        st <- statusOf reg ctx pid
        field' "code" st @?= Just (Aeson.Number 3)

restartTest :: Assertion
restartTest =
    withTool (serviceDescribe ", \"runtime\": {\"on_exit\": \"restart\", \"timeout\": {\"stop\": 0.2}}") "echo '{\"line\": \"ready\"}'\nsleep 0.1\nexit 1\n" $ \desc -> do
        reg <- newProcessRegistry
        ctx <- sessionContext 1
        pid <- startOk reg desc ctx
        threadDelay 1600000
        st <- statusOf reg ctx pid
        assertBool ("restarted at least once: " <> show st) (maybe False (>= 1) (numField "restarts" st))
        Right stopped <- runProcessesCommand reg ctx (ProcessesStop pid False)
        field "status" stopped @?= Just "exited"

idleTest :: Assertion
idleTest =
    withTool (serviceDescribe ", \"runtime\": {\"timeout\": {\"idle\": 0.3, \"stop\": 0.2}}") "echo '{\"line\": \"ready\"}'\nsleep 30\n" $ \desc -> do
        reg <- newProcessRegistry
        ctx <- sessionContext 1
        pid <- startOk reg desc ctx
        threadDelay 1500000
        st <- statusOf reg ctx pid
        field "status" st @?= Just "exited"

sessionStopTest :: Assertion
sessionStopTest =
    withTool (serviceDescribe "") echoBody $ \desc -> do
        reg <- newProcessRegistry
        ctx <- sessionContext 1
        pid <- startOk reg desc ctx
        stopSessionProcesses reg ctx.ctxSessionId
        st <- runProcessesCommand reg ctx (ProcessesStatus pid)
        assertBool "the process is forgotten" (isLeft st)

-------------------------------------------------------------------------------
-- Fixtures
-------------------------------------------------------------------------------

nullTracer :: Tracer IO a
nullTracer = Tracer (\_ -> pure ())

sessionContext :: Int -> IO ToolExecutionContext
sessionContext n = do
    let portal _ _ = error "no portal in tests"
        sid = SessionId (UUID.fromWords 0 0 0 (fromIntegral n))
    pure (mkMinimalContext sid (ConversationId UUID.nil) (TurnId UUID.nil) portal)

startOk :: ProcessRegistry -> Bash.ScriptDescription -> ToolExecutionContext -> IO Text
startOk reg desc ctx = do
    r <- startService defaultServiceOptions{svcRoundQuietMicros = 100000} defaultToolRuntimeConfig reg "bash_svc" desc ctx (Aeson.object [])
    case r of
        Right v -> case field "process" v of
            Just pid -> pure pid
            Nothing -> assertFailure ("no process in the handle: " <> show v)
        Left err -> assertFailure ("start failed: " <> show err)

statusOf :: ProcessRegistry -> ToolExecutionContext -> Text -> IO Aeson.Value
statusOf reg ctx pid = do
    r <- runProcessesCommand reg ctx (ProcessesStatus pid)
    either (\e -> assertFailure ("status failed: " <> e)) pure r

waitForMail :: Mailbox -> (Envelope -> Bool) -> IO (Maybe Envelope)
waitForMail mb ok = go (40 :: Int)
  where
    go 0 = pure Nothing
    go k = do
        envs <- atomically (mb.mbUnread 0)
        case filter ok envs of
            (e : _) -> pure (Just e)
            [] -> threadDelay 100000 >> go (k - 1)

field :: Text -> Aeson.Value -> Maybe Text
field k v = case field' k v of
    Just (Aeson.String t) -> Just t
    _ -> Nothing

numField :: Text -> Aeson.Value -> Maybe Double
numField k v = case field' k v of
    Just (Aeson.Number n) -> Just (realToFrac n)
    _ -> Nothing

field' :: Text -> Aeson.Value -> Maybe Aeson.Value
field' k (Aeson.Object o) = KeyMap.lookup (Key.fromText k) o
field' _ _ = Nothing

ping :: Aeson.Value
ping = Aeson.object ["op" Aeson..= ("ping" :: Text)]

inputSchema :: Aeson.Value
inputSchema =
    Aeson.object
        [ "type" Aeson..= ("object" :: Text)
        , "properties" Aeson..= Aeson.object ["op" Aeson..= Aeson.object ["enum" Aeson..= (["ping", "quit"] :: [Text])]]
        , "required" Aeson..= (["op"] :: [Text])
        ]

-- | A jsonl service that says it is ready, then echoes each event it reads; @quit@ exits 3.
echoBody :: String
echoBody =
    unlines
        [ "echo '{\"line\": \"ready\"}'"
        , "while IFS= read -r l; do"
        , "  case \"$l\" in *quit*) exit 3 ;; esac"
        , "  echo \"{\\\"echo\\\": $l}\""
        , "done"
        ]

serviceDescribe :: String -> String
serviceDescribe = serviceDescribeWith "{\"per\": \"round\"}"

serviceDescribeWith :: String -> String -> String
serviceDescribeWith digest =
    describeJson
        ( "{\"mode\": \"service\", \"ready\": {\"kind\": \"line\", \"match\": \"ready\"}, \"digest\": "
            <> digest
            <> ", \"input\": {\"framing\": \"jsonl\", \"event\": "
            <> inputSchemaJson
            <> "}}"
        )
  where
    inputSchemaJson = "{\"type\": \"object\", \"properties\": {\"op\": {\"enum\": [\"ping\", \"quit\"]}}, \"required\": [\"op\"]}"

describeJson :: String -> String -> String
describeJson process extra =
    "{\"protocol\": 2, \"slug\": \"svc\", \"description\": \"a test service\", \"args\": [], \"process\": "
        <> process
        <> extra
        <> "}"

withTool :: String -> String -> (Bash.ScriptDescription -> IO a) -> IO a
withTool json body k =
    withSystemTempDirectory "bash-tools-service" $ \dir -> do
        let path = dir <> "/svc"
        writeFile path (script json body)
        perms <- getPermissions path
        setPermissions path (setOwnerExecutable True perms)
        r <- Bash.loadScript nullTracer path
        case r of
            Left err -> assertFailure ("failed to load fixture: " <> show err)
            Right desc -> k desc

refused :: String -> Assertion
refused json =
    withSystemTempDirectory "bash-tools-service" $ \dir -> do
        let path = dir <> "/svc"
        writeFile path (script json "true")
        perms <- getPermissions path
        setPermissions path (setOwnerExecutable True perms)
        r <- Bash.loadScript nullTracer path
        assertBool "expected a load error" (isLeft r)

script :: String -> String -> String
script json body =
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
