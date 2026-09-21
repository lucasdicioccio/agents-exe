{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Tests for partial application of tool arguments
(@todos/tool-partial-application.md@, Phase 1): the generic combinator,
schema reduction, argument merging, glob matching, bash arity, and the
bash @env@ calling mode end to end.
-}
module BindingsTests (tests) where

import qualified Data.Aeson as Aeson
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Encoding.Error (lenientDecode)
import Prod.Tracer (Tracer (..))
import System.Directory (getPermissions, setOwnerExecutable, setPermissions)
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import System.Agents.ToolRegistration (ToolRegistration (..))
import qualified System.Agents.ToolRegistration as ToolRegistration
import System.Agents.Tools.Base (mapToolResult)
import System.Agents.Tools.Base (CallResult (..))
import System.Agents.ToolSchema (ParamProperty (..), ParamType (..), ToolDescription (..), ToolName (..))
import qualified System.Agents.Tools.Bash as Bash
import System.Agents.Tools.Bindings
import qualified System.Agents.Tools.Base as ToolBase
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.Agents.Tools.Context (ToolExecutionContext (..), mkMinimalContext)
import qualified System.Agents.Tools.Context as Context
import System.Agents.Tools.Params.Types (ParamValue (..))
import System.Agents.Base (ConversationId (..))
import System.Agents.Session.Types (
    LlmToolCall (..),
    Session (..),
    SessionId (..),
    SystemPrompt (..),
    Turn (..),
    TurnId (..),
    UserToolResponse (..),
    UserTurnContent (..),
 )
import Data.UUID (nil)
import qualified System.Agents.Tools.OpenAPIToolbox as OpenAPIToolbox
import qualified System.Agents.Tools.Secrets as Secrets

tests :: TestTree
tests =
    testGroup
        "Bindings Tests"
        [ globMatchingTests
        , schemaReductionTests
        , argumentMergeTests
        , paramBindingTests
        , exposeBindingTests
        , deriveAgentTableTests
        , specializeProcessParamsTests
        , resolveParamSecretsTests
        , bashArityTests
        , bashEnvModeTests
        ]

-------------------------------------------------------------------------------
-- Glob matching
-------------------------------------------------------------------------------

globMatchingTests :: TestTree
globMatchingTests =
    testGroup
        "glob matching"
        [ testCase "exact match" $ globMatches "bash_export-csv" "bash_export-csv" @?= True
        , testCase "exact mismatch" $ globMatches "bash_export-csv" "bash_export-json" @?= False
        , testCase "trailing star" $ globMatches "bash_export-*" "bash_export-csv" @?= True
        , testCase "trailing star, no match" $ globMatches "bash_query-*" "bash_export-csv" @?= False
        , testCase "leading star" $ globMatches "*-csv" "bash_export-csv" @?= True
        , testCase "star matches empty" $ globMatches "bash_export-*" "bash_export-" @?= True
        , testCase "bare star matches everything" $ globMatches "*" "anything" @?= True
        , testCase "star in the middle" $ globMatches "bash_*-csv" "bash_export-csv" @?= True
        , testCase "star in the middle, no match" $ globMatches "bash_*-csv" "bash_export-json" @?= False
        ]

-------------------------------------------------------------------------------
-- Schema reduction and argument merging, against a synthetic ToolRegistration
-------------------------------------------------------------------------------

nullTracer :: Tracer IO a
nullTracer = Tracer (const (pure ()))

dummyPortal :: Context.ToolPortal
dummyPortal _ _ = error "portal not used in these tests"

testCtx :: ToolExecutionContext
testCtx = mkMinimalContext (SessionId nil) (ConversationId nil) (TurnId nil) dummyPortal

-- | 'testCtx' with the given parameter values set.
ctxWithParams :: [(Text, ParamValue)] -> ToolExecutionContext
ctxWithParams kvs = testCtx{ctxParams = Map.fromList kvs}

-- | A minimal fake registration, standing in for any toolbox kind: applyBindings
-- only looks at 'declareTool', 'innerTool' and 'findTool'.
fakeRegistration :: IO (ToolRegistration, IO Aeson.Value)
fakeRegistration = do
    -- captures the last value seen by the underlying tool's run function
    seen <- newIORef Aeson.Null
    let toolDesc =
            ToolDescription
                { toolDescriptionName = ToolName "query_invoices"
                , toolDescriptionText = "query invoices"
                , toolDescriptionParamProperties =
                    [ ParamProperty "tenant_id" (OpaqueParamType "string") "tenant" True
                    , ParamProperty "token" (OpaqueParamType "string") "token" True
                    , ParamProperty "since" (OpaqueParamType "string") "since date" False
                    ]
                }
        tool =
            ToolBase.Tool
                (ToolBase.MetaTool "fake")
                (\_tracer _ctx val -> writeIORef seen val >> pure (ToolBase.BlobToolSuccess () "ok" Nothing))
        reg =
            ToolRegistration
                { innerTool = tool
                , declareTool = toolDesc
                , findTool = \call -> Just (mapToolResult (const call) tool)
                , toolActivation = Nothing
                }
    pure (reg, readIORef seen)

schemaReductionTests :: TestTree
schemaReductionTests = testCase "bound arguments disappear from the schema" $ do
    (reg, _) <- fakeRegistration
    let bindings =
            [ Binding (Just "query_*") "tenant_id" (Literal (Aeson.String "acme")) Fail
            , Binding Nothing "token" (Literal (Aeson.String "s3cr3t")) Fail
            ]
        reg' = applyBindings bindings reg
        remainingKeys = map propertyKey (toolDescriptionParamProperties (declareTool reg'))
    remainingKeys @?= ["since"]

argumentMergeTests :: TestTree
argumentMergeTests =
    testGroup
        "argument merge"
        [ testCase "bound value is merged in for the underlying tool" $ do
            (reg, getSeen) <- fakeRegistration
            let bindings = [Binding Nothing "tenant_id" (Literal (Aeson.String "acme")) Fail]
                reg' = applyBindings bindings reg
            case findTool reg' fakeCall of
                Nothing -> assertFailure "expected findTool to find the fake tool"
                Just t -> do
                    _ <- ToolBase.toolRun t nullTracer testCtx (Aeson.object ["since" Aeson..= ("2026-01-01" :: Text)])
                    seen <- getSeen
                    seen @?= Aeson.object ["tenant_id" Aeson..= ("acme" :: Text), "since" Aeson..= ("2026-01-01" :: Text)]
        , testCase "bound value wins even if the LLM sends the same key" $ do
            (reg, getSeen) <- fakeRegistration
            let bindings = [Binding Nothing "tenant_id" (Literal (Aeson.String "acme")) Fail]
                reg' = applyBindings bindings reg
            case findTool reg' fakeCall of
                Nothing -> assertFailure "expected findTool to find the fake tool"
                Just t -> do
                    _ <- ToolBase.toolRun t nullTracer testCtx (Aeson.object ["tenant_id" Aeson..= ("attacker-chosen" :: Text)])
                    seen <- getSeen
                    seen @?= Aeson.object ["tenant_id" Aeson..= ("acme" :: Text)]
        , testCase "no matching binding leaves the registration untouched" $ do
            (reg, _) <- fakeRegistration
            let bindings = [Binding (Just "other_tool") "tenant_id" (Literal (Aeson.String "acme")) Fail]
                reg' = applyBindings bindings reg
            map propertyKey (toolDescriptionParamProperties (declareTool reg'))
                @?= map propertyKey (toolDescriptionParamProperties (declareTool reg))
        ]
  where
    fakeCall = error "findTool in this fake ignores its argument"

-------------------------------------------------------------------------------
-- Param bindings, resolved from ctxParams at call time (Phase 2)
-------------------------------------------------------------------------------

paramBindingTests :: TestTree
paramBindingTests =
    testGroup
        "Param bindings"
        [ testCase "a bound Param resolves from ctxParams" $ do
            (reg, getSeen) <- fakeRegistration
            let bindings = [Binding Nothing "tenant_id" (Param "tenant") Fail]
                reg' = applyBindings bindings reg
                ctx = ctxWithParams [("tenant", ParamValue (Aeson.String "acme") False)]
            case findTool reg' fakeCall of
                Nothing -> assertFailure "expected findTool to find the fake tool"
                Just t -> do
                    _ <- ToolBase.toolRun t nullTracer ctx (Aeson.object ["since" Aeson..= ("2026-01-01" :: Text)])
                    seen <- getSeen
                    seen @?= Aeson.object ["tenant_id" Aeson..= ("acme" :: Text), "since" Aeson..= ("2026-01-01" :: Text)]
        , testCase "an unbound required Param fails the call, without reaching the tool" $ do
            (reg, getSeen) <- fakeRegistration
            let bindings = [Binding Nothing "tenant_id" (Param "tenant") Fail]
                reg' = applyBindings bindings reg
            case findTool reg' fakeCall of
                Nothing -> assertFailure "expected findTool to find the fake tool"
                Just t -> do
                    result <- ToolBase.toolRun t nullTracer testCtx (Aeson.object ["since" Aeson..= ("2026-01-01" :: Text)])
                    case result of
                        ToolNotFound _ -> pure ()
                        other -> assertFailure ("expected ToolNotFound, got: " <> show other)
                    seen <- getSeen
                    seen @?= Aeson.Null -- the underlying tool was never called
        , testCase "an unbound Param with whenUnbound=Omit lets the call through without it" $ do
            (reg, getSeen) <- fakeRegistration
            let bindings = [Binding Nothing "tenant_id" (Param "tenant") Omit]
                reg' = applyBindings bindings reg
            case findTool reg' fakeCall of
                Nothing -> assertFailure "expected findTool to find the fake tool"
                Just t -> do
                    _ <- ToolBase.toolRun t nullTracer testCtx (Aeson.object ["since" Aeson..= ("2026-01-01" :: Text)])
                    seen <- getSeen
                    seen @?= Aeson.object ["since" Aeson..= ("2026-01-01" :: Text)]
        ]
  where
    fakeCall = error "findTool in this fake ignores its argument"

-------------------------------------------------------------------------------
-- Expose: the argument reappears in the schema when unbound (Phase 7)
-------------------------------------------------------------------------------

exposeBindingTests :: TestTree
exposeBindingTests =
    testGroup
        "Expose bindings (§7)"
        [ testCase "applyBindings leaves an Expose argument in the static schema" $ do
            (reg, _) <- fakeRegistration
            let bindings = [Binding Nothing "tenant_id" (Param "tenant") Expose]
                reg' = applyBindings bindings reg
                remainingKeys = map propertyKey (toolDescriptionParamProperties (declareTool reg'))
            remainingKeys @?= ["tenant_id", "token", "since"]
        , testCase "narrowExposedSchema hides it once the parameter is bound" $ do
            (reg, _) <- fakeRegistration
            let bindings = [Binding Nothing "tenant_id" (Param "tenant") Expose]
                reg' = narrowExposedSchema (Map.fromList [("tenant", ParamValue (Aeson.String "acme") False)]) bindings reg
                remainingKeys = map propertyKey (toolDescriptionParamProperties (declareTool reg'))
            remainingKeys @?= ["token", "since"]
        , testCase "narrowExposedSchema leaves it visible while unbound" $ do
            (reg, _) <- fakeRegistration
            let bindings = [Binding Nothing "tenant_id" (Param "tenant") Expose]
                reg' = narrowExposedSchema Map.empty bindings reg
                remainingKeys = map propertyKey (toolDescriptionParamProperties (declareTool reg'))
            remainingKeys @?= ["tenant_id", "token", "since"]
        , testCase "bound: the value wins over whatever the model sent" $ do
            (reg, getSeen) <- fakeRegistration
            let bindings = [Binding Nothing "tenant_id" (Param "tenant") Expose]
                reg' = applyBindings bindings reg
                ctx = ctxWithParams [("tenant", ParamValue (Aeson.String "acme") False)]
            case findTool reg' fakeCall of
                Nothing -> assertFailure "expected findTool to find the fake tool"
                Just t -> do
                    _ <- ToolBase.toolRun t nullTracer ctx (Aeson.object ["tenant_id" Aeson..= ("attacker-chosen" :: Text), "since" Aeson..= ("2026-01-01" :: Text)])
                    seen <- getSeen
                    seen @?= Aeson.object ["tenant_id" Aeson..= ("acme" :: Text), "since" Aeson..= ("2026-01-01" :: Text)]
        , testCase "unbound: the model's own value passes through untouched" $ do
            (reg, getSeen) <- fakeRegistration
            let bindings = [Binding Nothing "tenant_id" (Param "tenant") Expose]
                reg' = applyBindings bindings reg
            case findTool reg' fakeCall of
                Nothing -> assertFailure "expected findTool to find the fake tool"
                Just t -> do
                    _ <- ToolBase.toolRun t nullTracer testCtx (Aeson.object ["tenant_id" Aeson..= ("model-chosen" :: Text), "since" Aeson..= ("2026-01-01" :: Text)])
                    seen <- getSeen
                    seen @?= Aeson.object ["tenant_id" Aeson..= ("model-chosen" :: Text), "since" Aeson..= ("2026-01-01" :: Text)]
        , testCase "exposedSecretBindings flags an Expose binding on a secret parameter" $ do
            let bindings =
                    [ Binding Nothing "tenant_id" (Param "tenant") Expose
                    , Binding Nothing "token" (Param "api_token") Expose
                    , Binding Nothing "since" (Literal (Aeson.String "2026-01-01")) Expose
                    ]
                flagged = exposedSecretBindings (Set.fromList ["api_token"]) bindings
            map bindArg flagged @?= ["token"]
        ]
  where
    fakeCall = error "findTool in this fake ignores its argument"

-------------------------------------------------------------------------------
-- Naming a narrowing: deriveAgentTable folds derive_agent calls (§8.4)
-------------------------------------------------------------------------------

-- | A native-shaped 'LlmToolCall' calling @io_derive_agent@ with the given arguments.
deriveCall :: Text -> Text -> [AgentBinding] -> Map.Map Text BindingValue -> LlmToolCall
deriveCall from slug bindings withMap =
    LlmToolCall $
        Aeson.object
            [ "callToolName" Aeson..= ("io_derive_agent" :: Text)
            , "callArgs"
                Aeson..= Aeson.object
                    ( ["from" Aeson..= from, "slug" Aeson..= slug]
                        ++ ["bindings" Aeson..= bindings | not (null bindings)]
                        ++ ["with" Aeson..= withMap | not (Map.null withMap)]
                    )
            ]

-- | The response @derive_agent@'s own tool gives on success.
storedResponse :: Text -> Text -> UserToolResponse
storedResponse from slug =
    TextResponse $
        Text.decodeUtf8With lenientDecode $
            LByteString.toStrict $
                Aeson.encode $
                    Aeson.object ["stored" Aeson..= True, "from" Aeson..= from, "slug" Aeson..= slug]

-- | A minimal session whose one turn carries the given (call, response) pairs.
sessionWith :: [(LlmToolCall, UserToolResponse)] -> Session
sessionWith pairs =
    Session
        [UserTurn (UserTurnContent (SystemPrompt "") [] Nothing pairs) Nothing]
        (SessionId nil)
        Nothing
        (TurnId nil)
        (Just 1)
        Nothing

deriveAgentTableTests :: TestTree
deriveAgentTableTests =
    testGroup
        "deriveAgentTable (§8.4)"
        [ testCase "a successful derive_agent call is recorded, keyed by (from, slug)" $ do
            let sess = sessionWith [(deriveCall "middle" "cached" [] Map.empty, storedResponse "middle" "cached")]
            Map.lookup ("middle", "cached") (deriveAgentTable sess) @?= Just (DerivedNarrowing [] Map.empty)
        , testCase "the bindings and with are carried over" $ do
            let binding = AgentBinding AgentHere Nothing "tenant_id" (Literal (Aeson.String "acme")) Fail
                withMap = Map.fromList [("api_key", Param "my_key")]
                sess = sessionWith [(deriveCall "middle" "cached" [binding] withMap, storedResponse "middle" "cached")]
            Map.lookup ("middle", "cached") (deriveAgentTable sess) @?= Just (DerivedNarrowing [binding] withMap)
        , testCase "a call whose response does not say 'stored: true' is not recorded" $ do
            let sess = sessionWith [(deriveCall "middle" "cached" [] Map.empty, TextResponse "unknown helper 'middle'")]
            Map.member ("middle", "cached") (deriveAgentTable sess) @?= False
        , testCase "a call to a different tool is ignored" $ do
            let otherCall = LlmToolCall $ Aeson.object ["callToolName" Aeson..= ("io_prompt_agent_middle" :: Text), "callArgs" Aeson..= Aeson.object ["what" Aeson..= ("hi" :: Text)]]
                sess = sessionWith [(otherCall, storedResponse "middle" "cached")]
            deriveAgentTable sess @?= Map.empty
        , testCase "a later call for the same key replaces the earlier one" $ do
            let binding = AgentBinding AgentHere Nothing "tenant_id" (Literal (Aeson.String "acme")) Fail
                sess =
                    sessionWith
                        [ (deriveCall "middle" "cached" [] Map.empty, storedResponse "middle" "cached")
                        , (deriveCall "middle" "cached" [binding] Map.empty, storedResponse "middle" "cached")
                        ]
            Map.lookup ("middle", "cached") (deriveAgentTable sess) @?= Just (DerivedNarrowing [binding] Map.empty)
        , testCase "different helpers or names are kept apart" $ do
            let sess =
                    sessionWith
                        [ (deriveCall "middle" "cached" [] Map.empty, storedResponse "middle" "cached")
                        , (deriveCall "middle" "other" [] Map.empty, storedResponse "middle" "other")
                        , (deriveCall "grandchild" "cached" [] Map.empty, storedResponse "grandchild" "cached")
                        ]
            Set.fromList (Map.keys (deriveAgentTable sess))
                @?= Set.fromList [("middle", "cached"), ("middle", "other"), ("grandchild", "cached")]
        ]

-------------------------------------------------------------------------------
-- specializeProcessParams: rewriting a process-scope Param into a Literal
-------------------------------------------------------------------------------

specializeProcessParamsTests :: TestTree
specializeProcessParamsTests =
    testGroup
        "specializeProcessParams"
        [ testCase "a resolved parameter becomes a Literal" $ do
            let resolved = Map.fromList [("tenant", ParamValue (Aeson.String "acme") False)]
                bindings = [Binding Nothing "tenant_id" (Param "tenant") Fail]
            specializeProcessParams resolved bindings
                @?= [Binding Nothing "tenant_id" (Literal (Aeson.String "acme")) Fail]
        , testCase "an unresolved parameter is left as a Param" $ do
            let bindings = [Binding Nothing "tenant_id" (Param "tenant") Fail]
            specializeProcessParams Map.empty bindings @?= bindings
        , testCase "a Literal binding is left untouched" $ do
            let bindings = [Binding Nothing "tenant_id" (Literal (Aeson.String "x")) Fail]
            specializeProcessParams Map.empty bindings @?= bindings
        ]

-------------------------------------------------------------------------------
-- ParamSource secrets, resolved per request (Phase 3)
-------------------------------------------------------------------------------

resolveParamSecretsTests :: TestTree
resolveParamSecretsTests =
    testGroup
        "OpenAPI ParamSource secrets"
        [ testCase "a bound Param resolves to a ResolvedSecret" $ do
            let secret = Secrets.Secret (Secrets.ParamSource "callback_token") (Secrets.Clear False) (Secrets.Header "Authorization" (Just "Bearer {{secret}}"))
                params = Map.fromList [("callback_token", ParamValue (Aeson.String "tok-123") True)]
            OpenAPIToolbox.resolveParamSecrets params [secret]
                @?= Right [Secrets.ResolvedSecret "tok-123" (Secrets.Header "Authorization" (Just "Bearer {{secret}}"))]
        , testCase "an unbound Param is an error, not a silent omission" $ do
            let secret = Secrets.Secret (Secrets.ParamSource "callback_token") (Secrets.Clear False) (Secrets.Header "Authorization" Nothing)
            case OpenAPIToolbox.resolveParamSecrets Map.empty [secret] of
                Left _ -> pure ()
                Right r -> assertFailure ("expected a resolution error, got: " <> show r)
        , testCase "a non-string parameter value is an error" $ do
            let secret = Secrets.Secret (Secrets.ParamSource "callback_token") (Secrets.Clear False) (Secrets.Header "Authorization" Nothing)
                params = Map.fromList [("callback_token", ParamValue (Aeson.Number 42) False)]
            case OpenAPIToolbox.resolveParamSecrets params [secret] of
                Left _ -> pure ()
                Right r -> assertFailure ("expected a resolution error, got: " <> show r)
        ]

-------------------------------------------------------------------------------
-- Bash arity (G5)
-------------------------------------------------------------------------------

bashArityTests :: TestTree
bashArityTests =
    testGroup
        "bash argument arity"
        [ testCase "Single arity is required" $
            propertyRequired (ToolRegistration.mapArg (mkArg Bash.Single Bash.Positional)) @?= True
        , testCase "Optional arity is not required" $
            propertyRequired (ToolRegistration.mapArg (mkArg Bash.Optional Bash.Positional)) @?= False
        ]
  where
    mkArg arity mode =
        Bash.ScriptArg
            { Bash.argName = "foo"
            , Bash.argDescription = "a foo"
            , Bash.argTypeString = "string"
            , Bash.argBackingTypeString = "string"
            , Bash.argTypeArity = arity
            , Bash.argCallingMode = mode
            }

-------------------------------------------------------------------------------
-- Bash env calling mode (G6), end to end with a fixture script
-------------------------------------------------------------------------------

bashEnvModeTests :: TestTree
bashEnvModeTests = testCase "env-mode argument reaches the script as an environment variable, not argv" $ do
    withSystemTempDirectory "bindings-env-test" $ \dir -> do
        let scriptPath = dir <> "/echo-token.sh"
        writeFile scriptPath echoTokenScript
        perms <- getPermissions scriptPath
        setPermissions scriptPath (setOwnerExecutable True perms)
        result <- Bash.loadScript nullTracer scriptPath
        case result of
            Left err -> assertFailure ("failed to load fixture script: " <> show err)
            Right desc -> do
                out <-
                    Bash.runValue
                        nullTracer
                        desc
                        Nothing
                        (Aeson.object ["token" Aeson..= ("s3cr3t-value" :: Text)])
                case out of
                    Left err -> assertFailure ("script failed: " <> show err)
                    Right bytes -> do
                        let outText = Text.decodeUtf8With lenientDecode bytes
                        assertBool
                            ("expected the token via env, got: " <> Text.unpack outText)
                            ("seen: s3cr3t-value" `Text.isInfixOf` outText)
  where
    echoTokenScript =
        unlines
            [ "#!/usr/bin/env bash"
            , "if [[ \"$1\" == \"describe\" ]]; then"
            , "  cat << 'EOF'"
            , "{\"slug\": \"echo-token\", \"description\": \"echoes TOKEN env var\", \"args\": ["
            , "  {\"name\": \"token\", \"description\": \"a token\", \"type\": \"string\", \"backing_type\": \"string\", \"arity\": \"single\", \"mode\": \"env\"}"
            , "]}"
            , "EOF"
            , "  exit 0"
            , "fi"
            , "if [[ \"$1\" == \"run\" ]]; then"
            , "  if [[ -n \"$2\" ]]; then"
            , "    echo \"FAIL: token leaked into argv: $2\""
            , "    exit 1"
            , "  fi"
            , "  if [[ -z \"$TOKEN\" ]]; then"
            , "    echo \"FAIL: TOKEN env var not set\""
            , "    exit 1"
            , "  fi"
            , "  echo \"seen: $TOKEN\""
            , "  exit 0"
            , "fi"
            ]
