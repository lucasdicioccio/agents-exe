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
import System.Agents.ToolSchema (ParamProperty (..), ParamType (..), ToolDescription (..), ToolName (..))
import qualified System.Agents.Tools.Bash as Bash
import System.Agents.Tools.Bindings
import qualified System.Agents.Tools.Base as ToolBase

tests :: TestTree
tests =
    testGroup
        "Bindings Tests"
        [ globMatchingTests
        , schemaReductionTests
        , argumentMergeTests
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
                    _ <- ToolBase.toolRun t nullTracer undefined (Aeson.object ["since" Aeson..= ("2026-01-01" :: Text)])
                    seen <- getSeen
                    seen @?= Aeson.object ["tenant_id" Aeson..= ("acme" :: Text), "since" Aeson..= ("2026-01-01" :: Text)]
        , testCase "bound value wins even if the LLM sends the same key" $ do
            (reg, getSeen) <- fakeRegistration
            let bindings = [Binding Nothing "tenant_id" (Literal (Aeson.String "acme")) Fail]
                reg' = applyBindings bindings reg
            case findTool reg' fakeCall of
                Nothing -> assertFailure "expected findTool to find the fake tool"
                Just t -> do
                    _ <- ToolBase.toolRun t nullTracer undefined (Aeson.object ["tenant_id" Aeson..= ("attacker-chosen" :: Text)])
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
