{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The @env@ of an MCP server: parsing, resolution against parameters, and keeping values out of traces.
module McpEnvTests (tests) where

import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import Data.List (isInfixOf)
import qualified Data.Text as Text
import qualified System.Process as Process
import Test.Tasty
import Test.Tasty.HUnit

import System.Agents.AgentTree.ToolLoader (mergeMcpEnv, resolveMcpEnv)
import System.Agents.Base (McpServerDescription (..), McpSimpleBinaryConfiguration (..))
import System.Agents.MCP.Client.Runtime (redactEnv)
import System.Agents.Tools.Bindings.Types (BindingValue (..))
import System.Agents.Tools.Params.Types (ParamValue (..))

tests :: TestTree
tests =
    testGroup
        "MCP server env"
        [ testCase "the config parses env bindings, and omits env when absent" $ do
            let json =
                    "{\"tag\":\"McpSimpleBinary\",\"contents\":{\"name\":\"s\",\"executable\":\"/bin/s\",\"args\":[],\"env\":{\"TOKEN\":{\"tag\":\"Param\",\"contents\":\"tok\"},\"MODE\":{\"tag\":\"Literal\",\"contents\":\"ro\"}}}}"
            case Aeson.eitherDecode json of
                Left err -> assertFailure err
                Right (McpSimpleBinary cfg) -> do
                    cfg.env @?= Just (Map.fromList [("TOKEN", Param "tok"), ("MODE", Literal (Aeson.String "ro"))])
                    -- round-trips
                    Aeson.decode (Aeson.encode (McpSimpleBinary cfg)) @?= Just (McpSimpleBinary cfg)
            case Aeson.eitherDecode "{\"tag\":\"McpSimpleBinary\",\"contents\":{\"name\":\"s\",\"executable\":\"/bin/s\",\"args\":[]}}" of
                Left err -> assertFailure err
                Right (McpSimpleBinary cfg) -> cfg.env @?= Nothing
        , testCase "literals and process parameters resolve; non-strings are encoded as JSON" $ do
            let params = Map.fromList [("tok", ParamValue (Aeson.String "s3cret") True), ("n", ParamValue (Aeson.Number 3) False)]
                env = Map.fromList [("A", Literal (Aeson.String "x")), ("B", Param "tok"), ("C", Param "n")]
            resolveMcpEnv params env @?= Right [("A", "x"), ("B", "s3cret"), ("C", "3")]
        , testCase "a parameter with no load-time value is an error that names it but carries no value" $ do
            let params = Map.fromList [("other", ParamValue (Aeson.String "s3cret") True)]
            case resolveMcpEnv params (Map.fromList [("B", Param "session_tok")]) of
                Right _ -> assertFailure "expected an error"
                Left msg -> do
                    assertBool "names the parameter" ("session_tok" `Text.isInfixOf` msg)
                    assertBool "explains why" ("process-scope" `Text.isInfixOf` msg)
                    assertBool "never carries a value" (not ("s3cret" `Text.isInfixOf` msg))
        , testCase "configured variables win over the inherited environment" $
            mergeMcpEnv [("A", "new"), ("C", "c")] [("A", "old"), ("B", "b")] @?= [("A", "new"), ("B", "b"), ("C", "c")]
        , testCase "the traced process description does not show env values" $ do
            let p = (Process.proc "/bin/s" []){Process.env = Just [("TOKEN", "s3cret")]}
            let shown = show (redactEnv p)
            assertBool "keeps the variable name" ("TOKEN" `isInfixOf` shown)
            assertBool "hides the value" (not ("s3cret" `isInfixOf` shown))
        ]
