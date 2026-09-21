{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Tests for narrowing helpers down the call chain
(@todos/tool-partial-application.md@, §8, Phase 6): the 'AgentAddress'
algebra, and @describe_agent@ end to end against a loaded three-level
agent tree (root -> middle -> grandchild).
-}
module NarrowingTests (tests) where

import Control.Concurrent.STM (readTVarIO)
import qualified Data.Aeson as Aeson
import Data.Aeson.Key (fromText)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Prod.Tracer (Tracer (..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Vector as Vector

import System.Agents.AgentFactory (defaultAgentDeps)
import System.Agents.AgentTree (
    LoadAgentResult (..),
    OSAgentNode (..),
    OSAgentTree (..),
    Props (..),
    withAgentTree,
 )
import qualified System.Agents.AgentTree.OneShotTool as OneShotTool
import System.Agents.Base (ConversationId (..))
import System.Agents.Session.Types (SessionId (..), TurnId (..))
import System.Agents.SessionStore (SessionCatalog (..))
import System.Agents.ToolRegistration (ToolRegistration (..))
import System.Agents.ToolSchema (ToolDescription (..), ToolName (..))
import System.Agents.Tools.Base (CallResult (..), toolRun)
import System.Agents.Tools.Bindings.Types (
    AgentAddress (..),
    ScopedBinding (..),
    descendAddress,
    parseAgentAddress,
    reRootBindings,
 )
import System.Agents.Tools.Context (mkMinimalContext)
import Data.UUID (nil)

tests :: TestTree
tests =
    testGroup
        "Narrowing helpers (Phase 6)"
        [ addressAlgebraTests
        , describeAgentTests
        , deriveAgentTests
        ]

-------------------------------------------------------------------------------
-- AgentAddress algebra
-------------------------------------------------------------------------------

addressAlgebraTests :: TestTree
addressAlgebraTests =
    testGroup
        "AgentAddress"
        [ testCase "absent 'agent' key parses as Here" $ parseAgentAddress Nothing @?= AgentHere
        , testCase "'**' parses as Everywhere" $ parseAgentAddress (Just "**") @?= AgentEverywhere
        , testCase "a slug parses as a one-element path" $ parseAgentAddress (Just "diff-reviewer") @?= AgentPath ["diff-reviewer"]
        , testCase "a slash-separated path parses as a multi-element path" $ parseAgentAddress (Just "a/b") @?= AgentPath ["a", "b"]
        , testCase "Here never descends" $ descendAddress "child" AgentHere @?= Nothing
        , testCase "a one-element path addressed at that child becomes Here" $
            descendAddress "child" (AgentPath ["child"]) @?= Just AgentHere
        , testCase "a one-element path addressed at another child does not apply" $
            descendAddress "other" (AgentPath ["child"]) @?= Nothing
        , testCase "a multi-element path drops its first element" $
            descendAddress "child" (AgentPath ["child", "grandchild"]) @?= Just (AgentPath ["grandchild"])
        , testCase "Everywhere always applies, and stays Everywhere" $
            descendAddress "anything" AgentEverywhere @?= Just AgentEverywhere
        , testCase "reRootBindings drops Here-addressed bindings and descends the rest" $
            let sbs =
                    [ ScopedBinding AgentHere Nothing "a" Aeson.Null False
                    , ScopedBinding (AgentPath ["child"]) Nothing "b" Aeson.Null False
                    , ScopedBinding (AgentPath ["other"]) Nothing "c" Aeson.Null False
                    , ScopedBinding AgentEverywhere Nothing "d" Aeson.Null False
                    ]
                rooted = reRootBindings "child" sbs
             in map sbArg rooted @?= ["b", "d"]
        ]

-------------------------------------------------------------------------------
-- describe_agent, end to end against a loaded tree
-------------------------------------------------------------------------------

nullTracer :: Tracer IO a
nullTracer = Tracer (const (pure ()))

emptyCatalog :: SessionCatalog
emptyCatalog = SessionCatalog{catList = pure [], catRead = const (pure Nothing)}

-- | Writes a three-level fixture (root -> middle -> grandchild) and returns the root's file path.
writeFixture :: FilePath -> Bool -> IO FilePath
writeFixture dir middleNarrowable = do
    createDirectoryIfMissing True dir
    writeFile (dir </> "grandchild.json") grandchildJson
    writeFile (dir </> "middle.json") middleJson
    writeFile (dir </> "root.json") (rootJson middleNarrowable)
    pure (dir </> "root.json")
  where
    grandchildJson =
        "{\"tag\": \"OpenAIAgentDescription\", \"contents\": {"
            <> "\"slug\": \"grandchild\", \"apiKeyId\": \"none\", \"flavor\": \"openai\","
            <> "\"modelUrl\": \"http://localhost:9\", \"modelName\": \"test-model\","
            <> "\"announce\": \"grandchild agent\", \"systemPrompt\": [\"grandchild\"],"
            <> "\"parameters\": [{\"name\": \"gh_token\", \"scope\": \"session\", \"required\": true, \"secret\": true, \"description\": \"a github token\"}]"
            <> "}}"
    middleJson =
        "{\"tag\": \"OpenAIAgentDescription\", \"contents\": {"
            <> "\"slug\": \"middle\", \"apiKeyId\": \"none\", \"flavor\": \"openai\","
            <> "\"modelUrl\": \"http://localhost:9\", \"modelName\": \"test-model\","
            <> "\"announce\": \"middle agent\", \"systemPrompt\": [\"middle\"],"
            <> "\"extraAgents\": [{\"slug\": \"grandchild\", \"path\": \"./grandchild.json\"}]"
            <> "}}"
    rootJson narrowable =
        "{\"tag\": \"OpenAIAgentDescription\", \"contents\": {"
            <> "\"slug\": \"root\", \"apiKeyId\": \"none\", \"flavor\": \"openai\","
            <> "\"modelUrl\": \"http://localhost:9\", \"modelName\": \"test-model\","
            <> "\"announce\": \"root agent\", \"systemPrompt\": [\"root\"],"
            <> "\"extraAgents\": [{\"slug\": \"middle\", \"path\": \"./middle.json\""
            <> (if narrowable then "" else ", \"narrowable\": false")
            <> "}]"
            <> "}}"

-- | Loads the fixture tree and hands the caller the root's registered tools.
withRootTools :: Bool -> ([ToolRegistration] -> IO a) -> IO a
withRootTools middleNarrowable action =
    withSystemTempDirectory "narrowing-tree" $ \dir -> do
        rootFile <- writeFixture dir middleNarrowable
        let deps = defaultAgentDeps []
            props =
                Props
                    { apiKeys = []
                    , apiKeysFile = "/nonexistent-api-keys-file.json"
                    , rootAgentFile = rootFile
                    , interactiveTracer = nullTracer
                    , agentToTool = OneShotTool.turnAgentRuntimeIntoIOTool nullTracer deps
                    , sessionCatalog = emptyCatalog
                    , processParams = Map.empty
                    }
        withAgentTree props $ \case
            Errors errs -> assertFailure ("expected the tree to load, got: " <> show errs)
            Initialized tree -> readTVarIO tree.osTreeRoot.osNodeTools >>= action

findToolNamed :: Text -> [ToolRegistration] -> Maybe ToolRegistration
findToolNamed name regs = find (\r -> toolNameOf r == name) regs
  where
    toolNameOf r = let ToolName n = toolDescriptionName (declareTool r) in n

callTool :: ToolRegistration -> Aeson.Value -> IO Aeson.Value
callTool reg args = do
    result <- toolRun (innerTool reg) nullTracer testCtx args
    case result of
        BlobToolSuccess _ bs _ -> case Aeson.eitherDecodeStrict bs of
            Right v -> pure v
            Left err -> assertFailure ("describe_agent did not return JSON: " <> err <> " (" <> show bs <> ")")
        other -> assertFailure ("expected a successful call, got: " <> show other)
  where
    testCtx = mkMinimalContext (SessionId nil) (ConversationId nil) (TurnId nil) (\_ _ -> error "portal not used")

field :: Text -> Aeson.Value -> Aeson.Value
field k (Aeson.Object o) = maybe (error ("missing field " <> Text.unpack k)) id (KeyMap.lookup (fromText k) o)
field _ v = error ("expected an object, got: " <> show v)

hasField :: Text -> Aeson.Value -> Bool
hasField k (Aeson.Object o) = KeyMap.member (fromText k) o
hasField _ _ = False

describeAgentTests :: TestTree
describeAgentTests =
    testGroup
        "describe_agent"
        [ testCase "root gets prompt_agent_middle and describe_agent" $
            withRootTools True $ \tools -> do
                assertBool "prompt_agent_middle is registered" (Just () == (const () <$> findToolNamed "io_prompt_agent_middle" tools))
                assertBool "describe_agent is registered" (Just () == (const () <$> findToolNamed "io_describe_agent" tools))
        , testCase "describe_agent on an unknown slug tells the model" $
            withRootTools True $ \tools ->
                case findToolNamed "io_describe_agent" tools of
                    Nothing -> assertFailure "describe_agent not registered"
                    Just reg -> do
                        result <- toolRun (innerTool reg) nullTracer ctx0 (Aeson.object ["slug" Aeson..= ("nope" :: Text)])
                        case result of
                            BlobToolSuccess _ bs _ -> assertBool "mentions the unknown slug" ("nope" `Text.isInfixOf` Text.pack (show bs))
                            other -> assertFailure ("expected a successful call, got: " <> show other)
        , testCase "describe_agent on a narrowable helper shows its open parameter, recursively" $
            withRootTools True $ \tools ->
                case findToolNamed "io_describe_agent" tools of
                    Nothing -> assertFailure "describe_agent not registered"
                    Just reg -> do
                        json <- callTool reg (Aeson.object ["slug" Aeson..= ("middle" :: Text)])
                        field "slug" json @?= Aeson.String "middle"
                        case field "helpers" json of
                            Aeson.Array helpers -> case Vector.toList helpers of
                                [grandchild] -> do
                                    field "slug" grandchild @?= Aeson.String "grandchild"
                                    case field "parameters" grandchild of
                                        Aeson.Array ps -> case Vector.toList ps of
                                            [p] -> field "bound" p @?= Aeson.Bool False
                                            other -> assertFailure ("expected one parameter, got: " <> show other)
                                        other -> assertFailure ("expected an array of parameters, got: " <> show other)
                                other -> assertFailure ("expected one helper (grandchild), got: " <> show other)
                            other -> assertFailure ("expected an array of helpers, got: " <> show other)
        , testCase "describe_agent on a non-narrowable helper shows only its announce" $
            withRootTools False $ \tools ->
                case findToolNamed "io_describe_agent" tools of
                    Nothing -> assertFailure "describe_agent not registered"
                    Just reg -> do
                        json <- callTool reg (Aeson.object ["slug" Aeson..= ("middle" :: Text)])
                        field "slug" json @?= Aeson.String "middle"
                        field "narrowable" json @?= Aeson.Bool False
                        assertBool "no 'helpers' key when refused" (not (hasField "helpers" json))
        ]
  where
    ctx0 = mkMinimalContext (SessionId nil) (ConversationId nil) (TurnId nil) (\_ _ -> error "portal not used")

-------------------------------------------------------------------------------
-- derive_agent, end to end against a loaded tree (§8.4)
-------------------------------------------------------------------------------

deriveAgentTests :: TestTree
deriveAgentTests =
    testGroup
        "derive_agent"
        [ testCase "root gets derive_agent alongside describe_agent" $
            withRootTools True $ \tools ->
                assertBool "derive_agent is registered" (Just () == (const () <$> findToolNamed "io_derive_agent" tools))
        , testCase "deriving a narrowing for a known, narrowable helper stores it" $
            withRootTools True $ \tools ->
                case findToolNamed "io_derive_agent" tools of
                    Nothing -> assertFailure "derive_agent not registered"
                    Just reg -> do
                        json <- callTool reg (Aeson.object ["from" Aeson..= ("middle" :: Text), "slug" Aeson..= ("cached" :: Text)])
                        field "stored" json @?= Aeson.Bool True
                        field "from" json @?= Aeson.String "middle"
                        field "slug" json @?= Aeson.String "cached"
        , testCase "deriving against an unknown helper tells the model" $
            withRootTools True $ \tools ->
                case findToolNamed "io_derive_agent" tools of
                    Nothing -> assertFailure "derive_agent not registered"
                    Just reg -> do
                        result <- toolRun (innerTool reg) nullTracer ctx0 (Aeson.object ["from" Aeson..= ("nope" :: Text), "slug" Aeson..= ("cached" :: Text)])
                        case result of
                            BlobToolSuccess _ bs _ -> assertBool "mentions the unknown helper" ("nope" `Text.isInfixOf` Text.pack (show bs))
                            other -> assertFailure ("expected a successful call, got: " <> show other)
        , testCase "deriving against a non-narrowable helper is refused" $
            withRootTools False $ \tools ->
                case findToolNamed "io_derive_agent" tools of
                    Nothing -> assertFailure "derive_agent not registered"
                    Just reg -> do
                        result <- toolRun (innerTool reg) nullTracer ctx0 (Aeson.object ["from" Aeson..= ("middle" :: Text), "slug" Aeson..= ("cached" :: Text)])
                        case result of
                            BlobToolSuccess _ bs _ -> assertBool "says not narrowable" ("not narrowable" `Text.isInfixOf` Text.pack (show bs))
                            other -> assertFailure ("expected a successful call, got: " <> show other)
        , testCase "prompt_agent_<slug> with an unknown 'as' tells the model to derive it first" $
            withRootTools True $ \tools ->
                case findToolNamed "io_prompt_agent_middle" tools of
                    Nothing -> assertFailure "prompt_agent_middle not registered"
                    Just reg -> do
                        result <- toolRun (innerTool reg) nullTracer ctx0 (Aeson.object ["what" Aeson..= ("hi" :: Text), "as" Aeson..= ("nope" :: Text)])
                        case result of
                            BlobToolSuccess _ bs _ -> do
                                let msg = Text.pack (show bs)
                                assertBool "mentions the unknown narrowing" ("nope" `Text.isInfixOf` msg)
                                assertBool "points at derive_agent" ("derive_agent" `Text.isInfixOf` msg)
                            other -> assertFailure ("expected a successful call, got: " <> show other)
        , testCase "prompt_agent_<slug> refuses 'as' when the helper is not narrowable" $
            withRootTools False $ \tools ->
                case findToolNamed "io_prompt_agent_middle" tools of
                    Nothing -> assertFailure "prompt_agent_middle not registered"
                    Just reg -> do
                        result <- toolRun (innerTool reg) nullTracer ctx0 (Aeson.object ["what" Aeson..= ("hi" :: Text), "as" Aeson..= ("cached" :: Text)])
                        case result of
                            BlobToolSuccess _ bs _ -> assertBool "says not narrowable" ("not narrowable" `Text.isInfixOf` Text.pack (show bs))
                            other -> assertFailure ("expected a successful call, got: " <> show other)
        ]
  where
    ctx0 = mkMinimalContext (SessionId nil) (ConversationId nil) (TurnId nil) (\_ _ -> error "portal not used")
