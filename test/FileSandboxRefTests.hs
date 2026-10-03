{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Named file sandboxes: an agent declares sandboxes once in
@fileSandboxes@ and its builtin toolboxes refer to them with
@{"ref": name}@; the inline form keeps parsing.
-}
module FileSandboxRefTests (tests) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import System.Agents.Base (
    Agent (..),
    AgentDescription (..),
    BuiltinToolboxDescription (..),
    DeveloperToolboxDescription (..),
    FileSandboxConfig (..),
    FileSandboxSpec (..),
    LuaToolboxDescription (..),
    SystemToolboxDescription (..),
    defaultDeveloperFileSandbox,
    effectiveFileSandbox,
    resolveBuiltinToolboxSandboxes,
 )
import System.Agents.FileSandbox.Predicate (PathPredicate (..))
import System.Agents.Tools.DeveloperToolbox.Validate (validateAgentStructure)

tests :: TestTree
tests =
    testGroup
        "named file sandboxes"
        [ testCase "a sandbox reference parses and round-trips" $ do
            decodeSpec "{\"ref\": \"project\"}" >>= (@?= NamedFileSandbox "project")
            Aeson.decode (Aeson.encode (NamedFileSandbox "project")) @?= Just (NamedFileSandbox "project")
        , testCase "the inline form still parses and round-trips" $ do
            spec <- decodeSpec "{\"fsbPredicate\": {\"tag\": \"DirectoryRecursive\", \"contents\": \"/srv\"}, \"fsbName\": \"code\"}"
            let expected =
                    InlineFileSandbox
                        FileSandboxConfig
                            { fsbPredicate = DirectoryRecursive "/srv"
                            , fsbMaxFileSize = Nothing
                            , fsbName = Just "code"
                            }
            spec @?= expected
            Aeson.decode (Aeson.encode expected) @?= Just expected
        , testCase "a reference mixed with an inline definition is refused" $
            case Aeson.eitherDecode "{\"ref\": \"project\", \"fsbPredicate\": {\"tag\": \"AlwaysAllow\"}}" of
                Left _ -> pure ()
                Right (spec :: FileSandboxSpec) -> assertFailure ("parsed: " <> show spec)
        , testCase "an agent file without fileSandboxes parses as before" $ do
            agent <- decodeAgent (agentJson "" inlineToolbox)
            agent.fileSandboxes @?= Nothing
            case resolveBuiltinToolboxSandboxes agent of
                Right [DeveloperToolbox dev] ->
                    dev.developerToolboxFileSandbox @?= Just (InlineFileSandbox projectSandbox{fsbName = Nothing})
                other -> assertFailure ("unexpected: " <> show other)
        , testCase "toolboxes of an agent share one named sandbox" $ do
            agent <- decodeAgent (agentJson declaration (Text.intercalate "," [refToolbox "DeveloperToolbox" devFields, refToolbox "LuaToolbox" luaFields, refToolbox "SystemToolbox" sysFields]))
            agent.fileSandboxes @?= Just (Map.singleton "project" projectSandbox{fsbName = Nothing})
            case resolveBuiltinToolboxSandboxes agent of
                Right [DeveloperToolbox dev, LuaToolbox lua, SystemToolbox sys] -> do
                    -- the resolved sandbox is named after the reference
                    let expected = Just (InlineFileSandbox projectSandbox)
                    dev.developerToolboxFileSandbox @?= expected
                    lua.luaToolboxFileSandbox @?= expected
                    sys.systemToolboxFileSandbox @?= expected
                other -> assertFailure ("unexpected: " <> show other)
        , testCase "a name set in the definition is kept" $ do
            let agentWith name =
                    decodeAgent
                        ( agentJson
                            ("\"fileSandboxes\": {\"project\": {\"fsbPredicate\": {\"tag\": \"AlwaysAllow\"}, \"fsbName\": \"" <> name <> "\"}},")
                            (refToolbox "DeveloperToolbox" devFields)
                        )
            agent <- agentWith "the-project"
            case resolveBuiltinToolboxSandboxes agent of
                Right [DeveloperToolbox dev] ->
                    fmap fsbNameOf dev.developerToolboxFileSandbox @?= Just (Just "the-project")
                other -> assertFailure ("unexpected: " <> show other)
        , testCase "an undeclared name is an error naming the toolbox and the sandbox" $ do
            agent <- decodeAgent (agentJson "" (refToolbox "DeveloperToolbox" devFields))
            case resolveBuiltinToolboxSandboxes agent of
                Left [err] -> do
                    assertBool (Text.unpack err) ("'dev'" `Text.isInfixOf` err)
                    assertBool (Text.unpack err) ("'project'" `Text.isInfixOf` err)
                other -> assertFailure ("unexpected: " <> show other)
            let (errors, _) = validateAgentStructure agent
            assertBool (show errors) (any ("project" `Text.isInfixOf`) errors)
        , testCase "validation accepts a declared reference" $ do
            agent <- decodeAgent (agentJson declaration (refToolbox "DeveloperToolbox" devFields))
            fst (validateAgentStructure agent) @?= []
        , testCase "a reference left unresolved denies everything" $ do
            let cfg = effectiveFileSandbox defaultDeveloperFileSandbox (Just (NamedFileSandbox "project"))
            cfg.fsbPredicate @?= AlwaysDeny
            effectiveFileSandbox defaultDeveloperFileSandbox Nothing @?= defaultDeveloperFileSandbox
            effectiveFileSandbox defaultDeveloperFileSandbox (Just (InlineFileSandbox projectSandbox)) @?= projectSandbox
        ]

fsbNameOf :: FileSandboxSpec -> Maybe Text
fsbNameOf (InlineFileSandbox cfg) = cfg.fsbName
fsbNameOf (NamedFileSandbox _) = Nothing

projectSandbox :: FileSandboxConfig
projectSandbox =
    FileSandboxConfig
        { fsbPredicate = DirectoryRecursive "/srv/app"
        , fsbMaxFileSize = Nothing
        , fsbName = Just "project"
        }

declaration :: Text
declaration =
    "\"fileSandboxes\": {\"project\": {\"fsbPredicate\": {\"tag\": \"DirectoryRecursive\", \"contents\": \"/srv/app\"}}},"

devFields, luaFields, sysFields :: Text
devFields = "\"Name\": \"dev\", \"Description\": \"d\", \"Capabilities\": [\"read-file-range\"]"
luaFields = "\"Name\": \"lua\", \"Description\": \"d\", \"MaxMemoryMB\": 64, \"MaxExecutionTimeSeconds\": 10, \"AllowedTools\": [], \"AllowedHosts\": []"
sysFields = "\"Name\": \"sys\", \"Description\": \"d\", \"Capabilities\": [\"attach-file\"]"

refToolbox :: Text -> Text -> Text
refToolbox tag fields =
    "{\"tag\": \"" <> tag <> "\", \"contents\": {" <> fields <> ", \"FileSandbox\": {\"ref\": \"project\"}}}"

inlineToolbox :: Text
inlineToolbox =
    "{\"tag\": \"DeveloperToolbox\", \"contents\": {"
        <> devFields
        <> ", \"FileSandbox\": {\"fsbPredicate\": {\"tag\": \"DirectoryRecursive\", \"contents\": \"/srv/app\"}}}}"

-- | An agent file: optional extra top-level fields, then its builtin toolboxes.
agentJson :: Text -> Text -> Text
agentJson extra toolboxes =
    "{\"tag\": \"OpenAIAgentDescription\", \"contents\": {"
        <> "\"slug\": \"a\", \"apiKeyId\": \"k\", \"flavor\": \"OpenAIv1\", \"modelUrl\": \"u\", \"modelName\": \"m\","
        <> "\"announce\": \"an agent\", \"systemPrompt\": [\"hello\"],"
        <> extra
        <> "\"builtinToolboxes\": ["
        <> toolboxes
        <> "]}}"

decodeAgent :: Text -> IO Agent
decodeAgent src =
    case Aeson.eitherDecode (LBS.pack (Text.unpack src)) of
        Left err -> assertFailure (err <> " in " <> Text.unpack src)
        Right (AgentDescription agent) -> pure agent

decodeSpec :: LBS.ByteString -> IO FileSandboxSpec
decodeSpec src = either assertFailure pure (Aeson.eitherDecode src)
