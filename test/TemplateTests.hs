{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Tests for agent files written as tramaj programs
("System.Agents.FileLoader.Template"): evaluation against process
parameters, the declared-parameter check, the standard library's shapes and
library directories.
-}
module TemplateTests (tests) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Data.ByteString.Lazy as LByteString
import Data.List (isInfixOf, sort)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import Prod.Tracer (silent)
import System.Directory (createDirectoryIfMissing, withCurrentDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import System.Agents.Base (
    Agent (..),
    AgentDescription (..),
    BashToolboxDescription,
    BuiltinToolboxDescription (..),
    DeveloperToolboxDescription (..),
    ExtraAgentRef (..),
    FileSandboxConfig (..),
    LuaToolboxDescription (..),
    McpServerDescription,
 )
import qualified System.Agents.CLI.ConfigLoader as ConfigLoader
import qualified System.Agents.FileLoader as FileLoader
import System.Agents.FileLoader.Template
import System.Agents.FileSandbox.Predicate (PathPredicate (..))
import System.Agents.Tools.Params.Types (ProcessParams, ProcessValue (..))

tests :: TestTree
tests =
    testGroup
        "Template"
        [ agentTemplateTests
        , parameterTests
        , standardLibraryTests
        , libraryDirectoryTests
        , fileLoaderTests
        ]

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

params :: [(Text, Aeson.Value)] -> ProcessParams
params kvs = Map.fromList [(k, ProcessValue v False) | (k, v) <- kvs]

envWith :: [(Text, Aeson.Value)] -> TemplateEnv
envWith kvs = defaultTemplateEnv{teParams = params kvs}

-- | An agent template whose only varying parts are the given lines.
agentSource :: [Text] -> Text -> Text
agentSource bindings fields =
    Text.unlines $
        ["@a=import(\"agents\", {}).vals"]
            <> bindings
            <> [ "$a.agent({"
               , "  slug: \"templated\","
               , "  apiKeyId: \"main-key\","
               , "  flavor: \"OpenAIv1\","
               , "  modelUrl: \"https://api.openai.com/v1\","
               , "  announce: \"a templated agent\","
               , fields
               , "})"
               ]

workspaceAgent :: Text
workspaceAgent =
    agentSource
        [ "@code=$a.sandbox({name: \"code\", allow: $a.under($ctx.workspace, [\"src\", \"test\"]), deny: [$a.pattern(\"*.key\")]})"
        ]
        ( Text.unlines
            [ "  modelName: $ctx.model,"
            , "  systemPrompt: [\"You work in `$ctx.workspace`.\"],"
            , "  parameters: [{name: \"workspace\"}, {name: \"model\"}],"
            , "  builtinToolboxes: ["
            , "    $a.developer-toolbox({name: \"dev\", capabilities: [\"read-file-range\"], sandbox: $code}),"
            , "    $a.lua-toolbox({name: \"lua\", sandbox: $code})"
            , "  ]"
            ]
        )

plainAgent :: Text -> Text
plainAgent extra =
    agentSource [] (Text.unlines ["  modelName: \"gpt-4o\",", "  systemPrompt: [\"hello\"]" <> extra])

expectError :: (TemplateError -> Bool) -> Either TemplateError a -> Assertion
expectError ok result = case result of
    Left e
        | ok e -> pure ()
        | otherwise -> assertFailure ("unexpected error: " <> renderTemplateError e)
    Right _ -> assertFailure "expected an error, the template evaluated"

-- | Evaluate an expression against the standard library's vals, bound to @$a@.
evalWithLibrary :: (Aeson.FromJSON b) => Text -> IO b
evalWithLibrary expr =
    case evalTemplate defaultTemplateEnv ("@a=import(\"agents\", {}).vals\n" <> expr) of
        Left e -> assertFailure (renderTemplateError e)
        Right (_, value) -> case Aeson.Types.parseEither Aeson.parseJSON value of
            Left err -> assertFailure (err <> " in " <> show value)
            Right v -> pure v

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

agentTemplateTests :: TestTree
agentTemplateTests =
    testGroup
        "agent templates"
        [ testCase "evaluates to an agent, sharing one sandbox across toolboxes" $ do
            let env = envWith [("workspace", "/srv/app"), ("model", "gpt-4o")]
            case evalAgentTemplate env workspaceAgent of
                Left e -> assertFailure (renderTemplateError e)
                Right (_, AgentDescription agent) -> do
                    agent.slug @?= "templated"
                    agent.modelName @?= "gpt-4o"
                    agent.systemPrompt @?= ["You work in /srv/app."]
                    let expected =
                            FileSandboxConfig
                                { fsbPredicate =
                                    And
                                        (Any [DirectoryRecursive "/srv/app/src", DirectoryRecursive "/srv/app/test"])
                                        (Not (Any [FilePattern "*.key"]))
                                , fsbMaxFileSize = Nothing
                                , fsbName = Just "code"
                                }
                    case agent.builtinToolboxes of
                        Just [DeveloperToolbox dev, LuaToolbox lua] -> do
                            dev.developerToolboxFileSandbox @?= Just expected
                            lua.luaToolboxFileSandbox @?= Just expected
                        other -> assertFailure ("unexpected toolboxes: " <> show other)
        , testCase "the evaluated value is the JSON an agent file holds" $ do
            case evalAgentTemplate defaultTemplateEnv (plainAgent "") of
                Left e -> assertFailure (renderTemplateError e)
                Right (value, desc) -> Aeson.Types.parseEither Aeson.parseJSON value @?= Right desc
        , testCase "a parse error is reported" $
            expectError isParseError (evalAgentTemplate defaultTemplateEnv "@a=(")
        , testCase "a document root is refused" $
            expectError (== TemplateNotAValue) (evalAgentTemplate defaultTemplateEnv ".div(\"x\")")
        , testCase "a value that is not an agent is refused" $
            expectError isNotAnAgent (evalAgentTemplate defaultTemplateEnv "{slug: \"x\"}")
        , testCase "an unknown library is an evaluation error" $
            expectError isEvalError (evalAgentTemplate defaultTemplateEnv "import(\"nope\", {}).rendered")
        ]
  where
    isParseError (TemplateParseError _) = True
    isParseError _ = False
    isNotAnAgent (TemplateNotAnAgent _) = True
    isNotAnAgent _ = False
    isEvalError (TemplateEvalError _) = True
    isEvalError _ = False

parameterTests :: TestTree
parameterTests =
    testGroup
        "parameters"
        [ testCase "a read without a supplied value names the parameter" $
            expectError
                (== TemplateParamNotSupplied "model")
                (evalAgentTemplate (envWith [("workspace", "/srv/app")]) workspaceAgent)
        , testCase "a read of an undeclared parameter is refused" $
            expectError
                (== TemplateUndeclaredParams ["tenant"])
                ( evalAgentTemplate
                    (envWith [("tenant", "acme")])
                    (agentSource [] "  modelName: \"gpt-4o\", systemPrompt: [$ctx.tenant]")
                )
        , testCase "a read of a secret parameter is refused" $
            expectError
                (== TemplateSecretParams ["token"])
                ( evalAgentTemplate
                    (envWith [("token", "s3cret")])
                    (agentSource [] "  modelName: \"gpt-4o\", systemPrompt: [$ctx.token], parameters: [{name: \"token\", secret: true}]")
                )
        , testCase "a read of a session-scope parameter is refused" $
            expectError
                (== TemplateNonProcessParams ["tenant"])
                ( evalAgentTemplate
                    (envWith [("tenant", "acme")])
                    (agentSource [] "  modelName: \"gpt-4o\", systemPrompt: [$ctx.tenant], parameters: [{name: \"tenant\", scope: \"session\"}]")
                )
        , testCase "$ctx as a whole is refused" $
            expectError
                (== TemplateReadsWholeContext)
                ( evalAgentTemplate
                    (envWith [("tenant", "acme")])
                    (agentSource [] "  modelName: lookup($ctx, \"tenant\", \"x\"), systemPrompt: []")
                )
        , testCase "a parameter nothing reads need not be declared" $
            case evalAgentTemplate (envWith [("unused", "x")]) (plainAgent "") of
                Left e -> assertFailure (renderTemplateError e)
                Right _ -> pure ()
        ]

standardLibraryTests :: TestTree
standardLibraryTests =
    testGroup
        "standard library"
        [ testCase "is the only library by default" $
            Map.keys standardLibraries @?= [standardLibraryName]
        , testCase "predicates" $ do
            ps <-
                evalWithLibrary
                    "[$a.dir(\"src\"), $a.dir-shallow(\"a\"), $a.dir-exactly(\"b\"), $a.file(\"README.md\"), $a.pattern(\"*.hs\"), $a.extensions([\"hs\"]), $a.any-of([$a.allow-all]), $a.all-of([$a.deny-all]), $a.none-of([$a.file(\"x\")])]"
            ps
                @?= [ DirectoryRecursive "src"
                    , DirectoryShallow "a"
                    , DirectoryExactly "b"
                    , FileExactly "README.md"
                    , FilePattern "*.hs"
                    , FileExtension ["hs"]
                    , Any [AlwaysAllow]
                    , All [AlwaysDeny]
                    , Not (Any [FileExactly "x"])
                    ]
        , testCase "sandbox without deny, with a size limit" $ do
            sb <- evalWithLibrary "$a.sandbox({allow: [$a.dir(\"src\")], maxFileSize: 1024})"
            sb @?= FileSandboxConfig (Any [DirectoryRecursive "src"]) (Just 1024) Nothing
        , testCase "builtin toolboxes parse" $ do
            boxes <-
                evalWithLibrary
                    "[$a.developer-toolbox({name: \"dev\", capabilities: [\"read-file-range\"], buildCommand: [\"make\"]}), $a.system-toolbox({name: \"sys\", capabilities: [\"date\"], sandbox: $a.sandbox({allow: [$a.deny-all]})}), $a.lua-toolbox({name: \"lua\", allowedTools: [\"bash\"]})]"
            length (boxes :: [BuiltinToolboxDescription]) @?= 3
        , testCase "bash toolboxes parse" $ do
            boxes <- evalWithLibrary "[$a.tools-dir({path: \"tools\", filter: \".sh\"}), $a.tool({path: \"tools/one.sh\"})]"
            length (boxes :: [BashToolboxDescription]) @?= 2
        , testCase "mcp servers parse" $ do
            servers <- evalWithLibrary "[$a.mcp({name: \"fs\", executable: \"mcp-fs\", args: [\"--root\", \".\"]}), $a.mcp({name: \"bare\", executable: \"x\"})]"
            length (servers :: [McpServerDescription]) @?= 2
        , testCase "helpers parse" $ do
            refs <- evalWithLibrary "[$a.helper({slug: \"faq\", path: \"faq.json\", narrowable: false})]"
            [(r.extraAgentSlug, r.extraAgentPath, r.extraAgentNarrowable) | r <- (refs :: [ExtraAgentRef])]
                @?= [("faq", "faq.json", Just False)]
        ]

libraryDirectoryTests :: TestTree
libraryDirectoryTests =
    testGroup
        "library directories"
        [ testCase "a file is a library named after it, next to the standard one" $
            withSystemTempDirectory "tramaj-libs" $ \dir -> do
                Text.IO.writeFile (dir </> "team.tramaj") "@model=\"gpt-4o\"\nnull"
                Text.IO.writeFile (dir </> "notes.txt") "not a library"
                Right libs <- loadLibraryDirectories [dir]
                Map.keys libs @?= ["agents", "team"]
                let env = TemplateEnv libs mempty
                    src = agentSource ["@team=import(\"team\", {}).vals"] "  modelName: $team.model, systemPrompt: []"
                case evalAgentTemplate env src of
                    Left e -> assertFailure (renderTemplateError e)
                    Right (_, AgentDescription agent) -> agent.modelName @?= "gpt-4o"
        , testCase "an earlier directory wins, and a directory wins over the standard library" $
            withSystemTempDirectory "tramaj-libs" $ \dir -> do
                let d1 = dir </> "one"
                    d2 = dir </> "two"
                mapM_ (createDirectoryIfMissing True) [d1, d2]
                Text.IO.writeFile (d1 </> "team.tramaj") "\"one\""
                Text.IO.writeFile (d2 </> "team.tramaj") "\"two\""
                Text.IO.writeFile (d2 </> "agents.tramaj") "\"mine\""
                Right libs <- loadLibraryDirectories [d1, d2]
                let run src = snd <$> evalTemplate (TemplateEnv libs mempty) src
                run "import(\"team\", {}).rendered" @?= Right (Aeson.String "one")
                run "import(\"agents\", {}).rendered" @?= Right (Aeson.String "mine")
        , testCase "a library that does not parse names its file" $
            withSystemTempDirectory "tramaj-libs" $ \dir -> do
                Text.IO.writeFile (dir </> "broken.tramaj") "@a=("
                result <- loadLibraryDirectories [dir]
                case result of
                    Left err -> assertBool err ("broken.tramaj" `isInfixOf` err)
                    Right _ -> assertFailure "expected a parse error"
        , testCase "a missing directory holds no library" $ do
            Right libs <- loadLibraryDirectories ["/nonexistent/tramaj-libs"]
            Map.keys libs @?= ["agents"]
        , testCase "agents-exe.cfg.json: tramajLibraries and .tramaj agents in agentsDirectories" $
            withSystemTempDirectory "tramaj-cwd" $ \cwd ->
                withSystemTempDirectory "tramaj-home" $ \home -> do
                    let libs = cwd </> "libs"
                        agentsDir = cwd </> "agents"
                    mapM_ (createDirectoryIfMissing True) [libs, agentsDir]
                    Text.IO.writeFile (libs </> "team.tramaj") "null"
                    Text.IO.writeFile (agentsDir </> "one.tramaj") (plainAgent "")
                    LByteString.writeFile (cwd </> "agents-exe.cfg.json") $
                        Aeson.encode $
                            Aeson.object
                                [ "agentsDirectories" Aeson..= [agentsDir]
                                , "tramajLibraries" Aeson..= [libs]
                                ]
                    rc <- withCurrentDirectory cwd (ConfigLoader.loadAgentsExeConfig home)
                    rc.rcAgentFiles @?= [agentsDir </> "one.tramaj"]
                    Map.keys rc.rcTemplateLibraries @?= ["agents", "team"]
                    resolved <-
                        ConfigLoader.resolveAgentFiles
                            (FileLoader.TemplateEnv rc.rcTemplateLibraries mempty)
                            rc.rcAgentFiles
                            (Just "templated")
                    resolved @?= Right [agentsDir </> "one.tramaj"]
        , testCase "--agent SLUG: a template that does not evaluate is set aside, and named when nothing matches" $
            withSystemTempDirectory "tramaj-agents" $ \dir -> do
                Text.IO.writeFile (dir </> "needy.tramaj") workspaceAgent
                Text.IO.writeFile (dir </> "plain.tramaj") (Text.replace "templated" "plain" (plainAgent ""))
                let files = [dir </> "needy.tramaj", dir </> "plain.tramaj"]
                found <- ConfigLoader.resolveAgentFiles defaultTemplateEnv files (Just "plain")
                found @?= Right [dir </> "plain.tramaj"]
                missing <- ConfigLoader.resolveAgentFiles defaultTemplateEnv files (Just "templated")
                case missing of
                    Left err -> do
                        assertBool "names the template" ("needy.tramaj" `Text.isInfixOf` err)
                        assertBool "says why" ("workspace" `Text.isInfixOf` err)
                    Right _ -> assertFailure "expected Left, got Right"
        ]

fileLoaderTests :: TestTree
fileLoaderTests =
    testGroup
        "file loader"
        [ testCase "listAgentDirectory lists .json and .tramaj files" $
            withSystemTempDirectory "tramaj-agents" $ \dir -> do
                mapM_ (\f -> Text.IO.writeFile (dir </> f) "") ["a.json", "b.tramaj", "c.sh"]
                files <- FileLoader.listAgentDirectory dir
                sort files @?= [dir </> "a.json", dir </> "b.tramaj"]
        , testCase "loadAgentFile evaluates a .tramaj file" $
            withSystemTempDirectory "tramaj-agents" $ \dir -> do
                let path = dir </> "agent.tramaj"
                Text.IO.writeFile path workspaceAgent
                let env = envWith [("workspace", "/srv/app"), ("model", "gpt-4o")]
                result <- FileLoader.loadAgentFile silent env path
                case result of
                    Right (AgentDescription agent) -> agent.slug @?= "templated"
                    Left err -> assertFailure (show err)
        , testCase "loadAgentFile reports a template's error with its path" $
            withSystemTempDirectory "tramaj-agents" $ \dir -> do
                let path = dir </> "agent.tramaj"
                Text.IO.writeFile path workspaceAgent
                result <- FileLoader.loadAgentFile silent FileLoader.defaultTemplateEnv path
                case result of
                    Left (FileLoader.LoadFailure p err) -> do
                        p @?= path
                        assertBool err ("--set" `isInfixOf` err)
                    Right _ -> assertFailure "expected a load failure"
        ]
