{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Tests for bash tools with subcommands (@todos/bash-tools-subcommands.md@):
a @describe@ listing @commands@ becomes one tool per subcommand, each with
the shared args plus its own, and @run@ receives the subcommand name.
-}
module BashSubcommandsTests (tests) where

import qualified Data.Aeson as Aeson
import Data.Either (isLeft)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Encoding.Error (lenientDecode)
import Prod.Tracer (Tracer (..))
import System.Directory (getPermissions, setOwnerExecutable, setPermissions)
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import qualified System.Agents.Tools.Bash as Bash

tests :: TestTree
tests =
    testGroup
        "Bash tool subcommands"
        [ testCase "a describe without commands is one tool, unchanged" $
            withFixture "single" singleScript $ \path -> do
                tools <- loadOk path
                fmap (.scriptInfo.scriptSlug) tools @?= ["single"]
                fmap (.scriptInfo.scriptSubcommand) tools @?= [Nothing]
        , testCase "commands expand to one tool per subcommand with shared + own args" $
            withFixture "kv" kvScript $ \path -> do
                tools <- loadOk path
                fmap (.scriptInfo.scriptSlug) tools @?= ["kv_get", "kv_set"]
                fmap (.scriptInfo.scriptSubcommand) tools @?= [Just "get", Just "set"]
                fmap (fmap (.argName) . (.scriptInfo.scriptArgs)) tools
                    @?= [["store", "key"], ["store", "key", "value"]]
                fmap (null . (.scriptInfo.scriptCommands)) tools @?= [True, True]
        , testCase "run receives the subcommand name before the arguments" $
            withFixture "kv" kvScript $ \path -> do
                tools <- loadOk path
                case tools of
                    [getTool, setTool] -> do
                        outGet <- runOk getTool (Aeson.object ["store" Aeson..= ("s1" :: Text), "key" Aeson..= ("k" :: Text)])
                        outGet @?= "get|--store|s1|--key|k\n"
                        outSet <- runOk setTool (Aeson.object ["store" Aeson..= ("s1" :: Text), "key" Aeson..= ("k" :: Text), "value" Aeson..= ("v" :: Text)])
                        outSet @?= "set|--store|s1|--key|k|v\n"
                    _ -> assertFailure "expected two tools"
        , testCase "a subcommand's empty-result overrides the top-level one" $
            withFixture "kv" kvScript $ \path -> do
                tools <- loadOk path
                fmap (.scriptInfo.scriptEmptyResultBehavior) tools
                    @?= [Just (Bash.AddMessage "no such key"), Just (Bash.AddMessage "done")]
        , testCase "loadDirectory exposes every subcommand tool" $
            withSystemTempDirectory "bash-subcommands-dir" $ \dir -> do
                writeExecutable (dir <> "/kv") kvScript
                writeExecutable (dir <> "/single") singleScript
                (scripts, errs) <- Bash.loadDirectory nullTracer dir
                length errs @?= 0
                let slugs = fmap (.scriptInfo.scriptSlug) scripts.scriptDescriptions
                assertBool ("slugs: " <> show slugs) (all (`elem` slugs) ["kv_get", "kv_set", "single"])
                length slugs @?= 3
        , testCase "duplicate subcommand names are rejected" $
            withFixture "dup" (describeOnly dupCommands) $ \path -> do
                r <- Bash.loadScript nullTracer path
                assertBool "expected a load error" (isLeft r)
        , testCase "a subcommand redeclaring a shared arg is rejected" $
            withFixture "clash" (describeOnly clashArgs) $ \path -> do
                r <- Bash.loadScript nullTracer path
                assertBool "expected a load error" (isLeft r)
        , testCase "a subcommand name unfit for a tool name is rejected" $
            withFixture "badname" (describeOnly badName) $ \path -> do
                r <- Bash.loadScript nullTracer path
                assertBool "expected a load error" (isLeft r)
        , testCase "selectSubcommand picks by name and refuses ambiguity" $
            withFixture "kv" kvScript $ \path -> do
                r <- Bash.loadScript nullTracer path
                case r of
                    Left err -> assertFailure (show err)
                    Right desc -> do
                        fmap (.scriptInfo.scriptSlug) (Bash.selectSubcommand (Just "set") desc) @?= Right "kv_set"
                        assertBool "no name, several subcommands" (isLeft (Bash.selectSubcommand Nothing desc))
                        assertBool "unknown name" (isLeft (Bash.selectSubcommand (Just "nope") desc))
        , testCase "ScriptInfo JSON round-trips commands and subcommand" $
            withFixture "kv" kvScript $ \path -> do
                r <- Bash.loadScript nullTracer path
                case r of
                    Left err -> assertFailure (show err)
                    Right desc -> do
                        Aeson.decode (Aeson.encode desc.scriptInfo) @?= Just desc.scriptInfo
                        let expanded = Bash.expandScriptCommands desc
                        fmap (Aeson.decode . Aeson.encode . (.scriptInfo)) expanded
                            @?= fmap (Just . (.scriptInfo)) expanded
        ]

nullTracer :: Tracer IO a
nullTracer = Tracer (\_ -> pure ())

loadOk :: FilePath -> IO [Bash.ScriptDescription]
loadOk path = do
    r <- Bash.loadScriptTools nullTracer path
    case r of
        Left err -> assertFailure ("failed to load fixture: " <> show err) >> pure []
        Right tools -> pure tools

runOk :: Bash.ScriptDescription -> Aeson.Value -> IO Text
runOk tool val = do
    r <- Bash.runValue nullTracer tool Nothing val
    case r of
        Left err -> assertFailure ("run failed: " <> show err) >> pure ""
        Right bytes -> pure (Text.decodeUtf8With lenientDecode bytes)

withFixture :: String -> String -> (FilePath -> IO a) -> IO a
withFixture name content k =
    withSystemTempDirectory "bash-subcommands" $ \dir -> do
        let path = dir <> "/" <> name
        writeExecutable path content
        k path

writeExecutable :: FilePath -> String -> IO ()
writeExecutable path content = do
    writeFile path content
    perms <- getPermissions path
    setPermissions path (setOwnerExecutable True perms)

arg :: String -> String -> String
arg name mode =
    "{\"name\": \"" <> name <> "\", \"description\": \"d\", \"type\": \"string\", \"backing_type\": \"string\", \"arity\": \"single\", \"mode\": \"" <> mode <> "\"}"

singleScript :: String
singleScript =
    unlines
        [ "#!/usr/bin/env bash"
        , "if [[ \"$1\" == \"describe\" ]]; then"
        , "  echo '{\"slug\": \"single\", \"description\": \"one thing\", \"args\": []}'"
        , "  exit 0"
        , "fi"
        , "echo ok"
        ]

-- | Prints its argv (after @run@) joined by @|@.
kvScript :: String
kvScript =
    unlines
        [ "#!/usr/bin/env bash"
        , "if [[ \"$1\" == \"describe\" ]]; then"
        , "  cat << 'EOF'"
        , "{\"slug\": \"kv\", \"description\": \"a key-value store\","
        , " \"args\": [" <> arg "store" "dashdashspace" <> "],"
        , " \"empty-result\": {\"tag\": \"AddMessage\", \"contents\": \"no such key\"},"
        , " \"commands\": ["
        , "   {\"name\": \"get\", \"description\": \"reads a key\", \"args\": [" <> arg "key" "dashdashspace" <> "]},"
        , "   {\"name\": \"set\", \"description\": \"writes a key\", \"args\": [" <> arg "key" "dashdashspace" <> ", " <> arg "value" "positional" <> "],"
        , "    \"empty-result\": {\"tag\": \"AddMessage\", \"contents\": \"done\"}}"
        , " ]}"
        , "EOF"
        , "  exit 0"
        , "fi"
        , "shift"
        , "(IFS='|'; echo \"$*\")"
        ]

describeOnly :: String -> String
describeOnly json =
    unlines
        [ "#!/usr/bin/env bash"
        , "cat << 'EOF'"
        , json
        , "EOF"
        ]

dupCommands :: String
dupCommands =
    "{\"slug\": \"dup\", \"description\": \"d\", \"args\": [], \"commands\": [{\"name\": \"a\", \"description\": \"x\"}, {\"name\": \"a\", \"description\": \"y\"}]}"

clashArgs :: String
clashArgs =
    "{\"slug\": \"clash\", \"description\": \"d\", \"args\": [" <> arg "k" "positional" <> "], \"commands\": [{\"name\": \"a\", \"description\": \"x\", \"args\": [" <> arg "k" "positional" <> "]}]}"

badName :: String
badName =
    "{\"slug\": \"bad\", \"description\": \"d\", \"args\": [], \"commands\": [{\"name\": \"a.b\", \"description\": \"x\"}]}"
