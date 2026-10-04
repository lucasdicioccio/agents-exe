{-# LANGUAGE OverloadedStrings #-}

{- | End-to-end tests of the terminal UI.

The real @agents-exe tui@ runs in a pseudo-terminal driven by @tuispec@: keys
go in, the screen is read back, and each scenario ends on snapshots compared
with the baselines under @test/tui-snapshots/snapshots/@. The model is a fake
OpenAI-compatible endpoint served by this process, so what is on screen only
depends on the keys sent.

With @AGENTS_TUI_E2E_PNG=1@ every snapshot is also rendered to
@test/tui-snapshots/png/@, the screenshots the documentation shows. See
"Screenshots and end-to-end tests" in @documentation/tui.md@.
-}
module Main (main) where

import Control.Monad (when)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LByteString
import Data.Char (isHexDigit)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Network.HTTP.Types (status200)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (testWithApplication)
import System.Directory (
    copyFile,
    createDirectoryIfMissing,
    doesFileExist,
    findExecutable,
    getPermissions,
    setOwnerExecutable,
    setPermissions,
 )
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (dropExtensions, takeDirectory, (<.>), (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure)
import TuiSpec
import TuiSpec.Runner (serializeAnsiSnapshot)
import TuiSpec.Types (Tui (..))

main :: IO ()
main = do
    exe <- locateAgentsExe
    withSystemTempDirectory "agents-tui-e2e" $ \tmp ->
        testWithApplication (pure fakeLlm) $ \port ->
            defaultMain (tests (Env exe tmp port))

-- | What every scenario needs: the binary, a scratch directory, the fake model's port.
data Env = Env
    { envExe :: FilePath
    , envTmp :: FilePath
    , envPort :: Int
    }

{- | The binary under test: @AGENTS_EXE@ when set, else the @agents-exe@ on
the PATH, which is where cabal puts the one it just built
(@build-tool-depends@).
-}
locateAgentsExe :: IO FilePath
locateAgentsExe = do
    override <- lookupEnv "AGENTS_EXE"
    found <- findExecutable "agents-exe"
    maybe (fail "agents-exe not found: set AGENTS_EXE or put it on the PATH") pure (firstJust override found)
  where
    firstJust (Just x) _ = Just x
    firstJust Nothing y = y

-------------------------------------------------------------------------------
-- Scenarios
-------------------------------------------------------------------------------

tests :: Env -> TestTree
tests e =
    testGroup
        "agents-tui-e2e"
        [ scenario e "launch" $ \tui -> do
            -- The TUI opens on the Agents tab, with the fixture's agents listed.
            waitForText tui (Exact "# Slug: assistant")
            snapshot tui "agents-tab"
        , scenario e "chat" $ \tui -> do
            waitForText tui (Exact "# Slug: assistant")
            newConversation tui
            typeText tui "Hello, who are you?"
            snapshot tui "message-typed"
            sendMessage tui
            waitForText tui (Exact "I am a scripted model")
            snapshot tui "reply"
        , scenario e "pending" $ \tui -> do
            waitForText tui (Exact "# Slug: assistant")
            -- The second agent defers its tool calls to an external worker.
            press tui ArrowDown
            waitForText tui (Exact "# Slug: clerk")
            newConversation tui
            typeText tui "Where is order 42?"
            sendMessage tui
            -- The run stops on the deferred call: the Pending panel lists it.
            waitForText tui (Exact "Pending (1)")
            snapshot tui "pending-panel"
            -- Answer it from the TUI; the run resumes and the model replies.
            press tui (Ctrl 'y')
            typeText tui "shipped on Monday"
            sendMessage tui
            waitForText tui (Exact "Order 42 has shipped")
            snapshot tui "pending-answered"
        ]

-- | Start a conversation with the agent selected in the Agents tab.
newConversation :: Tui -> IO ()
newConversation tui = do
    press tui (Ctrl 'n')
    -- Ctrl+], the next tab (Chats): the control character itself, which tuispec's 'Ctrl' only knows for letters.
    press tui (NamedKey "\GS")
    -- The focus is still on the agent list, last of this tab's focus ring:
    -- on to the conversation list, then to the message editor.
    press tui Tab
    press tui Tab
    waitForText tui (Exact "Message")

-- | The default send key, Meta+Enter, as a terminal sends it.
sendMessage :: Tui -> IO ()
sendMessage tui = press tui (NamedKey "\ESC\r")

{- | One scenario: a fresh fixture (agents, keys, database, HOME) and a fresh
@agents-exe tui@ in a PTY of a fixed size, quit at the end.
-}
scenario :: Env -> String -> (Tui -> IO ()) -> TestTree
scenario e name body =
    tuiTest runOptions name $ \tui -> do
        launchSpec <- writeFixture e name
        launch tui launchSpec
        body tui

runOptions :: RunOptions
runOptions =
    defaultRunOptions
        { timeoutSeconds = 20
        , terminalCols = 100
        , terminalRows = 30
        , artifactsDir = "test/tui-snapshots"
        , -- A fixed theme: the PNGs must not depend on the terminal running the tests.
          snapshotTheme = "dark"
        }

{- | Compare the settled screen with its baseline and, with
@AGENTS_TUI_E2E_PNG@ set, render that baseline to @test/tui-snapshots/png/@.

This is tuispec's @expectSnapshot@ (same files, same comparison of the
rendered cells, same @TUISPEC_UPDATE_SNAPSHOTS@), over a capture whose session
ids are masked: they are random, and the sidebar shows them. A baseline is
what the terminal received (@.ansi.txt@); its plain-text rendering (@.txt@) is
written next to it.
-}
snapshot :: Tui -> String -> IO ()
snapshot tui name = do
    waitForStable tui (defaultWaitOptionsFor tui) 400
    actualPath <- dumpView tui (SnapshotName (Text.pack name))
    actual <- maskSessionIds <$> Text.readFile actualPath
    Text.writeFile actualPath actual
    let options = tuiOptions tui
        baselinePath = tuiSnapshotRoot tui </> name <.> "ansi.txt"
        metaOf path = dropExtensions path <.> "meta.json"
        cells = serializeAnsiSnapshot (terminalRows options) (terminalCols options) (snapshotTheme options)
    baselineExists <- doesFileExist baselinePath
    if updateSnapshots options || not baselineExists
        then do
            createDirectoryIfMissing True (tuiSnapshotRoot tui)
            Text.writeFile baselinePath actual
            copyFile (metaOf actualPath) (metaOf baselinePath)
            -- The same screen as plain text, for reading a change in a diff.
            renderAnsiSnapshotTextFile Nothing Nothing baselinePath (dropExtensions baselinePath <.> "txt")
        else do
            baseline <- Text.readFile baselinePath
            when (cells baseline /= cells actual) $
                assertFailure
                    ( "Snapshot mismatch for '"
                        <> name
                        <> "'. Compare "
                        <> baselinePath
                        <> " and "
                        <> actualPath
                        <> " (tuispec render-text FILE shows one as plain text); TUISPEC_UPDATE_SNAPSHOTS=1 accepts the new screen."
                    )
    wantPng <- isJust <$> lookupEnv "AGENTS_TUI_E2E_PNG"
    when wantPng $
        renderPng (cells <$> Text.readFile baselinePath) (artifactsDir options </> "png" </> name <.> "png")

{- | Draw a screen, as tuispec serialises its cells (characters and colours),
with @test/tui-e2e/render_png.py@ rather than tuispec's own renderer: that one
spaces the cells by the width of a @W@, wider than the font's advance, which
leaves gaps in every box-drawing line, and has no fallback for a glyph the
font lacks (the status icons of the conversation list).
-}
renderPng :: IO String -> FilePath -> IO ()
renderPng getCells outPath = do
    payload <- getCells
    createDirectoryIfMissing True (takeDirectory outPath)
    withSystemTempDirectory "agents-tui-e2e-png" $ \dir -> do
        let payloadPath = dir </> "cells.json"
        writeFile payloadPath payload
        (code, _, err) <- readProcessWithExitCode "python3" [pngRenderer, payloadPath, outPath] ""
        when (code /= ExitSuccess) $
            assertFailure ("Rendering " <> outPath <> " failed (python3 with Pillow is needed): " <> err)

-- | The renderer, relative to the package root, where cabal runs the suite.
pngRenderer :: FilePath
pngRenderer = "test/tui-e2e/render_png.py"

-- | Zero the digits of every session id the TUI printed (@SessionId \<uuid\>@, possibly cut by a border).
maskSessionIds :: Text -> Text
maskSessionIds text =
    case Text.splitOn marker text of
        [] -> text
        first : rest -> Text.intercalate marker (first : map maskLeadingId rest)
  where
    marker = "SessionId "
    maskLeadingId piece =
        let (uuid, after) = Text.span (\c -> isHexDigit c || c == '-') piece
         in Text.map (\c -> if c == '-' then c else '0') uuid <> after

-------------------------------------------------------------------------------
-- Fixture
-------------------------------------------------------------------------------

{- | Write a scenario's files and return how to launch the TUI over them.

Nothing of the developer's own setup is read: HOME is a scratch directory
laid out like a real one (@~\/.config\/agents-exe@ with the keys file and the
agents in @default\/@, so that agents-exe finds everything where it looks by
default and prints no first-run message), and the working directory has no
@agents-exe.cfg.json@ above it.
-}
writeFixture :: Env -> String -> IO App
writeFixture e name = do
    let root = envTmp e </> name
        home = root </> "home"
        work = root </> "work"
        config = home </> ".config" </> "agents-exe"
        agents = config </> "default"
        tools = agents </> "clerk-tools"
        keys = config </> "secret-keys"
        url = "http://127.0.0.1:" <> show (envPort e) <> "/v1"
    mapM_ (createDirectoryIfMissing True) [work, tools]
    Aeson.encodeFile keys $
        Aeson.object ["keys" Aeson..= [Aeson.object ["id" Aeson..= ("fake" :: Text), "value" Aeson..= ("not-a-secret" :: Text)]]]
    writeFile (tools </> "lookup-order.sh") lookupOrderTool
    perms <- getPermissions (tools </> "lookup-order.sh")
    setPermissions (tools </> "lookup-order.sh") (setOwnerExecutable True perms)
    Aeson.encodeFile (agents </> "assistant.json") $
        agentFile
            url
            "assistant"
            "A helpful assistant, played by a scripted model."
            ["You are a helpful assistant."]
            []
    Aeson.encodeFile (agents </> "clerk.json") $
        agentFile
            url
            "clerk"
            "Looks orders up; its tool calls wait for an external worker."
            ["You answer questions about orders with the lookup_order tool."]
            [ "toolDirectory" Aeson..= tools
            , "executionMode" Aeson..= ("asynchronous" :: Text)
            , "toolCallPolicyConfig"
                Aeson..= Aeson.object
                    [ "default" Aeson..= Aeson.object ["tag" Aeson..= ("defer" :: Text), "reason" Aeson..= ("external" :: Text)]
                    , "rules" Aeson..= ([] :: [Aeson.Value])
                    ]
            ]
    pure
        (app (envExe e) ["tui", "--db", "sessions.db"])
            { env = Just [("HOME", Just home)]
            , cwd = Just work
            }

agentFile :: String -> Text -> Text -> [Text] -> [(Key.Key, Aeson.Value)] -> Aeson.Value
agentFile url slug announce prompt extra =
    Aeson.object
        [ "tag" Aeson..= ("OpenAIAgentDescription" :: Text)
        , "contents"
            Aeson..= Aeson.object
                ( [ "slug" Aeson..= slug
                  , "flavor" Aeson..= ("OpenAIv1" :: Text)
                  , "modelUrl" Aeson..= url
                  , "modelName" Aeson..= ("scripted-model" :: Text)
                  , "apiKeyId" Aeson..= ("fake" :: Text)
                  , "announce" Aeson..= announce
                  , "systemPrompt" Aeson..= prompt
                  ]
                    <> extra
                )
        ]

-- | A bash tool for the clerk. It is never run: the clerk's calls are deferred.
lookupOrderTool :: String
lookupOrderTool =
    unlines
        [ "#!/bin/bash"
        , "if [ \"${1:-}\" == \"describe\" ]; then"
        , "cat <<'EOF'"
        , "{"
        , "  \"slug\": \"lookup_order\","
        , "  \"description\": \"Looks an order up by its number.\","
        , "  \"args\": ["
        , "    {\"name\": \"order\", \"description\": \"The order number\", \"type\": \"string\", \"backing_type\": \"string\", \"arity\": \"single\", \"mode\": \"dashdashspace\"}"
        , "  ]"
        , "}"
        , "EOF"
        , "exit 0"
        , "fi"
        , "echo \"not reached\""
        ]

-------------------------------------------------------------------------------
-- The scripted model
-------------------------------------------------------------------------------

{- | An OpenAI-compatible chat-completions endpoint with fixed answers:

* a request offering tools, with no tool result yet, gets a call to the first tool;
* a request carrying a tool result gets the final answer about the order;
* anything else gets a greeting.

Answers are streamed when the request asks for it.
-}
fakeLlm :: Wai.Application
fakeLlm req respond = do
    body <- Wai.strictRequestBody req
    let request = fromMaybe Aeson.Null (Aeson.decode body)
        messages = elems (field "messages" request)
        toolAnswered = any ((== Aeson.String "tool") . field "role") messages
        firstTool = case elems (field "tools" request) of
            tool : _ -> Just (field "name" (field "function" tool))
            [] -> Nothing
        streamed = field "stream" request == Aeson.Bool True
        answer = case firstTool of
            Just toolName
                | not toolAnswered ->
                    ToolCall toolName "{\"order\": \"42\"}"
            _
                | toolAnswered -> Say "Order 42 has shipped: it left on Monday."
                | otherwise -> Say "Hello! I am a scripted model, here for the end-to-end tests."
    respond $
        if streamed
            then Wai.responseLBS status200 [("Content-Type", "text/event-stream")] (streamOf answer)
            else Wai.responseLBS status200 [("Content-Type", "application/json")] (Aeson.encode (completionOf answer))

data Answer
    = Say Text
    | ToolCall Aeson.Value Text

completionOf :: Answer -> Aeson.Value
completionOf answer =
    Aeson.object
        [ "choices"
            Aeson..= [ Aeson.object
                        [ "index" Aeson..= (0 :: Int)
                        , "message" Aeson..= message
                        , "finish_reason" Aeson..= finishReason answer
                        ]
                     ]
        ]
  where
    message = case answer of
        Say text -> Aeson.object ["role" Aeson..= ("assistant" :: Text), "content" Aeson..= text]
        ToolCall name arguments ->
            Aeson.object
                [ "role" Aeson..= ("assistant" :: Text)
                , "content" Aeson..= Aeson.Null
                , "tool_calls" Aeson..= [toolCall Nothing name arguments]
                ]

streamOf :: Answer -> LByteString.ByteString
streamOf answer =
    Builder.toLazyByteString $
        foldMap frame [chunk delta Aeson.Null, chunk (Aeson.object []) (Aeson.String (finishReason answer))]
            <> "data: [DONE]\n\n"
  where
    delta = case answer of
        Say text -> Aeson.object ["role" Aeson..= ("assistant" :: Text), "content" Aeson..= text]
        ToolCall name arguments ->
            Aeson.object
                [ "role" Aeson..= ("assistant" :: Text)
                , "tool_calls" Aeson..= [toolCall (Just 0) name arguments]
                ]
    chunk d finish =
        Aeson.object
            [ "id" Aeson..= ("chatcmpl-e2e" :: Text)
            , "model" Aeson..= ("scripted-model" :: Text)
            , "choices" Aeson..= [Aeson.object ["index" Aeson..= (0 :: Int), "delta" Aeson..= d, "finish_reason" Aeson..= finish]]
            ]
    frame v = "data: " <> Builder.lazyByteString (Aeson.encode v) <> "\n\n"

toolCall :: Maybe Int -> Aeson.Value -> Text -> Aeson.Value
toolCall index name arguments =
    Aeson.object $
        ["index" Aeson..= i | Just i <- [index]]
            <> [ "id" Aeson..= ("call_1" :: Text)
               , "type" Aeson..= ("function" :: Text)
               , "function" Aeson..= Aeson.object ["name" Aeson..= name, "arguments" Aeson..= arguments]
               ]

finishReason :: Answer -> Text
finishReason (Say _) = "stop"
finishReason (ToolCall _ _) = "tool_calls"

field :: Text -> Aeson.Value -> Aeson.Value
field k (Aeson.Object o) = fromMaybe Aeson.Null (KeyMap.lookup (Key.fromText k) o)
field _ _ = Aeson.Null

elems :: Aeson.Value -> [Aeson.Value]
elems (Aeson.Array xs) = toList xs
elems _ = []
