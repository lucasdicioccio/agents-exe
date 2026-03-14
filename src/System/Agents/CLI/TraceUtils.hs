{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Agents.CLI.TraceUtils (
    tracePrintingTextResponses,
    traceUsefulPromptStderr,
    traceUsefulPromptStdout,
) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import GHC.IO.Handle (Handle)
import Prod.Tracer (Tracer (..))
import System.IO (stderr, stdout)

import System.Agents.AgentTree
import qualified System.Agents.LLMs.OpenAI as OpenAI
import qualified System.Agents.Runtime.Trace as Runtime
import qualified System.Agents.Tools as Tools
import qualified System.Agents.Tools.Bash as Tools
import qualified System.Agents.Tools.BashToolbox as BashToolbox
import qualified System.Agents.Tools.IO as Tools
import qualified System.Agents.Tools.SqliteToolbox as SqliteTools
import qualified System.Agents.Tools.SystemToolbox as SystemTools

tracePrintingTextResponses :: Tracer IO Trace
tracePrintingTextResponses = Tracer f
  where
    f (AgentTrace (Runtime.AgentTrace_Conversation slug _ _ trace)) =
        g [slug] trace
    f (AgentTrace (Runtime.AgentTrace_Loading _ _ _)) = pure ()
    f (AgentTrace (Runtime.BuiltinToolboxTrace _ _)) = pure ()
    f (AgentTrace (Runtime.BuiltinToolboxInitError _ _)) = pure ()
    f (AgentTrace (Runtime.SystemToolboxTrace _ _)) = pure ()
    f (McpTrace _ _) = pure ()
    f (OpenAPITrace _ _) = pure ()
    f (PostgRESTTrace _ _) = pure ()
    f (DataLoadingTrace _) = pure ()
    f (ConfigLoadedTrace _) = pure ()
    f (CyclicReferencesWarning _) = pure ()
    f (ReferenceValidationTrace _) = pure ()

    g pfx (Runtime.LLMTrace _ (OpenAI.GotChatCompletion x bytes)) =
        case Aeson.parseEither OpenAI.parseLLMResponse x of
            Left _ -> pure ()
            Right rsp ->
                let byteInfo = " [" <> formatBytes bytes <> "]"
                 in Text.putStrLn $ Text.unwords [Text.intercalate "/" pfx, Maybe.fromMaybe "..." rsp.rspContent] <> byteInfo
    g pfx (Runtime.ChildrenTrace (Runtime.AgentTrace_Conversation childSlug _ _ sub)) =
        g (childSlug : pfx) sub
    g _ (Runtime.ChildrenTrace (Runtime.AgentTrace_Loading _ _ _)) = pure ()
    g _ (Runtime.ChildrenTrace (Runtime.BuiltinToolboxTrace _ _)) = pure ()
    g _ (Runtime.ChildrenTrace (Runtime.BuiltinToolboxInitError _ _)) = pure ()
    g _ (Runtime.ChildrenTrace (Runtime.SystemToolboxTrace _ _)) = pure ()
    g _ (Runtime.LLMTrace _ (OpenAI.HttpClientTrace _)) = pure ()
    g _ (Runtime.LLMTrace _ (OpenAI.CallChatCompletion _ bytes)) =
        Text.putStrLn $ "  [LLM request: " <> formatBytes bytes <> "]"
    g _ (Runtime.NewConversation) = pure ()
    g _ (Runtime.WaitingForPrompt) = pure ()
    g _ (Runtime.RunToolTrace _ _) = pure ()

traceUsefulPromptStdout :: Tracer IO Trace
traceUsefulPromptStdout = traceUsefulPromptHandle stdout

traceUsefulPromptStderr :: Tracer IO Trace
traceUsefulPromptStderr = traceUsefulPromptHandle stderr

traceUsefulPromptHandle :: Handle -> Tracer IO Trace
traceUsefulPromptHandle h = Tracer f
  where
    f (AgentTrace tr) =
        Text.hPutStrLn h $ renderAgentTrace tr
    f (McpTrace _ tr) =
        Text.hPutStrLn h (Text.pack $ show tr)
    f (OpenAPITrace _ tr) =
        Text.hPutStrLn h (Text.pack $ show tr)
    f (PostgRESTTrace _ tr) =
        Text.hPutStrLn h (Text.pack $ show tr)
    f (DataLoadingTrace x) = Text.hPutStrLn h (Text.pack $ show x)
    f (ConfigLoadedTrace x) =
        Text.hPutStrLn h (showTree 0 x)
      where
        showTree :: Int -> AgentConfigTree -> Text.Text
        showTree n v =
            Text.unlines
                [ Text.concat [Text.replicate n "  ", Text.pack v.agentConfigFile]
                , Text.unlines $ fmap (showTree (succ n)) v.agentConfigChildren
                ]
    f (CyclicReferencesWarning warns) =
        Text.hPutStrLn h $ "Cyclic references warning: " <> Text.pack (show warns)
    f (ReferenceValidationTrace refs) =
        Text.hPutStrLn h $ "Reference validation trace: " <> Text.pack (show refs)

-- | Format bytes in human-readable form.
formatBytes :: Int -> Text
formatBytes n
    | n >= 1024 * 1024 * 1024 = Text.pack (show (n `div` (1024 * 1024 * 1024))) <> " GiB"
    | n >= 1024 * 1024 = Text.pack (show (n `div` (1024 * 1024))) <> " MiB"
    | n >= 1024 = Text.pack (show (n `div` 1024)) <> " KiB"
    | otherwise = Text.pack (show n) <> " B"

renderAgentTrace :: Runtime.Trace -> Text
renderAgentTrace (Runtime.AgentTrace_Loading slug _ tr) =
    Text.unlines
        [ mconcat ["@", slug, ":"]
        , renderLoadingAgentTrace tr
        ]
renderAgentTrace (Runtime.AgentTrace_Conversation slug _ _ tr) =
    Text.unlines
        [ mconcat ["@", slug, ":"]
        , renderConversationAgentTrace tr
        ]
renderAgentTrace (Runtime.BuiltinToolboxTrace name tr) =
    Text.unlines
        [ mconcat ["@builtin/", name, ":"]
        , renderBuiltinToolboxTrace tr
        ]
renderAgentTrace (Runtime.BuiltinToolboxInitError name err) =
    Text.unlines
        [ mconcat ["@builtin/", name, ":"]
        , "Error: " <> Text.pack err
        ]
renderAgentTrace (Runtime.SystemToolboxTrace name tr) =
    Text.unlines
        [ mconcat ["@system/", name, ":"]
        , renderSystemToolboxTrace tr
        ]

renderLoadingAgentTrace :: BashToolbox.Trace -> Text
renderLoadingAgentTrace tr = case tr of
    BashToolbox.ReloadToolsTrace _ -> "(reload-tools...)"
    BashToolbox.BashToolsLoadingTrace _ -> "(reload-tools...)"

renderBuiltinToolboxTrace :: SqliteTools.Trace -> Text
renderBuiltinToolboxTrace tr = case tr of
    SqliteTools.ConnectionOpenedTrace _ -> "(connection opened)"
    SqliteTools.ConnectionClosedTrace -> "(connection closed)"
    SqliteTools.QueryStartedTrace _ -> "(query started)"
    SqliteTools.QueryCompletedTrace _ _ _ -> "(query completed)"
    SqliteTools.AccessViolationTrace _ _ _ -> "(access violation)"
    SqliteTools.QueryErrorTrace _ _ -> "(query error)"
    SqliteTools.WaitingForLockTrace _ -> "(waiting for lock)"

renderSystemToolboxTrace :: SystemTools.Trace -> Text
renderSystemToolboxTrace tr = case tr of
    SystemTools.SystemInfoRequestedTrace cap ->
        Text.unwords ["system-info requested:", cap]
    SystemTools.SystemInfoRetrievedTrace cap _ ->
        Text.unwords ["system-info retrieved:", cap]
    SystemTools.SystemInfoErrorTrace cap err ->
        Text.unwords ["system-info error:", cap, "-", err]

renderConversationAgentTrace :: Runtime.ConversationTrace -> Text
renderConversationAgentTrace tr = case tr of
    Runtime.NewConversation -> ""
    Runtime.WaitingForPrompt -> ""
    Runtime.RunToolTrace _ (Tools.BashToolsTrace (Tools.RunCommandStart p args)) ->
        Text.unwords ["bash-tool", "start", Text.pack p, Text.unwords $ map Text.pack args]
    Runtime.RunToolTrace _ (Tools.BashToolsTrace (Tools.RunCommandStopped p args code _ _)) ->
        Text.unlines
            [ Text.unwords ["bash-tool", "stopped", Text.pack p, Text.unwords $ map Text.pack args]
            , Text.pack $ show code
            ]
    Runtime.RunToolTrace _ (Tools.IOToolsTrace (Tools.IOScriptStarted desc _)) ->
        Text.unwords ["io-tool", desc.ioSlug, "start"]
    Runtime.RunToolTrace _ (Tools.IOToolsTrace (Tools.IOScriptStopped desc _ _)) ->
        Text.unwords ["io-tool", desc.ioSlug, "stop"]
    Runtime.RunToolTrace _ (Tools.SqliteToolsTrace _) ->
        "sqlite-tool: (trace)"
    Runtime.RunToolTrace _ (Tools.SystemToolsTrace _) ->
        "system-tool: (trace)"
    Runtime.RunToolTrace _ (Tools.LuaToolsTrace msg) ->
        Text.unwords ["lua-tool:", msg]
    Runtime.RunToolTrace _ (Tools.LuaToolCallTrace toolName args) ->
        Text.unwords ["lua-tool-call:", toolName, "args:", jsonTxt args]
    Runtime.LLMTrace _ (OpenAI.HttpClientTrace _) -> "(http)"
    Runtime.LLMTrace _ (OpenAI.CallChatCompletion _ bytes) ->
        Text.unwords ["to: llm", "[" <> formatBytes bytes <> "]"]
    Runtime.LLMTrace _ (OpenAI.GotChatCompletion x bytes) ->
        Text.unwords ["from: llm", "[" <> formatBytes bytes <> "]", jsonTxt x]
    Runtime.ChildrenTrace sub ->
        Text.unwords ["(", Runtime.traceAgentSlug sub, ")", renderAgentTrace sub]
  where
    jsonTxt :: (Aeson.ToJSON a) => a -> Text
    jsonTxt = Text.decodeUtf8 . LByteString.toStrict . Aeson.encode
