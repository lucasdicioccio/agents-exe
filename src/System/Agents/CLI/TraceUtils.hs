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
import qualified System.Agents.Tools.DeveloperToolbox as DeveloperTools
import qualified System.Agents.Tools.IO as Tools
import qualified System.Agents.Tools.LuaToolbox as LuaTools
import qualified System.Agents.Tools.Skills.Toolbox as SkillsToolbox
import qualified System.Agents.Tools.Skills.Types as SkillsTypes
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
    f (AgentTrace (Runtime.DeveloperToolboxTrace _ _)) = pure ()
    f (AgentTrace (Runtime.LuaToolboxTrace _ _)) = pure ()
    f (AgentTrace (Runtime.LuaToolboxInitError _ _)) = pure ()
    f (AgentTrace (Runtime.SkillsToolboxTrace _ _)) = pure ()
    f (AgentTrace (Runtime.SkillsToolboxInitError _ _)) = pure ()
    -- New sub-agent trace events
    f (AgentTrace (Runtime.AgentTrace_SubAgentStarted pSlug _ _ subSlug subConv _)) =
        Text.putStrLn $ Text.intercalate "/" [pSlug, subSlug] <> ": sub-agent started (" <> Text.pack (show subConv) <> ")"
    f (AgentTrace (Runtime.AgentTrace_SubAgentCompleted pSlug _ _ subSlug subConv _)) =
        Text.putStrLn $ Text.intercalate "/" [pSlug, subSlug] <> ": sub-agent completed (" <> Text.pack (show subConv) <> ")"
    f (AgentTrace (Runtime.AgentTrace_SubAgentFailed pSlug _ _ subSlug _ _ err)) =
        Text.putStrLn $ Text.intercalate "/" [pSlug, subSlug] <> ": sub-agent failed: " <> err
    f (McpTrace _ _) = pure ()
    f (OpenAPITrace _ _) = pure ()
    f (PostgRESTrace _ _) = pure ()
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
    g _ (Runtime.ChildrenTrace (Runtime.DeveloperToolboxTrace _ _)) = pure ()
    g _ (Runtime.ChildrenTrace (Runtime.LuaToolboxTrace _ _)) = pure ()
    g _ (Runtime.ChildrenTrace (Runtime.LuaToolboxInitError _ _)) = pure ()
    g _ (Runtime.ChildrenTrace (Runtime.SkillsToolboxTrace _ _)) = pure ()
    g _ (Runtime.ChildrenTrace (Runtime.SkillsToolboxInitError _ _)) = pure ()
    -- New sub-agent trace events in ChildrenTrace
    g pfx (Runtime.ChildrenTrace (Runtime.AgentTrace_SubAgentStarted _ _ _ subSlug _ _)) =
        Text.putStrLn $ Text.intercalate "/" (subSlug : pfx) <> ": sub-agent session started"
    g pfx (Runtime.ChildrenTrace (Runtime.AgentTrace_SubAgentCompleted _ _ _ subSlug _ _)) =
        Text.putStrLn $ Text.intercalate "/" (subSlug : pfx) <> ": sub-agent session completed"
    g pfx (Runtime.ChildrenTrace (Runtime.AgentTrace_SubAgentFailed _ _ _ subSlug _ _ err)) =
        Text.putStrLn $ Text.intercalate "/" (subSlug : pfx) <> ": sub-agent session failed: " <> err
    g _ (Runtime.LLMTrace _ (OpenAI.HttpClientTrace _)) = pure ()
    g _ (Runtime.LLMTrace _ (OpenAI.CallChatCompletion _ bytes)) =
        Text.putStrLn $ "  [LLM request: " <> formatBytes bytes <> "]"
    g _ (Runtime.LLMTrace _ (OpenAI.OverloadedBackoff attempt delay)) =
        Text.putStrLn $ "  [Moonshot/Kimi overloaded, retry " <> Text.pack (show attempt) <> " in " <> Text.pack (show delay) <> "s]"
    g _ (Runtime.NewConversation) = pure ()
    g _ (Runtime.WaitingForPrompt) = pure ()
    g _ (Runtime.RunToolTrace _ _) = pure ()
    -- New ConversationTrace events
    g _ (Runtime.SubAgentCallTrace subSlug subConv _) =
        Text.putStrLn $ "  [Sub-agent call: " <> subSlug <> " conv=" <> Text.pack (show subConv) <> "]"
    g _ (Runtime.SubAgentReturnTrace subSlug subConv) =
        Text.putStrLn $ "  [Sub-agent return: " <> subSlug <> " conv=" <> Text.pack (show subConv) <> "]"

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
    f (PostgRESTrace _ tr) =
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
renderAgentTrace (Runtime.DeveloperToolboxTrace name tr) =
    Text.unlines
        [ mconcat ["@developer/", name, ":"]
        , renderDeveloperToolboxTrace tr
        ]
renderAgentTrace (Runtime.LuaToolboxTrace name tr) =
    Text.unlines
        [ mconcat ["@lua/", name, ":"]
        , renderLuaToolboxTrace tr
        ]
renderAgentTrace (Runtime.LuaToolboxInitError name err) =
    Text.unlines
        [ mconcat ["@lua/", name, ":"]
        , "Error: " <> Text.pack err
        ]
renderAgentTrace (Runtime.SkillsToolboxTrace name tr) =
    Text.unlines
        [ mconcat ["@skills/", name, ":"]
        , renderSkillsToolboxTrace tr
        ]
renderAgentTrace (Runtime.SkillsToolboxInitError name err) =
    Text.unlines
        [ mconcat ["@skills/", name, ":"]
        , "Error: " <> Text.pack err
        ]
-- New sub-agent trace renderers
renderAgentTrace (Runtime.AgentTrace_SubAgentStarted pSlug _ pConv subSlug subConv sessId) =
    Text.unlines
        [ mconcat ["@", pSlug, " (sub-agent):"]
        , Text.unwords ["Sub-agent started:", subSlug]
        , Text.unwords ["  Parent conversation:", Text.pack (show pConv)]
        , Text.unwords ["  Sub-agent conversation:", Text.pack (show subConv)]
        , Text.unwords ["  Session ID:", Text.pack (show sessId)]
        ]
renderAgentTrace (Runtime.AgentTrace_SubAgentCompleted pSlug _ _ subSlug subConv _) =
    Text.unlines
        [ mconcat ["@", pSlug, " (sub-agent):"]
        , Text.unwords ["Sub-agent completed:", subSlug]
        , Text.unwords ["  Sub-agent conversation:", Text.pack (show subConv)]
        ]
renderAgentTrace (Runtime.AgentTrace_SubAgentFailed pSlug _ _ subSlug _ _ err) =
    Text.unlines
        [ mconcat ["@", pSlug, " (sub-agent):"]
        , Text.unwords ["Sub-agent failed:", subSlug]
        , Text.unwords ["  Error:", err]
        ]

renderLoadingAgentTrace :: BashToolbox.Trace -> Text
renderLoadingAgentTrace tr = case tr of
    BashToolbox.ReloadToolsTrace _ -> "(reload-tools...)"
    BashToolbox.BashToolsLoadingTrace _ -> "(reload-tools...)"
    BashToolbox.SourceLoadingError _ _ -> "(source loading error)"

renderBuiltinToolboxTrace :: SqliteTools.Trace -> Text
renderBuiltinToolboxTrace tr = case tr of
    SqliteTools.ConnectionOpenedTrace _ -> "(connection opened)"
    SqliteTools.ConnectionClosedTrace -> "(connection closed)"
    SqliteTools.QueryStartedTrace _ -> "(query started)"
    SqliteTools.QueryCompletedTrace _ _ _ -> "(query completed)"
    SqliteTools.AccessViolationTrace _ _ _ -> "(access violation)"
    SqliteTools.QueryErrorTrace _ _ -> "(query error)"
    SqliteTools.WaitingForLockTrace _ -> "(waiting for lock)"

renderLuaToolboxTrace :: LuaTools.Trace -> Text
renderLuaToolboxTrace tr = case tr of
    LuaTools.StateInitializedTrace name ->
        Text.unwords ["lua state initialized:", name]
    LuaTools.StateClosedTrace ->
        "lua state closed"
    LuaTools.ScriptStartedTrace script ->
        Text.unwords ["lua script started:", Text.take 50 script <> "..."]
    LuaTools.ScriptExecutionStartTrace script ->
        Text.unwords ["lua script execution start:", Text.take 100 script <> "..."]
    LuaTools.ScriptCompletedTrace duration ->
        Text.unwords ["lua script completed:", Text.pack (show duration)]
    LuaTools.ScriptExecutionEndTrace script results duration ->
        Text.unlines
            [ "lua script execution end:"
            , "  script: " <> Text.take 100 script <> "..."
            , "  results: " <> Text.pack (show $ length results) <> " value(s)"
            , "  duration: " <> Text.pack (show duration)
            ]
    LuaTools.ScriptTimeoutTrace timeout ->
        Text.unwords ["lua script timeout:", Text.pack (show timeout)]
    LuaTools.MemoryLimitExceededTrace limit ->
        Text.unwords ["lua memory limit exceeded:", Text.pack (show limit)]
    LuaTools.LuaErrorTrace err ->
        Text.unwords ["lua error:", err]
    LuaTools.SandboxViolationTrace msg ->
        Text.unwords ["lua sandbox violation:", msg]

-- LuaTools.ToolInvocationTrace toolTrace ->
--    Text.unwords ["lua tool invocation:", Text.pack (show toolTrace)]

renderSystemToolboxTrace :: SystemTools.Trace -> Text
renderSystemToolboxTrace tr = case tr of
    SystemTools.SystemInfoRequestedTrace cap ->
        Text.unwords ["system-info requested:", cap]
    SystemTools.SystemInfoRetrievedTrace cap _ ->
        Text.unwords ["system-info retrieved:", cap]
    SystemTools.SystemInfoErrorTrace cap err ->
        Text.unwords ["system-info error:", cap, "-", err]

renderDeveloperToolboxTrace :: DeveloperTools.Trace -> Text
renderDeveloperToolboxTrace tr = case tr of
    DeveloperTools.ValidateToolStartedTrace path ->
        Text.unwords ["validate-tool started:", Text.pack path]
    DeveloperTools.ValidateToolCompletedTrace path valid ->
        Text.unwords ["validate-tool completed:", Text.pack path, "valid:", Text.pack (show valid)]
    DeveloperTools.ScaffoldAgentStartedTrace template _ ->
        Text.unwords ["scaffold-agent started:", template]
    DeveloperTools.ScaffoldAgentCompletedTrace template _ ->
        Text.unwords ["scaffold-agent completed:", template]
    DeveloperTools.ScaffoldToolStartedTrace lang _ ->
        Text.unwords ["scaffold-tool started:", lang]
    DeveloperTools.ScaffoldToolCompletedTrace lang _ ->
        Text.unwords ["scaffold-tool completed:", lang]
    DeveloperTools.ShowSpecStartedTrace specName ->
        Text.unwords ["show-spec started:", specName]
    DeveloperTools.ShowSpecCompletedTrace specName ->
        Text.unwords ["show-spec completed:", specName]
    DeveloperTools.DeveloperToolErrorTrace op err ->
        Text.unwords ["developer-tool error:", op, "-", err]

renderSkillsToolboxTrace :: SkillsToolbox.Trace -> Text
renderSkillsToolboxTrace tr = case tr of
    SkillsToolbox.SkillsLoadingTrace _sources ->
        "(skills loading...)"
    SkillsToolbox.SkillLoadedTrace name _path ->
        Text.unwords ["skill loaded:", SkillsTypes.skillNameToText name]
    SkillsToolbox.SkillLoadErrorTrace path err ->
        Text.unwords ["skill load error:", Text.pack path, "-", err]
    SkillsToolbox.ScriptExecutedTrace sName scriptName exitCode ->
        Text.unwords ["script executed:", SkillsTypes.skillNameToText sName, "/", SkillsTypes.unScriptName scriptName, "(exit:", Text.pack (show exitCode), ")"]
    SkillsToolbox.ScriptExecutionErrorTrace sName scriptName err ->
        Text.unwords ["script execution error:", SkillsTypes.skillNameToText sName, "/", SkillsTypes.unScriptName scriptName, "-", err]
    SkillsToolbox.SkillEnabledTrace name ->
        Text.unwords ["skill enabled:", SkillsTypes.skillNameToText name]
    SkillsToolbox.SkillDisabledTrace name ->
        Text.unwords ["skill disabled:", SkillsTypes.skillNameToText name]

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
    Runtime.RunToolTrace _ (Tools.DeveloperToolsTrace devTr) ->
        Text.unwords ["developer-tool:", renderDeveloperToolboxTrace devTr]
    Runtime.RunToolTrace _ (Tools.LuaToolsTrace luaTrace) ->
        Text.unwords ["lua-tool:", renderLuaToolboxTrace luaTrace]
    Runtime.LLMTrace _ (OpenAI.HttpClientTrace _) -> "(http)"
    Runtime.LLMTrace _ (OpenAI.CallChatCompletion _ bytes) ->
        Text.unwords ["to: llm", "[" <> formatBytes bytes <> "]"]
    Runtime.LLMTrace _ (OpenAI.GotChatCompletion x bytes) ->
        Text.unwords ["from: llm", "[" <> formatBytes bytes <> "]", jsonTxt x]
    Runtime.LLMTrace _ (OpenAI.OverloadedBackoff attempt delay) ->
        Text.unwords ["moonshot/kimi overloaded:", "retry " <> Text.pack (show attempt), "in " <> Text.pack (show delay) <> "s"]
    Runtime.ChildrenTrace sub ->
        Text.unwords ["(", Runtime.traceAgentSlug sub, ")", renderAgentTrace sub]
    -- New ConversationTrace events
    Runtime.SubAgentCallTrace subSlug subConv sessId ->
        Text.unlines
            [ Text.unwords ["sub-agent call:", subSlug]
            , Text.unwords ["  conversation:", Text.pack (show subConv)]
            , Text.unwords ["  session:", Text.pack (show sessId)]
            ]
    Runtime.SubAgentReturnTrace subSlug subConv ->
        Text.unwords ["sub-agent return:", subSlug, "conv=", Text.pack (show subConv)]
  where
    jsonTxt :: (Aeson.ToJSON a) => a -> Text
    jsonTxt = Text.decodeUtf8 . LByteString.toStrict . Aeson.encode

