{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Agents.CLI.TraceUtils where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import GHC.IO.Handle (Handle)
import Prod.Tracer (Tracer (..), silent)
import System.IO (stderr, stdout)

import System.Agents.Agent
import qualified System.Agents.LLMs.OpenAI as OpenAI
import qualified System.Agents.Runtime.Trace as Runtime
import qualified System.Agents.Tools as Tools
import qualified System.Agents.Tools.Bash as Tools
import qualified System.Agents.Tools.BashToolbox as BashToolbox
import qualified System.Agents.Tools.IO as Tools

traceSilent :: Tracer IO Trace
traceSilent = silent

tracePrintingTextResponses :: Tracer IO Trace
tracePrintingTextResponses = Tracer f
  where
    f (AgentTrace (Runtime.AgentTrace_Conversation slug _ _ trace)) =
        g [slug] trace
    f _ = pure ()

    g pfx (Runtime.LLMTrace _ (OpenAI.GotChatCompletion x)) =
        case Aeson.parseEither OpenAI.parseLLMResponse x of
            Left _ -> pure ()
            Right rsp ->
                Text.putStrLn $ Text.unwords [Text.intercalate "/" pfx, Maybe.fromMaybe "..." rsp.rspContent]
    g pfx (Runtime.ChildrenTrace (Runtime.AgentTrace_Conversation childSlug _ _ sub)) =
        g (childSlug : pfx) sub
    g _ _ = pure ()

traceUsefulPromptStdout :: Tracer IO Trace
traceUsefulPromptStdout = traceUsefulPromptHandle stdout

traceUsefulPromptStderr :: Tracer IO Trace
traceUsefulPromptStderr = traceUsefulPromptHandle stderr

traceUsefulPromptHandle :: Handle -> Tracer IO Trace
traceUsefulPromptHandle h = Tracer f
  where
    f (AgentTrace tr) =
        Text.hPutStrLn h $ renderAgentTrace tr
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
renderAgentTrace (Runtime.AgentTrace_Memorize slug _ _ tr) =
    Text.unlines
        [ mconcat ["@", slug, ":"]
        , renderMemorizeAgentTrace tr
        ]

renderLoadingAgentTrace :: BashToolbox.Trace -> Text
renderLoadingAgentTrace tr = case tr of
    BashToolbox.ReloadToolsTrace _ -> "(reload-tools...)"
    BashToolbox.BashToolsLoadingTrace _ -> "(reload-tools...)"

renderMemorizeAgentTrace :: Runtime.MemorizeTrace -> Text
renderMemorizeAgentTrace tr = case tr of
    Runtime.Calling _ hist _ ->
        Text.unwords [Text.pack . show $ length hist, ">>>"]
    Runtime.GotResponse _ hist _ rsp ->
        Text.unwords [Text.pack . show $ length hist, Text.decodeUtf8 $ LByteString.toStrict $ Aeson.encode rsp.chosenMessage]
    Runtime.InteractionDone hist _ ->
        Text.unwords [Text.pack . show $ length hist, "<<<"]

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
    Runtime.LLMTrace _ (OpenAI.HttpClientTrace _) -> "(http)"
    Runtime.LLMTrace _ (OpenAI.CallChatCompletion _) ->
        Text.unwords ["to: llm"]
    Runtime.LLMTrace _ (OpenAI.GotChatCompletion x) ->
        Text.unwords ["from: llm", jsonTxt x]
    Runtime.ChildrenTrace sub ->
        Text.unwords ["(", Runtime.traceAgentSlug sub, ")", renderAgentTrace sub]
  where
    jsonTxt :: (Aeson.ToJSON a) => a -> Text
    jsonTxt = Text.decodeUtf8 . LByteString.toStrict . Aeson.encode
