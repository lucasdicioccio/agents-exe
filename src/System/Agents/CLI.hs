
module System.Agents.CLI where

import Data.Text (Text)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
-- import qualified Data.Aeson.Encode.Pretty as Aeson

import qualified System.Agents.AgentTree as AgentTree
import System.Agents.Base (McpServerDescription (..), McpSimpleBinaryConfiguration (..))
-- import System.Agents.CLI.Aliases ( AliasDefinition, defaultAliases, resolveAliases,)
-- import System.Agents.CLI.Base (makeFileJsonTracer, makeShowLogFileTracer)
import qualified System.Agents.CLI.Check as CheckCmd
-- import qualified System.Agents.CLI.CheckToolCall as CheckToolCallCmd
-- import qualified System.Agents.CLI.Cowsay as CowsayCmd
-- import qualified System.Agents.CLI.DescribeTool as DescribeToolCmd
-- import qualified System.Agents.CLI.EchoPrompt as EchoPromptCmd
-- import qualified System.Agents.CLI.Initialize as InitializeCmd
import qualified System.Agents.CLI.McpServer as McpServerCmd
-- import qualified System.Agents.CLI.New as NewCmd
import qualified System.Agents.CLI.OneShot as OneShotCmd
-- import qualified System.Agents.CLI.Paths as PathsCmd
-- import System.Agents.CLI.PromptScript (PromptScript, PromptScriptDirective (..))
-- import qualified System.Agents.CLI.ReplayToolCall as ReplayToolCallCmd
-- import qualified System.Agents.CLI.SelfDescribe as SelfDescribeCmd
-- import qualified System.Agents.CLI.SessionEdit as SessionEditCmd
-- import qualified System.Agents.CLI.Spec as SpecCmd
import qualified System.Agents.CLI.TUI as TUICmd
-- import qualified System.Agents.CLI.ToolCall as ToolCallCmd
-- import qualified System.Agents.FileLoader as FileLoader
-- import qualified System.Agents.HttpClient as HttpClient
-- import qualified System.Agents.HttpLogger as HttpLogger
import qualified System.Agents.MCP.Client as McpClient (LoopTrace (..))
import qualified System.Agents.CLI.ToolCall as ToolCall
import qualified System.Agents.MCP.Client.Runtime as McpClientRuntime
-- import qualified System.Agents.OneShot as OneShot
-- import System.Agents.SessionPrint (PrintAmount (..), PrintVisibility (..))
-- import qualified System.Agents.SessionPrint as SessionPrint
-- import qualified System.Agents.SessionPrint.Inject as SessionInject
-- import qualified System.Agents.SessionStore as SessionStore
import qualified System.Agents.Tools.McpToolbox as McpToolbox
-------------------------------------------------------------------------------
-- Trace
-------------------------------------------------------------------------------

data Trace
  = AgentTreeTrace !AgentTree.TreeTrace
  | McpServerCmdTrace !McpServerCmd.Trace
  | OneShotCmdTrace !OneShotCmd.Trace
  | ToolCallTrace !ToolCall.Trace
  | TUICmdTrace !TUICmd.Trace
  | CheckCmdTrace !CheckCmd.Trace
  deriving (Show)


{- | Convert TreeTrace to JSON for logging purposes.
This function is simplified for the OS-native migration and may not
cover all trace types that were previously supported.
-}
toJsonTrace :: Trace -> Maybe Aeson.Value
toJsonTrace (AgentTreeTrace x) = case x of
    AgentTree.McpTrace cfg tr -> encodeMcpTrace cfg tr
    AgentTree.OpenAPITrace _desc _v -> Nothing
    AgentTree.PostgRESTrace _desc _v -> Nothing
    AgentTree.DataLoadingTrace _ -> Nothing
    AgentTree.ConfigLoadedTrace _ -> Nothing
    AgentTree.CyclicReferencesWarning cycles ->
        Just $
            Aeson.object
                [ "type" .= ("cyclic-references-warning" :: Text)
                , "cycles" .= cycles
                ]
    AgentTree.ReferenceValidationTrace _ -> Nothing
    AgentTree.RuntimeTrace _ -> Nothing -- TODO: develop here
    AgentTree.BashToolboxTrace _ -> Nothing
    AgentTree.McpToolboxTrace _ -> Nothing
    AgentTree.OpenApiToolboxTrace _ -> Nothing
    AgentTree.PostgRESToolboxTrace _ -> Nothing
    AgentTree.SqliteToolboxTrace _ -> Nothing
    AgentTree.SystemToolboxTrace _ -> Nothing
    AgentTree.DeveloperToolboxTrace _ -> Nothing
    AgentTree.LuaToolboxTrace _ -> Nothing
  where
    encodeMcpTrace :: McpServerDescription -> McpToolbox.Trace -> Maybe Aeson.Value
    encodeMcpTrace (McpSimpleBinary cfg) tr = do
        baseVal <- encodeBaseMcpTrace tr
        Just $
            Aeson.object
                [ "e"
                    .= Aeson.object
                        [ "server" .= cfg.name
                        , "val" .= baseVal
                        ]
                ]

    encodeBaseMcpTrace :: McpToolbox.Trace -> Maybe Aeson.Value
    encodeBaseMcpTrace (McpToolbox.McpClientClientTrace _) = Nothing
    encodeBaseMcpTrace
        (McpToolbox.McpClientRunTrace (McpClientRuntime.RunBufferMoved _ _)) =
            Nothing
    encodeBaseMcpTrace
        (McpToolbox.McpClientRunTrace (McpClientRuntime.RunCommandStart _)) =
            Just $
                Aeson.object
                    [ "x" .= ("program-start" :: Text)
                    ]
    encodeBaseMcpTrace
        (McpToolbox.McpClientRunTrace (McpClientRuntime.RunCommandStopped _ code)) =
            Just $
                Aeson.object
                    [ "x" .= ("program-end" :: Text)
                    , "code-str" .= show code
                    ]
    encodeBaseMcpTrace
        (McpToolbox.McpClientLoopTrace McpClient.ExitingToolCallLoop) =
            Just $
                Aeson.object
                    [ "x" .= ("loop-end" :: Text)
                    ]
    encodeBaseMcpTrace
        (McpToolbox.McpClientLoopTrace (McpClient.ToolsRefreshed _)) =
            Just $
                Aeson.object
                    [ "x" .= ("tools-reloaded" :: Text)
                    ]
    encodeBaseMcpTrace
        (McpToolbox.McpClientLoopTrace (McpClient.StartToolCall n _)) =
            Just $
                Aeson.object
                    [ "x" .= ("tool-call-start" :: Text)
                    , "name" .= n
                    ]
    encodeBaseMcpTrace
        (McpToolbox.McpClientLoopTrace (McpClient.EndToolCall n _ _)) =
            Just $
                Aeson.object
                    [ "x" .= ("tool-call-end" :: Text)
                    , "name" .= n
                    ]
toJsonTrace (McpServerCmdTrace _) = Nothing
toJsonTrace (OneShotCmdTrace _) = Nothing
toJsonTrace (ToolCallTrace _) = Nothing
toJsonTrace (TUICmdTrace _) = Nothing
toJsonTrace (CheckCmdTrace _) = Nothing

