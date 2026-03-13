module System.Agents.Runtime.Trace where

import Data.Text (Text)

import System.Agents.Base
import qualified System.Agents.LLMs.OpenAI as OpenAI
import System.Agents.Tools
import qualified System.Agents.Tools.BashToolbox as BashToolbox
import qualified System.Agents.Tools.SqliteToolbox as SqliteToolbox
import qualified System.Agents.Tools.SystemToolbox as SystemToolbox

-------------------------------------------------------------------------------
data Trace
    = AgentTrace_Loading !AgentSlug !AgentId !BashToolbox.Trace
    | AgentTrace_Conversation !AgentSlug !AgentId !ConversationId !ConversationTrace
    | BuiltinToolboxTrace !Text !SqliteToolbox.Trace
    | BuiltinToolboxInitError !Text !String
    | SystemToolboxTrace !Text !SystemToolbox.Trace
    deriving (Show)

traceAgentSlug :: Trace -> AgentSlug
traceAgentSlug (AgentTrace_Loading aSlug _ _) = aSlug
traceAgentSlug (AgentTrace_Conversation aSlug _ _ _) = aSlug
traceAgentSlug (BuiltinToolboxTrace _ _) = "builtin"
traceAgentSlug (BuiltinToolboxInitError name _) = name
traceAgentSlug (SystemToolboxTrace name _) = name

traceAgentId :: Trace -> AgentId
traceAgentId (AgentTrace_Loading _ aId _) = aId
traceAgentId (AgentTrace_Conversation _ aId _ _) = aId
traceAgentId (BuiltinToolboxTrace _ _) = AgentId (read "00000000-0000-0000-0000-000000000000")
traceAgentId (BuiltinToolboxInitError _ _) = AgentId (read "00000000-0000-0000-0000-000000000000")
traceAgentId (SystemToolboxTrace _ _) = AgentId (read "00000000-0000-0000-0000-000000000000")

traceConversationId :: Trace -> Maybe ConversationId
traceConversationId (AgentTrace_Loading _ _ _) = Nothing
traceConversationId (AgentTrace_Conversation _ _ cId _) = Just cId
traceConversationId (BuiltinToolboxTrace _ _) = Nothing
traceConversationId (BuiltinToolboxInitError _ _) = Nothing
traceConversationId (SystemToolboxTrace _ _) = Nothing

data ConversationTrace
    = NewConversation
    | WaitingForPrompt
    | LLMTrace !StepId !OpenAI.Trace
    | RunToolTrace !StepId !ToolTrace
    | ChildrenTrace !Trace
    deriving (Show)

