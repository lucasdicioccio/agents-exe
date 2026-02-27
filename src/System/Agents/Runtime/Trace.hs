module System.Agents.Runtime.Trace where

import System.Agents.Base
import qualified System.Agents.LLMs.OpenAI as OpenAI
import System.Agents.Tools
import qualified System.Agents.Tools.BashToolbox as BashToolbox

-------------------------------------------------------------------------------
data Trace
    = AgentTrace_Loading !AgentSlug !AgentId !BashToolbox.Trace
    | AgentTrace_Conversation !AgentSlug !AgentId !ConversationId !ConversationTrace
    deriving (Show)

traceAgentSlug :: Trace -> AgentSlug
traceAgentSlug (AgentTrace_Loading aSlug _ _) = aSlug
traceAgentSlug (AgentTrace_Conversation aSlug _ _ _) = aSlug

traceAgentId :: Trace -> AgentId
traceAgentId (AgentTrace_Loading _ aId _) = aId
traceAgentId (AgentTrace_Conversation _ aId _ _) = aId

traceConversationId :: Trace -> Maybe ConversationId
traceConversationId (AgentTrace_Loading _ _ _) = Nothing
traceConversationId (AgentTrace_Conversation _ _ cId _) = Just cId

data ConversationTrace
    = NewConversation
    | WaitingForPrompt
    | LLMTrace !StepId !OpenAI.Trace
    | RunToolTrace !StepId !ToolTrace
    | ChildrenTrace !Trace
    deriving (Show)
