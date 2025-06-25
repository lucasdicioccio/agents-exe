module System.Agents.Runtime.Trace where

import System.Agents.Base
import qualified System.Agents.LLMs.OpenAI as LLM
import System.Agents.ToolRegistration
import System.Agents.Tools
import qualified System.Agents.Tools.Bash as BashTools
import qualified System.Agents.Tools.BashToolbox as BashToolbox
import qualified System.Agents.Tools.IO as IOTools

-------------------------------------------------------------------------------
data Trace
    = AgentTrace_Loading !AgentSlug !AgentId !BashToolbox.Trace
    | AgentTrace_Memorize !AgentSlug !AgentId !ConversationId !MemorizeTrace
    | AgentTrace_Conversation !AgentSlug !AgentId !ConversationId !ConversationTrace
    deriving (Show)

traceAgentSlug :: Trace -> AgentSlug
traceAgentSlug (AgentTrace_Loading slug _ _) = slug
traceAgentSlug (AgentTrace_Conversation slug _ _ _) = slug
traceAgentSlug (AgentTrace_Memorize slug _ _ _) = slug

traceAgentId :: Trace -> AgentId
traceAgentId (AgentTrace_Loading _ aId _) = aId
traceAgentId (AgentTrace_Conversation _ aId _ _) = aId
traceAgentId (AgentTrace_Memorize _ aId _ _) = aId

traceConversationId :: Trace -> Maybe ConversationId
traceConversationId (AgentTrace_Loading _ _ _) = Nothing
traceConversationId (AgentTrace_Conversation _ _ cId _) = Just cId
traceConversationId (AgentTrace_Memorize _ _ cId _) = Just cId

data ConversationTrace
    = NewConversation
    | WaitingForPrompt
    | LLMTrace !StepId !LLM.Trace
    | RunToolTrace !StepId !ToolTrace
    | ChildrenTrace !Trace
    deriving (Show)

data MemorizeTrace
    = Calling !PingPongQuery !LLM.History !StepId
    | GotResponse !PingPongQuery !LLM.History !StepId !LLM.Response
    | InteractionDone !LLM.History !StepId
    deriving (Show)
