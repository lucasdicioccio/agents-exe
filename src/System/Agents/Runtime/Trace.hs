module System.Agents.Runtime.Trace where

import Data.Text (Text)

import System.Agents.Base
import qualified System.Agents.LLMs.OpenAI as OpenAI
import System.Agents.Session.Base (SessionId)
import System.Agents.Tools
import qualified System.Agents.Tools.BashToolbox as BashToolbox
import qualified System.Agents.Tools.DeveloperToolbox as DeveloperToolbox
import qualified System.Agents.Tools.LuaToolbox as LuaToolbox
import qualified System.Agents.Tools.Skills.Toolbox as SkillsToolbox
import qualified System.Agents.Tools.SqliteToolbox as SqliteToolbox
import qualified System.Agents.Tools.SystemToolbox as SystemToolbox

-------------------------------------------------------------------------------

{- | Main trace type for all agent system events.

This type captures trace events from various sources:
* Agent loading and conversation events
* Builtin toolbox traces (SQLite, System, Developer, Lua, Skills)
* Sub-agent traces for recursive agent calls

The 'AgentTrace_SubAgentCall' constructor specifically captures the relationship
between parent and child agent calls, enabling proper trace tree
visualization for recursive agent invocations.
-}
data Trace
    = AgentTrace_Loading !AgentSlug !AgentId !BashToolbox.Trace
    | AgentTrace_Conversation !AgentSlug !AgentId !ConversationId !ConversationTrace
    | BuiltinToolboxTrace !Text !SqliteToolbox.Trace
    | BuiltinToolboxInitError !Text !String
    | LuaToolboxInitError !Text !String
    | SystemToolboxTrace !Text !SystemToolbox.Trace
    | DeveloperToolboxTrace !Text !DeveloperToolbox.Trace
    | LuaToolboxTrace !Text !LuaToolbox.Trace
    | SkillsToolboxTrace !Text !SkillsToolbox.Trace
    | SkillsToolboxInitError !Text !String
    | {- | Trace event for sub-agent (recursive) calls.
      Captures the relationship between parent and child agent calls
      along with detailed sub-agent lifecycle events.
      -}
      AgentTrace_SubAgentCall
        { subAgentCallerSlug :: AgentSlug
        -- ^ The slug of the calling agent
        , subAgentCallerId :: AgentId
        -- ^ The ID of the calling agent
        , subAgentParentConvId :: ConversationId
        -- ^ The parent conversation ID
        , subAgentSubSlug :: AgentSlug
        -- ^ The slug of the sub-agent being called
        , subAgentSubConvId :: ConversationId
        -- ^ The sub-agent's conversation ID
        , subAgentTrace :: SubAgentTrace
        -- ^ The detailed sub-agent trace event
        }
    deriving (Show)

traceAgentSlug :: Trace -> AgentSlug
traceAgentSlug (AgentTrace_Loading aSlug _ _) = aSlug
traceAgentSlug (AgentTrace_Conversation aSlug _ _ _) = aSlug
traceAgentSlug (BuiltinToolboxTrace _ _) = "builtin"
traceAgentSlug (BuiltinToolboxInitError tName _) = tName
traceAgentSlug (LuaToolboxInitError tName _) = tName
traceAgentSlug (SystemToolboxTrace tName _) = tName
traceAgentSlug (DeveloperToolboxTrace tName _) = tName
traceAgentSlug (LuaToolboxTrace tName _) = tName
traceAgentSlug (SkillsToolboxTrace tName _) = tName
traceAgentSlug (SkillsToolboxInitError tName _) = tName
traceAgentSlug AgentTrace_SubAgentCall{subAgentCallerSlug = callerSlug} = callerSlug

traceAgentId :: Trace -> AgentId
traceAgentId (AgentTrace_Loading _ aId _) = aId
traceAgentId (AgentTrace_Conversation _ aId _ _) = aId
traceAgentId (BuiltinToolboxTrace _ _) = AgentId (read "00000000-0000-0000-0000-000000000000")
traceAgentId (BuiltinToolboxInitError _ _) = AgentId (read "00000000-0000-0000-0000-000000000000")
traceAgentId (LuaToolboxInitError _ _) = AgentId (read "00000000-0000-0000-0000-000000000000")
traceAgentId (SystemToolboxTrace _ _) = AgentId (read "00000000-0000-0000-0000-000000000000")
traceAgentId (DeveloperToolboxTrace _ _) = AgentId (read "00000000-0000-0000-0000-000000000000")
traceAgentId (LuaToolboxTrace _ _) = AgentId (read "00000000-0000-0000-0000-000000000000")
traceAgentId (SkillsToolboxTrace _ _) = AgentId (read "00000000-0000-0000-0000-000000000000")
traceAgentId (SkillsToolboxInitError _ _) = AgentId (read "00000000-0000-0000-0000-000000000000")
traceAgentId AgentTrace_SubAgentCall{subAgentCallerId = callerId} = callerId

traceConversationId :: Trace -> Maybe ConversationId
traceConversationId (AgentTrace_Loading _ _ _) = Nothing
traceConversationId (AgentTrace_Conversation _ _ cId _) = Just cId
traceConversationId (BuiltinToolboxTrace _ _) = Nothing
traceConversationId (BuiltinToolboxInitError _ _) = Nothing
traceConversationId (LuaToolboxInitError _ _) = Nothing
traceConversationId (SystemToolboxTrace _ _) = Nothing
traceConversationId (DeveloperToolboxTrace _ _) = Nothing
traceConversationId (LuaToolboxTrace _ _) = Nothing
traceConversationId (SkillsToolboxTrace _ _) = Nothing
traceConversationId (SkillsToolboxInitError _ _) = Nothing
traceConversationId AgentTrace_SubAgentCall{subAgentParentConvId = parentConvId} = Just parentConvId

{- | Detailed trace events for sub-agent (recursive) calls.

These events capture the full lifecycle of a sub-agent execution:
* When the sub-agent starts
* LLM calls made by the sub-agent
* Tool calls made by the sub-agent
* Completion with final response
* Failure with error message

This enables comprehensive debugging of recursive agent calls.
-}
data SubAgentTrace
    = -- | Sub-agent session started
      SubAgentStarted
        { subAgentSessionId :: SessionId
        -- ^ The unique session ID for this sub-agent execution
        }
    | -- | LLM trace from sub-agent
      SubAgentLLMTrace
        { subAgentStepId :: StepId
        -- ^ The step ID within the sub-agent's execution
        , subAgentLLMTrace :: OpenAI.Trace
        -- ^ The OpenAI trace event
        }
    | -- | Tool trace from sub-agent
      SubAgentToolTrace
        { subAgentStepId :: StepId
        -- ^ The step ID within the sub-agent's execution
        , subAgentToolTrace :: ToolTrace
        -- ^ The tool execution trace
        }
    | -- | Sub-agent completed successfully with final response
      SubAgentCompleted
        { subAgentResponse :: Text
        -- ^ The final text response from the sub-agent
        }
    | -- | Sub-agent failed with an error
      SubAgentFailed
        { subAgentError :: Text
        -- ^ The error message explaining the failure
        }
    deriving (Show)

data ConversationTrace
    = NewConversation
    | WaitingForPrompt
    | LLMTrace !StepId !OpenAI.Trace
    | RunToolTrace !StepId !ToolTrace
    | ChildrenTrace !Trace
    deriving (Show)

