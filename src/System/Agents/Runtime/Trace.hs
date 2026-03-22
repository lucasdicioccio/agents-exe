module System.Agents.Runtime.Trace where

import Data.Text (Text)

import System.Agents.Base
import qualified System.Agents.LLMs.OpenAI as OpenAI
import System.Agents.Session.Types (SessionId)
import System.Agents.Tools
import qualified System.Agents.Tools.BashToolbox as BashToolbox
import qualified System.Agents.Tools.DeveloperToolbox as DeveloperToolbox
import qualified System.Agents.Tools.LuaToolbox as LuaToolbox
import qualified System.Agents.Tools.Skills.Toolbox as SkillsToolbox
import qualified System.Agents.Tools.SqliteToolbox as SqliteToolbox
import qualified System.Agents.Tools.SystemToolbox as SystemToolbox

-------------------------------------------------------------------------------
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
    -- NEW TRACE EVENTS for sub-agent correlation:
    | AgentTrace_SubAgentStarted
        !AgentSlug -- ^ Parent agent slug
        !AgentId -- ^ Parent agent ID
        !ConversationId -- ^ Parent conversation ID
        !AgentSlug -- ^ Sub-agent slug
        !ConversationId -- ^ Sub-agent conversation ID
        !SessionId -- ^ Sub-agent session ID
    | AgentTrace_SubAgentCompleted
        !AgentSlug -- ^ Parent agent slug
        !AgentId -- ^ Parent agent ID
        !ConversationId -- ^ Parent conversation ID
        !AgentSlug -- ^ Sub-agent slug
        !ConversationId -- ^ Sub-agent conversation ID
        !SessionId -- ^ Sub-agent session ID
    | AgentTrace_SubAgentFailed
        !AgentSlug -- ^ Parent agent slug
        !AgentId -- ^ Parent agent ID
        !ConversationId -- ^ Parent conversation ID
        !AgentSlug -- ^ Sub-agent slug
        !ConversationId -- ^ Sub-agent conversation ID
        !SessionId -- ^ Sub-agent session ID
        !Text -- ^ Error message
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
traceAgentSlug (AgentTrace_SubAgentStarted pSlug _ _ _ _ _) = pSlug
traceAgentSlug (AgentTrace_SubAgentCompleted pSlug _ _ _ _ _) = pSlug
traceAgentSlug (AgentTrace_SubAgentFailed pSlug _ _ _ _ _ _) = pSlug

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
traceAgentId (AgentTrace_SubAgentStarted _ pId _ _ _ _) = pId
traceAgentId (AgentTrace_SubAgentCompleted _ pId _ _ _ _) = pId
traceAgentId (AgentTrace_SubAgentFailed _ pId _ _ _ _ _) = pId

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
traceConversationId (AgentTrace_SubAgentStarted _ _ pConvId _ _ _) = Just pConvId
traceConversationId (AgentTrace_SubAgentCompleted _ _ pConvId _ _ _) = Just pConvId
traceConversationId (AgentTrace_SubAgentFailed _ _ pConvId _ _ _ _) = Just pConvId

-- | Get the parent conversation ID from a trace, if applicable.
-- This returns the conversation ID of the parent agent that initiated
-- a sub-agent call.
traceParentConversationId :: Trace -> Maybe ConversationId
traceParentConversationId (AgentTrace_SubAgentStarted _ _ parentConvId _ _ _) =
    Just parentConvId
traceParentConversationId (AgentTrace_SubAgentCompleted _ _ parentConvId _ _ _) =
    Just parentConvId
traceParentConversationId (AgentTrace_SubAgentFailed _ _ parentConvId _ _ _ _) =
    Just parentConvId
traceParentConversationId _ = Nothing

-- | Get the sub-agent conversation ID from a trace, if applicable.
-- This returns the conversation ID of the sub-agent being called.
traceSubAgentConversationId :: Trace -> Maybe ConversationId
traceSubAgentConversationId (AgentTrace_SubAgentStarted _ _ _ _ subConvId _) =
    Just subConvId
traceSubAgentConversationId (AgentTrace_SubAgentCompleted _ _ _ _ subConvId _) =
    Just subConvId
traceSubAgentConversationId (AgentTrace_SubAgentFailed _ _ _ _ subConvId _ _) =
    Just subConvId
traceSubAgentConversationId _ = Nothing

-- | Check if a trace represents a sub-agent lifecycle event.
-- Returns True for SubAgentStarted, SubAgentCompleted, and SubAgentFailed events.
isSubAgentTrace :: Trace -> Bool
isSubAgentTrace (AgentTrace_SubAgentStarted {}) = True
isSubAgentTrace (AgentTrace_SubAgentCompleted {}) = True
isSubAgentTrace (AgentTrace_SubAgentFailed {}) = True
isSubAgentTrace _ = False

data ConversationTrace
    = NewConversation
    | WaitingForPrompt
    | LLMTrace !StepId !OpenAI.Trace
    | RunToolTrace !StepId !ToolTrace
    | ChildrenTrace !Trace
    -- NEW: Sub-agent call correlation traces
    | SubAgentCallTrace
        !Text -- ^ Sub-agent slug
        !ConversationId -- ^ Sub-agent conversation ID
        !SessionId -- ^ Sub-agent session ID
    | SubAgentReturnTrace
        !Text -- ^ Sub-agent slug
        !ConversationId -- ^ Sub-agent conversation ID
    deriving (Show)

