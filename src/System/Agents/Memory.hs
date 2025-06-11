module System.Agents.Memory where

import Data.Aeson (ToJSON (..), (.=))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import System.Agents.Base
import qualified System.Agents.LLMs.OpenAI as LLM
import qualified System.Agents.Runtime as Runtime

data MemoryItem
    = MemoryItem
    { rootConversationId :: ConversationId
    , conversationId :: ConversationId
    , stepId :: Runtime.StepId
    , agentSlug :: AgentSlug
    , agentId :: AgentId
    , parentConversationId :: Maybe ConversationId
    , parentAgentSlug :: Maybe AgentSlug
    , parentAgentId :: Maybe AgentId
    , pendingQuery :: Runtime.PendingQuery
    , llmHistory :: LLM.History
    }
    deriving (Show)

instance ToJSON MemoryItem where
    toJSON m =
        Aeson.object
            [ "root_conversation_id" .= m.rootConversationId
            , "conversation_id" .= m.conversationId
            , "agent_slug" .= m.agentSlug
            , "agent_execution_id" .= m.agentId
            , "parent_conversation_id" .= m.parentConversationId
            , "parent_agent_slug" .= m.parentAgentSlug
            , "parent_agent_execution_id" .= m.parentAgentId
            , "pending_query" .= encodeQuery m.pendingQuery
            , "llm_history" .= fmap encodeHistoryItem m.llmHistory
            ]
      where
        encodeQuery :: Runtime.PendingQuery -> Aeson.Value
        encodeQuery (Runtime.SomeQuery txt) =
            Aeson.object
                [ "type" .= ("query" :: Text)
                , "q" .= txt
                ]
        encodeQuery (Runtime.GaveToolAnswers) =
            Aeson.object
                [ "type" .= ("gave-tool-answers" :: Text)
                ]
        encodeQuery (Runtime.Done) =
            Aeson.object
                [ "type" .= ("done" :: Text)
                ]
        encodeHistoryItem :: LLM.HistoryItem -> Aeson.Value
        encodeHistoryItem (LLM.PromptAnswered prompt rsp) =
            Aeson.object
                [ "type" .= ("prompt-answered" :: Text)
                , "prompt" .= prompt
                , "raw" .= rsp.rawResponse
                , "msg" .= rsp.chosenMessage
                ]
        encodeHistoryItem (LLM.ToolCalled (tool, res)) =
            Aeson.object
                [ "type" .= ("tool-called" :: Text)
                , "raw" .= tool.rawToolCall
                , "tool" .= tool.toolCallFunction.toolCallFunctionName.getToolName
                , "args" .= tool.toolCallFunction.toolCallFunctionArgs
                , "result" .= Text.decodeUtf8 res
                ]
