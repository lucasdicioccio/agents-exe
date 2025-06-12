module System.Agents.CLI.State where

import qualified Data.List as List
import Data.List.NonEmpty
import System.Agents.Dialogues

data CliState
    = CliState
    { loadedAgents :: NonEmpty LoadedAgent
    , ongoingConversations :: [OngoingConversation]
    }

insertConversation :: CliState -> OngoingConversation -> CliState
insertConversation st conv =
    st{ongoingConversations = conv : st.ongoingConversations}

removeConversation :: CliState -> OngoingConversation -> CliState
removeConversation st conv =
    st{ongoingConversations = List.filter (\conv2 -> conv2.conversationId /= conv.conversationId) st.ongoingConversations}
