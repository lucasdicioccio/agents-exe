#!/bin/bash
# Fix Session constructor calls by adding the 6th argument (sessionExecutionMode = Nothing)

# Fix src/System/Agents/TUI/Event.hs line 1307
sed -i 's/^session <- liftIO (Session \[\] <\$> newSessionId <\*> pure Nothing <\*> newTurnId <\*> pure (Just 1))$/            session <- liftIO (Session [] <$> newSessionId <*> pure Nothing <*> newTurnId <*> pure (Just 1) <*> pure Nothing)/' src/System/Agents/TUI/Event.hs

# Fix src/System/Agents/AgentTree/OneShotTool.hs line 277
sed -i 's/^session0 <- Session \[\] <\$> newSessionId <\*> pure Nothing <\*> newTurnId <\*> pure (Just 1)$/        session0 <- Session [] <$> newSessionId <*> pure Nothing <*> newTurnId <*> pure (Just 1) <*> pure Nothing/' src/System/Agents/AgentTree/OneShotTool.hs

# Fix src/System/Agents/MCP/Server.hs line 343
sed -i 's/^session0 <- Session \[\] <\$> newSessionId <\*> pure Nothing <\*> newTurnId <\*> pure (Just 1)$/    session0 <- Session [] <$> newSessionId <*> pure Nothing <*> newTurnId <*> pure (Just 1) <*> pure Nothing/' src/System/Agents/MCP/Server.hs

echo "Fixed Session constructor calls"
