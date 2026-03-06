{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Agents.Session.Loop where

import System.Agents.Base (ConversationId)
import System.Agents.Session.Base
import System.Agents.Session.Step


-- | Keeps running an agent until it stops.
run :: forall r. ConversationId -> Agent r -> Session -> IO r
run convId agent sess =
    go agent sess
  where
    go :: Agent r -> Session -> IO r
    go agent0 sess0 = do
      (agent1, res) <- runStepM convId agent0 sess0
      case res of
        Left r -> pure r
        Right sess1 -> go agent1 sess1

