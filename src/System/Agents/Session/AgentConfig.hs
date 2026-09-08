{-# LANGUAGE OverloadedStrings #-}

{- | Apply the execution settings of a JSON agent description to a runtime
session 'Agent'.

Every agent builder (one-shot, TUI, durable session commands, sub-agents)
goes through 'applyAgentDurableConfig' so that @executionMode@,
@toolCallPolicyConfig@, @asyncYieldStrategy@ and @maxConcurrency@ behave the
same everywhere.
-}
module System.Agents.Session.AgentConfig (
    applyAgentDurableConfig,
    buildToolCallPolicy,
    llmToolCallName,
) where

import Data.List (find)

import qualified System.Agents.Base as Base
import System.Agents.Session.Base

-- | Build a runtime 'ToolCallPolicy' from a declarative policy config.
buildToolCallPolicy :: Base.ToolCallPolicyConfig -> ToolCallPolicy
buildToolCallPolicy cfg _ctx call =
    maybe (Base.tpcDefaultDisposition cfg) Base.tprDisposition $
        find (\rule -> Base.tprToolName rule == llmToolCallName call) (Base.tpcRules cfg)

{- | Apply execution settings from the JSON agent config to a runtime agent.

Fields absent from the JSON leave the runtime agent unchanged.
-}
applyAgentDurableConfig :: Base.Agent -> Agent r -> Agent r
applyAgentDurableConfig jsonAgent =
    setMaybe (Base.asyncCallTimeoutSeconds jsonAgent) (\n a -> a{ctxAsyncCallTimeout = Just n})
        . setMaybe (Base.maxConcurrency jsonAgent) (\n a -> a{ctxMaxConcurrency = Just n})
        . setMaybe (Base.asyncYieldStrategy jsonAgent) withAsyncYieldStrategy
        . setMaybe (Base.toolCallPolicyConfig jsonAgent) (withToolCallPolicy . buildToolCallPolicy)
        . setMaybe (Base.executionMode jsonAgent) withExecutionMode
  where
    setMaybe :: Maybe a -> (a -> b -> b) -> b -> b
    setMaybe m f x = maybe x (`f` x) m
