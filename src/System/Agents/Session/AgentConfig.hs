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
    matchGlob,
) where

import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as Text

import qualified System.Agents.Base as Base
import System.Agents.Session.Base

{- | Build a runtime 'ToolCallPolicy' from a declarative policy config.

The base disposition still comes from the first matching 'tpcRules' entry
(or 'tpcDefaultDisposition'), exact tool name match, first match wins. Every
'tpcWrappers' rule whose glob matches the call's tool name then contributes
its decorators, in file order (outermost first), composed as a single
'Decorate' around that base.
-}
buildToolCallPolicy :: Base.ToolCallPolicyConfig -> ToolCallPolicy
buildToolCallPolicy cfg _ctx call =
    let base =
            maybe (Base.tpcDefaultDisposition cfg) Base.tprDisposition $
                find (\rule -> Base.tprToolName rule == toolName) (Base.tpcRules cfg)
        decorators =
            concat
                [ Base.twrDecorators rule
                | rule <- Base.tpcWrappers cfg
                , wrapperMatches (Base.twrMatch rule)
                ]
     in if null decorators then base else Decorate decorators base
  where
    toolName = llmToolCallName call
    wrapperMatches m = maybe True (`matchGlob` toolName) (Base.wmTool m)

{- | Match a name against a glob pattern that supports only the @*@
wildcard (matches any run of characters, including none).
-}
matchGlob :: Text -> Text -> Bool
matchGlob pattern = go (Text.unpack pattern) . Text.unpack
  where
    go [] [] = True
    go ('*' : ps) cs = go ps cs || case cs of
        [] -> False
        (_ : rest) -> go ('*' : ps) rest
    go (p : ps) (c : cs) = p == c && go ps cs
    go [] (_ : _) = False
    go (_ : _) [] = False

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
