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
    wrapperMatchHolds,
    argPredicateHolds,
    argAtPath,
    llmToolCallArguments,
) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KeyMap
import Data.List (find)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Vector
import Text.Read (readMaybe)

import qualified System.Agents.Base as Base
import System.Agents.Session.Base

{- | Build a runtime 'ToolCallPolicy' from a declarative policy config.

The base disposition still comes from the first matching 'tpcRules' entry
(or 'tpcDefaultDisposition'), exact tool name match, first match wins. Every
'tpcWrappers' rule whose match holds for the call (its glob on the tool name
and its predicate on the arguments, see 'wrapperMatchHolds') then contributes
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
                , wrapperMatchHolds (Base.twrMatch rule) call
                ]
     in if null decorators then base else Decorate decorators base
  where
    toolName = llmToolCallName call

{- | Whether a wrapper rule applies to a call: its tool glob matches the
call's name and its argument predicate holds for the call's arguments. An
absent part holds.
-}
wrapperMatchHolds :: Base.WrapperMatch -> LlmToolCall -> Bool
wrapperMatchHolds m call =
    maybe True (`matchGlob` llmToolCallName call) (Base.wmTool m)
        && maybe True (`argPredicateHolds` llmToolCallArguments call) (Base.wmArgs m)

{- | The arguments of an LLM tool call as JSON: what the model sent, before
any binding is merged in. Providers send them as a JSON-encoded string;
an object is accepted as well. Arguments that are absent or do not decode
are 'Aeson.Null', so that a predicate still gets evaluated against them.
-}
llmToolCallArguments :: LlmToolCall -> Aeson.Value
llmToolCallArguments (LlmToolCall (Aeson.Object obj))
    | Just (Aeson.Object func) <- KeyMap.lookup "function" obj
    , Just args <- KeyMap.lookup "arguments" func =
        decoded args
    | Just args <- KeyMap.lookup "arguments" obj = decoded args
  where
    decoded (Aeson.String txt) = fromMaybe Aeson.Null (Aeson.decodeStrict (Text.encodeUtf8 txt))
    decoded val = val
llmToolCallArguments _ = Aeson.Null

{- | Evaluate an argument predicate against the arguments of a call.

* @equals@: there is a value at the path and it is that JSON value.
* @glob@: there is a string at the path and it matches the glob. A value
  that is not a string never matches.
* @exists@: whether there is a value at the path; @null@ is a value.
* @all@, @any@, @not@: the usual connectives.

Because a missing value fails @equals@ and @glob@, a rule meant to apply
unless an argument has a given value is written with @not@, and then also
applies to calls that leave the argument out.
-}
argPredicateHolds :: Base.ArgPredicate -> Aeson.Value -> Bool
argPredicateHolds predicate args = case predicate of
    Base.ArgEquals path expected -> argAtPath path args == Just expected
    Base.ArgGlob path glob -> case argAtPath path args of
        Just (Aeson.String txt) -> matchGlob glob txt
        _ -> False
    Base.ArgExists path wanted -> isJust (argAtPath path args) == wanted
    Base.ArgAll ps -> all (`argPredicateHolds` args) ps
    Base.ArgAny ps -> any (`argPredicateHolds` args) ps
    Base.ArgNot p -> not (argPredicateHolds p args)

{- | The value at a dot-separated path of object keys and array indices
(@"options.targets.0"@). The empty path is the value itself.
-}
argAtPath :: Text -> Aeson.Value -> Maybe Aeson.Value
argAtPath path
    | Text.null path = Just
    | otherwise = go (Text.splitOn "." path)
  where
    go [] val = Just val
    go (segment : rest) val = case val of
        Aeson.Object obj -> KeyMap.lookup (AesonKey.fromText segment) obj >>= go rest
        Aeson.Array items -> do
            ix <- readMaybe (Text.unpack segment)
            (items Vector.!? ix) >>= go rest
        _ -> Nothing

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
        . setMaybe (Base.interruptCompletions jsonAgent) withInterruptCompletions
        . setMaybe (Base.mailInToolResult jsonAgent) withMailInToolResult
  where
    setMaybe :: Maybe a -> (a -> b -> b) -> b -> b
    setMaybe m f x = maybe x (`f` x) m
