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
    applyAgentDurableConfigWith,
    DeclaredDispositions,
    buildToolCallPolicy,
    buildToolCallPolicyWith,
    withDeclaredDispositions,
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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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
buildToolCallPolicy = buildToolCallPolicyWith Map.empty

{- | The dispositions tools declare for their own calls, keyed by the name
the LLM calls them by. Today only a v2 bash tool declares one, with
@process.background@ (a @RunAsync@).
-}
type DeclaredDispositions = Map Text ToolCallDisposition

{- | 'buildToolCallPolicy', also honouring what tools declare for their own
calls. The base disposition of a call is, first match wins:

1. the agent's 'Base.tpcRules' entry naming the tool: the agent always has
   the last word on a tool it names;
2. the tool's own declaration ('DeclaredDispositions');
3. the agent's 'Base.tpcDefaultDisposition'.

So a tool declaring @process.background@ runs in the background under an
agent whose default is @runSync@, unless that agent has a rule for it.
'Base.tpcWrappers' decorate whichever base was picked, as before.
-}
buildToolCallPolicyWith :: DeclaredDispositions -> Base.ToolCallPolicyConfig -> ToolCallPolicy
buildToolCallPolicyWith declared cfg _ctx call =
    let base =
            maybe (fromMaybe (Base.tpcDefaultDisposition cfg) (Map.lookup toolName declared)) Base.tprDisposition $
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
applyAgentDurableConfig = applyAgentDurableConfigWith Map.empty

{- | 'applyAgentDurableConfig', also honouring what the agent's tools declare
for their own calls. With a @toolCallPolicyConfig@, the policy is
'buildToolCallPolicyWith' (the agent's rule for a tool, then the tool's
declaration, then the agent's default). Without one, a declaring tool's
calls get its declaration and every other call keeps the agent's current
policy ('withDeclaredDispositions').
-}
applyAgentDurableConfigWith :: DeclaredDispositions -> Base.Agent -> Agent r -> Agent r
applyAgentDurableConfigWith declared jsonAgent =
    setMaybe (Base.asyncCallTimeoutSeconds jsonAgent) (\n a -> a{ctxAsyncCallTimeout = Just n})
        . setMaybe (Base.maxConcurrency jsonAgent) (\n a -> a{ctxMaxConcurrency = Just n})
        . setMaybe (Base.asyncYieldStrategy jsonAgent) withAsyncYieldStrategy
        . maybe (withDeclaredDispositions declared) (withToolCallPolicy . buildToolCallPolicyWith declared) (Base.toolCallPolicyConfig jsonAgent)
        . setMaybe (Base.executionMode jsonAgent) withExecutionMode
        . setMaybe (Base.interruptCompletions jsonAgent) withInterruptCompletions
        . setMaybe (Base.mailInToolResult jsonAgent) withMailInToolResult
  where
    setMaybe :: Maybe a -> (a -> b -> b) -> b -> b
    setMaybe m f x = maybe x (`f` x) m

{- | Give the calls of each declaring tool its declared disposition, and
leave every other call to the agent's current policy. An empty map leaves
the agent unchanged.
-}
withDeclaredDispositions :: DeclaredDispositions -> Agent r -> Agent r
withDeclaredDispositions declared agent
    | Map.null declared = agent
    | otherwise =
        agent
            { ctxToolCallPolicy = \ctx call ->
                fromMaybe (ctxToolCallPolicy agent ctx call) (Map.lookup (llmToolCallName call) declared)
            }
