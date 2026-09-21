{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Partial application of tool arguments.

A 'Binding' ties one tool argument to a fixed value or to an agent
parameter. Applying a list of bindings to a 'ToolRegistration' removes
the bound arguments from the tool's LLM-visible schema, and merges the
bound values into the call's argument object just before dispatch, with
the bound value always winning over anything the LLM sent.

'Param' bindings are resolved from the per-call 'ToolExecutionContext'
('ctxParams'), so the same registration serves every session with
whatever values that session's context carries (see
@todos/tool-partial-application.md@, Phase 2). A required parameter with
no value at call time fails the call before it reaches the underlying
tool: the model gets a generic \"tool not found\" result, and the operator
gets a trace naming the parameter (D8: the model is never told why).
-}
module System.Agents.Tools.Bindings (
    module System.Agents.Tools.Bindings.Types,
    applyBindings,
    specializeProcessParams,
    narrowExposedSchema,
    exposedSecretBindings,
) where

import qualified Data.Aeson as Aeson
import Data.Aeson.Key (fromText)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Prod.Tracer (runTracer)

import System.Agents.ToolRegistration (Tool, ToolRegistration (..), Trace (..))
import System.Agents.ToolSchema (ParamProperty (..), ToolDescription (..), ToolName (..))
import System.Agents.Tools.Base (CallResult (..))
import qualified System.Agents.Tools.Base as ToolBase
import System.Agents.Tools.Bindings.Types
import System.Agents.Tools.Context (ToolExecutionContext (..))
import System.Agents.Tools.Params.Types (ParamName, ParamValue (..))

{- | Applies the bindings that match this registration's tool name: drops
the bound arguments from the schema, and merges their values into every
call before it reaches the underlying tool. Bound values always win over
whatever the LLM sent. A registration with no matching bindings is
returned unchanged.
-}
applyBindings :: [Binding] -> ToolRegistration -> ToolRegistration
applyBindings allBindings tr =
    case relevant of
        [] -> tr
        _ ->
            tr
                { declareTool = reduceSchema staticallyHidden tr.declareTool
                , innerTool = wrapTool toolNameText relevant () tr.innerTool
                , findTool = \call -> fmap (wrapTool toolNameText relevant call) (tr.findTool call)
                }
  where
    ToolName toolNameText = tr.declareTool.toolDescriptionName
    relevant = bindingsForTool toolNameText allBindings
    -- 'Expose' bindings (§7) are deliberately left out of the *static*
    -- reduction: whether their argument is hidden depends on whether the
    -- parameter is bound for the current session, which is decided fresh on
    -- every read by 'narrowExposedSchema', not once when the registration is
    -- built (D3's exception).
    staticallyHidden = filter ((/= Expose) . bindWhenUnbound) relevant

reduceSchema :: [Binding] -> ToolDescription -> ToolDescription
reduceSchema bindings td =
    td
        { toolDescriptionParamProperties =
            filter (\p -> propertyKey p `notElem` boundArgs) td.toolDescriptionParamProperties
        }
  where
    boundArgs = map bindArg bindings

{- | Re-hides an 'Expose' binding's argument from a registration's schema
when its parameter happens to be bound in the given 'Params' (§7). Meant to
be called fresh on every read of the tool list (see
'System.Agents.Combinators.ProgressiveDisclosure.agentEvaluateActiveTools'),
against the current session's resolved parameters, so the same shared
registration can show a different schema to different sessions without ever
being mutated. A registration with no 'Expose' binding for a currently-bound
parameter is returned unchanged.

The value itself is not merged here: 'wrapTool' (via 'applyBindings') already
merges any bound value at call time, from the call's own 'ToolExecutionContext',
independently of what the schema showed when the model chose to call the tool.
-}
narrowExposedSchema :: Map.Map ParamName ParamValue -> [Binding] -> ToolRegistration -> ToolRegistration
narrowExposedSchema params allBindings tr =
    case boundNow of
        [] -> tr
        _ -> tr{declareTool = reduceSchema boundNow tr.declareTool}
  where
    ToolName toolNameText = tr.declareTool.toolDescriptionName
    exposable = filter ((== Expose) . bindWhenUnbound) (bindingsForTool toolNameText allBindings)
    boundNow = filter isResolved exposable
    isResolved :: Binding -> Bool
    isResolved b = case bindValue b of
        Literal _ -> True
        Param p -> Map.member p params

{- | 'Expose' bindings whose parameter is declared @secret@ (§6): exposing the
argument back to the model would let it simply retype the secret value in
plain text, defeating the reason it was marked secret in the first place.
Meant for a load-time check (an error in @agents-exe check@, like the other
binding-safety checks), not for filtering at read time.
-}
exposedSecretBindings :: Set.Set ParamName -> [Binding] -> [Binding]
exposedSecretBindings secretParamNames = filter isExposedSecret
  where
    isExposedSecret :: Binding -> Bool
    isExposedSecret b =
        bindWhenUnbound b == Expose && case bindValue b of
            Param p -> p `Set.member` secretParamNames
            Literal _ -> False

wrapTool :: Text -> [Binding] -> call -> Tool call -> Tool call
wrapTool toolName bindings call (ToolBase.Tool def run) =
    ToolBase.Tool def $ \tracer ctx val ->
        case resolveBindings ctx.ctxParams bindings of
            Left (arg, paramName) -> do
                runTracer tracer (BindingsTrace (UnboundRequiredParameter toolName arg paramName))
                pure (ToolNotFound call)
            Right pairs -> run tracer ctx (mergeValue pairs val)

-- | Resolves every binding's value, or the (argument, parameter) that a Fail policy could not resolve.
resolveBindings :: Map.Map Text ParamValue -> [Binding] -> Either (Text, Text) [(Aeson.Key, Aeson.Value)]
resolveBindings params = go []
  where
    go :: [(Aeson.Key, Aeson.Value)] -> [Binding] -> Either (Text, Text) [(Aeson.Key, Aeson.Value)]
    go acc [] = Right acc
    go acc (b : bs) = case b.bindValue of
        Literal v -> go ((fromText b.bindArg, v) : acc) bs
        Param p -> case Map.lookup p params of
            Just pv -> go ((fromText b.bindArg, pv.pvValue) : acc) bs
            Nothing -> case b.bindWhenUnbound of
                Fail -> Left (b.bindArg, p)
                -- Omit: the argument is left out, silently.
                -- Expose (§7): same merge behaviour — nothing to override,
                -- so whatever the model itself sent for the argument (now
                -- visible in its schema, per 'narrowExposedSchema') passes
                -- through untouched.
                _ -> go acc bs

mergeValue :: [(Aeson.Key, Aeson.Value)] -> Aeson.Value -> Aeson.Value
mergeValue pairs val =
    case val of
        Aeson.Object o -> Aeson.Object (KeyMap.union (KeyMap.fromList pairs) o)
        other -> other

{- | Rewrites every 'Param' binding whose parameter resolved to a
process-level value into a 'Literal', so it needs no 'ctxParams' at call
time. Used for process-scope parameters (Phase 2): the value is fixed for
the life of the process, so there is no reason to re-resolve it on every
call. Bindings referencing a parameter not in the map (session/message
scope, not yet resolvable at process level) are left as 'Param', and
resolve normally against 'ctxParams' once that is wired (Phase 4).
-}
specializeProcessParams :: Map.Map Text ParamValue -> [Binding] -> [Binding]
specializeProcessParams resolved = map go
  where
    go b = case b.bindValue of
        Param p | Just pv <- Map.lookup p resolved -> b{bindValue = Literal pv.pvValue}
        _ -> b
