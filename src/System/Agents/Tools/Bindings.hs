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
) where

import qualified Data.Aeson as Aeson
import Data.Aeson.Key (fromText)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Prod.Tracer (runTracer)

import System.Agents.ToolRegistration (Tool, ToolRegistration (..), Trace (..))
import System.Agents.ToolSchema (ParamProperty (..), ToolDescription (..), ToolName (..))
import System.Agents.Tools.Base (CallResult (..))
import qualified System.Agents.Tools.Base as ToolBase
import System.Agents.Tools.Bindings.Types
import System.Agents.Tools.Context (ToolExecutionContext (..))
import System.Agents.Tools.Params.Types (ParamValue (..))

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
                { declareTool = reduceSchema relevant tr.declareTool
                , innerTool = wrapTool toolNameText relevant () tr.innerTool
                , findTool = \call -> fmap (wrapTool toolNameText relevant call) (tr.findTool call)
                }
  where
    ToolName toolNameText = tr.declareTool.toolDescriptionName
    relevant = bindingsForTool toolNameText allBindings

reduceSchema :: [Binding] -> ToolDescription -> ToolDescription
reduceSchema bindings td =
    td
        { toolDescriptionParamProperties =
            filter (\p -> propertyKey p `notElem` boundArgs) td.toolDescriptionParamProperties
        }
  where
    boundArgs = map bindArg bindings

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
                _ -> go acc bs -- Omit / (Expose, treated as Omit until Phase 7)

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
