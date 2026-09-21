{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Partial application of tool arguments.

A 'Binding' ties one tool argument to a fixed value. Applying a list of
bindings to a 'ToolRegistration' removes the bound arguments from the
tool's LLM-visible schema, and merges the bound values into the call's
argument object just before dispatch, with the bound value always
winning over anything the LLM sent.

This is the generic combinator mentioned in @todos/tool-partial-application.md@,
Phase 1: it supports 'Literal' bindings only. 'Param' bindings (resolved from
the per-call 'ToolExecutionContext') land in a later phase.
-}
module System.Agents.Tools.Bindings (
    module System.Agents.Tools.Bindings.Types,
    applyBindings,
) where

import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import Data.Aeson.Key (fromText)
import qualified Data.Aeson.KeyMap as KeyMap

import System.Agents.ToolRegistration (ToolRegistration (..))
import System.Agents.ToolSchema (ParamProperty (..), ToolDescription (..), ToolName (..))
import System.Agents.Tools.Base (Tool (..))
import System.Agents.Tools.Bindings.Types

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
                , innerTool = wrapTool relevant tr.innerTool
                , findTool = \call -> fmap (wrapTool relevant) (tr.findTool call)
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

wrapTool :: [Binding] -> Tool trace call -> Tool trace call
wrapTool bindings (Tool def run) =
    Tool def $ \tracer ctx val -> run tracer ctx (mergeBindings bindings val)

mergeBindings :: [Binding] -> Value -> Value
mergeBindings bindings val =
    case val of
        Aeson.Object o -> Aeson.Object (KeyMap.union boundObj o)
        other -> other
  where
    boundObj = KeyMap.fromList [(fromText b.bindArg, v) | b <- bindings, Literal v <- [b.bindValue]]
