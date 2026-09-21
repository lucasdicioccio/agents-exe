{-# LANGUAGE OverloadedStrings #-}

{- | Resolving agent parameters against operator-supplied (process-level)
values and declared defaults (@todos/tool-partial-application.md@, Phase 2).

Session- and message-scope resolution (a client overlaying values per
@agents-server@ session or run) is Phase 4 and is not implemented here: a
parameter with one of those scopes simply has no process-level value unless
the operator also supplied one, which is the correct, safe default (any
tool argument bound to it fails the call rather than silently omitting it,
per 'System.Agents.Tools.Bindings.Types.WhenUnbound').
-}
module System.Agents.Tools.Params (
    resolveProcessParameters,
) where

import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import System.Agents.Tools.Params.Types (
    ParamScope (..),
    ParamValue (..),
    Params,
    ParameterDecl (..),
    ProcessParams,
    ProcessValue (..),
 )
import System.Agents.Tools.Secrets (resolveSecretSource)

{- | Resolves every declared parameter against the operator's overrides and
the declaration's default, in the order documented in
@todos/tool-partial-application.md@ §1: an operator override wins, then the
default (resolved once here), then the parameter is left unbound.

Returns the resolved values plus the names of required parameters that
could not be resolved (the caller decides whether that is fatal: a
process-scope parameter must fail startup, a session/message-scope one is
simply left for a later phase to fill in).
-}
resolveProcessParameters ::
    -- | Path to the API keys file, for defaults sourced from 'ApiKeySource'
    FilePath ->
    ProcessParams ->
    [ParameterDecl] ->
    IO (Params, [Text])
resolveProcessParameters apiKeysFile overrides decls = do
    resolved <- mapM resolveOne decls
    let params = Map.fromList [(p.paramName, v) | (p, Just v) <- zip decls resolved]
        -- Only a process-scope parameter can be fatal here: session/message-scope
        -- ones are expected to be filled in later (agents-server, Phase 4).
        missing =
            [ p.paramName
            | (p, Nothing) <- zip decls resolved
            , p.paramRequired
            , p.paramScope == ScopeProcess
            ]
    pure (params, missing)
  where
    resolveOne :: ParameterDecl -> IO (Maybe ParamValue)
    resolveOne decl = case Map.lookup decl.paramName overrides of
        Just ov -> pure $ Just (ParamValue ov.pvRawValue decl.paramSecret)
        Nothing -> case decl.paramDefault of
            Nothing -> pure Nothing
            Just src -> do
                result <- resolveSecretSource apiKeysFile src
                case result of
                    Left _err -> pure Nothing
                    Right txt -> pure $ Just (ParamValue (Aeson.String txt) decl.paramSecret)
