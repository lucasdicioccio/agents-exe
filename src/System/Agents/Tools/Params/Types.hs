{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Pure data types for agent parameters
(@todos/tool-partial-application.md@, Phase 2).

A 'ParameterDecl' is a named hole declared by an agent's author; a caller
(process operator, or later an @agents-server@ client) supplies a value for
it. 'Binding's (in "System.Agents.Tools.Bindings.Types") tie tool arguments
to parameters by name.

Kept as a leaf module (no dependency on 'ToolRegistration' or any toolbox)
so that "System.Agents.Base" and "System.Agents.Tools.Context" can both
depend on it without an import cycle.
-}
module System.Agents.Tools.Params.Types (
    ParamName,
    ParamScope (..),
    ParameterDecl (..),
    ParamValue (..),
    Params,
    ProcessValue (..),
    ProcessParams,
) where

import qualified Data.Aeson as Aeson
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

import System.Agents.Tools.Secrets (SecretSource)

-- | The name of a parameter, e.g. @tenant@. Matches @[a-z][a-z0-9_]*@ by convention.
type ParamName = Text

{- | Who may supply a value for a parameter.

* 'ScopeProcess': only the operator, at process start (CLI / environment).
* 'ScopeSession': a client may set it per @agents-server@ session (Phase 4).
* 'ScopeMessage': a client may set it with a request that starts a run;
  the value lasts for that run only (Phase 4).
-}
data ParamScope
    = ScopeProcess
    | ScopeSession
    | ScopeMessage
    deriving (Show, Eq, Ord, Generic)

instance Aeson.ToJSON ParamScope where
    toJSON ScopeProcess = Aeson.String "process"
    toJSON ScopeSession = Aeson.String "session"
    toJSON ScopeMessage = Aeson.String "message"

instance Aeson.FromJSON ParamScope where
    parseJSON (Aeson.String "process") = pure ScopeProcess
    parseJSON (Aeson.String "session") = pure ScopeSession
    parseJSON (Aeson.String "message") = pure ScopeMessage
    parseJSON _ = fail "expecting one of 'process', 'session', 'message'"

-- | A parameter declared by an agent's author.
data ParameterDecl = ParameterDecl
    { paramName :: ParamName
    , paramDescription :: Maybe Text
    , paramSecret :: Bool
    -- ^ The value is never persisted, traced, or returned by the API. Default: False.
    , paramScope :: ParamScope
    -- ^ Default: 'ScopeProcess'.
    , paramRequired :: Bool
    -- ^ Default: True.
    , paramDefault :: Maybe SecretSource
    -- ^ Resolved once at tree load, if no caller-supplied value wins first.
    }
    deriving (Show, Eq, Generic)

instance Aeson.ToJSON ParameterDecl where
    toJSON p =
        Aeson.object $
            [ "name" Aeson..= paramName p
            , "secret" Aeson..= paramSecret p
            , "scope" Aeson..= paramScope p
            , "required" Aeson..= paramRequired p
            ]
                ++ maybe [] (\d -> ["description" Aeson..= d]) (paramDescription p)
                ++ maybe [] (\d -> ["default" Aeson..= d]) (paramDefault p)

instance Aeson.FromJSON ParameterDecl where
    parseJSON = Aeson.withObject "ParameterDecl" $ \o ->
        ParameterDecl
            <$> o Aeson..: "name"
            <*> o Aeson..:? "description"
            <*> (maybe False id <$> o Aeson..:? "secret")
            <*> (maybe ScopeProcess id <$> o Aeson..:? "scope")
            <*> (maybe True id <$> o Aeson..:? "required")
            <*> o Aeson..:? "default"

{- | The value resolved for a parameter, at call time. 'Show' and 'ToJSON'
redact the value when 'pvSecret' is set, so that a secret parameter never
leaks through a derived debug trace or JSON dump by accident.
-}
data ParamValue = ParamValue
    { pvValue :: Aeson.Value
    , pvSecret :: Bool
    }
    deriving (Eq)

instance Show ParamValue where
    show pv
        | pvSecret pv = "ParamValue {pvValue = <secret>, pvSecret = True}"
        | otherwise = "ParamValue {pvValue = " <> show (pvValue pv) <> ", pvSecret = False}"

instance Aeson.ToJSON ParamValue where
    toJSON pv
        | pvSecret pv = Aeson.String "<secret>"
        | otherwise = pvValue pv

{- | Parses back as a non-secret value: a redacted secret cannot be
recovered from JSON, and nothing that serializes a 'ParamValue' is
expected to have written a secret one in the first place (see
'System.Agents.Tools.Context.tecsParams').
-}
instance Aeson.FromJSON ParamValue where
    parseJSON v = pure (ParamValue v False)

-- | Resolved parameter values, keyed by name, as carried in a 'ToolExecutionContext'.
type Params = Map ParamName ParamValue

{- | A value supplied by the operator at process start (@--set@ / @--set-json@
/ @--params-file@ / @--pin@ / @--pin-json@). 'pvPinned' clients cannot
override it once session-level parameters exist (Phase 4); at process
level today it makes no difference.
-}
data ProcessValue = ProcessValue
    { pvRawValue :: Aeson.Value
    , pvPinned :: Bool
    }
    deriving (Show, Eq, Generic)

instance Aeson.ToJSON ProcessValue
instance Aeson.FromJSON ProcessValue

-- | Operator-supplied values, keyed by parameter name, before they are resolved against an agent's declarations.
type ProcessParams = Map ParamName ProcessValue
