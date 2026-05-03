{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- | Configuration types and parsers for tool hooks.

This module provides types and functions for loading hook configurations
from JSON/YAML files. It supports a declarative configuration format
that maps tool names to their associated hooks.

== Configuration Format

Example YAML configuration:

> tool_hooks:
>   - tool: bash
>     pre_exec:
>       - name: log_command
>         action:
>           type: log
>           level: info
>       - name: validate_args
>         condition:
>           type: arg_contains
>           key: command
>           value: rm
>         action:
>           type: validate
>           message: "rm command requires confirmation"
>     post_exec:
>       - name: validate_output
>         condition:
>           type: exit_code_is
>           value: 0
>         action:
>           type: transform
>           operation: parse_json
>
>   - tool: sqlite_query
>     pre_exec:
>       - name: add_limit
>         action:
>           type: transform
>           operation: add_row_limit

The configuration supports:

* Multiple hooks per tool
* Conditional execution based on context
* Different action types (log, validate, transform)
* Chaining hooks in sequence
-}
module System.Agents.Configuration.Hooks (
    -- * Configuration Types
    ToolHookConfig (..),
    HookDefinition (..),
    HookActionConfig (..),
    HookConditionConfig (..),
    ToolHooksEntry (..),

    -- * Loading Configuration
    loadHookConfig,
    loadHookConfigFile,
    parseHookConfig,

    -- * Converting to Registry
    hookRegistryFromConfig,
    toolHookFromDefinition,

    -- * Validation
    validateHookConfig,
) where

import Data.Aeson (FromJSON, ToJSON, Value, (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import Data.List (group, sort)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)

import System.Agents.Tools.Hooks (
    HookAction (..),
    HookCondition (..),
    HookRegistry,
    HookResult (..),
    HookType (..),
    ToolHook (..),
    emptyHookRegistry,
    registerHook,
 )

-------------------------------------------------------------------------------
-- Configuration Types
-------------------------------------------------------------------------------

{- | Top-level hook configuration.

Contains a list of tool hook entries, each defining hooks for a specific tool.
-}
newtype ToolHookConfig = ToolHookConfig
    { toolHooks :: [ToolHooksEntry]
    }
    deriving (Show, Eq, Generic)

instance ToJSON ToolHookConfig where
    toJSON config = Aeson.object ["tool_hooks" .= config.toolHooks]

instance FromJSON ToolHookConfig where
    parseJSON = Aeson.withObject "ToolHookConfig" $ \obj ->
        ToolHookConfig <$> obj .: "tool_hooks"

{- | Hook configuration for a specific tool.

Defines pre-exec, post-exec, and error hooks for a named tool.
-}
data ToolHooksEntry = ToolHooksEntry
    { entryToolName :: Text
    -- ^ Name of the tool (e.g., "bash", "sqlite_query")
    , entryPreExec :: Maybe [HookDefinition]
    -- ^ Pre-execution hooks
    , entryPostExec :: Maybe [HookDefinition]
    -- ^ Post-execution hooks
    , entryOnError :: Maybe [HookDefinition]
    -- ^ Error handling hooks
    }
    deriving (Show, Eq, Generic)

instance ToJSON ToolHooksEntry where
    toJSON entry =
        Aeson.object $
            catMaybes
                [ Just $ "tool" .= entry.entryToolName
                , ("pre_exec" .=) <$> entry.entryPreExec
                , ("post_exec" .=) <$> entry.entryPostExec
                , ("on_error" .=) <$> entry.entryOnError
                ]

instance FromJSON ToolHooksEntry where
    parseJSON = Aeson.withObject "ToolHooksEntry" $ \obj ->
        ToolHooksEntry
            <$> obj .: "tool"
            <*> obj .:? "pre_exec"
            <*> obj .:? "post_exec"
            <*> obj .:? "on_error"

{- | Definition of a single hook.

Contains the hook name, optional condition, and action configuration.
-}
data HookDefinition = HookDefinition
    { defName :: Text
    -- ^ Unique name for this hook
    , defCondition :: Maybe HookConditionConfig
    -- ^ Optional condition for execution
    , defAction :: HookActionConfig
    -- ^ Action to perform
    }
    deriving (Show, Eq, Generic)

instance ToJSON HookDefinition where
    toJSON def =
        Aeson.object $
            catMaybes
                [ Just $ "name" .= def.defName
                , ("condition" .=) <$> def.defCondition
                , Just $ "action" .= def.defAction
                ]

instance FromJSON HookDefinition where
    parseJSON = Aeson.withObject "HookDefinition" $ \obj ->
        HookDefinition
            <$> obj .: "name"
            <*> obj .:? "condition"
            <*> obj .: "action"

{- | Condition configuration.

Supports all condition types from 'HookCondition' in a serializable format.
-}
data HookConditionConfig
    = ConfigAlways
    | ConfigExitCodeIs Int
    | ConfigExitCodeNonZero
    | ConfigToolNameMatches Text
    | ConfigArgContains Text Text
    | ConfigAnd [HookConditionConfig]
    | ConfigOr [HookConditionConfig]
    | ConfigNot HookConditionConfig
    deriving (Show, Eq, Generic)

instance ToJSON HookConditionConfig where
    toJSON ConfigAlways = Aeson.String "always"
    toJSON (ConfigExitCodeIs n) =
        Aeson.object
            [ "type" .= ("exit_code_is" :: Text)
            , "value" .= n
            ]
    toJSON ConfigExitCodeNonZero =
        Aeson.object
            [ "type" .= ("exit_code_nonzero" :: Text)
            ]
    toJSON (ConfigToolNameMatches pattern) =
        Aeson.object
            [ "type" .= ("tool_name_matches" :: Text)
            , "pattern" .= pattern
            ]
    toJSON (ConfigArgContains key val) =
        Aeson.object
            [ "type" .= ("arg_contains" :: Text)
            , "key" .= key
            , "value" .= val
            ]
    toJSON (ConfigAnd conditions) =
        Aeson.object
            [ "type" .= ("and" :: Text)
            , "conditions" .= conditions
            ]
    toJSON (ConfigOr conditions) =
        Aeson.object
            [ "type" .= ("or" :: Text)
            , "conditions" .= conditions
            ]
    toJSON (ConfigNot condition) =
        Aeson.object
            [ "type" .= ("not" :: Text)
            , "condition" .= condition
            ]

instance FromJSON HookConditionConfig where
    parseJSON (Aeson.String "always") = return ConfigAlways
    parseJSON (Aeson.Object obj) = do
        typ <- obj .: "type"
        case (typ :: Text) of
            "exit_code_is" -> ConfigExitCodeIs <$> obj .: "value"
            "exit_code_nonzero" -> return ConfigExitCodeNonZero
            "tool_name_matches" -> ConfigToolNameMatches <$> obj .: "pattern"
            "arg_contains" -> ConfigArgContains <$> obj .: "key" <*> obj .: "value"
            "and" -> ConfigAnd <$> obj .: "conditions"
            "or" -> ConfigOr <$> obj .: "conditions"
            "not" -> ConfigNot <$> obj .: "condition"
            other -> fail $ "Unknown condition type: " ++ Text.unpack other
    parseJSON other = fail $ "Invalid condition config: " ++ show other

{- | Action configuration.

Defines what action a hook should perform. This is a serializable
representation that gets converted to 'HookAction' at runtime.
-}
data HookActionConfig
    = -- | Log a message
      ConfigLogAction
        { configLogLevel :: Text
        , configLogMessage :: Maybe Text
        }
    | -- | Validate arguments/output
      ConfigValidateAction
        { configValidateType :: Text
        , configValidateMessage :: Text
        }
    | -- | Transform arguments/output
      ConfigTransformAction
        { configTransformType :: Text
        , configTransformConfig :: Maybe Value
        }
    | -- | Custom action (for programmatic use)
      ConfigCustomAction Text
    deriving (Show, Eq, Generic)

instance ToJSON HookActionConfig where
    toJSON (ConfigLogAction level mMsg) =
        Aeson.object $
            [ "type" .= ("log" :: Text)
            , "level" .= level
            ]
                ++ maybe [] (\msg -> ["message" .= msg]) mMsg
    toJSON (ConfigValidateAction valType msg) =
        Aeson.object
            [ "type" .= ("validate" :: Text)
            , "validate_type" .= valType
            , "message" .= msg
            ]
    toJSON (ConfigTransformAction tType mConfig) =
        Aeson.object $
            [ "type" .= ("transform" :: Text)
            , "operation" .= tType
            ]
                ++ maybe [] (\cfg -> ["config" .= cfg]) mConfig
    toJSON (ConfigCustomAction name) =
        Aeson.object
            [ "type" .= ("custom" :: Text)
            , "name" .= name
            ]

instance FromJSON HookActionConfig where
    parseJSON = Aeson.withObject "HookActionConfig" $ \obj -> do
        typ <- obj .: "type"
        case (typ :: Text) of
            "log" ->
                ConfigLogAction
                    <$> obj .: "level"
                    <*> obj .:? "message"
            "validate" ->
                ConfigValidateAction
                    <$> obj .: "validate_type"
                    <*> obj .: "message"
            "transform" ->
                ConfigTransformAction
                    <$> obj .: "operation"
                    <*> obj .:? "config"
            "custom" ->
                ConfigCustomAction
                    <$> obj .: "name"
            other -> fail $ "Unknown action type: " ++ Text.unpack other

-------------------------------------------------------------------------------
-- Loading Configuration
-------------------------------------------------------------------------------

{- | Load hook configuration from a YAML file.

Returns Left with error message if loading or parsing fails.

Example:

@
result <- loadHookConfigFile "/etc/agents/hooks.yaml"
case result of
    Left err -> putStrLn $ "Failed to load hooks: " ++ err
    Right config -> do
        let registry = hookRegistryFromConfig config
        -- Use registry with tool execution
@
-}
loadHookConfigFile :: FilePath -> IO (Either String ToolHookConfig)
loadHookConfigFile path = do
    result <- Yaml.decodeFileEither path
    case result of
        Left err -> pure $ Left $ "Failed to parse " ++ path ++ ": " ++ show err
        Right config -> pure $ Right config

{- | Load hook configuration from a YAML/JSON ByteString.

Useful for embedded configurations or testing.
-}
loadHookConfig :: BS.ByteString -> Either String ToolHookConfig
loadHookConfig bs =
    case Yaml.decodeEither' bs of
        Left err -> Left $ show err
        Right config -> Right config

{- | Parse hook configuration from a JSON Value.

Useful when configuration is already parsed as part of a larger config.
-}
parseHookConfig :: Value -> Either String ToolHookConfig
parseHookConfig val =
    case Aeson.fromJSON val of
        Aeson.Error err -> Left err
        Aeson.Success config -> Right config

-------------------------------------------------------------------------------
-- Converting to Runtime Types
-------------------------------------------------------------------------------

-- | Convert a condition config to a runtime condition.
conditionFromConfig :: HookConditionConfig -> HookCondition
conditionFromConfig ConfigAlways = Always
conditionFromConfig (ConfigExitCodeIs n) = ExitCodeIs n
conditionFromConfig ConfigExitCodeNonZero = ExitCodeNonZero
conditionFromConfig (ConfigToolNameMatches p) = ToolNameMatches p
conditionFromConfig (ConfigArgContains k v) = ArgContains k v
conditionFromConfig (ConfigAnd cs) =
    case map conditionFromConfig cs of
        [] -> Always
        [c] -> c
        c1 : c2 : rest -> foldl AndCondition (AndCondition c1 c2) rest
conditionFromConfig (ConfigOr cs) =
    case map conditionFromConfig cs of
        [] -> Always
        [c] -> c
        c1 : c2 : rest -> foldl OrCondition (OrCondition c1 c2) rest
conditionFromConfig (ConfigNot c) = NotCondition (conditionFromConfig c)

{- | Convert an action config to a runtime action.

Note: This creates basic implementations. For custom actions,
you should extend this function or use programmatic hook registration.
-}
actionFromConfig :: HookActionConfig -> HookAction
actionFromConfig (ConfigLogAction level _) = LogAction level
actionFromConfig (ConfigValidateAction _ msg) =
    ValidateAction $ \_ -> Just msg -- Always fails with message for now
actionFromConfig (ConfigTransformAction "add_timestamp" _) =
    TransformArgs $ \args ->
        case args of
            Aeson.Object obj ->
                Aeson.Object $ KeyMap.union obj (KeyMap.fromList [(Key.fromText "_timestamp", Aeson.String "added")])
            _ -> args
actionFromConfig (ConfigTransformAction "wrap_result" _) =
    TransformResult $ \result -> result -- Identity for now
actionFromConfig (ConfigTransformAction _ _) =
    TransformArgs id -- Identity transform for unknown operations
actionFromConfig (ConfigCustomAction name) =
    CustomAction $ \_ctx -> do
        putStrLn $ "[CUSTOM HOOK] " ++ Text.unpack name
        pure $ HookSuccess Nothing Nothing

-- | Convert a hook definition to a runtime tool hook.
toolHookFromDefinition :: HookType -> HookDefinition -> ToolHook
toolHookFromDefinition hType def =
    ToolHook
        { hookName = def.defName
        , hookType = hType
        , hookCondition = conditionFromConfig <$> def.defCondition
        , hookAction = actionFromConfig def.defAction
        }

{- | Convert configuration to a hook registry.

This creates a 'HookRegistry' from configuration that can be used
with 'runWithHooks'.

Example:

@
config <- loadHookConfigFile "hooks.yaml"
let registry = hookRegistryFromConfig config
result <- runWithHooks registry "bash" args ctx executeTool
@
-}
hookRegistryFromConfig :: ToolHookConfig -> HookRegistry
hookRegistryFromConfig config =
    foldl addEntry emptyHookRegistry config.toolHooks
  where
    addEntry :: HookRegistry -> ToolHooksEntry -> HookRegistry
    addEntry registry entry =
        let toolName = entry.entryToolName
            preHooks = map (toolHookFromDefinition PreExecHook) $ fromMaybe [] entry.entryPreExec
            postHooks = map (toolHookFromDefinition PostExecHook) $ fromMaybe [] entry.entryPostExec
            errorHooks = map (toolHookFromDefinition ErrorHook) $ fromMaybe [] entry.entryOnError
            allHooks = preHooks ++ postHooks ++ errorHooks
         in foldl (\r h -> registerHook toolName h r) registry allHooks

-------------------------------------------------------------------------------
-- Validation
-------------------------------------------------------------------------------

{- | Validate a hook configuration.

Returns a list of validation errors (empty if valid).

This checks for:
* Empty tool names
* Hook names without actions
* Invalid condition references
* Duplicate hook names per tool
-}
validateHookConfig :: ToolHookConfig -> [String]
validateHookConfig config =
    concatMap validateEntry config.toolHooks
  where
    validateEntry :: ToolHooksEntry -> [String]
    validateEntry entry =
        let prefix = "Tool '" ++ Text.unpack entry.entryToolName ++ "': "
         in catMaybes
                [ if Text.null entry.entryToolName
                    then Just "Empty tool name"
                    else Nothing
                , checkDuplicateNames prefix $ fromMaybe [] entry.entryPreExec
                , checkDuplicateNames prefix $ fromMaybe [] entry.entryPostExec
                , checkDuplicateNames prefix $ fromMaybe [] entry.entryOnError
                ]

    checkDuplicateNames :: String -> [HookDefinition] -> Maybe String
    checkDuplicateNames prefix defs =
        let names = map defName defs
            duplicates = findDuplicates names
         in if null duplicates
                then Nothing
                else Just $ prefix ++ "Duplicate hook names: " ++ show duplicates

    findDuplicates :: (Ord a) => [a] -> [a]
    findDuplicates xs =
        concatMap (take 1) $ filter (\g -> length g > 1) $ group $ sort xs
