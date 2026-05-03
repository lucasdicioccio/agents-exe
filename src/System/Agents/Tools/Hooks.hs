{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Tool hooks system for customizing tool behavior.

This module provides a hooks system that allows users to register functions
to run before/after tool execution, enabling:

* **Customization**: Modify tool inputs/outputs
* **Integration**: Connect tools to external systems
* **Validation**: Add custom validation logic
* **Logging**: Enhanced audit trails
* **Transformation**: Automatic data transformations

== Hook Types

1. **Pre-execution**: Modify arguments before tool runs
2. **Post-execution**: Modify results after tool completes
3. **Error hook**: Handle or transform errors
4. **Condition**: Only run hook when condition is met

== Basic Usage

Define hooks and execute tools with them:

@
import System.Agents.Tools.Hooks

-- Create a simple logging hook
logHook :: ToolHook
logHook = ToolHook
    { hookName = "log_command"
    , hookType = PreExecHook
    , hookCondition = Nothing
    , hookAction = LogAction "info"
    }

-- Create a registry and run with hooks
let registry = registerHooks "bash" [logHook] emptyHookRegistry
result <- runWithHooks registry toolName args ctx executeTool
@

== Configuration-Based Usage

Hooks can be loaded from configuration:

@
config <- loadHookConfig "hooks.yaml"
let registry = hookRegistryFromConfig config
@
-}
module System.Agents.Tools.Hooks (
    -- * Core Types
    HookType (..),
    HookCondition (..),
    HookAction (..),
    ToolHook (..),
    HookResult (..),
    HookContext (..),

    -- * Hook Registry
    HookRegistry,
    ToolName,
    emptyHookRegistry,
    registerHook,
    registerHooks,
    lookupHooks,

    -- * Hook Execution
    runWithHooks,
    executeHooks,

    -- * Condition Evaluation
    evaluateCondition,

    -- * Hook Chain Execution
    runPreExecHooks,
    runPostExecHooks,
    runErrorHooks,
) where

import Control.Monad (foldM)
import Data.Aeson (FromJSON, ToJSON, Value (..), (.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)

import System.Agents.Tools.Base (CallResult (..))
import System.Agents.Tools.Context (ToolExecutionContext)

-------------------------------------------------------------------------------
-- Core Types
-------------------------------------------------------------------------------

-- | Type alias for tool names in hook context.
type ToolName = Text

{- | Types of hooks that can be registered.

Hooks are executed at different points in the tool lifecycle:

* 'PreExecHook': Runs before the tool executes, can modify arguments
* 'PostExecHook': Runs after successful execution, can modify results
* 'ErrorHook': Runs when the tool fails, can handle or transform errors
-}
data HookType
    = PreExecHook
    | PostExecHook
    | ErrorHook
    deriving (Show, Eq, Generic)

instance ToJSON HookType where
    toJSON PreExecHook = Aeson.String "pre_exec"
    toJSON PostExecHook = Aeson.String "post_exec"
    toJSON ErrorHook = Aeson.String "on_error"

instance FromJSON HookType where
    parseJSON = Aeson.withText "HookType" $ \txt ->
        case txt of
            "pre_exec" -> return PreExecHook
            "post_exec" -> return PostExecHook
            "on_error" -> return ErrorHook
            other -> fail $ "Invalid HookType: " ++ Text.unpack other ++ ". Expected 'pre_exec', 'post_exec', or 'on_error'."

{- | Conditions for conditional hook execution.

Hooks can specify conditions that determine whether they should run.
Conditions can check:

* Exit codes
* Tool names (patterns)
* Argument values
* Custom predicates
-}
data HookCondition
    = -- | Always execute the hook (default)
      Always
    | -- | Execute only if exit code matches
      ExitCodeIs Int
    | -- | Execute only if exit code is non-zero
      ExitCodeNonZero
    | -- | Execute only for tools matching pattern
      ToolNameMatches Text
    | -- | Execute only if argument contains key with value
      ArgContains Text Text
    | -- | Logical AND of conditions
      AndCondition HookCondition HookCondition
    | -- | Logical OR of conditions
      OrCondition HookCondition HookCondition
    | -- | Negate a condition
      NotCondition HookCondition
    deriving (Show, Eq, Generic)

instance ToJSON HookCondition where
    toJSON Always = Aeson.String "always"
    toJSON (ExitCodeIs n) = Aeson.object ["type" .= ("exit_code_is" :: Text), "value" .= n]
    toJSON ExitCodeNonZero = Aeson.object ["type" .= ("exit_code_nonzero" :: Text)]
    toJSON (ToolNameMatches pattern) = Aeson.object ["type" .= ("tool_name_matches" :: Text), "pattern" .= pattern]
    toJSON (ArgContains key val) = Aeson.object ["type" .= ("arg_contains" :: Text), "key" .= key, "value" .= val]
    toJSON (AndCondition c1 c2) = Aeson.object ["type" .= ("and" :: Text), "conditions" .= [c1, c2]]
    toJSON (OrCondition c1 c2) = Aeson.object ["type" .= ("or" :: Text), "conditions" .= [c1, c2]]
    toJSON (NotCondition c) = Aeson.object ["type" .= ("not" :: Text), "condition" .= c]

instance FromJSON HookCondition where
    parseJSON (Aeson.String "always") = return Always
    parseJSON (Aeson.Object obj) = do
        typ <- obj .: "type"
        case (typ :: Text) of
            "exit_code_is" -> ExitCodeIs <$> obj .: "value"
            "exit_code_nonzero" -> return ExitCodeNonZero
            "tool_name_matches" -> ToolNameMatches <$> obj .: "pattern"
            "arg_contains" -> ArgContains <$> obj .: "key" <*> obj .: "value"
            "and" -> do
                conditions <- obj .: "conditions"
                case conditions of
                    [c1, c2] -> return $ AndCondition c1 c2
                    _ -> fail "'and' condition requires exactly 2 sub-conditions"
            "or" -> do
                conditions <- obj .: "conditions"
                case conditions of
                    [c1, c2] -> return $ OrCondition c1 c2
                    _ -> fail "'or' condition requires exactly 2 sub-conditions"
            "not" -> NotCondition <$> obj .: "condition"
            other -> fail $ "Unknown condition type: " ++ Text.unpack other
    parseJSON other = fail $ "Invalid HookCondition: " ++ show other

{- | Actions that hooks can perform.

Hook actions define what transformation or side effect occurs:

* 'TransformArgs': Modify the input arguments (pre-exec only)
* 'TransformResult': Modify the result (post-exec only)
* 'LogAction': Log information at specified level
* 'ValidateAction': Validate input/output against rules
* 'CustomAction': User-defined custom action
-}
data HookAction
    = -- | Transform the arguments JSON
      TransformArgs (Value -> Value)
    | -- | Transform the call result
      TransformResult (forall call. CallResult call -> CallResult call)
    | -- | Log a message at the specified level
      LogAction Text
    | -- | Validate and potentially fail
      ValidateAction (Value -> Maybe Text)
    | -- | Custom action with access to full context
      CustomAction (HookContext -> IO HookResult)

instance Show HookAction where
    show (TransformArgs _) = "TransformArgs <function>"
    show (TransformResult _) = "TransformResult <function>"
    show (LogAction level) = "LogAction " ++ show level
    show (ValidateAction _) = "ValidateAction <function>"
    show (CustomAction _) = "CustomAction <function>"

{- | Context passed to hooks during execution.

Contains all relevant information about the current tool invocation
that hooks might need to access.
-}
data HookContext = HookContext
    { hookCtxToolName :: ToolName
    -- ^ Name of the tool being invoked
    , hookCtxArguments :: Value
    -- ^ Current arguments (may be modified by previous hooks)
    , hookCtxResult :: Maybe (CallResult ())
    -- ^ Result (only for post-exec and error hooks)
    , hookCtxExecutionContext :: ToolExecutionContext
    -- ^ Full execution context
    , hookCtxExitCode :: Maybe Int
    -- ^ Exit code if available
    }
    deriving (Show)

{- | Result of hook execution.

Hooks return a result indicating success with optional modifications,
or failure with an error message.
-}
data HookResult
    = -- | Hook executed successfully, possibly with modifications
      HookSuccess
        { modifiedArgs :: Maybe Value
        -- ^ Modified arguments (for pre-exec hooks)
        , modifiedResult :: Maybe (CallResult ())
        -- ^ Modified result (for post-exec/error hooks)
        }
    | -- | Hook failed, abort tool execution
      HookFailure
        -- | Error message
        Text
    deriving (Show)

{- | Definition of a tool hook.

Hooks have a name, type, optional condition, and action to perform.
-}
data ToolHook = ToolHook
    { hookName :: Text
    -- ^ Human-readable name for the hook
    , hookType :: HookType
    -- ^ When this hook executes
    , hookCondition :: Maybe HookCondition
    -- ^ Optional condition for execution (default: always)
    , hookAction :: HookAction
    -- ^ What the hook does
    }

instance Show ToolHook where
    show h =
        "ToolHook {"
            ++ " name = "
            ++ show h.hookName
            ++ ", type = "
            ++ show h.hookType
            ++ ", condition = "
            ++ show h.hookCondition
            ++ ", action = "
            ++ show h.hookAction
            ++ " }"

-------------------------------------------------------------------------------
-- Hook Registry
-------------------------------------------------------------------------------

{- | Registry mapping tool names to their associated hooks.

The registry maintains separate lists of hooks per tool name,
allowing efficient lookup during tool execution.
-}
newtype HookRegistry = HookRegistry
    { unHookRegistry :: Map ToolName [ToolHook]
    }
    deriving (Show)

-- | Create an empty hook registry.
emptyHookRegistry :: HookRegistry
emptyHookRegistry = HookRegistry Map.empty

{- | Register a single hook for a tool.

If the tool already has hooks, the new hook is appended to the list.
Hooks are executed in registration order.

Example:

@
let registry = registerHook "bash" myHook emptyHookRegistry
@
-}
registerHook :: ToolName -> ToolHook -> HookRegistry -> HookRegistry
registerHook toolName hook (HookRegistry reg) =
    HookRegistry $ Map.alter (Just . (hook :) . fromMaybe []) toolName reg

{- | Register multiple hooks for a tool.

Hooks are added in the order provided (each is prepended, so reverse order).

Example:

@
let registry = registerHooks "bash" [hook1, hook2] emptyHookRegistry
-- hook2 will execute before hook1
@
-}
registerHooks :: ToolName -> [ToolHook] -> HookRegistry -> HookRegistry
registerHooks toolName hooks registry =
    foldr (registerHook toolName) registry hooks

{- | Look up hooks for a specific tool.

Returns all hooks registered for the tool, in execution order.
Returns empty list if no hooks are registered.
-}
lookupHooks :: ToolName -> HookRegistry -> [ToolHook]
lookupHooks toolName (HookRegistry reg) =
    fromMaybe [] $ Map.lookup toolName reg

-------------------------------------------------------------------------------
-- Condition Evaluation
-------------------------------------------------------------------------------

{- | Evaluate a condition against the current context.

Returns True if the condition is satisfied, False otherwise.
-}
evaluateCondition :: HookCondition -> HookContext -> Bool
evaluateCondition Always _ = True
evaluateCondition (ExitCodeIs expected) ctx =
    case ctx.hookCtxExitCode of
        Just actual -> actual == expected
        Nothing -> False
evaluateCondition ExitCodeNonZero ctx =
    case ctx.hookCtxExitCode of
        Just code -> code /= 0
        Nothing -> False
evaluateCondition (ToolNameMatches pattern) ctx =
    pattern `Text.isInfixOf` ctx.hookCtxToolName
evaluateCondition (ArgContains key expectedVal) ctx =
    case ctx.hookCtxArguments of
        Aeson.Object obj ->
            case KeyMap.lookup (Key.fromText key) obj of
                Just (Aeson.String actualVal) -> actualVal == expectedVal
                Just (Aeson.Number n) ->
                    expectedVal == Text.pack (show n)
                _ -> False
        _ -> False
evaluateCondition (AndCondition c1 c2) ctx =
    evaluateCondition c1 ctx && evaluateCondition c2 ctx
evaluateCondition (OrCondition c1 c2) ctx =
    evaluateCondition c1 ctx || evaluateCondition c2 ctx
evaluateCondition (NotCondition c) ctx =
    not $ evaluateCondition c ctx

-------------------------------------------------------------------------------
-- Hook Execution
-------------------------------------------------------------------------------

{- | Execute hooks of a specific type for a tool.

This function filters hooks by type and condition, then executes them
in order, threading any modifications through the chain.

For PreExec hooks: modifies arguments
For PostExec hooks: modifies results
For Error hooks: can transform or handle errors
-}
executeHooks ::
    HookType ->
    HookRegistry ->
    ToolName ->
    HookContext ->
    IO HookResult
executeHooks hType registry toolName ctx = do
    let allHooks = lookupHooks toolName registry
        typedHooks = filter (\h -> h.hookType == hType) allHooks
        applicableHooks = filter (shouldRunHook ctx) typedHooks

    foldM runHook (HookSuccess Nothing Nothing) applicableHooks
  where
    shouldRunHook :: HookContext -> ToolHook -> Bool
    shouldRunHook context hook =
        case hook.hookCondition of
            Nothing -> True
            Just cond -> evaluateCondition cond context

    runHook :: HookResult -> ToolHook -> IO HookResult
    runHook (HookFailure err) _ = pure $ HookFailure err
    runHook (HookSuccess mArgs mResult) hook =
        case hook.hookAction of
            TransformArgs f ->
                let currentArgs = fromMaybe ctx.hookCtxArguments mArgs
                    newArgs = f currentArgs
                 in pure $ HookSuccess (Just newArgs) mResult
            TransformResult f ->
                case mResult of
                    Just result ->
                        let newResult = f result
                         in pure $ HookSuccess mArgs (Just newResult)
                    Nothing -> pure $ HookSuccess mArgs mResult
            LogAction level -> do
                -- Simple console logging - can be extended
                putStrLn $ "[HOOK " ++ Text.unpack level ++ "] " ++ Text.unpack hook.hookName
                pure $ HookSuccess mArgs mResult
            ValidateAction f ->
                let currentArgs = fromMaybe ctx.hookCtxArguments mArgs
                 in case f currentArgs of
                        Just err -> pure $ HookFailure err
                        Nothing -> pure $ HookSuccess mArgs mResult
            CustomAction action -> do
                let customCtx =
                        ctx
                            { hookCtxArguments = fromMaybe ctx.hookCtxArguments mArgs
                            , hookCtxResult = mResult
                            }
                action customCtx

{- | Run pre-execution hooks and return potentially modified arguments.

Returns Left with error message if any hook fails.
Returns Right with modified arguments if all hooks succeed.
-}
runPreExecHooks ::
    HookRegistry ->
    ToolName ->
    Value ->
    ToolExecutionContext ->
    IO (Either Text Value)
runPreExecHooks registry toolName args execCtx = do
    let ctx =
            HookContext
                { hookCtxToolName = toolName
                , hookCtxArguments = args
                , hookCtxResult = Nothing
                , hookCtxExecutionContext = execCtx
                , hookCtxExitCode = Nothing
                }
    result <- executeHooks PreExecHook registry toolName ctx
    case result of
        HookFailure err -> pure $ Left err
        HookSuccess mArgs _ -> pure $ Right $ fromMaybe args mArgs

{- | Run post-execution hooks and return potentially modified result.

Takes the original result and applies post-exec hooks to transform it.
-}
runPostExecHooks ::
    HookRegistry ->
    ToolName ->
    CallResult () ->
    Value ->
    ToolExecutionContext ->
    IO (Either Text (CallResult ()))
runPostExecHooks registry toolName originalResult args execCtx = do
    let ctx =
            HookContext
                { hookCtxToolName = toolName
                , hookCtxArguments = args
                , hookCtxResult = Just originalResult
                , hookCtxExecutionContext = execCtx
                , hookCtxExitCode = extractExitCode originalResult
                }
    result <- executeHooks PostExecHook registry toolName ctx
    case result of
        HookFailure err -> pure $ Left err
        HookSuccess _ mResult -> pure $ Right $ fromMaybe originalResult mResult

{- | Run error hooks when a tool fails.

Error hooks can transform the error or provide recovery.
-}
runErrorHooks ::
    HookRegistry ->
    ToolName ->
    CallResult () ->
    Value ->
    ToolExecutionContext ->
    IO (Either Text (CallResult ()))
runErrorHooks registry toolName errorResult args execCtx = do
    let ctx =
            HookContext
                { hookCtxToolName = toolName
                , hookCtxArguments = args
                , hookCtxResult = Just errorResult
                , hookCtxExecutionContext = execCtx
                , hookCtxExitCode = extractExitCode errorResult
                }
    result <- executeHooks ErrorHook registry toolName ctx
    case result of
        HookFailure err -> pure $ Left err
        HookSuccess _ mResult -> pure $ Right $ fromMaybe errorResult mResult

-- | Extract exit code from a CallResult if available.
extractExitCode :: CallResult () -> Maybe Int
extractExitCode (BashToolError _ _) = Just 1 -- bash errors have exit code 1
extractExitCode (IOToolError _ _) = Just 1
extractExitCode (McpToolError _ _) = Just 1
extractExitCode (OpenAPIToolError _ _) = Just 1
extractExitCode (PostgRESToolError _ _) = Just 1
extractExitCode (SqliteToolError _ _) = Just 1
extractExitCode (SystemToolError _ _) = Just 1
extractExitCode (DeveloperToolError _ _) = Just 1
extractExitCode (LuaToolError _ _) = Just 1
extractExitCode (ToolNotFound _) = Just 127 -- command not found
extractExitCode _ = Just 0 -- success

-------------------------------------------------------------------------------
-- Main Execution Entry Point
-------------------------------------------------------------------------------

{- | Execute a tool with hooks.

This is the main entry point for hook-aware tool execution.
It:
1. Runs pre-execution hooks
2. Executes the tool
3. Runs post-execution hooks on success
4. Runs error hooks on failure

Example:

@
let registry = registerHooks "bash" myHooks emptyHookRegistry
result <- runWithHooks registry "bash" (Object args) toolExecCtx executeTool
@
-}
runWithHooks ::
    HookRegistry ->
    -- | Tool name for hook lookup
    ToolName ->
    -- | Original arguments
    Value ->
    -- | Execution context
    ToolExecutionContext ->
    -- | The actual tool execution function
    (Value -> IO (CallResult ())) ->
    IO (CallResult ())
runWithHooks registry toolName args execCtx executeTool = do
    -- Step 1: Run pre-execution hooks
    preResult <- runPreExecHooks registry toolName args execCtx
    case preResult of
        Left _err ->
            -- Pre-hook failed, return as error
            pure $ ToolNotFound $ ()
        Right newArgs -> do
            -- Step 2: Execute the tool
            toolResult <- executeTool newArgs

            -- Step 3: Determine if success or error and run appropriate hooks
            case isErrorResult toolResult of
                False -> do
                    -- Success - run post-exec hooks
                    postResult <- runPostExecHooks registry toolName toolResult newArgs execCtx
                    case postResult of
                        Left _ -> pure toolResult -- If post-hook fails, return original
                        Right newResult -> pure newResult
                True -> do
                    -- Error - run error hooks
                    errorResult <- runErrorHooks registry toolName toolResult newArgs execCtx
                    case errorResult of
                        Left _ -> pure toolResult -- If error hook fails, return original
                        Right newResult -> pure newResult
  where
    isErrorResult :: CallResult () -> Bool
    isErrorResult (ToolNotFound _) = True
    isErrorResult (BashToolError _ _) = True
    isErrorResult (IOToolError _ _) = True
    isErrorResult (McpToolError _ _) = True
    isErrorResult (OpenAPIToolError _ _) = True
    isErrorResult (PostgRESToolError _ _) = True
    isErrorResult (SqliteToolError _ _) = True
    isErrorResult (SystemToolError _ _) = True
    isErrorResult (DeveloperToolError _ _) = True
    isErrorResult (LuaToolError _ _) = True
    isErrorResult _ = False
