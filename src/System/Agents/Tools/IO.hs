-- | Defines tools as Haskell IO action.
module System.Agents.Tools.IO where

import Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Text as Text
import Prod.Tracer (Tracer, runTracer)

import System.Agents.Tools.Context (ToolExecutionContext)

data Trace a b
    = IOScriptStarted IOScriptDescription a
    | IOScriptStopped IOScriptDescription a b
    deriving (Show)

adaptTraceInput :: (a1 -> a2) -> Trace a1 b -> Trace a2 b
adaptTraceInput f (IOScriptStarted slug a) = IOScriptStarted slug (f a)
adaptTraceInput f (IOScriptStopped slug a b) = IOScriptStopped slug (f a) b

type Slug = Text

data IOScriptDescription
    = IOScriptDescription
    { ioSlug :: Slug
    , ioDescription :: Text
    }
    deriving (Show)

-- | An IO script that executes a Haskell IO action with access to session context.
--
-- Unlike bash tools that receive context via environment variables, IO tools
-- receive the 'ToolExecutionContext' directly as a function argument. This provides
-- type-safe access to session metadata (session ID, conversation ID, turn ID, etc.)
-- without exposing these details to the LLM.
--
-- Example:
--
-- @
-- myTool :: IOScript MyArg ByteString
-- myTool = IOScript
--     { description = IOScriptDescription "my-tool" "Does something"
--     , ioRun = \ctx arg -> do
--         let sessionId = ctxSessionId ctx
--         -- Access complete session context
--         pure $ result arg
--     }
-- @
data IOScript llmArg b
    = IOScript
    { description :: IOScriptDescription
    , ioRun :: ToolExecutionContext -> llmArg -> IO b
    }

data RunError
    = SerializeArgumentErrors String
    | ScriptExecutionError String
    deriving (Show)

-- | Run an IO script with the given execution context and JSON argument.
--
-- The 'ToolExecutionContext' provides the script with access to session metadata
-- without exposing these details to the LLM.
runValue ::
    (FromJSON llmArg) =>
    Tracer IO (Trace llmArg b) ->
    IOScript llmArg b ->
    ToolExecutionContext ->
    Aeson.Value ->
    IO (Either RunError b)
runValue tracer script ctx val = do
    case Aeson.parseEither Aeson.parseJSON val of
        Left err ->
            pure $ Left $ SerializeArgumentErrors err
        Right argz -> do
            runTracer tracer (IOScriptStarted script.description argz)
            out <- script.ioRun ctx argz
            runTracer tracer (IOScriptStopped script.description argz out)
            pure $ Right out

