-- | Defines tools as Haskell IO action.
module System.Agents.Tools.IO where

import Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Text as Text
import Prod.Tracer (Tracer, runTracer)

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

data IOScript a b
    = IOScript
    { description :: IOScriptDescription
    , ioRun :: a -> IO b
    }

data RunError
    = SerializeArgumentErrors String
    | ScriptExecutionError String
    deriving (Show)

runValue :: (FromJSON a) => Tracer IO (Trace a b) -> IOScript a b -> Aeson.Value -> IO (Either RunError b)
runValue tracer script val = do
    case Aeson.parseEither Aeson.parseJSON val of
        Left err ->
            pure $ Left $ SerializeArgumentErrors err
        Right argz -> do
            runTracer tracer (IOScriptStarted script.description argz)
            out <- script.ioRun argz
            runTracer tracer (IOScriptStopped script.description argz out)
            pure $ Right out
