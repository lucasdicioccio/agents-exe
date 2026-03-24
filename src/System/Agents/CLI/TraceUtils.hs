{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Agents.CLI.TraceUtils (
    tracePrintingTextResponses,
    traceUsefulPromptStderr,
    traceUsefulPromptStdout,
) where

import Data.Text ()
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import GHC.IO.Handle (Handle)
import Prod.Tracer (Tracer (..))
import System.IO (stderr, stdout)

import System.Agents.AgentTree (AgentConfigTree (..), TreeTrace (..))

tracePrintingTextResponses :: Tracer IO TreeTrace
tracePrintingTextResponses = Tracer f
  where
    f _ = pure () -- Stub implementation

traceUsefulPromptStdout :: Tracer IO TreeTrace
traceUsefulPromptStdout = traceUsefulPromptHandle stdout

traceUsefulPromptStderr :: Tracer IO TreeTrace
traceUsefulPromptStderr = traceUsefulPromptHandle stderr

traceUsefulPromptHandle :: Handle -> Tracer IO TreeTrace
traceUsefulPromptHandle h = Tracer f
  where
    f (McpTrace _ tr) =
        Text.hPutStrLn h (Text.pack $ show tr)
    f (OpenAPITrace _ tr) =
        Text.hPutStrLn h (Text.pack $ show tr)
    f (PostgRESTrace _ tr) =
        Text.hPutStrLn h (Text.pack $ show tr)
    f (DataLoadingTrace x) = Text.hPutStrLn h (Text.pack $ show x)
    f (ConfigLoadedTrace x) =
        Text.hPutStrLn h (showTree 0 x)
      where
        showTree :: Int -> AgentConfigTree -> Text.Text
        showTree n v =
            Text.unlines
                [ Text.concat [Text.replicate n "  ", Text.pack v.agentConfigFile]
                , Text.unlines $ fmap (showTree (succ n)) v.agentConfigChildren
                ]
    f (CyclicReferencesWarning warns) =
        Text.hPutStrLn h $ "Cyclic references warning: " <> Text.pack (show warns)
    f (ReferenceValidationTrace refs) =
        Text.hPutStrLn h $ "Reference validation trace: " <> Text.pack (show refs)

