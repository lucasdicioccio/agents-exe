{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Agents.CLI.TraceUtils (
    traceUsefulPromptStderr,
) where

import Data.Text ()
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import GHC.IO.Handle (Handle)
import Prod.Tracer (Tracer (..))
import System.IO (stderr)

import System.Agents.AgentTree (AgentConfigTree (..), TreeTrace (..))

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
    f (RuntimeTrace tr) =
        Text.hPutStrLn h (Text.pack $ show tr)
    f (DataLoadingTrace x) = Text.hPutStrLn h (Text.pack $ show x)
    f (BashToolboxTrace x) = Text.hPutStrLn h (Text.pack $ show x)
    f (McpToolboxTrace x) = Text.hPutStrLn h (Text.pack $ show x)
    f (OpenApiToolboxTrace x) = Text.hPutStrLn h (Text.pack $ show x)
    f (PostgRESToolboxTrace x) = Text.hPutStrLn h (Text.pack $ show x)
    f (SqliteToolboxTrace x) = Text.hPutStrLn h (Text.pack $ show x)
    f (SystemToolboxTrace x) = Text.hPutStrLn h (Text.pack $ show x)
    f (DeveloperToolboxTrace x) = Text.hPutStrLn h (Text.pack $ show x)
    f (LuaToolboxTrace x) = Text.hPutStrLn h (Text.pack $ show x)
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
