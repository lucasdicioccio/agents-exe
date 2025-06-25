{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Provides a runtime value capable to load and reload a filesystem directory of bash tools.
Note that reloads are asynchronous.
-}
module System.Agents.Tools.BashToolbox where

import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVarIO, takeTMVar, tryPutTMVar)
import qualified Prod.Background as Background
import Prod.Tracer (Tracer, contramap)

import qualified System.Agents.Tools.Bash as BashTools

-------------------------------------------------------------------------------
data Trace
    = BashToolsLoadingTrace !BashTools.LoadTrace
    | ReloadToolsTrace !(Background.Track [BashTools.ScriptDescription])
    deriving (Show)

data BackgroundBashTools = BackgroundBashTools
    { directory :: FilePath
    , tools :: Background.BackgroundVal [BashTools.ScriptDescription]
    , triggerReload :: STM Bool
    }

data LoadingError
    = LoadingError String [BashTools.InvalidScriptError]
    deriving (Show)

initializeBackroundToolbox ::
    Tracer IO Trace ->
    FilePath ->
    IO (Either LoadingError BackgroundBashTools)
initializeBackroundToolbox tracer tooldir = do
    (startingTools, errs) <- loadToolsOnce
    if null errs
        then do
            lock <- newEmptyTMVarIO
            bkgTools <-
                Background.background
                    (contramap ReloadToolsTrace tracer)
                    ()
                    startingTools
                    (const (reloadToolsOnTrigger lock))
            let triggerReloadTools = tryPutTMVar lock ()
            pure $ Right (BackgroundBashTools tooldir bkgTools triggerReloadTools)
        else do
            pure $ Left (LoadingError "errors when loading tools" errs)
  where
    loadToolsOnce :: IO ([BashTools.ScriptDescription], [BashTools.InvalidScriptError])
    loadToolsOnce = do
        (scripts, errs) <- BashTools.loadDirectory (contramap BashToolsLoadingTrace tracer) tooldir
        pure (scripts.scriptDescriptions, errs)

    -- returns an extra '()' as a state for Background.background
    reloadToolsOnTrigger :: TMVar () -> IO ([BashTools.ScriptDescription], ())
    reloadToolsOnTrigger lock = do
        _ <- atomically $ takeTMVar lock
        tools <- reloadTools
        pure (tools, ())

    reloadTools :: IO [BashTools.ScriptDescription]
    reloadTools = do
        (tools, _) <- loadToolsOnce
        pure tools
