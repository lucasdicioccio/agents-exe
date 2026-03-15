{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Provides a runtime value capable to load and reload bash tools from multiple sources.

This module supports loading tools from:
- Multiple filesystem directories (with optional basename filtering)
- Single executable files

Note that reloads are asynchronous.
-}
module System.Agents.Tools.BashToolbox where

import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVarIO, takeTMVar, tryPutTMVar)
import Data.Either (partitionEithers)
import qualified Data.List as List
import Data.Maybe (maybeToList, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Prod.Background as Background
import Prod.Tracer (Tracer, contramap)
import System.FilePath (takeFileName, (</>))

import System.Agents.Base (BashToolboxDescription (..), FileSystemDirectoryDescription (..), SingleToolDescription (..))
import qualified System.Agents.Tools.Bash as BashTools

-------------------------------------------------------------------------------

-- | Trace events for monitoring bash toolbox operations.
data Trace
    = BashToolsLoadingTrace !BashTools.LoadTrace
    | ReloadToolsTrace !(Background.Track [BashTools.ScriptDescription])
    | SourceLoadingError !FilePath String
    deriving (Show)

-------------------------------------------------------------------------------

{- | A source of bash tools.
Tracks the source configuration and the tools loaded from it.
-}
data ToolSource
    = -- | Directory source with filter and full path
      DirectorySource FilePath (Maybe Text) FilePath
    | -- | Single executable file
      SingleSource FilePath
    deriving (Show)

-- | Get the source path for display/tracing.
sourcePath :: ToolSource -> FilePath
sourcePath (DirectorySource origPath _ _) = origPath
sourcePath (SingleSource path) = path

-- | Get the resolved path for loading.
resolvedPath :: ToolSource -> FilePath
resolvedPath (DirectorySource _ _ resolved) = resolved
resolvedPath (SingleSource path) = path

-------------------------------------------------------------------------------

-- | Background bash tools state for a single source.
data BackgroundBashTools = BackgroundBashTools
    { source :: ToolSource
    , tools :: Background.BackgroundVal [BashTools.ScriptDescription]
    , triggerReload :: STM Bool
    }

-- | Collection of background bash tools from multiple sources.
data MultiSourceBashTools = MultiSourceBashTools
    { sources :: [BackgroundBashTools]
    , triggerAllReloads :: STM Bool
    }

-------------------------------------------------------------------------------

{- | Convert a BashToolboxDescription to a ToolSource.
The FilePath parameter is the base directory for resolving relative paths.
-}
descriptionToSource :: FilePath -> BashToolboxDescription -> ToolSource
descriptionToSource baseDir (FileSystemDirectory desc) =
    let fsDir = fromMaybe "" desc.fsDirRoot </> desc.fsDirPath
        resolved = if isRelative fsDir then baseDir ++ "/" ++ fsDir else desc.fsDirPath
     in DirectorySource desc.fsDirPath desc.fsDirBasenameFilter resolved
descriptionToSource baseDir (SingleTool desc) =
    let resolved = if isRelative desc.singleToolPath then baseDir ++ "/" ++ desc.singleToolPath else desc.singleToolPath
     in SingleSource resolved

-- | Check if a path is relative (simple heuristic).
isRelative :: FilePath -> Bool
isRelative path = not (List.isPrefixOf "/" path) && not (List.isPrefixOf "~" path)

-------------------------------------------------------------------------------

-- | Loading error type.
data LoadingError
    = LoadingError String [BashTools.InvalidScriptError]
    deriving (Show)

{- | Initialize multiple bash tool sources.

This creates background loaders for each source, allowing async reloading.
-}
initializeMultiSourceToolbox ::
    Tracer IO Trace ->
    -- | Base directory for resolving relative paths
    FilePath ->
    [BashToolboxDescription] ->
    IO (Either LoadingError MultiSourceBashTools)
initializeMultiSourceToolbox tracer baseDir descriptions = do
    let sources = map (descriptionToSource baseDir) descriptions
    results <- mapM (initializeSource tracer) sources
    let (errors, bgTools) = partitionEithers results
    if null errors
        then do
            -- Combine all reload triggers into one
            let combinedTrigger = fmap or $ sequence $ map triggerReload bgTools
            pure $ Right $ MultiSourceBashTools bgTools combinedTrigger
        else
            pure $ Left $ LoadingError "errors when loading tool sources" (concatMap extractScriptErrors errors)
  where
    extractScriptErrors :: LoadingError -> [BashTools.InvalidScriptError]
    extractScriptErrors (LoadingError _ errs) = errs

-- | Initialize a single tool source.
initializeSource ::
    Tracer IO Trace ->
    ToolSource ->
    IO (Either LoadingError BackgroundBashTools)
initializeSource tracer source = case source of
    DirectorySource _ mFilter path -> initializeDirectorySource tracer mFilter path source
    SingleSource path -> initializeSingleSource tracer path source

-- | Initialize a directory source.
initializeDirectorySource ::
    Tracer IO Trace ->
    Maybe Text ->
    FilePath ->
    ToolSource ->
    IO (Either LoadingError BackgroundBashTools)
initializeDirectorySource tracer mFilter path source = do
    (startingTools, errs) <- loadDirectoryOnce mFilter
    if null errs
        then do
            lock <- newEmptyTMVarIO
            bkgTools <-
                Background.background
                    (contramap ReloadToolsTrace tracer)
                    ()
                    startingTools.scriptDescriptions
                    (const (reloadDirectoryOnTrigger lock mFilter))
            let triggerReloadTools = tryPutTMVar lock ()
            pure $ Right (BackgroundBashTools source bkgTools triggerReloadTools)
        else do
            pure $ Left (LoadingError ("errors when loading directory: " ++ path) errs)
  where
    loadDirectoryOnce :: Maybe Text -> IO (BashTools.Scripts, [BashTools.InvalidScriptError])
    loadDirectoryOnce filterMay = do
        (scripts, errs) <- BashTools.loadDirectory (contramap BashToolsLoadingTrace tracer) path
        let filtered = case filterMay of
                Nothing -> scripts
                Just filt -> filterScriptsByBasename filt scripts
        pure (filtered, errs)

    reloadDirectoryOnTrigger :: TMVar () -> Maybe Text -> IO ([BashTools.ScriptDescription], ())
    reloadDirectoryOnTrigger lock filterMay = do
        _ <- atomically $ takeTMVar lock
        (scripts, _) <- loadDirectoryOnce filterMay
        pure (scripts.scriptDescriptions, ())

-- | Initialize a single tool source.
initializeSingleSource ::
    Tracer IO Trace ->
    FilePath ->
    ToolSource ->
    IO (Either LoadingError BackgroundBashTools)
initializeSingleSource tracer path source = do
    result <- loadScriptOnce
    case result of
        Left err -> pure $ Left (LoadingError ("error when loading script: " ++ path) [err])
        Right script -> do
            lock <- newEmptyTMVarIO
            bkgTools <-
                Background.background
                    (contramap ReloadToolsTrace tracer)
                    ()
                    [script]
                    (const (reloadScriptOnTrigger lock))
            let triggerReloadTools = tryPutTMVar lock ()
            pure $ Right (BackgroundBashTools source bkgTools triggerReloadTools)
  where
    loadScriptOnce :: IO (Either BashTools.InvalidScriptError BashTools.ScriptDescription)
    loadScriptOnce = BashTools.loadScript (contramap BashToolsLoadingTrace tracer) path

    reloadScriptOnTrigger :: TMVar () -> IO ([BashTools.ScriptDescription], ())
    reloadScriptOnTrigger lock = do
        _ <- atomically $ takeTMVar lock
        result <- loadScriptOnce
        case result of
            Left _ -> pure ([], ()) -- Keep empty on error, will retry on next reload
            Right script -> pure ([script], ())

{- | Filter scripts by basename filter.
Only keeps scripts whose filename contains the filter string.
-}
filterScriptsByBasename :: Text -> BashTools.Scripts -> BashTools.Scripts
filterScriptsByBasename filt scripts =
    let filteredDescs = filter matchesFilter scripts.scriptDescriptions
        matchesFilter :: BashTools.ScriptDescription -> Bool
        matchesFilter desc = Text.isInfixOf filt (Text.pack $ takeFileName desc.scriptPath)
     in scripts{BashTools.scriptDescriptions = filteredDescs}

-------------------------------------------------------------------------------

{- | Legacy compatibility: Initialize a single directory toolbox.

This is kept for backward compatibility with existing code.
-}
initializeBackroundToolbox ::
    Tracer IO Trace ->
    FilePath ->
    IO (Either LoadingError BackgroundBashTools)
initializeBackroundToolbox tracer tooldir = do
    let source = DirectorySource tooldir Nothing tooldir
    initializeSource tracer source

-- | Read all tools from a multi-source toolbox.
readMultiSourceTools :: MultiSourceBashTools -> IO [BashTools.ScriptDescription]
readMultiSourceTools multi = do
    allTools <- mapM (Background.readBackgroundVal . tools) multi.sources
    pure $ concat allTools
