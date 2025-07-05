-- todo: upstream to prodapi once the design settles
{-# LANGUAGE ScopedTypeVariables #-}

module System.Agents.FileNotification where

import Control.Concurrent.STM (STM, TMVar)
import qualified Control.Concurrent.STM as STM
import Control.Monad (void)
import Data.Foldable (forM_)
import Prod.Tracer (Tracer, runTracer)
import qualified System.FSNotify as FSNotify

-------------------------------------------------------------------------------
data Trace obj
    = NotifyEvent obj FSNotify.Event

-------------------------------------------------------------------------------
-- note: would be nice to have some way to dynamically change the set of watched dirs
data Runtime obj
    = Runtime
    { notifyManager :: FSNotify.WatchManager
    , waitForChange :: STM obj
    }

-------------------------------------------------------------------------------
initRuntime ::
    forall obj.
    Tracer IO (Trace obj) ->
    [(obj, FilePath)] ->
    (obj -> FSNotify.Event -> Bool) ->
    IO (Runtime obj)
initRuntime tracer paths shouldNotify = do
    fsTVar <- STM.newEmptyTMVarIO
    notify <- FSNotify.startManager
    forM_ paths $ \(obj, path) ->
        FSNotify.watchDir notify path (shouldNotify obj) (handleFSEvent fsTVar obj)
    pure $ Runtime notify (STM.takeTMVar fsTVar)
  where
    handleFSEvent :: TMVar obj -> obj -> FSNotify.Event -> IO ()
    handleFSEvent x obj ev = do
        runTracer tracer $ NotifyEvent obj ev
        void $ STM.atomically $ STM.tryPutTMVar x obj

isAboutFileChange :: FSNotify.Event -> Bool
isAboutFileChange ev = case ev of
    FSNotify.Added _ _ _ -> True
    FSNotify.CloseWrite _ _ _ -> True
    FSNotify.Modified _ _ _ -> True
    FSNotify.ModifiedAttributes _ _ _ -> True
    FSNotify.Removed _ _ _ -> True
    FSNotify.Unknown _ _ _ _ -> False
    FSNotify.WatchedDirectoryRemoved _ _ _ -> False

getFilePath :: FSNotify.Event -> FilePath
getFilePath ev = case ev of
    FSNotify.Added p _ _ -> p
    FSNotify.CloseWrite p _ _ -> p
    FSNotify.Modified p _ _ -> p
    FSNotify.ModifiedAttributes p _ _ -> p
    FSNotify.Removed p _ _ -> p
    FSNotify.Unknown p _ _ _ -> p
    FSNotify.WatchedDirectoryRemoved p _ _ -> p
