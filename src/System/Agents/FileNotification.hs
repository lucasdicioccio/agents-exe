-- todo: upstream to prodapi once the design settles
module System.Agents.FileNotification where

import Control.Concurrent.STM (STM, TMVar)
import qualified Control.Concurrent.STM as STM
import Control.Monad (void)
import Prod.Tracer (Tracer, runTracer)
import qualified System.FSNotify as FSNotify

-------------------------------------------------------------------------------
data Trace
    = NotifyEvent FSNotify.Event

-------------------------------------------------------------------------------
-- note: would be nice to have some way to dynamically change the set of watched dirs
data Runtime
    = Runtime
    { notifyManager :: FSNotify.WatchManager
    , waitForChange :: STM ()
    }

-------------------------------------------------------------------------------
initRuntime :: Tracer IO Trace -> FilePath -> (FSNotify.Event -> Bool) -> IO Runtime
initRuntime tracer path shouldNotify = do
    fsTVar <- STM.newEmptyTMVarIO
    notify <- FSNotify.startManager
    _ <- FSNotify.watchDir notify path shouldNotify (handleFSEvent fsTVar)
    pure $ Runtime notify (STM.takeTMVar fsTVar)
  where
    handleFSEvent :: TMVar () -> FSNotify.Event -> IO ()
    handleFSEvent x ev = do
        runTracer tracer $ NotifyEvent ev
        void $ STM.atomically $ STM.tryPutTMVar x ()

isAboutFileChange :: FSNotify.Event -> Bool
isAboutFileChange ev = case ev of
    FSNotify.Added _ _ _ -> True
    FSNotify.CloseWrite _ _ _ -> True
    FSNotify.Modified _ _ _ -> True
    FSNotify.ModifiedAttributes _ _ _ -> False
    FSNotify.Unknown _ _ _ _ -> False
    FSNotify.Removed _ _ _ -> False
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
