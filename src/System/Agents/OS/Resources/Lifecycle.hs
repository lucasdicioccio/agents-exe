{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Agents.OS.Resources.Lifecycle (
    SessionLifecycle (..),
    SessionState (..),
    ManagedResource (..),
    ResourceScope (..),
    ResourcePersistability (..),
    SessionManager,
    newSessionManager,
    sessionManagerRegistry,
    suspendSession,
    resumeSession,
    forceCleanupSession,
    withResourceTracking,
    registerCleanup,
    registerManagedResource,
    startTimeoutWatcher,
    stopTimeoutWatcher,
    pingSessionActivity,
    SessionTimeoutConfig (..),
    defaultTimeoutConfig,
    cleanupIdleResources,
    getIdleResourceInfo,
    IdleResourceInfo (..),
    SuspendedSession (..),
    ResourceSnapshot (..),
    PersistedResource (..),
    takeResourceSnapshot,
    restoreFromSnapshot,
) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Exception (bracket, catch, try)
import qualified Control.Exception
import Control.Monad (forM_, forever, unless, when)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import Data.List (sortOn)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time
import GHC.Generics (Generic)

import System.Agents.OS.Core.Types
import System.Agents.OS.Resources.Types hiding (ResourceScope)
import System.Agents.Session.Types hiding (TurnId)

data SessionLifecycle = Active | Suspending | Suspended | Resuming
    deriving (Show, Eq, Ord, Generic)

data SessionState = SessionState
    { ssLifecycle :: SessionLifecycle
    , ssSessionId :: SessionId
    , ssConversationId :: ConversationId
    , ssLastActivity :: UTCTime
    , ssResourceCount :: Int
    }
    deriving (Show, Generic)

data ResourcePersistability = Persistent | Transient | Recreatable
    deriving (Show, Eq, Ord, Generic)

data ManagedResource = ManagedResource
    { mrHandle :: ResourceHandle
    , mrPersistability :: ResourcePersistability
    , mrCreatedAt :: UTCTime
    , mrLastAccessed :: TVar UTCTime
    , mrScope :: ResourceScope
    , mrSnapshot :: Maybe ResourceSnapshot
    }

instance Show ManagedResource where
    show mr = "ManagedResource{" ++ show (mrPersistability mr) ++ "}"

data ResourceScope
    = ScopeProgram
    | ScopeSession SessionId
    | ScopeConversation ConversationId
    | ScopeTurn TurnId
    | ScopeToolCall Text
    deriving (Show, Eq, Ord, Generic)

data ResourceSnapshot = ResourceSnapshot
    { rsResourceId :: ResourceId
    , rsResourceType :: Text
    , rsSerializedState :: Maybe Text
    , rsConfigHash :: Text
    }
    deriving (Show, Generic)

data PersistedResource = PersistedResource
    { prResourceId :: ResourceId
    , prSnapshot :: ResourceSnapshot
    , prRestoreAction :: IO (Either String ResourceHandle)
    }

instance Show PersistedResource where
    show pr = "PersistedResource{" ++ show (prResourceId pr) ++ "}"

data SessionManager = SessionManager
    { smRegistry :: ResourceRegistry
    , smState :: TVar SessionState
    , smResources :: TVar (HashMap ResourceId ManagedResource)
    , smCleanupActions :: TVar [IO ()]
    , smTimeoutWatcher :: TVar (Maybe TimeoutWatcherHandle)
    , _smSuspendedSessions :: TVar (HashMap SessionId SuspendedSession)
    }

newtype TimeoutWatcherHandle = TimeoutWatcherHandle
    {unTimeoutWatcher :: TVar Bool}

sessionManagerRegistry :: SessionManager -> ResourceRegistry
sessionManagerRegistry = smRegistry

newSessionManager :: ResourceRegistry -> SessionId -> ConversationId -> IO SessionManager
newSessionManager registry sessId convId = do
    now <- getCurrentTime
    stateVar <- newTVarIO $ SessionState Active sessId convId now 0
    resourcesVar <- newTVarIO HashMap.empty
    cleanupVar <- newTVarIO []
    timeoutVar <- newTVarIO Nothing
    suspendedVar <- newTVarIO HashMap.empty
    return $ SessionManager registry stateVar resourcesVar cleanupVar timeoutVar suspendedVar

data SuspendedSession = SuspendedSession
    { suspendState :: SessionState
    , suspendResourceSnapshots :: [ResourceSnapshot]
    , suspendTimestamp :: UTCTime
    , suspendPersistentResources :: [PersistedResource]
    }
    deriving (Show, Generic)

partitionHashMap :: (Hashable k) => (v -> Bool) -> HashMap k v -> (HashMap k v, HashMap k v)
partitionHashMap p = HashMap.foldrWithKey go (HashMap.empty, HashMap.empty)
  where
    go k v (as, bs)
        | p v = (HashMap.insert k v as, bs)
        | otherwise = (as, HashMap.insert k v bs)

suspendSession :: SessionManager -> SessionState -> IO SuspendedSession
suspendSession mgr state = do
    now <- getCurrentTime
    atomically $ writeTVar (smState mgr) $ state{ssLifecycle = Suspending}
    resources <- readTVarIO (smResources mgr)
    let (transient, persistent) = partitionHashMap ((== Transient) . mrPersistability) resources
    cleanupResourcesByAge mgr transient
    snapshots <- catMaybes <$> mapM takeResourceSnapshot (HashMap.elems persistent)
    let persisted = mapMaybe mkPersistedResource (HashMap.toList persistent)
    atomically $ writeTVar (smState mgr) $ state{ssLifecycle = Suspended, ssResourceCount = length persisted, ssLastActivity = now}
    return $ SuspendedSession (state{ssLifecycle = Suspended, ssLastActivity = now}) snapshots now persisted
  where
    mkPersistedResource (rid, mr) = do
        snapshot <- mrSnapshot mr
        return $ PersistedResource rid snapshot (return $ Left "Not implemented")

resumeSession :: SessionManager -> SuspendedSession -> IO (Either String SessionState)
resumeSession mgr suspended = do
    now <- getCurrentTime
    currentState <- readTVarIO (smState mgr)
    case ssLifecycle currentState of
        Suspended -> do
            atomically $ writeTVar (smState mgr) $ currentState{ssLifecycle = Resuming}
            restoreResults <- mapM restorePersistedResource (suspendPersistentResources suspended)
            let failures = lefts restoreResults
            if not (null failures)
                then do
                    atomically $ writeTVar (smState mgr) $ currentState{ssLifecycle = Suspended}
                    return $ Left $ "Failed to restore resources: " ++ unlines failures
                else do
                    let restoredCount = length (rights restoreResults)
                    let newState = (suspendState suspended){ssLifecycle = Active, ssLastActivity = now, ssResourceCount = restoredCount}
                    atomically $ writeTVar (smState mgr) newState
                    return $ Right newState
        other -> return $ Left $ "Cannot resume from state: " ++ show other
  where
    lefts = foldr (\e acc -> case e of Left x -> x : acc; _ -> acc) []
    rights = foldr (\e acc -> case e of Right x -> x : acc; _ -> acc) []
    restorePersistedResource pr = do
        result <- try (prRestoreAction pr)
        case result of
            Left (e :: Control.Exception.SomeException) -> return $ Left $ show e
            Right (Left err) -> return $ Left err
            Right (Right _) -> return $ Right ()

forceCleanupSession :: SessionManager -> IO ()
forceCleanupSession mgr = do
    resources <- readTVarIO (smResources mgr)
    cleanupResourcesByAge mgr resources
    actions <- readTVarIO (smCleanupActions mgr)
    forM_ actions $ \action -> catch action (\(_ :: Control.Exception.SomeException) -> return ())
    atomically $ do
        writeTVar (smResources mgr) HashMap.empty
        writeTVar (smCleanupActions mgr) []
    stopTimeoutWatcher mgr

cleanupResourcesByAge :: SessionManager -> HashMap ResourceId ManagedResource -> IO ()
cleanupResourcesByAge mgr resources = do
    let sorted = sortOn (mrCreatedAt . snd) (HashMap.toList resources)
    forM_ (reverse sorted) $ \(_, mr) -> do
        result <- try (handleCleanup (mrHandle mr))
        case result of
            Left (e :: Control.Exception.SomeException) -> putStrLn $ "Error during resource cleanup: " ++ show e
            Right () -> return ()
    atomically $ do
        current <- readTVar (smResources mgr)
        let remaining = foldr HashMap.delete current (map fst (HashMap.toList resources))
        writeTVar (smResources mgr) remaining

withResourceTracking :: SessionManager -> ResourceScope -> IO a -> IO a
withResourceTracking mgr scope action = bracket (return ()) (\_ -> cleanupScopeResources mgr scope) (\_ -> action)

cleanupScopeResources :: SessionManager -> ResourceScope -> IO ()
cleanupScopeResources mgr scope = do
    resources <- readTVarIO (smResources mgr)
    let inScope = HashMap.filter ((== scope) . mrScope) resources
    cleanupResourcesByAge mgr inScope

registerCleanup :: SessionManager -> IO () -> IO ()
registerCleanup mgr action = atomically $ modifyTVar' (smCleanupActions mgr) (action :)

registerManagedResource :: SessionManager -> ResourceId -> ResourceHandle -> ResourcePersistability -> ResourceScope -> IO ()
registerManagedResource mgr rid handle persistability scope = do
    now <- getCurrentTime
    lastAccessed <- newTVarIO now
    let managedRes = ManagedResource handle persistability now lastAccessed scope Nothing
    atomically $ do
        modifyTVar' (smResources mgr) $ HashMap.insert rid managedRes
        resources <- readTVar (smResources mgr)
        modifyTVar' (smState mgr) $ \s -> s{ssResourceCount = HashMap.size resources}

data SessionTimeoutConfig = SessionTimeoutConfig
    { stcIdleTimeout :: NominalDiffTime
    , stcCheckInterval :: NominalDiffTime
    , stcOnTimeout :: SessionManager -> IO ()
    }

defaultTimeoutConfig :: SessionTimeoutConfig
defaultTimeoutConfig = SessionTimeoutConfig (secondsToNominalDiffTime 300) (secondsToNominalDiffTime 30) defaultTimeoutHandler

defaultTimeoutHandler :: SessionManager -> IO ()
defaultTimeoutHandler mgr = do
    state <- readTVarIO (smState mgr)
    case ssLifecycle state of
        Active -> do
            putStrLn $ "Session " ++ show (ssSessionId state) ++ " idle timeout reached, suspending..."
            _ <- suspendSession mgr state
            return ()
        _ -> return ()

startTimeoutWatcher :: SessionManager -> SessionTimeoutConfig -> IO ()
startTimeoutWatcher mgr config = do
    stopTimeoutWatcher mgr
    stopVar <- newTVarIO False
    let handle = TimeoutWatcherHandle stopVar
    atomically $ writeTVar (smTimeoutWatcher mgr) (Just handle)
    _ <- forkIO $ timeoutWatcherLoop mgr config stopVar
    return ()

timeoutWatcherLoop :: SessionManager -> SessionTimeoutConfig -> TVar Bool -> IO ()
timeoutWatcherLoop mgr config stopVar = forever $ do
    shouldStop <- readTVarIO stopVar
    when shouldStop $ return ()
    let delayMicros = round (stcCheckInterval config * 1000000)
    threadDelay delayMicros
    shouldStop2 <- readTVarIO stopVar
    unless shouldStop2 $ do
        isIdle <- isSessionIdle mgr (stcIdleTimeout config)
        when isIdle $ stcOnTimeout config mgr

isSessionIdle :: SessionManager -> NominalDiffTime -> IO Bool
isSessionIdle mgr timeout = do
    state <- readTVarIO (smState mgr)
    case ssLifecycle state of
        Active -> do
            now <- getCurrentTime
            return $ diffUTCTime now (ssLastActivity state) > timeout
        _ -> return False

stopTimeoutWatcher :: SessionManager -> IO ()
stopTimeoutWatcher mgr = do
    mHandle <- atomically $ do
        mH <- readTVar (smTimeoutWatcher mgr)
        writeTVar (smTimeoutWatcher mgr) Nothing
        return mH
    case mHandle of
        Nothing -> return ()
        Just handle -> atomically $ writeTVar (unTimeoutWatcher handle) True

pingSessionActivity :: SessionManager -> IO ()
pingSessionActivity mgr = do
    now <- getCurrentTime
    atomically $ do
        state <- readTVar (smState mgr)
        when (ssLifecycle state == Active) $ writeTVar (smState mgr) $ state{ssLastActivity = now}

data IdleResourceInfo = IdleResourceInfo
    { iriResourceId :: ResourceId
    , iriIdleTime :: NominalDiffTime
    , iriScope :: ResourceScope
    , iriPersistability :: ResourcePersistability
    }
    deriving (Show, Generic)

getIdleResourceInfo :: SessionManager -> NominalDiffTime -> IO [IdleResourceInfo]
getIdleResourceInfo mgr threshold = do
    now <- getCurrentTime
    resources <- readTVarIO (smResources mgr)
    catMaybes <$> mapM (checkIdle now) (HashMap.toList resources)
  where
    checkIdle now (rid, mr) = do
        lastAccess <- readTVarIO (mrLastAccessed mr)
        let idleTime = diffUTCTime now lastAccess
        if idleTime > threshold
            then return $ Just $ IdleResourceInfo rid idleTime (mrScope mr) (mrPersistability mr)
            else return Nothing

cleanupIdleResources :: SessionManager -> NominalDiffTime -> IO Int
cleanupIdleResources mgr threshold = do
    idleResources <- getIdleResourceInfo mgr threshold
    let toCleanup = filter ((== Transient) . iriPersistability) idleResources
    resources <- readTVarIO (smResources mgr)
    let cleanupMap = HashMap.filterWithKey (\k _ -> k `elem` map iriResourceId toCleanup) resources
    cleanupResourcesByAge mgr cleanupMap
    return $ HashMap.size cleanupMap

takeResourceSnapshot :: ManagedResource -> IO (Maybe ResourceSnapshot)
takeResourceSnapshot mr = do
    case mrPersistability mr of
        Persistent -> do
            let rid = handleId (mrHandle mr)
            now <- getCurrentTime
            return $ Just $ ResourceSnapshot rid "generic" Nothing (Text.pack $ show now)
        _ -> return Nothing

restoreFromSnapshot :: ResourceSnapshot -> IO (Either String ResourceHandle)
restoreFromSnapshot _ = return $ Left "restoreFromSnapshot: not fully implemented"
