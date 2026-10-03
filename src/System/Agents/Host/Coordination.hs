{- | What several host processes sharing one database need from it to run
each session's run on exactly one of them: a lease on the run, and signals
about what the other processes wrote.

A process holds a session's /run lease/ for as long as it runs it, and
renews it while the run lasts. A lease that is not renewed expires, after
which another process may take the session over. A process with no lease on
a session treats a run held elsewhere as busy: messages go to the session's
mail, and nothing is stored over the run's own writes.

'noCoordination' is the single-process case (SQLite, tests): every lease is
granted and nothing is signalled. @agents-postgres@ provides the real one,
on the @run_owner@ and @run_lease_until@ columns and @LISTEN@\/@NOTIFY@.
-}
module System.Agents.Host.Coordination (
    Coordination (..),
    LeaseResult (..),
    SessionSignal (..),
    noCoordination,
) where

import Data.Text (Text)
import Data.Time (NominalDiffTime)

import System.Agents.Session.Types (SessionId)

-- | The answer to a lease request.
data LeaseResult
    = -- | This process now holds the lease.
      LeaseAcquired
    | -- | Another process holds it, and it has not expired.
      LeaseHeldBy Text
    | -- | There is no such session.
      LeaseNoSession
    deriving (Show, Eq)

-- | Something another process did to a session.
data SessionSignal
    = -- | It accepted mail for the session.
      MailAccepted
    | -- | It stored a new version of the session, or deleted it.
      SessionStored
    deriving (Show, Eq, Ord)

data Coordination = Coordination
    { coEnabled :: Bool
    {- ^ Whether other processes may run sessions of the same database. When
    'False', a runner starts no heartbeat and asks for no lease.
    -}
    , coInstance :: Text
    -- ^ This process's name as a lease owner; unique among the processes.
    , coAcquire :: SessionId -> NominalDiffTime -> IO LeaseResult
    {- ^ Take the session's run lease for the given time, if it is free,
    expired, or already this process's.
    -}
    , coRenew :: [SessionId] -> NominalDiffTime -> IO [SessionId]
    {- ^ Extend the leases this process still holds among the given
    sessions, and answer with those. A session missing from the answer was
    taken over by another process.
    -}
    , coRelease :: SessionId -> IO ()
    -- ^ Give the session's lease back, if this process holds it.
    , coHolder :: SessionId -> IO (Maybe Text)
    -- ^ The other process holding the session's unexpired lease, if any.
    , coExpired :: IO [SessionId]
    {- ^ Sessions stored as running whose lease, held by another process,
    has expired: their owner stopped renewing.
    -}
    , coAbandoned :: IO [SessionId]
    {- ^ Like 'coExpired', and also running sessions with no lease at all
    (written before leases existed). For startup only.
    -}
    , coListen :: (SessionId -> SessionSignal -> IO ()) -> IO (IO ())
    {- ^ Call the handler for each signal from another process, until the
    returned action is run. The handler must not block.
    -}
    }

-- | A single process: every lease is granted, and there are no signals.
noCoordination :: Coordination
noCoordination =
    Coordination
        { coEnabled = False
        , coInstance = "local"
        , coAcquire = \_ _ -> pure LeaseAcquired
        , coRenew = \sids _ -> pure sids
        , coRelease = \_ -> pure ()
        , coHolder = \_ -> pure Nothing
        , coExpired = pure []
        , coAbandoned = pure []
        , coListen = \_ -> pure (pure ())
        }
