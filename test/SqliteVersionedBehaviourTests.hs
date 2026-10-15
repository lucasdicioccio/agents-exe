{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : SqliteVersionedBehaviourTests
Description : End-to-end behaviour of a versioned SQLite toolbox

Write, take the returned handle, restore from it, check what it holds;
branch from an older snapshot; promote a snapshot to the base database; and
the four sharing scopes. Listing, retention and unresolvable handles have
their own modules ('SqliteVersionsTests', 'SqliteRestoreFromTests').
-}
module SqliteVersionedBehaviourTests (tests) where

import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.UUID as UUID
import qualified Database.SQLite.Simple as Simple
import Prod.Tracer (silent)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import System.Agents.Base (
    ConversationId (..),
    SqliteSharing (..),
    SqliteToolboxDescription (..),
    SqliteVersionHandle (..),
    SqliteVersioningConfig (..),
    keepAllSnapshots,
 )
import System.Agents.Session.Types (SessionId (..), TurnId (..))
import System.Agents.Tools.Context (ToolExecutionContext, ToolResult (..), mkMinimalContext)
import System.Agents.Tools.SqliteToolbox

tests :: TestTree
tests =
    testGroup
        "SQLite versioned behaviour"
        [ testGroup
            "write, handle, restore"
            [ testCase "a write returns a handle naming its snapshot; a read returns none and stores nothing" $
                withVersioned SharingConversation $ \env box -> do
                    h <- write box (ctx 1 1) "INSERT INTO t VALUES (1)"
                    vhToolboxName h @?= "mem"
                    vhToolCallIndex h @?= 0
                    path <- expectRight =<< resolveVersionPath (versioning box) "mem" h
                    path @?= buildSnapshotPath (envRoot env) (sid 1) (tid 1) 0 "mem"
                    r <- expectRight =<< executeQuery silent box (ctx 1 1) (SqliteQueryInput "SELECT x FROM t" Nothing)
                    resultVersionHandle r @?= Nothing
                    found <- scanSnapshots (envRoot env) "mem" Nothing
                    length found @?= 1
            , testCase "writes never touch the base database" $
                withVersioned SharingConversation $ \env box -> do
                    _ <- write box (ctx 1 1) "INSERT INTO t VALUES (1)"
                    _ <- write box (ctx 1 1) "INSERT INTO t VALUES (2)"
                    baseCount env >>= (@?= 0)
            , testCase "a handle keeps reading its own state after later writes" $
                withVersioned SharingConversation $ \_ box -> do
                    h1 <- write box (ctx 1 1) "INSERT INTO t VALUES (1)"
                    _ <- write box (ctx 1 1) "INSERT INTO t VALUES (2)"
                    _ <- write box (ctx 1 1) "DELETE FROM t WHERE x = 1"
                    values box (ctx 1 1) (Just h1) >>= (@?= [1])
                    values box (ctx 1 1) Nothing >>= (@?= [2])
            , testCase "a write from restore_from branches off that snapshot and becomes the head" $
                withVersioned SharingConversation $ \_ box -> do
                    h1 <- write box (ctx 1 1) "INSERT INTO t VALUES (1)"
                    _ <- write box (ctx 1 1) "INSERT INTO t VALUES (2)"
                    branch <- writeFrom box (ctx 1 1) h1 "INSERT INTO t VALUES (9)"
                    values box (ctx 1 1) Nothing >>= (@?= [1, 9])
                    values box (ctx 1 1) (Just branch) >>= (@?= [1, 9])
                    values box (ctx 1 1) (Just h1) >>= (@?= [1])
            , testCase "a read from restore_from leaves the head where it was" $
                withVersioned SharingConversation $ \_ box -> do
                    h1 <- write box (ctx 1 1) "INSERT INTO t VALUES (1)"
                    _ <- write box (ctx 1 1) "INSERT INTO t VALUES (2)"
                    values box (ctx 1 1) (Just h1) >>= (@?= [1])
                    values box (ctx 1 1) Nothing >>= (@?= [1, 2])
            ]
        , testGroup
            "promote"
            [ testCase "without a handle, the current head becomes the base for new conversations" $
                withVersioned SharingConversation $ \env box -> do
                    _ <- write box (ctx 1 1) "INSERT INTO t VALUES (1)"
                    _ <- write box (ctx 1 1) "INSERT INTO t VALUES (2)"
                    r <- expectRight =<< promoteSnapshot box (ctx 1 1) (PromoteInput Nothing)
                    promotedTo r @?= envBase env
                    baseCount env >>= (@?= 2)
                    values box (ctx 2 1) Nothing >>= (@?= [1, 2])
            , testCase "with a handle, that snapshot becomes the base" $
                withVersioned SharingConversation $ \env box -> do
                    h1 <- write box (ctx 1 1) "INSERT INTO t VALUES (1)"
                    _ <- write box (ctx 1 1) "INSERT INTO t VALUES (2)"
                    r <- expectRight =<< promoteSnapshot box (ctx 1 1) (PromoteInput (Just h1))
                    promotedFrom r @?= buildSnapshotPath (envRoot env) (sid 1) (tid 1) 0 "mem"
                    baseCount env >>= (@?= 1)
                    values box (ctx 2 1) Nothing >>= (@?= [1])
                    -- the promoting conversation keeps its own head
                    values box (ctx 1 1) Nothing >>= (@?= [1, 2])
            , testCase "a promoted base survives a restart" $
                withVersioned SharingConversation $ \env box -> do
                    _ <- write box (ctx 1 1) "INSERT INTO t VALUES (7)"
                    _ <- expectRight =<< promoteSnapshot box (ctx 1 1) (PromoteInput Nothing)
                    fresh <- expectRightS =<< initializeToolbox silent (versionedDesc SharingConversation env)
                    values fresh (ctx 1 1) Nothing >>= (@?= [7])
            , testCase "read-only and read-write toolboxes cannot promote" $
                withSystemTempDirectory "sqlite-behaviour" $ \dir -> do
                    let path = dir </> "plain.sqlite"
                    seedBase path
                    ro <- expectRightS =<< initializeToolbox silent (plainDesc (SqliteReadOnly path))
                    rw <- expectRightS =<< initializeToolbox silent (plainDesc (SqliteReadWrite path))
                    mapM_
                        ( \b -> do
                            r <- promoteSnapshot b (ctx 1 1) (PromoteInput Nothing)
                            either (const (pure ())) (\_ -> assertFailure "promote succeeded") r
                        )
                        [ro, rw]
            ]
        , testGroup
            "sharing scopes"
            [ scopeCase SharingGlobal [1] [1] [1]
            , scopeCase SharingConversation [1] [1] []
            , scopeCase SharingTurn [1] [] []
            , scopeCase SharingToolCall [] [] []
            , testCase "tool-call: every write starts from the base, its handle still restores it" $
                withVersioned SharingToolCall $ \_ box -> do
                    h1 <- write box (ctx 1 1) "INSERT INTO t VALUES (1)"
                    h2 <- write box (ctx 1 1) "INSERT INTO t VALUES (2)"
                    values box (ctx 1 1) (Just h1) >>= (@?= [1])
                    values box (ctx 1 1) (Just h2) >>= (@?= [2])
                    -- chaining is explicit: write from a handle
                    h3 <- writeFrom box (ctx 1 1) h2 "INSERT INTO t VALUES (3)"
                    values box (ctx 1 1) (Just h3) >>= (@?= [2, 3])
            , testCase "turn: a new turn of the same conversation starts from the base again" $
                withVersioned SharingTurn $ \_ box -> do
                    _ <- write box (ctx 1 1) "INSERT INTO t VALUES (1)"
                    _ <- write box (ctx 1 2) "INSERT INTO t VALUES (2)"
                    values box (ctx 1 1) Nothing >>= (@?= [1])
                    values box (ctx 1 2) Nothing >>= (@?= [2])
            ]
        , testCase "a read-only toolbox refuses writes" $
            withSystemTempDirectory "sqlite-behaviour" $ \dir -> do
                let path = dir </> "plain.sqlite"
                seedBase path
                ro <- expectRightS =<< initializeToolbox silent (plainDesc (SqliteReadOnly path))
                r <- executeQuery silent ro (ctx 1 1) (SqliteQueryInput "INSERT INTO t VALUES (1)" Nothing)
                either (const (pure ())) (\_ -> assertFailure "write succeeded") r
                values ro (ctx 1 1) Nothing >>= (@?= [])
        , testCase "a versioned toolbox creates a missing base database" $
            withSystemTempDirectory "sqlite-behaviour" $ \dir -> do
                let env = Env (dir </> "versions") (dir </> "new.sqlite")
                _ <- expectRightS =<< initializeToolbox silent (versionedDesc SharingConversation env)
                doesFileExist (envBase env) >>= (@?= True)
        ]

{- | One write in session 1, turn 1; then the rows seen, without a handle,
by the same turn, the next turn of the same session, and another session.
-}
scopeCase :: SqliteSharing -> [Int] -> [Int] -> [Int] -> TestTree
scopeCase sharing sameTurn nextTurn otherSession =
    testCase (show sharing <> ": who sees a write") $
        withVersioned sharing $ \_ box -> do
            _ <- write box (ctx 1 1) "INSERT INTO t VALUES (1)"
            seen <- mapM (\c -> values box c Nothing) [ctx 1 1, ctx 1 2, ctx 2 1]
            seen @?= [sameTurn, nextTurn, otherSession]

data Env = Env
    { envRoot :: FilePath
    , envBase :: FilePath
    }

-- | A versioned toolbox named @mem@ whose base holds an empty table @t (x INTEGER)@.
withVersioned :: SqliteSharing -> (Env -> Toolbox -> IO a) -> IO a
withVersioned sharing act =
    withSystemTempDirectory "sqlite-behaviour" $ \dir -> do
        let env = Env (dir </> "versions") (dir </> "base.sqlite")
        seedBase (envBase env)
        box <- expectRightS =<< initializeToolbox silent (versionedDesc sharing env)
        act env box

versionedDesc :: SqliteSharing -> Env -> SqliteToolboxDescription
versionedDesc sharing env =
    plainDesc (SqliteVersioned sharing (Just (envRoot env)) (envBase env) keepAllSnapshots)

plainDesc :: SqliteVersioningConfig -> SqliteToolboxDescription
plainDesc v =
    SqliteToolboxDescription
        { sqliteToolboxName = "mem"
        , sqliteToolboxDescription = "memory"
        , sqliteToolboxVersioning = v
        , sqliteToolboxActivation = Nothing
        }

versioning :: Toolbox -> SqliteVersioningConfig
versioning = sqliteToolboxVersioning . toolboxConfig

seedBase :: FilePath -> IO ()
seedBase path =
    Simple.withConnection path $ \conn ->
        Simple.execute_ conn "CREATE TABLE t (x INTEGER)"

-- | Rows of @t@ in the base database, read without the toolbox.
baseCount :: Env -> IO Int
baseCount env =
    Simple.withConnection (envBase env) $ \conn -> do
        rows <- Simple.query_ conn "SELECT count(*) FROM t" :: IO [Simple.Only Int]
        pure $ case rows of
            [Simple.Only n] -> n
            _ -> -1

-- | The values of @t@, in order, as the toolbox reads them.
values :: Toolbox -> ToolExecutionContext -> Maybe SqliteVersionHandle -> IO [Int]
values box c from = do
    r <- expectRight =<< executeQuery silent box c (SqliteQueryInput "SELECT x FROM t ORDER BY rowid" from)
    mapM cell (resultRows r)
  where
    cell [Aeson.Number n] = pure (round n)
    cell other = assertFailure ("unexpected row: " <> show other)

write :: Toolbox -> ToolExecutionContext -> Text -> IO SqliteVersionHandle
write box c = writeWith box c Nothing

writeFrom :: Toolbox -> ToolExecutionContext -> SqliteVersionHandle -> Text -> IO SqliteVersionHandle
writeFrom box c h = writeWith box c (Just h)

writeWith :: Toolbox -> ToolExecutionContext -> Maybe SqliteVersionHandle -> Text -> IO SqliteVersionHandle
writeWith box c from sql = do
    r <- expectRight =<< executeQuery silent box c (SqliteQueryInput sql from)
    maybe (assertFailure "write returned no versionHandle") pure (resultVersionHandle r)

sid :: Int -> SessionId
sid n = SessionId (UUID.fromWords 0 0 0 (fromIntegral n))

tid :: Int -> TurnId
tid t = TurnId (UUID.fromWords 0 0 0 (fromIntegral (100 + t)))

ctx :: Int -> Int -> ToolExecutionContext
ctx s t =
    mkMinimalContext
        (sid s)
        (ConversationId (UUID.fromWords 0 0 0 (fromIntegral s)))
        (tid t)
        (\_ _ -> pure (ToolResult Aeson.Null 0 "test"))

expectRight :: (Show e) => Either e a -> IO a
expectRight = either (\e -> assertFailure ("unexpected error: " ++ show e)) pure

expectRightS :: Either String a -> IO a
expectRightS = either (\e -> assertFailure ("unexpected error: " ++ e)) pure
