{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : SqliteVersionsTests
Description : Listing the snapshots of a versioned SQLite toolbox
-}
module SqliteVersionsTests (tests) where

import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Data.Time (addUTCTime, getCurrentTime)
import qualified Data.UUID as UUID
import Prod.Tracer (silent)
import System.Directory (listDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import System.Agents.Base (
    ConversationId (..),
    SqliteRetention (..),
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
        "SQLite versions listing"
        [ testCase "lists this session's snapshots, newest first, with the head marked" $
            withToolbox $ \_ _ box -> do
                h0 <- write box (ctx 1 1) "CREATE TABLE t (x INTEGER)"
                h1 <- write box (ctx 1 1) "INSERT INTO t VALUES (1)"
                r <- expectRight =<< listVersions box (ctx 1 1) (ListVersionsInput False Nothing)
                listedTotal r @?= 2
                map (key . versionHandle) (listedVersions r) @?= [key h1, key h0]
                map versionIsHead (listedVersions r) @?= [True, False]
                -- other sessions see nothing unless they ask for all of them
                other <- expectRight =<< listVersions box (ctx 2 1) (ListVersionsInput False Nothing)
                listedTotal other @?= 0
                everyone <- expectRight =<< listVersions box (ctx 2 1) (ListVersionsInput True Nothing)
                listedTotal everyone @?= 2
        , testCase "limit keeps the most recent, total counts all" $
            withToolbox $ \_ _ box -> do
                _ <- write box (ctx 1 1) "CREATE TABLE t (x INTEGER)"
                h1 <- write box (ctx 1 2) "INSERT INTO t VALUES (1)"
                r <- expectRight =<< listVersions box (ctx 1 2) (ListVersionsInput False (Just 1))
                listedTotal r @?= 2
                map (key . versionHandle) (listedVersions r) @?= [key h1]
        , testCase "after a restart, listed handles still restore" $
            withToolbox $ \root base box -> do
                _ <- write box (ctx 1 1) "CREATE TABLE t (x INTEGER)"
                _ <- write box (ctx 1 1) "INSERT INTO t VALUES (1)"
                fresh <- expectRightS =<< initializeToolbox silent (versionedDesc root base)
                r <- expectRight =<< listVersions fresh (ctx 3 1) (ListVersionsInput True Nothing)
                map versionIsHead (listedVersions r) @?= [False, False]
                counts <-
                    mapM
                        ( \e -> do
                            q <- expectRight =<< executeQuery silent fresh (ctx 3 1) (SqliteQueryInput "SELECT count(*) FROM t" (Just (versionHandle e)))
                            pure (resultRows q)
                        )
                        (listedVersions r)
                counts @?= [[[Aeson.Number 1]], [[Aeson.Number 0]]]
        , testCase "a read-write toolbox has no versions" $
            withSystemTempDirectory "sqlite-versions" $ \dir -> do
                box <-
                    expectRightS
                        =<< initializeToolbox
                            silent
                            SqliteToolboxDescription
                                { sqliteToolboxName = "mem"
                                , sqliteToolboxDescription = "memory"
                                , sqliteToolboxVersioning = SqliteReadWrite (dir </> "rw.sqlite")
                                , sqliteToolboxActivation = Nothing
                                }
                r <- listVersions box (ctx 1 1) (ListVersionsInput False Nothing)
                case r of
                    Left _ -> pure ()
                    Right _ -> assertFailure "expected an error"
        , testGroup
            "retention"
            [ testCase "maxSnapshots keeps the newest ones, older handles stop resolving" $
                withToolboxR (SqliteRetention (Just 2) Nothing) $ \_ _ box -> do
                    h0 <- write box (ctx 1 1) "CREATE TABLE t (x INTEGER)"
                    _ <- write box (ctx 1 1) "INSERT INTO t VALUES (1)"
                    h2 <- write box (ctx 1 1) "INSERT INTO t VALUES (2)"
                    h3 <- write box (ctx 1 1) "INSERT INTO t VALUES (3)"
                    r <- expectRight =<< listVersions box (ctx 1 1) (ListVersionsInput True Nothing)
                    map (key . versionHandle) (listedVersions r) @?= [key h3, key h2]
                    gone <- resolveVersionPath (sqliteToolboxVersioning (toolboxConfig box)) "mem" h0
                    case gone of
                        Left _ -> pure ()
                        Right p -> assertFailure ("pruned snapshot still resolves: " ++ p)
                    -- the head still holds every write
                    q <- expectRight =<< executeQuery silent box (ctx 1 1) (SqliteQueryInput "SELECT count(*) FROM t" Nothing)
                    resultRows q @?= [[Aeson.Number 3]]
            , testCase "a live head of another session is never pruned" $
                withToolboxR (SqliteRetention (Just 1) Nothing) $ \_ _ box -> do
                    _ <- write box (ctx 1 1) "CREATE TABLE t (x INTEGER)"
                    other <- write box (ctx 2 1) "CREATE TABLE u (y INTEGER)"
                    mine <- write box (ctx 1 1) "INSERT INTO t VALUES (1)"
                    r <- expectRight =<< listVersions box (ctx 1 1) (ListVersionsInput True Nothing)
                    map (key . versionHandle) (listedVersions r) @?= [key mine, key other]
            , testCase "maxAgeDays deletes old snapshots and empty directories, sparing kept paths" $
                withToolbox $ \root _ box -> do
                    _ <- write box (ctx 1 1) "CREATE TABLE t (x INTEGER)"
                    _ <- write box (ctx 2 1) "CREATE TABLE t (x INTEGER)"
                    found <- scanSnapshots root "mem" Nothing
                    newest <- case found of
                        ((_, p) : _) -> pure p
                        [] -> assertFailure "no snapshot written"
                    now <- getCurrentTime
                    let later = addUTCTime (3 * 86400) now
                        byAge = SqliteRetention Nothing (Just 1)
                    none <- pruneSnapshots byAge root "mem" now []
                    none @?= []
                    spared <- pruneSnapshots byAge root "mem" later [newest]
                    length spared @?= 1
                    rest <- pruneSnapshots byAge root "mem" later []
                    rest @?= [newest]
                    leftovers <- listDirectory root
                    leftovers @?= []
            , testCase "keepAllSnapshots deletes nothing" $
                withToolbox $ \root _ box -> do
                    _ <- write box (ctx 1 1) "CREATE TABLE t (x INTEGER)"
                    later <- addUTCTime (365 * 86400) <$> getCurrentTime
                    deleted <- pruneSnapshots keepAllSnapshots root "mem" later []
                    deleted @?= []
            ]
        ]

key :: SqliteVersionHandle -> (Text, Text, Int, Text)
key h = (vhSessionId h, vhTurnId h, vhToolCallIndex h, vhToolboxName h)

versionedDesc :: FilePath -> FilePath -> SqliteToolboxDescription
versionedDesc = versionedDescR keepAllSnapshots

versionedDescR :: SqliteRetention -> FilePath -> FilePath -> SqliteToolboxDescription
versionedDescR retention root base =
    SqliteToolboxDescription
        { sqliteToolboxName = "mem"
        , sqliteToolboxDescription = "memory"
        , sqliteToolboxVersioning = SqliteVersioned SharingConversation (Just root) base retention
        , sqliteToolboxActivation = Nothing
        }

withToolbox :: (FilePath -> FilePath -> Toolbox -> IO a) -> IO a
withToolbox = withToolboxR keepAllSnapshots

withToolboxR :: SqliteRetention -> (FilePath -> FilePath -> Toolbox -> IO a) -> IO a
withToolboxR retention act =
    withSystemTempDirectory "sqlite-versions" $ \dir -> do
        let root = dir </> "snapshots"
            base = dir </> "base.sqlite"
        box <- expectRightS =<< initializeToolbox silent (versionedDescR retention root base)
        act root base box

ctx :: Int -> Int -> ToolExecutionContext
ctx s t =
    mkMinimalContext
        (SessionId (uuid s))
        (ConversationId (uuid s))
        (TurnId (uuid (100 + t)))
        (\_ _ -> pure (ToolResult Aeson.Null 0 "test"))
  where
    uuid n = UUID.fromWords 0 0 0 (fromIntegral n)

write :: Toolbox -> ToolExecutionContext -> Text -> IO SqliteVersionHandle
write box c sql = do
    r <- expectRight =<< executeQuery silent box c (SqliteQueryInput sql Nothing)
    maybe (assertFailure "write returned no versionHandle") pure (resultVersionHandle r)

expectRight :: (Show e) => Either e a -> IO a
expectRight = either (\e -> assertFailure ("unexpected error: " ++ show e)) pure

expectRightS :: Either String a -> IO a
expectRightS = either (\e -> assertFailure ("unexpected error: " ++ e)) pure
