{-# LANGUAGE OverloadedStrings #-}

{- | A @restore_from@ handle that does not resolve to a snapshot must be an
error, both for queries and for promotion: falling back to the base
database would silently read or promote the wrong data.
-}
module SqliteRestoreFromTests (tests) where

import qualified Data.Aeson as Aeson
import Data.Maybe (fromJust)
import qualified Data.UUID as UUID
import Prod.Tracer (silent)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import System.Agents.Base (ConversationId (..), SqliteSharing (..), SqliteToolboxDescription (..), SqliteVersionHandle (..), SqliteVersioningConfig (..))
import System.Agents.Session.Types (SessionId (..), TurnId (..))
import System.Agents.Tools.Context (ToolExecutionContext, ToolPortal, ToolResult (..), mkMinimalContext)
import System.Agents.Tools.SqliteToolbox

tests :: TestTree
tests =
    testGroup
        "SQLite versioned restore_from"
        [ testCase "query with a handle to a missing snapshot is a RestoreError" $
            withToolbox $ \box -> do
                handle <- writeOnce box
                r <- executeQuery silent box testContext (SqliteQueryInput "SELECT * FROM t" (Just handle{vhToolCallIndex = 99}))
                assertRestoreError r
        , testCase "query with a handle for another toolbox is a RestoreError" $
            withToolbox $ \box -> do
                handle <- writeOnce box
                r <- executeQuery silent box testContext (SqliteQueryInput "SELECT * FROM t" (Just handle{vhToolboxName = "other"}))
                assertRestoreError r
        , testCase "query with a malformed handle is a RestoreError" $
            withToolbox $ \box -> do
                handle <- writeOnce box
                r <- executeQuery silent box testContext (SqliteQueryInput "SELECT * FROM t" (Just handle{vhSessionId = "not-a-uuid"}))
                assertRestoreError r
        , testCase "write with an unresolvable handle leaves the head unchanged" $
            withToolbox $ \box -> do
                handle <- writeOnce box
                r <- executeQuery silent box testContext (SqliteQueryInput "INSERT INTO t VALUES (2)" (Just handle{vhToolCallIndex = 99}))
                assertRestoreError r
                rows <- executeQuery silent box testContext (SqliteQueryInput "SELECT * FROM t" Nothing)
                fmap resultRowCount rows @?= Right 1
        , testCase "query with a valid handle reads that snapshot" $
            withToolbox $ \box -> do
                handle <- writeOnce box
                r <- executeQuery silent box testContext (SqliteQueryInput "SELECT * FROM t" (Just handle))
                fmap resultRowCount r @?= Right 1
        , testCase "promote with an unresolvable handle is a RestoreError" $
            withToolbox $ \box -> do
                handle <- writeOnce box
                r <- promoteSnapshot box testContext (PromoteInput (Just handle{vhToolCallIndex = 99}))
                case r of
                    Left (RestoreError _) -> pure ()
                    other -> assertFailure ("expected a RestoreError, got " <> show (fmap promotedFrom other))
        ]

withToolbox :: (Toolbox -> IO a) -> IO a
withToolbox act =
    withSystemTempDirectory "sqlite-restore-from" $ \dir -> do
        let desc =
                SqliteToolboxDescription
                    { sqliteToolboxName = "mem"
                    , sqliteToolboxDescription = "test database"
                    , sqliteToolboxVersioning = SqliteVersioned SharingConversation (Just (dir </> "versions")) (dir </> "base.sqlite")
                    , sqliteToolboxActivation = Nothing
                    }
        r <- initializeToolbox silent desc
        case r of
            Left err -> assertFailure err >> fail err
            Right box -> act box

-- | Create a table holding one row, returning the handle of the last snapshot.
writeOnce :: Toolbox -> IO SqliteVersionHandle
writeOnce box = do
    _ <- expectRight =<< executeQuery silent box testContext (SqliteQueryInput "CREATE TABLE t (x INTEGER)" Nothing)
    r <- expectRight =<< executeQuery silent box testContext (SqliteQueryInput "INSERT INTO t VALUES (1)" Nothing)
    maybe (assertFailure "write returned no version handle" >> fail "no handle") pure (resultVersionHandle r)

expectRight :: (Show e) => Either e a -> IO a
expectRight (Right a) = pure a
expectRight (Left e) = assertFailure (show e) >> fail (show e)

assertRestoreError :: Either QueryError QueryResult -> Assertion
assertRestoreError (Left (RestoreError _)) = pure ()
assertRestoreError other = assertFailure ("expected a RestoreError, got " <> show (fmap resultRowCount other))

testContext :: ToolExecutionContext
testContext =
    mkMinimalContext
        (SessionId $ fromJust $ UUID.fromText "550e8400-e29b-41d4-a716-446655440001")
        (ConversationId $ fromJust $ UUID.fromText "550e8400-e29b-41d4-a716-446655440002")
        (TurnId $ fromJust $ UUID.fromText "550e8400-e29b-41d4-a716-446655440003")
        noPortal

noPortal :: ToolPortal
noPortal _ _ =
    pure
        ToolResult
            { resultData = Aeson.object []
            , resultDuration = 0
            , resultTraceId = "none"
            }
