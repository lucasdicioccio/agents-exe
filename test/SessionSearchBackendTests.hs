{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The search index and the durable CLI look at the SQLite sessions
database as well as the file store ('SessionStore.withStoredSessions').
-}
module SessionSearchBackendTests (tests) where

import qualified Data.Text as Text
import Database.SQLite.Simple (open)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import System.Agents.Session.Base
import System.Agents.Session.Search.Index (createSearchIndex)
import System.Agents.Session.Search.Query (executeSearchWithOptions)
import qualified System.Agents.Session.Search.Types as Search
import System.Agents.Session.Types
import System.Agents.SessionStore

tests :: TestTree
tests =
    testGroup
        "session search on SessionBackend"
        [ testCase "an indexed SQLite session is found by search" $
            withSystemTempDirectory "search-backend" $ \dir -> do
                let db = dir </> "agents-server.db"
                    files = mkSimpleSessionStore dir
                seed db "hello zanzibar"
                withStoredSessions files (Just db) $ \catalog _ -> do
                    let config =
                            (Search.defaultSearchIndexConfig catalog){Search.indexDbPath = dir </> "idx.db"}
                    createSearchIndex config
                    result <- executeSearchWithOptions config (searchFor "zanzibar")
                    length result.resultItems @?= 1
                    result <- executeSearchWithOptions config (searchFor "nonexistentword")
                    length result.resultItems @?= 0
        , testCase "without a database only the file store is seen" $
            withSystemTempDirectory "search-backend" $ \dir -> do
                let files = mkSimpleSessionStore dir
                withStoredSessions files (Just (dir </> "missing.db")) $ \catalog mBackend -> do
                    entries <- catList catalog
                    length entries @?= 0
                    assertBool "no backend" (null (fmap (const ()) mBackend))
        , testCase "unionCatalog keeps the first of duplicate sessions" $
            withSystemTempDirectory "search-backend" $ \dir -> do
                let db = dir </> "agents-server.db"
                seed db "one"
                withStoredSessions (mkSimpleSessionStore dir) (Just db) $ \catalog _ -> do
                    entries <- catList (unionCatalog [catalog, catalog])
                    length entries @?= 1
        ]
  where
    seed db text = do
        conn <- open db
        backend <- mkSqliteSessionStore conn
        sid <- newSessionId
        sess <- newSessionFromPrompt sid (SystemPrompt "sys") [] (UserQuery text [])
        sbStore backend sid sess

    searchFor q =
        Search.SearchOptions
            { Search.searchQuery = Text.pack q
            , Search.searchDateFilter = Nothing
            , Search.searchTools = []
            , Search.searchAgent = Nothing
            , Search.searchIncludeToolOutputs = False
            , Search.searchJsonOutput = False
            , Search.searchPreviewLines = 0
            , Search.searchLimit = Nothing
            , Search.searchAutoUpdate = False
            }
