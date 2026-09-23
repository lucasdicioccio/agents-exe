{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | 'Host.hcLegacySessionDirs' (@todos/os-as-standalone-server.md@ §6): a
session written by the old file store, in a directory the config lists as
a read prefix but does not write to, must be readable through a 'Host'
opened with 'Host.withHost' over a *different* (SQLite) primary backend.
This is the same code path the TUI (@tui/System/Agents/CLI/TUI.hs@) and
@agents-exe serve@ (@app/Main.hs@'s @handleServe@) now both go through,
instead of each hand-rolling the composite store.
-}
module HostLegacySessionTests (tests) where

import qualified Data.Map.Strict as Map
import Data.Time (getCurrentTime)
import Prod.Tracer (silent)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import System.Agents.Host (defaultHostConfig, hcLegacySessionDirs, hostBackend, withHost)
import System.Agents.Session.Base (SessionStatus (..), newSessionFromPrompt, newSessionId)
import System.Agents.Session.Types (Session (..), SystemPrompt (..), UserQuery (..))
import System.Agents.SessionStore (SessionMeta (..), mkFileSessionStore, sbCompareAndStore, sbLoad)

tests :: TestTree
tests =
    testGroup
        "Host.hcLegacySessionDirs (a legacy file-store session is readable through a host opened over SQLite)"
        [testCase "sbLoad finds a session written only to a legacy read-prefix directory" legacySessionReadableTest]

legacySessionReadableTest :: Assertion
legacySessionReadableTest =
    withSystemTempDirectory "host-legacy-session" $ \dir -> do
        let legacyDir = dir </> "legacy-sessions" </> ""
            dbPath = dir </> "host.db"
        legacyBackend <- mkFileSessionStore legacyDir

        -- Write a session directly to the legacy file store, as an old
        -- `conv.<uuid>.json` write would have, without the SQLite backend
        -- (opened below) ever seeing it.
        sid <- newSessionId
        sess <- newSessionFromPrompt sid (SystemPrompt "sys") [] (UserQuery "hello, legacy" [])
        now <- getCurrentTime
        let meta =
                SessionMeta
                    { smSessionId = sid
                    , smAgent = Just "test-agent"
                    , smParent = Nothing
                    , smOwner = Nothing
                    , smStatus = StatusReady
                    , smStatusDetail = Nothing
                    , smVersion = 0
                    , smCreatedAt = now
                    , smUpdatedAt = now
                    , smParams = Map.empty
                    }
        stored <- sbCompareAndStore legacyBackend meta sess
        case stored of
            Left _ -> assertFailure "failed to write the legacy fixture session"
            Right _ -> pure ()

        -- Open a host over an unrelated, empty SQLite database, with the
        -- legacy directory as a read fallback only (§6). No agent files
        -- are needed here: only 'hostBackend' is under test.
        let cfg = (defaultHostConfig [] "/dev/null" dbPath){hcLegacySessionDirs = [legacyDir]}
        withHost cfg silent $ \host -> do
            found <- sbLoad host.hostBackend sid
            case found of
                Nothing -> assertFailure "expected the legacy session to be readable through the composite backend"
                Just loaded -> loaded.sessionId @?= sid
