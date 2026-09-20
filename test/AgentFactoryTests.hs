{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests for 'System.Agents.AgentFactory', the single agent constructor.
module AgentFactoryTests (
    tests,

    -- * Fixtures
    mockDeps,
    mockCompletion,
    testNode,
    newConvId,
) where

import Control.Concurrent.STM (newTVarIO)
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Maybe (isJust)
import Data.UUID.V4 (nextRandom)
import Database.SQLite.Simple (open)
import Prod.Tracer (silent)
import System.Directory (doesFileExist, listDirectory)
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import System.Agents.AgentFactory
import System.Agents.AgentTree (OSAgentNode (..))
import System.Agents.Base (AgentId (..), ConversationId (..))
import qualified System.Agents.Base as Base
import System.Agents.Session.Base
import System.Agents.Session.Loop (run)
import qualified System.Agents.SessionStore as SessionStore
import System.Agents.Tools.Context (CallStackEntry (..))

tests :: TestTree
tests =
    testGroup
        "AgentFactory"
        [ testCase "backend sink stores under the session id and writes no files" $
            withSystemTempDirectory "agent-factory" $ \dir -> do
                backend <- SessionStore.mkSqliteSessionStore =<< open ":memory:"
                node <- testNode "{}"
                convId <- newConvId
                let deps = mockDeps{adSessionSink = SinkBackend backend}
                sess <- runToCompletion deps convId node
                stored <- backend.sbLoad sess.sessionId
                fmap (map turnKind . turns) stored @?= Just ["llm", "user"]
                files <- listDirectory dir
                files @?= []
        , testCase "file sink stores under the conversation id" $
            withSystemTempDirectory "agent-factory" $ \dir -> do
                let store = SessionStore.mkSimpleSessionStore dir
                node <- testNode "{}"
                convId <- newConvId
                _ <- runToCompletion (mockDeps{adSessionSink = SinkFiles store}) convId node
                exists <- doesFileExist (SessionStore.sessionWritePath store convId)
                assertBool "session file written" exists
        , testCase "a backend installed after building does not redirect storage" $
            withSystemTempDirectory "agent-factory" $ \dir -> do
                backend <- SessionStore.mkSqliteSessionStore =<< open ":memory:"
                let store = SessionStore.mkSimpleSessionStore dir
                node <- testNode "{}"
                convId <- newConvId
                agent <- buildAgent silent (mockDeps{adSessionSink = SinkFiles store}) RootAgent convId node
                sess0 <- initialSession agent
                (_, sess) <- run convId (withSessionBackend backend agent) sess0
                exists <- doesFileExist (SessionStore.sessionWritePath store convId)
                assertBool "file sink chosen at build time is kept" exists
                stored <- backend.sbLoad sess.sessionId
                assertBool "backend untouched" (not (isJust stored))
        , testCase "the completion override is used" $ do
            calls <- newIORef (0 :: Int)
            node <- testNode "{}"
            convId <- newConvId
            let deps =
                    mockDeps
                        { adCompletion = Just $ \_ completion -> do
                            modifyIORef' calls (+ 1)
                            mockCompletion completion
                        }
            _ <- runToCompletion deps convId node
            readIORef calls >>= (@?= 1)
        , testCase "root agents start their own call stack" $ do
            node <- testNode "{}"
            convId <- newConvId
            agent <- buildAgent silent mockDeps RootAgent convId node
            agent.ctxCallStack @?= [CallStackEntry "root" convId 0]
            agent.ctxParentConversation @?= Nothing
        , testCase "sub-agents record their parent and call stack" $ do
            node <- testNode "{}"
            parent <- newConvId
            child <- newConvId
            let stack = [CallStackEntry "helper" child 1, CallStackEntry "root" parent 0]
            agent <- buildAgent silent mockDeps (SubAgent parent stack) child node
            agent.ctxCallStack @?= stack
            agent.ctxParentConversation @?= Just parent
        , testCase "execution settings from the agent JSON are applied" $ do
            node <- testNode "{\"executionMode\": \"asynchronous\", \"maxConcurrency\": 7}"
            convId <- newConvId
            agent <- buildAgent silent mockDeps RootAgent convId node
            agent.ctxExecutionMode @?= Asynchronous
            agent.ctxMaxConcurrency @?= Just 7
        , testCase "newSessionFromPrompt holds one user turn with the query" $ do
            sid <- newSessionId
            sess <- newSessionFromPrompt sid (SystemPrompt "sys") [] (UserQuery "hello" [])
            sess.sessionId @?= sid
            sess.sessionExecutionMode @?= Just Asynchronous
            case sess.turns of
                [UserTurn content Nothing] -> fmap (.queryText) content.userQuery @?= Just "hello"
                _ -> assertFailure "expected a single user turn"
        ]

-------------------------------------------------------------------------------
-- Fixtures
-------------------------------------------------------------------------------

-- | Dependencies with no storage and a mock LLM.
mockDeps :: AgentDeps
mockDeps = (defaultAgentDeps []){adCompletion = Just (const mockCompletion)}

-- | An LLM that answers immediately without calling tools.
mockCompletion :: Completion
mockCompletion _ = pure (LlmResponse (Just "done") Nothing Aeson.Null Nothing, [])

-- | An agent node whose JSON config is the base config merged with the given fields.
testNode :: String -> IO OSAgentNode
testNode extraFields = do
    let base =
            Aeson.object
                [ "slug" Aeson..= ("test-agent" :: String)
                , "apiKeyId" Aeson..= ("none" :: String)
                , "flavor" Aeson..= ("openai" :: String)
                , "modelUrl" Aeson..= ("http://localhost:1" :: String)
                , "modelName" Aeson..= ("mock" :: String)
                , "announce" Aeson..= ("a test agent" :: String)
                , "systemPrompt" Aeson..= ["You are a test" :: String]
                ]
    extra <- either fail pure (Aeson.eitherDecode (LBS8.pack extraFields))
    cfg <- case Aeson.fromJSON (merge base extra) of
        Aeson.Success c -> pure c
        Aeson.Error err -> fail err
    tools <- newTVarIO []
    agentId <- AgentId <$> nextRandom
    pure
        OSAgentNode
            { osNodeFile = "test-agent.json"
            , osNodeConfig = cfg :: Base.Agent
            , osNodeAgentId = agentId
            , osNodeChildren = []
            , osNodeTools = tools
            }
  where
    merge (Aeson.Object a) (Aeson.Object b) = Aeson.Object (b <> a)
    merge a _ = a

newConvId :: IO ConversationId
newConvId = ConversationId <$> nextRandom

initialSession :: Agent r -> IO Session
initialSession agent = do
    sPrompt <- agent.sysPrompt
    sTools <- agent.sysTools
    sid <- newSessionId
    newSessionFromPrompt sid sPrompt sTools (UserQuery "hello" [])

-- | Build an agent, run it on a fresh session, and return the final session.
runToCompletion :: AgentDeps -> ConversationId -> OSAgentNode -> IO Session
runToCompletion deps convId node = do
    agent <- buildAgent silent deps RootAgent convId node
    sess0 <- initialSession agent
    snd <$> run convId agent sess0

turnKind :: Turn -> String
turnKind UserTurn{} = "user"
turnKind LlmTurn{} = "llm"
turnKind PartialUserTurn{} = "partial"
