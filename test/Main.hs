module Main where

import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Data.Time (UTCTime (..), fromGregorian)
import System.Posix.Types (CMode (..))

import qualified System.Agents.Base as Base
import qualified System.Agents.ExportImport.Namespace as NS
import System.Agents.ExportImport.Namespace (Namespace (..))
import qualified System.Agents.ExportImport.Types as EI
import System.Agents.ExportImport.Types (
    AgentExport (..),
    ExportPackage (..),
    McpServerExport (..),
    PackageMetadata (..),
    SchemaVersion (..),
    ToolContentMode (..),
    ToolExport (..),
 )
-- Import the JSON module to get the ToJSON/FromJSON instances
import System.Agents.ExportImport.JSON ()
import System.Agents.LLMs.OpenAI as OpenAI

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "All Tests"
        [ openaiRateLimitTests
        , namespaceParsingTests
        , exportImportRoundTripTests
        ]

-------------------------------------------------------------------------------
-- OpenAI Rate Limit Tests (existing)
-------------------------------------------------------------------------------

openaiRateLimitTests :: TestTree
openaiRateLimitTests =
    testGroup
        "OpenAI rate-limits"
        [ testCase "1d1h1m1s" $
            parseRateLimitDelay "1d1h1m1s" @?= 1 + 60 + 3600 + 86400
        , testCase "1h1m1s" $
            parseRateLimitDelay "1h1m1s" @?= 1 + 60 + 3600
        , testCase "1m1s" $
            parseRateLimitDelay "1m1s" @?= 1 + 60
        , testCase "1s" $
            parseRateLimitDelay "1s" @?= 1
        , testCase "1.376s" $
            parseRateLimitDelay "1.376s" @?= 2
        , testCase "120ms" $
            parseRateLimitDelay "120ms" @?= 1
        , testCase "26.001s" $
            parseRateLimitDelay "26.001s" @?= 27
        ]

-------------------------------------------------------------------------------
-- Namespace Parsing Tests
-------------------------------------------------------------------------------

namespaceParsingTests :: TestTree
namespaceParsingTests =
    testGroup
        "Namespace parsing"
        [ testCase "parse dot notation" $
            NS.parseNamespace "team1.project-a"
                @?= Right (Namespace ["team1", "project-a"])
        , testCase "parse path notation" $
            NS.parseNamespace "team1/project-a"
                @?= Right (Namespace ["team1", "project-a"])
        , testCase "reject empty namespace" $
            case NS.parseNamespace "" of
                Left _ -> pure ()
                Right _ -> assertFailure "Should reject empty namespace"
        , testCase "reject mixed separators" $
            case NS.parseNamespace "team1.project-a/agent" of
                Left _ -> pure ()
                Right _ -> assertFailure "Should reject mixed separators"
        , testCase "namespaceToPath" $
            NS.namespaceToPath (Namespace ["a", "b", "c"])
                @?= "a/b/c"
        , testCase "namespaceToGitPath" $
            NS.namespaceToGitPath (Namespace ["a", "b", "c"])
                @?= "a/b/c"
        , testCase "namespaceToDotNotation" $
            NS.namespaceToDotNotation (Namespace ["a", "b", "c"])
                @?= "a.b.c"
        , testCase "namespaceParent" $
            NS.namespaceParent (Namespace ["a", "b", "c"])
                @?= Just (Namespace ["a", "b"])
        , testCase "namespaceParent of single component" $
            NS.namespaceParent (Namespace ["a"])
                @?= Nothing
        , testCase "namespaceBaseName" $
            NS.namespaceBaseName (Namespace ["a", "b", "c"])
                @?= Just "c"
        , testCase "isValidNamespaceComponent valid" $
            NS.isValidNamespaceComponent "valid-name" @?= True
        , testCase "isValidNamespaceComponent invalid with dot" $
            NS.isValidNamespaceComponent "invalid.name" @?= False
        , testCase "isValidNamespaceComponent empty" $
            NS.isValidNamespaceComponent "" @?= False
        , testCase "namespaceDepth" $
            NS.namespaceDepth (Namespace ["a", "b", "c"]) @?= 3
        , testCase "parseNamespaceLenient with mixed separators" $
            NS.parseNamespaceLenient "team1.project-a/agent-name"
                @?= Right (Namespace ["team1", "project-a", "agent-name"])
        ]

-------------------------------------------------------------------------------
-- Export/Import Round-Trip Tests
-------------------------------------------------------------------------------

exportImportRoundTripTests :: TestTree
exportImportRoundTripTests =
    testGroup
        "Export/Import JSON round-trip"
        [ testCase "SchemaVersion round-trip" $ do
            let original = SchemaVersion 1 2 3
            let encoded = encode original
            case decode encoded of
                Just decoded -> decoded @?= original
                Nothing -> assertFailure "Failed to decode SchemaVersion"
        , testCase "ToolContentMode EmbeddedContent round-trip" $ do
            let original = EmbeddedContent "test content"
            let encoded = encode original
            case decode encoded of
                Just decoded -> decoded @?= original
                Nothing -> assertFailure "Failed to decode EmbeddedContent"
        , testCase "ToolContentMode ReferencedContent round-trip" $ do
            let original = ReferencedContent "tools/my-tool.sh"
            let encoded = encode original
            case decode encoded of
                Just decoded -> decoded @?= original
                Nothing -> assertFailure "Failed to decode ReferencedContent"
        , testCase "CMode round-trip" $ do
            let original = CMode 0o755
            let encoded = encode original
            case decode encoded of
                Just decoded -> decoded @?= original
                Nothing -> assertFailure "Failed to decode CMode"
        , testCase "PackageMetadata round-trip" $ do
            let original =
                    PackageMetadata
                        { packageSchemaVersion = SchemaVersion 1 0 0
                        , packageCreatedAt = sampleTime
                        , packageDescription = Just "Test description"
                        , packageSource = Just "https://github.com/test/repo"
                        }
            let encoded = encode original
            case decode encoded of
                Just decoded -> decoded @?= original
                Nothing -> assertFailure "Failed to decode PackageMetadata"
        , testCase "ToolExport round-trip" $ do
            let original =
                    ToolExport
                        { EI.toolName = "my-tool"
                        , EI.toolContent = EmbeddedContent "#!/bin/bash\necho hello"
                        , EI.toolPermissions = CMode 0o755
                        , EI.toolNamespace = Just "team1/project-a"
                        , EI.toolMetadata = Just "commit: abc123"
                        }
            let encoded = encode original
            case decode encoded of
                Just decoded -> decoded @?= original
                Nothing -> assertFailure "Failed to decode ToolExport"
        , testCase "McpServerExport round-trip" $ do
            let original =
                    McpServerExport
                        { EI.mcpConfig = sampleMcpConfig
                        , EI.mcpNamespace = Just "infra/servers"
                        }
            let encoded = encode original
            case decode encoded of
                Just decoded -> decoded @?= original
                Nothing -> assertFailure "Failed to decode McpServerExport"
        ]

-------------------------------------------------------------------------------
-- Sample Data for Tests
-------------------------------------------------------------------------------

sampleTime :: UTCTime
sampleTime = UTCTime (fromGregorian 2024 1 15) (fromIntegral $ 10 * 3600 + 30 * 60)

-- | Sample MCP server configuration for testing
sampleMcpConfig :: Base.McpServerDescription
sampleMcpConfig =
    Base.McpSimpleBinary
        Base.McpSimpleBinaryConfiguration
            { Base.name = "test-server"
            , Base.executable = "/usr/local/bin/test-server"
            , Base.args = ["--config", "/etc/config.json"]
            }

