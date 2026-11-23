{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Tests for the Lua toolbox HTTP modes: the default allowlist, the
denylist (@"HttpMode": "denylist"@ with @DeniedHosts@), and redirects, which
are re-checked hop by hop in both modes.

The request tests run against a local Warp server on 127.0.0.1, reached
either as @127.0.0.1@ or as @localhost@ so that one server stands for an
allowed and a denied host.
-}
module LuaToolboxHttpDenylistTests (luaToolboxHttpDenylistTests) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.UUID as UUID
import Network.HTTP.Types (status200, status302, status404)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Prod.Tracer (silent)
import Test.Tasty
import Test.Tasty.HUnit

import System.Agents.Base (
    ConversationId (..),
    LuaHttpMode (..),
    LuaToolboxDescription (..),
    luaToolboxEffectiveDeniedHosts,
    luaToolboxEffectiveHttpMode,
 )
import System.Agents.Session.Types (SessionId (..), TurnId (..))
import System.Agents.Tools.Context (ToolExecutionContext, ToolPortal, ToolResult (..), mkMinimalContext)
import qualified System.Agents.Tools.LuaToolbox as LuaToolbox
import qualified System.Agents.Tools.LuaToolbox.Modules.Http as HttpMod

luaToolboxHttpDenylistTests :: TestTree
luaToolboxHttpDenylistTests =
    testGroup
        "LuaToolbox HTTP allowlist/denylist modes"
        [ jsonTests
        , validateTests
        , resolveTests
        , requestTests
        ]

-------------------------------------------------------------------------------
-- JSON
-------------------------------------------------------------------------------

decodeDesc :: LBS8.ByteString -> Either String LuaToolboxDescription
decodeDesc = Aeson.eitherDecode

-- | An allowlist config as written before HttpMode existed.
legacyAllowlistJson :: LBS8.ByteString
legacyAllowlistJson =
    "{\"Name\": \"lua\", \"Description\": \"d\", \"MaxMemoryMB\": 64, \"MaxExecutionTimeSeconds\": 10, \"AllowedTools\": [], \"AllowedHosts\": [\"localhost\", \"127.0.0.1\"]}"

-- | The denylist example from the docs ('LuaToolboxDescription' haddock and documentation/tools.md).
denylistDocExampleJson :: LBS8.ByteString
denylistDocExampleJson =
    LBS8.unlines
        [ "{"
        , "  \"tag\": \"LuaToolbox\","
        , "  \"contents\": {"
        , "    \"Name\": \"lua\","
        , "    \"Description\": \"Lua with open web access, minus instance metadata\","
        , "    \"MaxMemoryMB\": 256,"
        , "    \"MaxExecutionTimeSeconds\": 300,"
        , "    \"AllowedTools\": [],"
        , "    \"HttpMode\": \"denylist\","
        , "    \"DeniedHosts\": [\"169.254.169.254\", \"fd00:ec2::254\", \"metadata.google.internal\"]"
        , "  }"
        , "}"
        ]

jsonTests :: TestTree
jsonTests =
    testGroup
        "JSON"
        [ testCase "an existing allowlist config parses unchanged (allowlist, nothing denied)" $ do
            d <- either (assertFailure . ("parse: " <>)) pure (decodeDesc legacyAllowlistJson)
            d.luaToolboxAllowedHosts @?= ["localhost", "127.0.0.1"]
            d.luaToolboxHttpMode @?= Nothing
            luaToolboxEffectiveHttpMode d @?= LuaHttpAllowlist
            luaToolboxEffectiveDeniedHosts d @?= []
            -- and re-encodes without the new keys
            case Aeson.toJSON d of
                Aeson.Object o -> do
                    assertBool "no HttpMode key" (not (KeyMap.member "HttpMode" o))
                    assertBool "no DeniedHosts key" (not (KeyMap.member "DeniedHosts" o))
                other -> assertFailure ("expected an object, got " <> show other)
        , testCase "the denylist doc example parses (AllowedHosts may be left out)" $ do
            v <- either (assertFailure . ("parse: " <>)) pure (Aeson.eitherDecode denylistDocExampleJson :: Either String Aeson.Value)
            contents <- case v of
                Aeson.Object o | Just c <- KeyMap.lookup "contents" o -> pure c
                _ -> assertFailure "no contents"
            d <- case Aeson.fromJSON contents of
                Aeson.Success d -> pure d
                Aeson.Error e -> assertFailure ("parse: " <> e)
            luaToolboxEffectiveHttpMode d @?= LuaHttpDenylist
            d.luaToolboxAllowedHosts @?= []
            luaToolboxEffectiveDeniedHosts d @?= ["169.254.169.254", "fd00:ec2::254", "metadata.google.internal"]
        , testCase "denylist mode with a non-empty AllowedHosts is a parse error" $
            case decodeDesc "{\"Name\": \"lua\", \"Description\": \"d\", \"MaxMemoryMB\": 64, \"MaxExecutionTimeSeconds\": 10, \"AllowedTools\": [], \"AllowedHosts\": [\"a.com\"], \"HttpMode\": \"denylist\"}" of
                Left e -> assertBool ("mentions AllowedHosts: " <> e) ("AllowedHosts" `Text.isInfixOf` Text.pack e)
                Right d -> assertFailure ("expected a parse error, got " <> show d)
        , testCase "an unknown HttpMode is a parse error" $
            case decodeDesc "{\"Name\": \"lua\", \"Description\": \"d\", \"MaxMemoryMB\": 64, \"MaxExecutionTimeSeconds\": 10, \"AllowedTools\": [], \"HttpMode\": \"open\"}" of
                Left _ -> pure ()
                Right d -> assertFailure ("expected a parse error, got " <> show d)
        , testCase "allowlist mode accepts DeniedHosts" $ do
            d <- either (assertFailure . ("parse: " <>)) pure $
                decodeDesc "{\"Name\": \"lua\", \"Description\": \"d\", \"MaxMemoryMB\": 64, \"MaxExecutionTimeSeconds\": 10, \"AllowedTools\": [], \"AllowedHosts\": [\"a.com\"], \"HttpMode\": \"allowlist\", \"DeniedHosts\": [\"a.com\"]}"
            luaToolboxEffectiveHttpMode d @?= LuaHttpAllowlist
            luaToolboxEffectiveDeniedHosts d @?= ["a.com"]
        ]

-------------------------------------------------------------------------------
-- validateHost (by name)
-------------------------------------------------------------------------------

denyCfg :: [Text] -> HttpMod.HttpConfig
denyCfg denied = HttpMod.HttpConfig{HttpMod.httpMode = LuaHttpDenylist, HttpMod.httpAllowedHosts = [], HttpMod.httpDeniedHosts = denied}

allowCfg :: [Text] -> [Text] -> HttpMod.HttpConfig
allowCfg allowed denied = HttpMod.HttpConfig{HttpMod.httpMode = LuaHttpAllowlist, HttpMod.httpAllowedHosts = allowed, HttpMod.httpDeniedHosts = denied}

isLeft' :: Either a b -> Bool
isLeft' = either (const True) (const False)

validateTests :: TestTree
validateTests =
    testGroup
        "validateHost"
        [ testCase "denylist: an unlisted host is allowed" $
            HttpMod.validateHost (denyCfg ["169.254.169.254"]) "example.com" @?= Right ()
        , testCase "denylist: a listed host is refused" $
            case HttpMod.validateHost (denyCfg ["169.254.169.254"]) "169.254.169.254" of
                Left msg -> assertBool (Text.unpack msg) ("denied list" `Text.isInfixOf` msg)
                Right () -> assertFailure "expected refusal"
        , testCase "denylist: matching ignores case and a trailing dot" $
            assertBool "refused" (isLeft' (HttpMod.validateHost (denyCfg ["metadata.google.internal"]) "Metadata.Google.Internal."))
        , testCase "denylist: entries are exact hosts, not suffixes" $
            HttpMod.validateHost (denyCfg ["google.internal"]) "metadata.google.internal" @?= Right ()
        , testCase "denylist: an empty list allows everything" $
            HttpMod.validateHost (denyCfg []) "example.com" @?= Right ()
        , testCase "allowlist: DeniedHosts win over AllowedHosts" $
            assertBool "refused" (isLeft' (HttpMod.validateHost (allowCfg ["a.com", "b.com"] ["b.com"]) "b.com"))
        , testCase "allowlist: an allowed, undenied host passes" $
            HttpMod.validateHost (allowCfg ["a.com", "b.com"] ["b.com"]) "a.com" @?= Right ()
        ]

-------------------------------------------------------------------------------
-- checkUrl (resolution against IP-literal entries)
-------------------------------------------------------------------------------

resolveTests :: TestTree
resolveTests =
    testGroup
        "checkUrl"
        [ testCase "a host name resolving to a denied IP is refused" $ do
            r <- HttpMod.checkUrl (denyCfg ["127.0.0.1", "::1"]) "http://localhost/"
            assertBool (show r) (isLeft' r)
        , testCase "another spelling of a denied IPv4 is refused" $ do
            r <- HttpMod.checkUrl (denyCfg ["127.0.0.1"]) "http://2130706433/"
            assertBool (show r) (isLeft' r)
        , testCase "an IPv4-mapped IPv6 address of a denied IPv4 is refused" $ do
            r <- HttpMod.checkUrl (denyCfg ["127.0.0.1"]) "http://[::ffff:127.0.0.1]/"
            assertBool (show r) (isLeft' r)
        , testCase "a bracketed IPv6 entry is matched" $ do
            r <- HttpMod.checkUrl (denyCfg ["[::1]"]) "http://[::1]:8080/"
            assertBool (show r) (isLeft' r)
        , testCase "an address not on the list passes" $ do
            r <- HttpMod.checkUrl (denyCfg ["169.254.169.254"]) "http://127.0.0.1/"
            r @?= Right ()
        ]

-------------------------------------------------------------------------------
-- Requests through Lua against a local server
-------------------------------------------------------------------------------

{- | @/ok@ answers 200 "ok"; @/to-localhost@ redirects to @localhost@ on the
same port; @/to-self@ redirects to @/ok@ on the same host.
-}
testApp :: IO Int -> Wai.Application
testApp getPort req respond =
    case Wai.pathInfo req of
        ["ok"] -> respond (Wai.responseLBS status200 [] "ok")
        ["to-localhost"] -> do
            port <- getPort
            respond (Wai.responseLBS status302 [("Location", BS8.pack ("http://localhost:" <> show port <> "/ok"))] "")
        ["to-self"] -> respond (Wai.responseLBS status302 [("Location", "/ok")] "")
        _ -> respond (Wai.responseLBS status404 [] "")

withServer :: (Int -> IO a) -> IO a
withServer k = do
    portRef <- newIORef 0
    Warp.testWithApplication (pure (testApp (readIORef portRef))) $ \port -> do
        writeIORef portRef port
        k port

mkDesc :: Maybe LuaHttpMode -> [Text] -> Maybe [Text] -> LuaToolboxDescription
mkDesc mode allowed denied =
    LuaToolboxDescription
        { luaToolboxName = "test"
        , luaToolboxDescription = "HTTP mode test toolbox"
        , luaToolboxMaxMemoryMB = 64
        , luaToolboxMaxExecutionTimeSeconds = 10
        , luaToolboxAllowedTools = []
        , luaToolboxAllowedHosts = allowed
        , luaToolboxHttpMode = mode
        , luaToolboxDeniedHosts = denied
        , luaToolboxActivation = Nothing
        , luaToolboxFileSandbox = Nothing
        }

testCtx :: ToolExecutionContext
testCtx =
    mkMinimalContext
        (SessionId $ fromJust $ UUID.fromText "550e8400-e29b-41d4-a716-446655440011")
        (ConversationId $ fromJust $ UUID.fromText "550e8400-e29b-41d4-a716-446655440012")
        (TurnId $ fromJust $ UUID.fromText "550e8400-e29b-41d4-a716-446655440013")
        noPortal

noPortal :: ToolPortal
noPortal _ _ = pure ToolResult{resultData = Aeson.Null, resultDuration = 0, resultTraceId = "none"}

-- | Run @http.get(url)@ with a toolbox for @desc@.
luaGet :: LuaToolboxDescription -> String -> IO [Aeson.Value]
luaGet desc url = do
    initResult <- LuaToolbox.initializeToolbox silent desc
    case initResult of
        Left err -> assertFailure ("init: " <> err)
        Right box -> do
            r <- LuaToolbox.executeScriptWithPortal silent box ("return http.get('" <> Text.pack url <> "')") testCtx noPortal
            case r of
                Left err -> assertFailure ("script: " <> show err)
                Right res -> pure res.resultValues

assertOk :: [Aeson.Value] -> Assertion
assertOk vals = case vals of
    (Aeson.Object o : _) -> do
        KeyMap.lookup "status" o @?= Just (Aeson.Number 200)
        KeyMap.lookup "body" o @?= Just (Aeson.String "ok")
    _ -> assertFailure ("expected a 200 response table, got " <> show vals)

assertRefused :: Text -> [Aeson.Value] -> Assertion
assertRefused needle vals = case vals of
    (Aeson.String msg : Aeson.Null : _) ->
        assertBool ("expected " <> show needle <> " in " <> show msg) (needle `Text.isInfixOf` msg)
    _ -> assertFailure ("expected (nil, error), got " <> show vals)

requestTests :: TestTree
requestTests =
    testGroup
        "requests"
        [ testCase "denylist: an unlisted host is reached" $ withServer $ \port ->
            luaGet denyLocalhost ("http://127.0.0.1:" <> show port <> "/ok") >>= assertOk
        , testCase "denylist: a listed host is refused" $ withServer $ \port ->
            luaGet denyLocalhost ("http://localhost:" <> show port <> "/ok") >>= assertRefused "denied list"
        , testCase "denylist: a host resolving to a denied IP is refused" $ withServer $ \port ->
            luaGet (mkDesc (Just LuaHttpDenylist) [] (Just ["127.0.0.1", "::1"])) ("http://localhost:" <> show port <> "/ok")
                >>= assertRefused "denied address"
        , testCase "denylist: a redirect to an allowed host is followed" $ withServer $ \port ->
            luaGet denyLocalhost ("http://127.0.0.1:" <> show port <> "/to-self") >>= assertOk
        , testCase "denylist: a redirect to a listed host is refused" $ withServer $ \port ->
            luaGet denyLocalhost ("http://127.0.0.1:" <> show port <> "/to-localhost") >>= assertRefused "Redirect to http://localhost:"
        , testCase "allowlist (unchanged config): an allowed host is reached" $ withServer $ \port ->
            luaGet allow127 ("http://127.0.0.1:" <> show port <> "/ok") >>= assertOk
        , testCase "allowlist (unchanged config): another host is refused" $ withServer $ \port ->
            luaGet allow127 ("http://localhost:" <> show port <> "/ok") >>= assertRefused "Host not in allowed list"
        , testCase "allowlist: a redirect to a host off the list is refused" $ withServer $ \port ->
            luaGet allow127 ("http://127.0.0.1:" <> show port <> "/to-localhost") >>= assertRefused "refused: Host not in allowed list"
        ]
  where
    denyLocalhost = mkDesc (Just LuaHttpDenylist) [] (Just ["localhost"])
    allow127 = mkDesc Nothing ["127.0.0.1"] Nothing
