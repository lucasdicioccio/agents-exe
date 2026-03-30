{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module LuaToolboxJsonValueTests where

import Test.Tasty
import Test.Tasty.HUnit

import System.Agents.Base (LuaToolboxDescription (..))

testLuaToolbox :: LuaToolboxDescription
testLuaToolbox =
    LuaToolboxDescription
        { luaToolboxName = "test"
        , luaToolboxDescription = "Test toolbox"
        , luaToolboxMaxMemoryMB = 64
        , luaToolboxMaxExecutionTimeSeconds = 10
        , luaToolboxAllowedTools = []
        , luaToolboxAllowedPaths = []
        , luaToolboxAllowedHosts = []
        , luaToolboxLifetime = Nothing
        , luaToolboxActivation = Nothing
        }

luaToJsonValueTests :: TestTree
luaToJsonValueTests = testGroup "luaToJsonValue Tests" []

