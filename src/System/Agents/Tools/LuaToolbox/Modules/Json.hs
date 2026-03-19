{-# LANGUAGE OverloadedStrings #-}

-- | JSON module for LuaToolbox
--
-- Provides json.encode and json.decode functions for Lua scripts.
-- Uses Aeson for JSON handling.
module System.Agents.Tools.LuaToolbox.Modules.Json
    ( registerJsonModule
    , luaEncode
    , luaDecode
    , encodeLuaTable
    , decodeToLuaTable
    , makeLuaArray
    , makeLuaObject
    , isLuaArray
    ) where

import Data.Aeson (Value(..), encode, decodeStrict)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import qualified Data.Vector as Vector

-- | Register the json module in a Lua state
-- In a real implementation, this would register global functions
registerJsonModule :: () -> IO ()
registerJsonModule _ = do
    -- Register json.encode and json.decode as global functions
    pure ()

-- | Encode a Lua value (represented as Aeson Value) to JSON string
-- Returns either an error message or the JSON string
luaEncode :: Value -> Either String Text.Text
luaEncode value = 
    let jsonBytes = encode value
    in Right $ TextEnc.decodeUtf8 $ LBS.toStrict jsonBytes

-- | Decode a JSON string to a Lua value (represented as Aeson Value)
-- Returns either nil and an error message, or the decoded value and nil
luaDecode :: Text.Text -> Either (Value, Text.Text) (Value, Value)
luaDecode jsonText =
    let bs = TextEnc.encodeUtf8 jsonText
    in case decodeStrict bs of
        Nothing -> 
            -- Invalid JSON - return nil and error message
            let errMsg = Text.pack "Invalid JSON: parse error"
            in Left (Null, errMsg)
        Just val -> 
            -- Success - return decoded value and nil
            Right (val, Null)

-- | Convert a Lua table (represented as Aeson Object) to a JSON string
-- This is a helper for the encode function exposed to Lua
encodeLuaTable :: Aeson.Object -> Either String Text.Text
encodeLuaTable obj = luaEncode (Object obj)

-- | Decode a JSON string to a Lua table
-- Returns nil and error message on failure, or the table and nil on success
decodeToLuaTable :: Text.Text -> Either (Value, Text.Text) (Value, Value)
decodeToLuaTable = luaDecode

-- | Helper to create a Lua array from a list of values
-- In Lua, arrays are tables with integer keys starting at 1
makeLuaArray :: [Value] -> Value
makeLuaArray values = 
    Array $ Vector.fromList values

-- | Helper to create a Lua object from key-value pairs
makeLuaObject :: [(Text.Text, Value)] -> Value
makeLuaObject pairs =
    Object $ KeyMap.fromList $ map (\(k, v) -> (Key.fromText k, v)) pairs

-- | Check if a value represents a Lua array
-- Lua arrays have consecutive integer keys starting at 1
isLuaArray :: Aeson.Object -> Bool
isLuaArray obj = 
    let keys = map (Text.unpack . Key.toText) $ KeyMap.keys obj
        intKeys = [read k :: Int | k <- keys, all (`elem` ['0'..'9']) k, not (null k)]
    in case intKeys of
        [] -> False
        xs -> 
            let sorted = [1..maximum xs]
            in sorted == xs

