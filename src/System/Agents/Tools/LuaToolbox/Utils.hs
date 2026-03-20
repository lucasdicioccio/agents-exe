{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module System.Agents.Tools.LuaToolbox.Utils (
  luaToJsonValue,
  toName
) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Vector
import qualified HsLua as Lua

-- | Convert Text to Lua Name
toName :: Text.Text -> Lua.Name
toName = Lua.Name . Text.encodeUtf8

-------------------------------------------------------------------------------
-- Lua to JSON Conversion
-------------------------------------------------------------------------------

{- | Convert the value at the top of the Lua stack to JSON.

Handles:
* nil -> Null
* boolean -> Bool
* number -> Number (integer if whole number)
* string -> String
* table -> Object or Array (detected by keys)
-}
luaToJsonValue :: Lua.Lua Aeson.Value
luaToJsonValue = do
    ltype <- Lua.ltype (Lua.nthTop 1)
    case ltype of
        Lua.TypeNil -> do
            Lua.pop 1
            pure Aeson.Null
        Lua.TypeBoolean -> do
            b <- Lua.toboolean (Lua.nthTop 1)
            Lua.pop 1
            pure $ Aeson.Bool b
        Lua.TypeNumber -> do
            mNum <- Lua.tonumber (Lua.nthTop 1)
            Lua.pop 1
            case mNum of
                Nothing -> pure Aeson.Null
                Just n -> do
                    -- Check if it's an integer
                    let intVal = round n :: Integer
                    if fromIntegral intVal == n
                        then pure $ Aeson.Number (fromIntegral intVal)
                        else pure $ Aeson.Number (fromRational (toRational n))
        Lua.TypeString -> do
            s <- Lua.tostring' (Lua.nthTop 1)
            Lua.pop 2 -- tostring' actually pushes the converted string
            pure $ Aeson.String (Text.decodeUtf8 s)
        Lua.TypeTable -> do
            convertTable
        _ -> do
            -- For other types (function, userdata, thread, lightuserdata),
            -- convert to string representation using tostring
            Lua.pushglobaltable
            Lua.pushName (toName "tostring")
            _ <- Lua.gettable (Lua.nthTop 2)
            Lua.insert (Lua.nthTop 2) -- move tostring before value
            _ <- Lua.pcall 1 1 Nothing
            s <- Lua.tostring' (Lua.nthTop 1)
            Lua.pop 1
            pure $ Aeson.String (Text.decodeUtf8 s)

-- | Convert a Lua table to JSON (object or array)
convertTable :: Lua.Lua Aeson.Value
convertTable = do
    -- Check if it's an array (integer keys starting at 1, contiguous)
    isArr <- isArray
    if isArr
        then convertArray
        else convertObject

-- | Check if the table at top of stack is an array
isArray :: Lua.Lua Bool
isArray = do
    Lua.pushnil -- first key
    isSequential 1
  where
    isSequential :: Int -> Lua.Lua Bool
    isSequential expectedIdx = do
        hasNext <- Lua.next (Lua.nthTop 2)
        if not hasNext
            then pure True
            else do
                -- Check if key is the expected integer
                isNum <- Lua.isnumber (Lua.nthTop 1)
                if isNum
                    then do
                        mIdx <- Lua.tointeger (Lua.nthTop 1)
                        case mIdx of
                            Just idx
                                | idx == fromIntegral expectedIdx -> do
                                    Lua.pop 1 -- pop value
                                    isSequential (expectedIdx + 1)
                            _ -> do
                              Lua.pop 2 -- pop key and value
                              pure False
                    else do
                        Lua.pop 2 -- pop key and value
                        pure False

-- | Convert array table to JSON Array
convertArray :: Lua.Lua Aeson.Value
convertArray = do
    vals <- collectArrayValues
    pure $ Aeson.Array (Vector.fromList vals)

{- | Collect values from an array table using Lua # operator
Pop the table when done
-}
collectArrayValues :: Lua.Lua [Aeson.Value]
collectArrayValues = do
    -- Get table length using # operator
    len' <- Lua.rawlen (Lua.nthTop 1)

    vals <-
        mapM
            ( \(i :: Integer) -> do
                Lua.pushinteger (fromIntegral i)
                _ <- Lua.gettable (Lua.nthTop 2)
                luaToJsonValue -- This will pop the value
            )
            [1 .. fromIntegral len']

    Lua.pop 1 -- pop the table
    pure vals

-- | Convert object table to JSON Object
convertObject :: Lua.Lua Aeson.Value
convertObject = do
    -- Pop the table when done
    pairs <- collectObjectPairs
    pure $ Aeson.Object (KeyMap.fromList pairs)

collectObjectPairs :: Lua.Lua [(Aeson.Key, Aeson.Value)]
collectObjectPairs = do
    Lua.pushnil -- first key
    go []
  where
    go :: [(Aeson.Key, Aeson.Value)] -> Lua.Lua [(Aeson.Key, Aeson.Value)]
    go acc = do
        hasNext <- Lua.next (Lua.nthTop 2)
        if not hasNext
            then do
                Lua.pop 1 -- pop the table
                pure acc
            else do
                -- Stack: table, key, value
                key <- Lua.tostring' (Lua.nthTop 2)
                Lua.pop 1
                -- Stack: table, key, value, str-key
                -- Stack: table, key, value
                val <- luaToJsonValue -- This pops the value
                -- Stack: table, key
                go ((Aeson.Key.fromText (Text.decodeUtf8 key), val) : acc)

