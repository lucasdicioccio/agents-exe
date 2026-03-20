{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | JSON module for LuaToolbox.
module System.Agents.Tools.LuaToolbox.Modules.Json (
    JsonTrace (..),
    registerJsonModule,
) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as AesonText
import Data.Scientific (floatingOrInteger, fromFloatDigits)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Vector as Vector
import qualified HsLua as Lua
import Prod.Tracer (Tracer (..), runTracer)

import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KeyMap

{- | Trace events for JSON operations.

These events allow tracking of all JSON encoding/decoding operations for debugging,
auditing, and monitoring purposes.
-}
data JsonTrace
    = -- | JSON encode operation
      JsonEncodeTrace !Aeson.Value !Text
    | -- | JSON decode operation
      JsonDecodeTrace !Text !(Either Text Aeson.Value)
    deriving (Show, Eq)

-- | Register the json module in the Lua state.
registerJsonModule :: Tracer IO JsonTrace -> Lua.State -> IO ()
registerJsonModule tracer lstate = Lua.runWith lstate $ do
    Lua.newtable

    Lua.pushName "encode"
    Lua.pushHaskellFunction (luaEncode tracer)
    Lua.settable (Lua.nthTop 3)

    Lua.pushName "decode"
    Lua.pushHaskellFunction (luaDecode tracer)
    Lua.settable (Lua.nthTop 3)

    Lua.setglobal (Lua.Name "json")

-- | Encode a Lua value to JSON string.
luaEncode :: Tracer IO JsonTrace -> Lua.LuaE Lua.Exception Lua.NumResults
luaEncode tracer = do
    jsonVal <- luaToAesonValue
    Lua.liftIO $ print jsonVal
    let txt = LazyText.toStrict $ AesonText.encodeToLazyText jsonVal
    Lua.liftIO $ runTracer tracer (JsonEncodeTrace jsonVal txt)
    Lua.liftIO $ print txt
    Lua.pushstring (Text.encodeUtf8 txt)
    pure 1

-- | Decode a JSON string to a Lua value.
luaDecode :: Tracer IO JsonTrace -> Lua.LuaE Lua.Exception Lua.NumResults
luaDecode tracer = do
    str <- Lua.tostring' (Lua.nthTop 1)
    Lua.pop 1
    let textStr = Text.decodeUtf8 str
    case Aeson.eitherDecodeStrict' str of
        Left err -> do
            Lua.liftIO $ runTracer tracer (JsonDecodeTrace textStr (Left (Text.pack err)))
            Lua.pushnil
            Lua.pushstring (Text.encodeUtf8 $ Text.pack err)
            pure 2
        Right val -> do
            Lua.liftIO $ runTracer tracer (JsonDecodeTrace textStr (Right val))
            aesonToLuaValue val
            pure 1

-- | Convert Lua value at top of stack to Aeson Value.
luaToAesonValue :: Lua.LuaE Lua.Exception Aeson.Value
luaToAesonValue = do
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
                Just n -> pure $ Aeson.Number $ fromFloatDigits n
        Lua.TypeString -> do
            s <- Lua.tostring' (Lua.nthTop 1)
            Lua.pop 1
            pure $ Aeson.String (Text.decodeUtf8 s)
        Lua.TypeTable -> do
            convertTable
        _ -> do
            str <- luaValueToString
            pure $ Aeson.String str
  where
    luaValueToString :: Lua.LuaE Lua.Exception Text
    luaValueToString = do
        Lua.pushglobaltable
        Lua.pushName (Lua.Name "tostring")
        _ <- Lua.gettable (Lua.nthTop 2)
        Lua.insert (Lua.nthTop 2)
        _ <- Lua.pcall 1 1 Nothing
        s <- Lua.tostring' (Lua.nthTop 1)
        Lua.pop 1
        pure $ Text.decodeUtf8 s

convertTable :: Lua.LuaE Lua.Exception Aeson.Value
convertTable = do
    isArr <- isArray
    if isArr
        then convertArray
        else convertObject

isArray :: Lua.LuaE Lua.Exception Bool
isArray = do
    Lua.pushnil
    isSequential (1 :: Int)
  where
    isSequential :: Int -> Lua.LuaE Lua.Exception Bool
    isSequential expectedIdx = do
        hasNext <- Lua.next (Lua.nthTop 2)
        if not hasNext
            then pure True
            else do
                isNum <- Lua.isnumber (Lua.nthTop 1)
                if isNum
                    then do
                        mIdx <- Lua.tointeger (Lua.nthTop 1)
                        Lua.pop 2
                        case mIdx of
                            Just idx
                                | idx == fromIntegral expectedIdx ->
                                    isSequential (expectedIdx + 1)
                            _ -> pure False
                    else do
                        Lua.pop 2
                        pure False

convertArray :: Lua.LuaE Lua.Exception Aeson.Value
convertArray = do
    vals <- collectArrayValues
    pure $ Aeson.Array (Vector.fromList vals)

collectArrayValues :: Lua.LuaE Lua.Exception [Aeson.Value]
collectArrayValues = do
    len' <- Lua.rawlen (Lua.nthTop 1)
    vals <-
        mapM
            ( \(i :: Int) -> do
                Lua.pushinteger (fromIntegral i)
                _ <- Lua.gettable (Lua.nthTop 2)
                val <- luaToAesonValue
                pure val
            )
            [1 .. fromIntegral len']
    Lua.pop 1
    pure vals

convertObject :: Lua.LuaE Lua.Exception Aeson.Value
convertObject = do
    pairs <- collectObjectPairs
    pure $ Aeson.Object (KeyMap.fromList pairs)

collectObjectPairs :: Lua.LuaE Lua.Exception [(AesonKey.Key, Aeson.Value)]
collectObjectPairs = do
    Lua.pushnil
    go []
  where
    go acc = do
        hasNext <- Lua.next (Lua.nthTop 2)
        if not hasNext
            then do
                Lua.pop 1
                pure acc
            else do
                key <- Lua.tostring' (Lua.nthTop 2)
                val <- luaToAesonValue
                Lua.pop 1
                go ((AesonKey.fromText (Text.decodeUtf8 key), val) : acc)

aesonToLuaValue :: Aeson.Value -> Lua.LuaE Lua.Exception ()
aesonToLuaValue Aeson.Null = Lua.pushnil
aesonToLuaValue (Aeson.Bool b) = Lua.pushboolean b
aesonToLuaValue (Aeson.Number n) =
    case floatingOrInteger @Double @Integer n of
        Left d -> Lua.pushnumber (realToFrac d)
        Right i -> Lua.pushinteger (fromIntegral i)
aesonToLuaValue (Aeson.String t) = Lua.pushstring (Text.encodeUtf8 t)
aesonToLuaValue (Aeson.Array arr) = do
    Lua.newtable
    mapM_
        ( \(i :: Int, val) -> do
            aesonToLuaValue val
            Lua.pushinteger (fromIntegral (i + 1))
            Lua.insert (Lua.nthTop 2)
            Lua.settable (Lua.nthTop 3)
        )
        (zip [0 ..] (Vector.toList arr))
aesonToLuaValue (Aeson.Object obj) = do
    Lua.newtable
    mapM_
        ( \(k, val) -> do
            Lua.pushstring (Text.encodeUtf8 $ AesonKey.toText k)
            aesonToLuaValue val
            Lua.settable (Lua.nthTop 3)
        )
        (KeyMap.toList obj)

