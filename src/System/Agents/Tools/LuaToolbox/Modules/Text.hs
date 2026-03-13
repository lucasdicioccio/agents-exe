{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | UTF-8 text utilities module for LuaToolbox.
module System.Agents.Tools.LuaToolbox.Modules.Text (
    registerTextModule,
) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified HsLua as Lua

stackIdxToInt :: Lua.StackIndex -> Int
stackIdxToInt (Lua.StackIndex n) = fromIntegral n

intToStackIdx :: Int -> Lua.StackIndex
intToStackIdx = Lua.StackIndex . fromIntegral

getStackInt :: Lua.StackIndex -> Int
getStackInt = stackIdxToInt

-- | Register the text module in the Lua state.
registerTextModule :: Lua.State -> IO ()
registerTextModule lstate = Lua.runWith lstate $ do
    Lua.newtable

    Lua.pushName "split"
    Lua.pushHaskellFunction luaSplit
    Lua.settable (Lua.nthTop 3)

    Lua.pushName "find"
    Lua.pushHaskellFunction luaFind
    Lua.settable (Lua.nthTop 3)

    Lua.pushName "gsub"
    Lua.pushHaskellFunction luaGsub
    Lua.settable (Lua.nthTop 3)

    Lua.pushName "trim"
    Lua.pushHaskellFunction luaTrim
    Lua.settable (Lua.nthTop 3)

    Lua.pushName "startswith"
    Lua.pushHaskellFunction luaStartswith
    Lua.settable (Lua.nthTop 3)

    Lua.pushName "endswith"
    Lua.pushHaskellFunction luaEndswith
    Lua.settable (Lua.nthTop 3)

    Lua.pushName "lower"
    Lua.pushHaskellFunction luaLower
    Lua.settable (Lua.nthTop 3)

    Lua.pushName "upper"
    Lua.pushHaskellFunction luaUpper
    Lua.settable (Lua.nthTop 3)

    Lua.pushName "len"
    Lua.pushHaskellFunction luaLen
    Lua.settable (Lua.nthTop 3)

    Lua.pushName "sub"
    Lua.pushHaskellFunction luaSub
    Lua.settable (Lua.nthTop 3)

    Lua.setglobal (Lua.Name "text")

-- | Split a string by a delimiter.
luaSplit :: Lua.LuaE Lua.Exception Lua.NumResults
luaSplit = do
    str <- getTextArg (Lua.nthTop 2)
    delim <- getTextArg (Lua.nthTop 1)
    Lua.pop 2
    let parts = Text.splitOn delim str
    pushTextList parts
    pure 1

-- | Find substring position.
luaFind :: Lua.LuaE Lua.Exception Lua.NumResults
luaFind = do
    str <- getTextArg (Lua.nthTop 2)
    pattern <- getTextArg (Lua.nthTop 1)
    Lua.pop 2
    case Text.breakOn pattern str of
        (before, "") -> do
            Lua.pushnil
            pure 1
        (before, _) -> do
            let startPos = Text.length before + 1
            let endPos = startPos + Text.length pattern - 1
            Lua.pushinteger (fromIntegral startPos)
            Lua.pushinteger (fromIntegral endPos)
            pure 2

-- | Global substitution.
luaGsub :: Lua.LuaE Lua.Exception Lua.NumResults
luaGsub = do
    str <- getTextArg (Lua.nthTop 3)
    pattern <- getTextArg (Lua.nthTop 2)
    replacement <- getTextArg (Lua.nthTop 1)
    Lua.pop 3
    let parts = Text.splitOn pattern str
    let count = length parts - 1
    let result = Text.intercalate replacement parts
    Lua.pushstring (Text.encodeUtf8 result)
    Lua.pushinteger (fromIntegral count)
    pure 2

-- | Trim whitespace from both ends.
luaTrim :: Lua.LuaE Lua.Exception Lua.NumResults
luaTrim = do
    str <- getTextArg (Lua.nthTop 1)
    Lua.pop 1
    Lua.pushstring (Text.encodeUtf8 $ Text.strip str)
    pure 1

-- | Check if string starts with prefix.
luaStartswith :: Lua.LuaE Lua.Exception Lua.NumResults
luaStartswith = do
    str <- getTextArg (Lua.nthTop 2)
    prefix <- getTextArg (Lua.nthTop 1)
    Lua.pop 2
    Lua.pushboolean (prefix `Text.isPrefixOf` str)
    pure 1

-- | Check if string ends with suffix.
luaEndswith :: Lua.LuaE Lua.Exception Lua.NumResults
luaEndswith = do
    str <- getTextArg (Lua.nthTop 2)
    suffix <- getTextArg (Lua.nthTop 1)
    Lua.pop 2
    Lua.pushboolean (suffix `Text.isSuffixOf` str)
    pure 1

-- | Convert to lowercase.
luaLower :: Lua.LuaE Lua.Exception Lua.NumResults
luaLower = do
    str <- getTextArg (Lua.nthTop 1)
    Lua.pop 1
    Lua.pushstring (Text.encodeUtf8 $ Text.toLower str)
    pure 1

-- | Convert to uppercase.
luaUpper :: Lua.LuaE Lua.Exception Lua.NumResults
luaUpper = do
    str <- getTextArg (Lua.nthTop 1)
    Lua.pop 1
    Lua.pushstring (Text.encodeUtf8 $ Text.toUpper str)
    pure 1

-- | Get string length.
luaLen :: Lua.LuaE Lua.Exception Lua.NumResults
luaLen = do
    str <- getTextArg (Lua.nthTop 1)
    Lua.pop 1
    Lua.pushinteger (fromIntegral $ Text.length str)
    pure 1

-- | Get substring.
luaSub :: Lua.LuaE Lua.Exception Lua.NumResults
luaSub = do
    top <- Lua.gettop
    let topInt = getStackInt top
    let nargs = topInt
    str <- getTextArg (Lua.nthTop (fromIntegral topInt))
    let startIdx = if nargs >= 2 then topInt - 1 else topInt
    mStart <- Lua.tointeger (Lua.nthTop (fromIntegral startIdx))
    Lua.pop topInt
    case mStart of
        Nothing -> do
            Lua.pushnil
            Lua.pushstring "Invalid start index"
            pure 2
        Just startIdx' -> do
            let len = Text.length str
            let start = fromIntegral startIdx' - 1
            let actualStart = if start < 0 then len + start + 1 else start
            if actualStart < 0 || actualStart >= len
                then do
                    Lua.pushstring ""
                    pure 1
                else do
                    let result = Text.drop actualStart str
                    Lua.pushstring (Text.encodeUtf8 result)
                    pure 1

-- | Helper to get a text argument from the stack.
getTextArg :: Lua.StackIndex -> Lua.LuaE Lua.Exception Text
getTextArg idx = do
    bs <- Lua.tostring' idx
    pure $ Text.decodeUtf8 bs

-- | Push a list of Text values as a Lua table.
pushTextList :: [Text] -> Lua.LuaE Lua.Exception ()
pushTextList items = do
    Lua.newtable
    mapM_
        ( \(i, txt) -> do
            Lua.pushstring (Text.encodeUtf8 txt)
            Lua.pushinteger (fromIntegral i)
            Lua.insert (Lua.nthTop 2)
            Lua.settable (Lua.nthTop 3)
        )
        (zip [1 ..] items)
