{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | UTF-8 text utilities module for LuaToolbox.
module System.Agents.Tools.LuaToolbox.Modules.Text (
    TextTrace (..),
    registerTextModule,
) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified HsLua as Lua
import Prod.Tracer (Tracer (..), runTracer)

stackIdxToInt :: Lua.StackIndex -> Int
stackIdxToInt (Lua.StackIndex n) = fromIntegral n

getStackInt :: Lua.StackIndex -> Int
getStackInt = stackIdxToInt

{- | Trace events for text operations.

These events allow tracking of all text manipulation operations for debugging,
auditing, and monitoring purposes.
-}
data TextTrace
    = -- | Split text by delimiter
      TextSplitTrace !Text !Text ![Text]
    | -- | Find substring position
      TextFindTrace !Text !Text !(Maybe (Int, Int))
    | -- | Global substitution
      TextGsubTrace !Text !Text !Text !Text !Int
    | -- | Trim whitespace
      TextTrimTrace !Text !Text
    | -- | Check if starts with prefix
      TextStartswithTrace !Text !Text !Bool
    | -- | Check if ends with suffix
      TextEndswithTrace !Text !Text !Bool
    | -- | Convert to lowercase
      TextLowerTrace !Text !Text
    | -- | Convert to uppercase
      TextUpperTrace !Text !Text
    | -- | Get string length
      TextLenTrace !Text !Int
    | -- | Get substring
      TextSubTrace !Text !Int !Int !Text
    deriving (Show, Eq)

-- | Register the text module in the Lua state.
registerTextModule :: Tracer IO TextTrace -> Lua.State -> IO ()
registerTextModule tracer lstate = Lua.runWith lstate $ do
    Lua.newtable

    Lua.pushName "split"
    Lua.pushHaskellFunction (luaSplit tracer)
    Lua.settable (Lua.nthTop 3)

    Lua.pushName "find"
    Lua.pushHaskellFunction (luaFind tracer)
    Lua.settable (Lua.nthTop 3)

    Lua.pushName "gsub"
    Lua.pushHaskellFunction (luaGsub tracer)
    Lua.settable (Lua.nthTop 3)

    Lua.pushName "trim"
    Lua.pushHaskellFunction (luaTrim tracer)
    Lua.settable (Lua.nthTop 3)

    Lua.pushName "startswith"
    Lua.pushHaskellFunction (luaStartswith tracer)
    Lua.settable (Lua.nthTop 3)

    Lua.pushName "endswith"
    Lua.pushHaskellFunction (luaEndswith tracer)
    Lua.settable (Lua.nthTop 3)

    Lua.pushName "lower"
    Lua.pushHaskellFunction (luaLower tracer)
    Lua.settable (Lua.nthTop 3)

    Lua.pushName "upper"
    Lua.pushHaskellFunction (luaUpper tracer)
    Lua.settable (Lua.nthTop 3)

    Lua.pushName "len"
    Lua.pushHaskellFunction (luaLen tracer)
    Lua.settable (Lua.nthTop 3)

    Lua.pushName "sub"
    Lua.pushHaskellFunction (luaSub tracer)
    Lua.settable (Lua.nthTop 3)

    Lua.setglobal (Lua.Name "text")

-- | Split a string by a delimiter.
luaSplit :: Tracer IO TextTrace -> Lua.LuaE Lua.Exception Lua.NumResults
luaSplit tracer = do
    str <- getTextArg (Lua.nthTop 2)
    delim <- getTextArg (Lua.nthTop 1)
    Lua.pop 2
    let parts = Text.splitOn delim str
    Lua.liftIO $ runTracer tracer (TextSplitTrace str delim parts)
    pushTextList parts
    pure 1

-- | Find substring position.
luaFind :: Tracer IO TextTrace -> Lua.LuaE Lua.Exception Lua.NumResults
luaFind tracer = do
    str <- getTextArg (Lua.nthTop 2)
    pattern <- getTextArg (Lua.nthTop 1)
    Lua.pop 2
    case Text.breakOn pattern str of
        (_, "") -> do
            Lua.liftIO $ runTracer tracer (TextFindTrace str pattern Nothing)
            Lua.pushnil
            pure 1
        (before, _) -> do
            let startPos = Text.length before + 1
            let endPos = startPos + Text.length pattern - 1
            Lua.liftIO $ runTracer tracer (TextFindTrace str pattern (Just (startPos, endPos)))
            Lua.pushinteger (fromIntegral startPos)
            Lua.pushinteger (fromIntegral endPos)
            pure 2

-- | Global substitution.
luaGsub :: Tracer IO TextTrace -> Lua.LuaE Lua.Exception Lua.NumResults
luaGsub tracer = do
    str <- getTextArg (Lua.nthTop 3)
    pattern <- getTextArg (Lua.nthTop 2)
    replacement <- getTextArg (Lua.nthTop 1)
    Lua.pop 3
    let parts = Text.splitOn pattern str
    let count = length parts - 1
    let result = Text.intercalate replacement parts
    Lua.liftIO $ runTracer tracer (TextGsubTrace str pattern replacement result count)
    Lua.pushstring (Text.encodeUtf8 result)
    Lua.pushinteger (fromIntegral count)
    pure 2

-- | Trim whitespace from both ends.
luaTrim :: Tracer IO TextTrace -> Lua.LuaE Lua.Exception Lua.NumResults
luaTrim tracer = do
    str <- getTextArg (Lua.nthTop 1)
    Lua.pop 1
    let result = Text.strip str
    Lua.liftIO $ runTracer tracer (TextTrimTrace str result)
    Lua.pushstring (Text.encodeUtf8 result)
    pure 1

-- | Check if string starts with prefix.
luaStartswith :: Tracer IO TextTrace -> Lua.LuaE Lua.Exception Lua.NumResults
luaStartswith tracer = do
    str <- getTextArg (Lua.nthTop 2)
    prefix <- getTextArg (Lua.nthTop 1)
    Lua.pop 2
    let result = prefix `Text.isPrefixOf` str
    Lua.liftIO $ runTracer tracer (TextStartswithTrace str prefix result)
    Lua.pushboolean result
    pure 1

-- | Check if string ends with suffix.
luaEndswith :: Tracer IO TextTrace -> Lua.LuaE Lua.Exception Lua.NumResults
luaEndswith tracer = do
    str <- getTextArg (Lua.nthTop 2)
    suffix <- getTextArg (Lua.nthTop 1)
    Lua.pop 2
    let result = suffix `Text.isSuffixOf` str
    Lua.liftIO $ runTracer tracer (TextEndswithTrace str suffix result)
    Lua.pushboolean result
    pure 1

-- | Convert to lowercase.
luaLower :: Tracer IO TextTrace -> Lua.LuaE Lua.Exception Lua.NumResults
luaLower tracer = do
    str <- getTextArg (Lua.nthTop 1)
    Lua.pop 1
    let result = Text.toLower str
    Lua.liftIO $ runTracer tracer (TextLowerTrace str result)
    Lua.pushstring (Text.encodeUtf8 result)
    pure 1

-- | Convert to uppercase.
luaUpper :: Tracer IO TextTrace -> Lua.LuaE Lua.Exception Lua.NumResults
luaUpper tracer = do
    str <- getTextArg (Lua.nthTop 1)
    Lua.pop 1
    let result = Text.toUpper str
    Lua.liftIO $ runTracer tracer (TextUpperTrace str result)
    Lua.pushstring (Text.encodeUtf8 result)
    pure 1

-- | Get string length.
luaLen :: Tracer IO TextTrace -> Lua.LuaE Lua.Exception Lua.NumResults
luaLen tracer = do
    str <- getTextArg (Lua.nthTop 1)
    Lua.pop 1
    let len = Text.length str
    Lua.liftIO $ runTracer tracer (TextLenTrace str len)
    Lua.pushinteger (fromIntegral len)
    pure 1

-- | Get substring.
luaSub :: Tracer IO TextTrace -> Lua.LuaE Lua.Exception Lua.NumResults
luaSub tracer = do
    top <- Lua.gettop
    let topInt = getStackInt top
    let nargs = topInt
    str <- getTextArg (Lua.nthTop (fromIntegral topInt))
    mStart <- Lua.tointeger (Lua.nthTop (fromIntegral (topInt - 1)))
    mEnd <- if nargs >= 3 then Lua.tointeger (Lua.nthTop 1) else pure Nothing
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
            let endPos = maybe len (fromIntegral) mEnd
            let actualEnd = if endPos < 0 then len + endPos + 1 else endPos
            if actualStart < 0 || actualStart >= len
                then do
                    Lua.liftIO $ runTracer tracer (TextSubTrace str (fromIntegral startIdx') (maybe (-1) fromIntegral mEnd) "")
                    Lua.pushstring ""
                    pure 1
                else do
                    let result = Text.take (actualEnd - actualStart) (Text.drop actualStart str)
                    Lua.liftIO $ runTracer tracer (TextSubTrace str (fromIntegral startIdx') (maybe (-1) fromIntegral mEnd) result)
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
        ( \(i :: Int, txt) -> do
            Lua.pushstring (Text.encodeUtf8 txt)
            Lua.pushinteger (fromIntegral i)
            Lua.insert (Lua.nthTop 2)
            Lua.settable (Lua.nthTop 3)
        )
        (zip [1 ..] items)

