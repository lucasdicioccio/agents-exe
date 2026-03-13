{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Time module for LuaToolbox.
-}
module System.Agents.Tools.LuaToolbox.Modules.Time (
    registerTimeModule,
) where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime, diffUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified HsLua as Lua
import Foreign.C.Types (CInt(..))

stackIdxToInt :: Lua.StackIndex -> Int
stackIdxToInt (Lua.StackIndex n) = fromIntegral n

getStackInt :: Lua.StackIndex -> Int
getStackInt = stackIdxToInt

-- | Register the time module in the Lua state.
registerTimeModule :: Lua.State -> IO ()
registerTimeModule lstate = Lua.runWith lstate $ do
    Lua.newtable

    Lua.pushName "now"
    Lua.pushHaskellFunction luaNow
    Lua.settable (Lua.nthTop 3)

    Lua.pushName "sleep"
    Lua.pushHaskellFunction luaSleep
    Lua.settable (Lua.nthTop 3)

    Lua.pushName "format"
    Lua.pushHaskellFunction luaFormat
    Lua.settable (Lua.nthTop 3)

    Lua.pushName "diff"
    Lua.pushHaskellFunction luaDiff
    Lua.settable (Lua.nthTop 3)

    Lua.setglobal (Lua.Name "time")

-- | Get current timestamp.
luaNow :: Lua.LuaE Lua.Exception Lua.NumResults
luaNow = do
    now <- liftIO getCurrentTime
    let seconds = realToFrac $ utcTimeToPOSIXSeconds now
    Lua.pushnumber (seconds :: Lua.Number)
    pure 1

-- | Sleep for specified seconds.
luaSleep :: Lua.LuaE Lua.Exception Lua.NumResults
luaSleep = do
    mSeconds <- Lua.tonumber (Lua.nthTop 1)
    Lua.pop 1
    case mSeconds of
        Nothing -> do
            Lua.pushboolean False
            Lua.pushstring "Invalid argument: expected number"
            pure 2
        Just seconds -> do
            liftIO $ threadDelay (round $ seconds * 1000000)
            Lua.pushboolean True
            pure 1

-- | Format timestamp.
luaFormat :: Lua.LuaE Lua.Exception Lua.NumResults
luaFormat = do
    top <- Lua.gettop
    let topInt = getStackInt top
    if topInt < 2
        then do
            Lua.pop topInt
            Lua.pushnil
            Lua.pushstring "Usage: time.format(timestamp, format)"
            pure 2
        else do
            mTimestamp <- Lua.tonumber (Lua.nthTop 2)
            fmtBs <- Lua.tostring' (Lua.nthTop 1)
            Lua.pop topInt
            case mTimestamp of
                Nothing -> do
                    Lua.pushnil
                    Lua.pushstring "Invalid timestamp"
                    pure 2
                Just timestamp -> do
                    let utcTime = posixSecondsToUTCTime $ realToFrac timestamp
                    let fmt = Text.unpack $ Text.decodeUtf8 fmtBs
                    let formatted = formatTime defaultTimeLocale fmt utcTime
                    Lua.pushstring (Text.encodeUtf8 $ Text.pack formatted)
                    pure 1

-- | Calculate difference between two timestamps.
luaDiff :: Lua.LuaE Lua.Exception Lua.NumResults
luaDiff = do
    top <- Lua.gettop
    let topInt = getStackInt top
    if topInt < 2
        then do
            Lua.pop topInt
            Lua.pushnil
            Lua.pushstring "Usage: time.diff(timestamp1, timestamp2)"
            pure 2
        else do
            mT1 <- Lua.tonumber (Lua.nthTop 2)
            mT2 <- Lua.tonumber (Lua.nthTop 1)
            Lua.pop topInt
            case (mT1, mT2) of
                (Just t1, Just t2) -> do
                    let diff = t1 - t2
                    Lua.pushnumber diff
                    pure 1
                _ -> do
                    Lua.pushnil
                    Lua.pushstring "Invalid timestamp arguments"
                    pure 2

