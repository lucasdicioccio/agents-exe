{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Time module for LuaToolbox.
module System.Agents.Tools.LuaToolbox.Modules.Time (
    TimeTrace (..),
    registerTimeModule,
) where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import qualified HsLua as Lua
import Prod.Tracer (Tracer (..), runTracer)

stackIdxToInt :: Lua.StackIndex -> Int
stackIdxToInt (Lua.StackIndex n) = fromIntegral n

getStackInt :: Lua.StackIndex -> Int
getStackInt = stackIdxToInt

{- | Trace events for time operations.

These events allow tracking of all time-related operations for debugging,
auditing, and monitoring purposes.
-}
data TimeTrace
    = -- | Get current timestamp
      TimeNowTrace !Double !UTCTime
    | -- | Sleep for specified seconds
      TimeSleepTrace !Double
    | -- | Format timestamp
      TimeFormatTrace !Double !Text !Text
    | -- | Calculate difference between two timestamps
      TimeDiffTrace !Double !Double !Double
    deriving (Show, Eq)

-- | Register the time module in the Lua state.
registerTimeModule :: Tracer IO TimeTrace -> Lua.State -> IO ()
registerTimeModule tracer lstate = Lua.runWith lstate $ do
    Lua.newtable

    Lua.pushName "now"
    Lua.pushHaskellFunction (luaNow tracer)
    Lua.settable (Lua.nthTop 3)

    Lua.pushName "sleep"
    Lua.pushHaskellFunction (luaSleep tracer)
    Lua.settable (Lua.nthTop 3)

    Lua.pushName "format"
    Lua.pushHaskellFunction (luaFormat tracer)
    Lua.settable (Lua.nthTop 3)

    Lua.pushName "diff"
    Lua.pushHaskellFunction (luaDiff tracer)
    Lua.settable (Lua.nthTop 3)

    Lua.setglobal (Lua.Name "time")

-- | Get current timestamp.
luaNow :: Tracer IO TimeTrace -> Lua.LuaE Lua.Exception Lua.NumResults
luaNow tracer = do
    now <- liftIO getCurrentTime
    let seconds = realToFrac $ utcTimeToPOSIXSeconds now :: Double
    liftIO $ runTracer tracer (TimeNowTrace seconds now)
    Lua.pushnumber (realToFrac seconds)
    pure 1

-- | Sleep for specified seconds.
luaSleep :: Tracer IO TimeTrace -> Lua.LuaE Lua.Exception Lua.NumResults
luaSleep tracer = do
    mSeconds <- Lua.tonumber (Lua.nthTop 1)
    Lua.pop 1
    case mSeconds of
        Nothing -> do
            Lua.pushboolean False
            Lua.pushstring "Invalid argument: expected number"
            pure 2
        Just seconds -> do
            let secondsDouble = realToFrac seconds :: Double
            liftIO $ runTracer tracer (TimeSleepTrace secondsDouble)
            liftIO $ threadDelay (round $ secondsDouble * 1000000)
            Lua.pushboolean True
            pure 1

-- | Format timestamp.
luaFormat :: Tracer IO TimeTrace -> Lua.LuaE Lua.Exception Lua.NumResults
luaFormat tracer = do
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
                    let timestampDouble = realToFrac timestamp :: Double
                    let utcTime = posixSecondsToUTCTime $ realToFrac timestampDouble
                    let fmt = Text.unpack $ Text.decodeUtf8 fmtBs
                    let formatted = formatTime defaultTimeLocale fmt utcTime
                    let result = Text.pack formatted
                    liftIO $ runTracer tracer (TimeFormatTrace timestampDouble (Text.decodeUtf8 fmtBs) result)
                    Lua.pushstring (Text.encodeUtf8 result)
                    pure 1

-- | Calculate difference between two timestamps.
luaDiff :: Tracer IO TimeTrace -> Lua.LuaE Lua.Exception Lua.NumResults
luaDiff tracer = do
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
                    let t1Double = realToFrac t1 :: Double
                    let t2Double = realToFrac t2 :: Double
                    let diff = t1Double - t2Double
                    liftIO $ runTracer tracer (TimeDiffTrace t1Double t2Double diff)
                    Lua.pushnumber (realToFrac diff)
                    pure 1
                _ -> do
                    Lua.pushnil
                    Lua.pushstring "Invalid timestamp arguments"
                    pure 2
