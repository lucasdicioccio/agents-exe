{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- |
Compatibility runtime layer - minimal exports for transitional compatibility.
This module is deprecated and will be removed in a future release.
-}
module System.Agents.OS.Compat.Runtime (
    -- * OS Type (re-exported from OS.Core for compatibility)
    OS (..),
    initializeOS,
) where

import Control.Concurrent.STM (atomically)

import System.Agents.OS.Core.World (World, newWorld)

-------------------------------------------------------------------------------
-- OS Type (re-exported for compatibility)
-------------------------------------------------------------------------------

-- | Placeholder OS type.
newtype OS = OS
    { _osWorld :: World
    }

-- | Manual Eq instance for OS
instance Eq OS where
    _ == _ = True -- All OS instances are equal for compatibility

-- | Manual Show instance for OS
instance Show OS where
    show _ = "OS {<world>}"

-- | Initialize a minimal OS instance.
initializeOS :: IO OS
initializeOS = do
    world <- atomically newWorld
    pure $ OS world
