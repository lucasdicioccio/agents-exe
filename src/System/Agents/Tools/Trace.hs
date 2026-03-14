{-# LANGUAGE PatternSynonyms #-}

-- | Tool trace events for debugging and auditing.
-- This module re-exports the 'ToolTrace' type from System.Agents.Tools.Base
-- to maintain backward compatibility. The type was moved to Base to avoid
-- module cycles.
module System.Agents.Tools.Trace (
    ToolTrace (..),
) where

import System.Agents.Tools.Base (ToolTrace (..))

