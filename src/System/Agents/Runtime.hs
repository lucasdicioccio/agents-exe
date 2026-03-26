{- | System.Agents.Runtime - Runtime module

This module is deprecated. Use System.Agents.OS.Agents for OS-native agent operations.
-}
module System.Agents.Runtime (
    -- * Deprecated - Use System.Agents.OS.Agents instead
    RuntimeDeprecated (..),
) where

{- | Placeholder type indicating Runtime is deprecated.
Use System.Agents.OS.Agents for new code.
-}
data RuntimeDeprecated = RuntimeDeprecated
    { deprecatedMessage :: String
    }
    deriving (Show, Eq)
