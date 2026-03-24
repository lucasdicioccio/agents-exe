-- This module has been removed as part of the final OS migration phase.
-- The compatibility layer tests are no longer needed.
module OS.CompatibilityTests (tests) where

import Test.Tasty (TestTree, testGroup)

-- Empty test suite for compatibility
tests :: TestTree
tests = testGroup "OS.Compatibility (removed - migration complete)" []

