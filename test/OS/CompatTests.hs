-- This module has been removed as part of the final OS migration phase.
-- The compatibility layer tests are no longer needed.
module OS.CompatTests (tests) where

import Test.Tasty (TestTree, testGroup)

-- Empty test suite for compatibility
tests :: TestTree
tests = testGroup "OS.Compat (removed - migration complete)" []

