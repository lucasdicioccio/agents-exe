#!/bin/bash
# Fix the test file by removing agentToolboxBindings references

sed -i '/agentToolboxBindings = \[\]/d' test/Main.hs
sed -i '/agentToolboxBindings = \[\]/d' test/OS/CoreTests.hs

echo "Fixed test files"

