#!/bin/bash
luaScript=$1
agentFile="localdev-agents/kimi-03.json"
lua_toolname="lua_lua_execute"
python3 reprolua.py "${luaScript}" | cabal run -- agents-exe --agent-file "${agentFile}" tool-call "${lua_toolname}" -l /dev/stderr
