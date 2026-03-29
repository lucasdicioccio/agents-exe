#!/bin/bash
luaScript=$1
agentFile="localdev-agents/kimi-03.lua.json"
lua_toolname="lua_lua_execute"
python3 repro-tools/lua-toolcalls/reprolua.py "${luaScript}" | cabal run -- agents-exe --agent-file "${agentFile}" tool-call "${lua_toolname}" -l /dev/stderr
