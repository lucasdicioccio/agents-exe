#!/usr/bin/env python3
"""A minimal MCP server over stdio used by McpProgressTests.

Advertises one tool, `slow`. On tools/call it sends two
notifications/progress for the token the client put in _meta (and one for an
unrelated token, which must be ignored), then answers. Pass `listChanged` as
the first argument to advertise the tools listChanged capability.
"""
import json
import sys

list_changed = "listChanged" in sys.argv[1:]


def send(obj):
    sys.stdout.write(json.dumps(obj) + "\n")
    sys.stdout.flush()


for line in sys.stdin:
    line = line.strip()
    if not line:
        continue
    msg = json.loads(line)
    method = msg.get("method")
    rid = msg.get("id")
    if method == "initialize":
        send({"jsonrpc": "2.0", "id": rid, "result": {
            "protocolVersion": "2024-11-05",
            "capabilities": {"tools": {"listChanged": list_changed}},
            "serverInfo": {"name": "progress-test", "version": "1.0.0"},
        }})
    elif method == "tools/list":
        send({"jsonrpc": "2.0", "id": rid, "result": {"tools": [
            {"name": "slow", "description": "reports progress",
             "inputSchema": {"type": "object"}}]}})
    elif method == "tools/call":
        token = (msg.get("params", {}).get("_meta") or {}).get("progressToken")
        if token is not None:
            send({"jsonrpc": "2.0", "method": "notifications/progress",
                  "params": {"progressToken": "someone-else", "progress": 99}})
            send({"jsonrpc": "2.0", "method": "notifications/progress",
                  "params": {"progressToken": token, "progress": 1, "total": 2, "message": "half"}})
            send({"jsonrpc": "2.0", "method": "notifications/progress",
                  "params": {"progressToken": token, "progress": 2, "total": 2}})
        send({"jsonrpc": "2.0", "id": rid, "result": {
            "content": [{"type": "text", "text": "done" if token is not None else "no-token"}]}})
