#!/usr/bin/env python3
"""A scriptable OpenAI-compatible chat-completions server, for end-to-end
checks that need more than a fixed reply (tool calls, sub-agents, deferred
calls, several turns), with no real LLM.

    fake_llm.py PORT SCRIPT.json [--log REQUESTS.jsonl]

SCRIPT.json is a list of rules; for each request the first rule whose
`match` holds answers it (a rule with `"once": true` answers only once).
No rule matching answers `default` text (see `--default`).

    [
      {"match": {"system_contains": "root agent", "last_role": "user"},
       "respond": {"tool_calls": [{"name": "io_prompt_agent_helper",
                                   "arguments": {"what": "say hi"}}]}},
      {"match": {"system_contains": "helper"},
       "respond": {"text": "helper says hi"}},
      {"match": {"system_contains": "root agent", "last_role": "tool"},
       "respond": {"text": "all done"}}
    ]

Matchers (all given ones must hold):
  nth                 request index, from 0, over all requests
  system_contains     substring of the first (system) message
  last_role           role of the last message: "user", "tool", ...
  last_contains       substring of the last message's content
  has_tool            a tool of this name is offered in the request
Responses:
  {"text": "..."}                       a final answer
  {"tool_calls": [{"name": "...", "arguments": {...}}]}   tool calls
  either may carry "delay": seconds to wait before answering.

`--log` appends one JSON line per request: index, matched rule (or null),
the last message, every tool message by tool_call_id, and the names of the tools offered, so a check can also
assert what the model was shown.
"""
import argparse
import http.server
import json
import threading
import time


def content_text(msg):
    c = msg.get("content")
    if isinstance(c, str):
        return c
    if isinstance(c, list):
        return " ".join(p.get("text", "") for p in c if isinstance(p, dict))
    return ""


def tool_names(req):
    return [t.get("function", {}).get("name", "") for t in req.get("tools") or []]


def matches(match, index, req):
    msgs = req.get("messages") or [{}]
    first, last = msgs[0], msgs[-1]
    if "nth" in match and match["nth"] != index:
        return False
    if "system_contains" in match and match["system_contains"] not in content_text(first):
        return False
    if "last_role" in match and match["last_role"] != last.get("role"):
        return False
    if "last_contains" in match and match["last_contains"] not in content_text(last):
        return False
    if "has_tool" in match and match["has_tool"] not in tool_names(req):
        return False
    return True


def completion(respond):
    if "tool_calls" in respond:
        calls = [
            {
                "id": c.get("id", f"call_{i}"),
                "type": "function",
                "function": {"name": c["name"], "arguments": json.dumps(c.get("arguments", {}))},
            }
            for i, c in enumerate(respond["tool_calls"])
        ]
        message = {"role": "assistant", "content": None, "tool_calls": calls, "finish_reason": "tool_calls"}
        finish = "tool_calls"
    else:
        message = {"role": "assistant", "content": respond.get("text", ""), "finish_reason": "stop"}
        finish = "stop"
    return {
        "id": "chatcmpl-fake",
        "object": "chat.completion",
        "model": "fake-model",
        "choices": [{"index": 0, "finish_reason": finish, "message": message}],
        "usage": {"prompt_tokens": 1, "completion_tokens": 1, "total_tokens": 2},
    }


class Script:
    def __init__(self, rules, default, log):
        self.rules = rules
        self.default = default
        self.log = log
        self.used = set()
        self.count = 0
        self.lock = threading.Lock()

    def answer(self, req):
        with self.lock:
            index = self.count
            self.count += 1
            chosen = None
            for i, rule in enumerate(self.rules):
                if rule.get("once") and i in self.used:
                    continue
                if matches(rule.get("match", {}), index, req):
                    chosen = i
                    if rule.get("once"):
                        self.used.add(i)
                    break
            respond = self.rules[chosen]["respond"] if chosen is not None else {"text": self.default}
            if self.log:
                msgs = req.get("messages") or [{}]
                with open(self.log, "a") as f:
                    f.write(
                        json.dumps(
                            {
                                "index": index,
                                "rule": chosen,
                                "last": {"role": msgs[-1].get("role"), "content": content_text(msgs[-1])},
                                "tools": tool_names(req),
                                "tool_messages": {
                                    m.get("tool_call_id"): content_text(m)
                                    for m in msgs
                                    if m.get("role") == "tool"
                                },
                            }
                        )
                        + "\n"
                    )
        return respond


def make_server(port, rules, default="(no scripted answer)", log=None):
    script = Script(rules, default, log)

    class Handler(http.server.BaseHTTPRequestHandler):
        def log_message(self, fmt, *args):
            pass

        def do_POST(self):
            length = int(self.headers.get("Content-Length", "0"))
            req = json.loads(self.rfile.read(length) or b"{}")
            respond = script.answer(req)
            if respond.get("delay"):
                time.sleep(respond["delay"])
            body = json.dumps(completion(respond)).encode("utf-8")
            self.send_response(200)
            self.send_header("Content-Type", "application/json")
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            self.wfile.write(body)

    return http.server.ThreadingHTTPServer(("127.0.0.1", port), Handler)


def main():
    p = argparse.ArgumentParser()
    p.add_argument("port", type=int)
    p.add_argument("script")
    p.add_argument("--log")
    p.add_argument("--default", default="(no scripted answer)")
    a = p.parse_args()
    with open(a.script) as f:
        rules = json.load(f)
    server = make_server(a.port, rules, a.default, a.log)
    print(server.server_address[1], flush=True)
    server.serve_forever()


if __name__ == "__main__":
    main()
