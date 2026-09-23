#!/usr/bin/env python3
"""Tiny fixed-response OpenAI-compatible chat-completions server, for the
Phase 3b-iv TUI end-to-end smoke test (no real LLM)."""
import http.server
import json
import sys
import threading

PORT = int(sys.argv[1]) if len(sys.argv) > 1 else 0
REPLY = sys.argv[2] if len(sys.argv) > 2 else "Hello from the fake model!"

class Handler(http.server.BaseHTTPRequestHandler):
    def log_message(self, fmt, *args):
        pass

    def do_POST(self):
        length = int(self.headers.get("Content-Length", "0"))
        _body = self.rfile.read(length)
        resp = {
            "id": "chatcmpl-fake",
            "object": "chat.completion",
            "model": "fake-model",
            "choices": [
                {
                    "index": 0,
                    "finish_reason": "stop",
                    "message": {
                        "role": "assistant",
                        "content": REPLY,
                        "finish_reason": "stop",
                    },
                }
            ],
            "usage": {"prompt_tokens": 1, "completion_tokens": 1, "total_tokens": 2},
        }
        body = json.dumps(resp).encode("utf-8")
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

if __name__ == "__main__":
    server = http.server.HTTPServer(("127.0.0.1", PORT), Handler)
    port = server.server_address[1]
    print(port, flush=True)
    server.serve_forever()
