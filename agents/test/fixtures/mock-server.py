#!/usr/bin/env python3
"""Minimal mock server for agents-exe OpenAPI/PostgREST feature tests.

Uses only the Python standard library.  Start with:

    python3 agents/test/fixtures/mock-server.py

Defaults:
    OpenAPI mock API   -> http://localhost:8765
    PostgREST mock API -> http://localhost:3000
"""

import argparse
import json
import pathlib
import threading
from http.server import BaseHTTPRequestHandler, HTTPServer


FIXTURES = pathlib.Path(__file__).resolve().parent


def json_response(handler, status, body):
    handler.send_response(status)
    handler.send_header("Content-Type", "application/json")
    handler.end_headers()
    handler.wfile.write(json.dumps(body).encode("utf-8"))


def file_response(handler, status, path):
    handler.send_response(status)
    handler.send_header("Content-Type", "application/json")
    handler.end_headers()
    handler.wfile.write(path.read_bytes())


class OpenAPIHandler(BaseHTTPRequestHandler):
    """Tiny handlers for the OpenAPI sample spec."""

    _items = [
        {"id": "a1", "name": "alpha", "count": 1},
        {"id": "a2", "name": "beta", "count": 2},
    ]

    def log_message(self, fmt, *args):
        print(f"[openapi] {self.address_string()} {fmt % args}")

    def do_GET(self):
        if self.path == "/openapi.json":
            file_response(self, 200, FIXTURES / "sample-openapi.json")
        elif self.path == "/items":
            json_response(self, 200, self._items)
        elif self.path.startswith("/items/"):
            item_id = self.path[len("/items/"):]
            for item in self._items:
                if item["id"] == item_id:
                    json_response(self, 200, item)
                    return
            json_response(self, 404, {"error": "not found"})
        else:
            json_response(self, 404, {"error": "unknown path"})

    def do_POST(self):
        if self.path == "/items":
            length = int(self.headers.get("Content-Length", 0))
            body = self.rfile.read(length)
            try:
                payload = json.loads(body.decode("utf-8"))
            except json.JSONDecodeError:
                json_response(self, 400, {"error": "invalid json"})
                return
            new_item = {
                "id": f"new-{len(self._items) + 1}",
                "name": payload.get("name", "unnamed"),
                "count": payload.get("count", 0),
            }
            self._items.append(new_item)
            json_response(self, 201, new_item)
        else:
            json_response(self, 404, {"error": "unknown path"})


class PostgRESTHandler(BaseHTTPRequestHandler):
    """Tiny handlers for the PostgREST sample spec."""

    _tasks = [
        {"id": 1, "title": "learn agents-exe", "done": False},
        {"id": 2, "title": "write tests", "done": True},
    ]
    _next_id = 3

    def log_message(self, fmt, *args):
        print(f"[postgrest] {self.address_string()} {fmt % args}")

    def _read_body(self):
        length = int(self.headers.get("Content-Length", 0))
        if length == 0:
            return {}
        return json.loads(self.rfile.read(length).decode("utf-8"))

    def do_GET(self):
        if self.path == "/":
            file_response(self, 200, FIXTURES / "sample-postgrest-openapi.json")
        elif self.path == "/tasks":
            json_response(self, 200, self._tasks)
        elif self.path.startswith("/tasks/"):
            try:
                task_id = int(self.path[len("/tasks/"):])
            except ValueError:
                json_response(self, 400, {"error": "bad id"})
                return
            for task in self._tasks:
                if task["id"] == task_id:
                    json_response(self, 200, task)
                    return
            json_response(self, 404, {"error": "not found"})
        else:
            json_response(self, 404, {"error": "unknown path"})

    def do_POST(self):
        if self.path == "/tasks":
            payload = self._read_body()
            new_task = {
                "id": self._next_id,
                "title": payload.get("title", "untitled"),
                "done": payload.get("done", False),
            }
            self._next_id += 1
            self._tasks.append(new_task)
            json_response(self, 201, new_task)
        else:
            json_response(self, 404, {"error": "unknown path"})

    def do_PATCH(self):
        if self.path.startswith("/tasks/"):
            try:
                task_id = int(self.path[len("/tasks/"):])
            except ValueError:
                json_response(self, 400, {"error": "bad id"})
                return
            payload = self._read_body()
            for task in self._tasks:
                if task["id"] == task_id:
                    task.update(payload)
                    json_response(self, 204, {})
                    return
            json_response(self, 404, {"error": "not found"})
        else:
            json_response(self, 404, {"error": "unknown path"})

    def do_DELETE(self):
        if self.path.startswith("/tasks/"):
            try:
                task_id = int(self.path[len("/tasks/"):])
            except ValueError:
                json_response(self, 400, {"error": "bad id"})
                return
            for idx, task in enumerate(self._tasks):
                if task["id"] == task_id:
                    del self._tasks[idx]
                    json_response(self, 204, {})
                    return
            json_response(self, 404, {"error": "not found"})
        else:
            json_response(self, 404, {"error": "unknown path"})


def serve_forever(server):
    server.serve_forever()


def main():
    parser = argparse.ArgumentParser(
        description="Mock server for agents-exe feature tests"
    )
    parser.add_argument(
        "--openapi-port",
        type=int,
        default=8765,
        help="Port for the OpenAPI mock API (default: 8765)",
    )
    parser.add_argument(
        "--postgrest-port",
        type=int,
        default=3000,
        help="Port for the PostgREST mock API (default: 3000)",
    )
    args = parser.parse_args()

    openapi_server = HTTPServer(("127.0.0.1", args.openapi_port), OpenAPIHandler)
    postgrest_server = HTTPServer(("127.0.0.1", args.postgrest_port), PostgRESTHandler)

    openapi_thread = threading.Thread(
        target=serve_forever, args=(openapi_server,), daemon=True
    )
    postgrest_thread = threading.Thread(
        target=serve_forever, args=(postgrest_server,), daemon=True
    )

    openapi_thread.start()
    postgrest_thread.start()

    print(f"OpenAPI mock API listening on http://127.0.0.1:{args.openapi_port}")
    print(f"PostgREST mock API listening on http://127.0.0.1:{args.postgrest_port}")
    print("Press Ctrl-C to stop.")

    try:
        while True:
            threading.Event().wait(1)
    except KeyboardInterrupt:
        print("\nShutting down...")
        openapi_server.shutdown()
        postgrest_server.shutdown()


if __name__ == "__main__":
    main()

