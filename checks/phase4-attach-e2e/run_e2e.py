#!/usr/bin/env python3
"""Phase 4 end-to-end attach test, no real LLM.

Starts `agents-exe serve` against a fake OpenAI-compatible endpoint, then
drives `agents-exe tui --attach ...` through a pty with pexpect, exactly
like checks/phase3b-iv-e2e/run_e2e.py drives the embedded TUI: select the
(only) agent, open a new conversation, type a message, send it (triple
newline), wait for the fake model's reply to render, Ctrl+Q twice.

Modes (first argument):
  http    (default) --attach http://127.0.0.1:PORT
  socket  --attach unix:///.../agents.sock (serve --socket)
  token   serve --auth-tokens, --attach http://... --token-file FILE

Asserts: the TUI exits 0; the SERVER's database (not a TUI-local one) has
exactly one session with at least two turns; the TUI's working directory
got no database of its own; the server is still up after the TUI quit, and
lists the session over HTTP.
"""
import glob
import json
import os
import socket
import sqlite3
import subprocess
import sys
import tempfile
import time
import urllib.request

import pexpect

HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.dirname(os.path.dirname(HERE))
FAKE_SERVER = os.path.join(REPO, "checks", "phase3b-iv-e2e", "fake_openai.py")
REPLY_TEXT = "Hello from the fake model!"
TOKEN = "e2e-secret-token"


def free_port():
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.bind(("127.0.0.1", 0))
    port = s.getsockname()[1]
    s.close()
    return port


def find_binary():
    out = subprocess.check_output(["cabal", "list-bin", "agents-exe"], cwd=REPO, text=True)
    return out.strip()


def http_get(port, path, token=None):
    req = urllib.request.Request(f"http://127.0.0.1:{port}{path}")
    if token:
        req.add_header("Authorization", f"Bearer {token}")
    with urllib.request.urlopen(req, timeout=5) as rsp:
        return json.loads(rsp.read())


def wait_for(predicate, what, timeout=15):
    deadline = time.time() + timeout
    while time.time() < deadline:
        try:
            if predicate():
                return
        except Exception:
            pass
        time.sleep(0.1)
    raise RuntimeError(f"timed out waiting for {what}")


def main():
    mode = sys.argv[1] if len(sys.argv) > 1 else "http"
    if mode not in ("http", "socket", "token"):
        print(f"unknown mode {mode}")
        sys.exit(2)

    workdir = tempfile.mkdtemp(prefix=f"agents-attach-e2e-{mode}-")
    server_dir = os.path.join(workdir, "server")
    tui_dir = os.path.join(workdir, "tui")
    os.makedirs(server_dir)
    os.makedirs(tui_dir)
    print(f"[e2e:{mode}] workdir: {workdir}")

    llm_port = free_port()
    fake = subprocess.Popen(
        [sys.executable, FAKE_SERVER, str(llm_port), REPLY_TEXT],
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True,
    )
    time.sleep(0.3)
    if fake.poll() is not None:
        print(f"[e2e:{mode}] fake OpenAI server failed to start")
        sys.exit(1)
    print(f"[e2e:{mode}] fake OpenAI-compatible server on 127.0.0.1:{llm_port}")

    server = None
    server_log = open(os.path.join(workdir, "server.log"), "w")
    try:
        agent_path = os.path.join(server_dir, "agent.json")
        keys_path = os.path.join(server_dir, "keys.json")
        db_path = os.path.join(server_dir, "server.db")
        sock_path = os.path.join(server_dir, "agents.sock")
        tokens_path = os.path.join(server_dir, "tokens.json")
        token_file = os.path.join(tui_dir, "token")
        keymap_path = os.path.join(tui_dir, "keymap.json")

        with open(agent_path, "w") as f:
            json.dump(
                {
                    "tag": "OpenAIAgentDescription",
                    "contents": {
                        "slug": "fake-agent",
                        "apiKeyId": "none",
                        "flavor": "OpenAIv1",
                        "modelUrl": f"http://127.0.0.1:{llm_port}/v1",
                        "modelName": "fake-model",
                        "announce": "a fake test agent for the Phase 4 attach e2e test",
                        "systemPrompt": ["You are a test agent. Reply briefly."],
                        "builtinToolboxes": [],
                        "mcpServers": [],
                    },
                },
                f,
            )
        with open(keys_path, "w") as f:
            json.dump({}, f)
        with open(tokens_path, "w") as f:
            json.dump({"tokens": [{"owner": "e2e-owner", "token": TOKEN}]}, f)
        with open(token_file, "w") as f:
            f.write(TOKEN + "\n")
        with open(keymap_path, "w") as f:
            json.dump({"input": {"send_trigger": "triple_newline", "show_send_indicator": True}}, f)

        binary = find_binary()
        port = free_port()
        serve_cmd = [
            binary,
            "--agent-file", agent_path,
            "--api-keys", keys_path,
            "serve",
            "--db", db_path,
            "--port", str(port),
            "--no-ui",
            "--shutdown-grace", "1",
        ]
        if mode == "socket":
            serve_cmd += ["--socket", sock_path]
        if mode == "token":
            serve_cmd += ["--auth-tokens", tokens_path]
        print(f"[e2e:{mode}] starting server: {' '.join(serve_cmd)}")
        server = subprocess.Popen(serve_cmd, cwd=server_dir, stdout=server_log, stderr=subprocess.STDOUT)
        wait_for(lambda: http_get(port, "/healthz")["ok"], "the server's /healthz")
        if mode == "socket":
            wait_for(lambda: os.path.exists(sock_path), "the server's socket")
        print(f"[e2e:{mode}] server up on 127.0.0.1:{port}")

        if mode == "socket":
            attach = f"--attach unix://{sock_path}"
        elif mode == "token":
            attach = f"--attach http://127.0.0.1:{port} --token-file {token_file}"
        else:
            attach = f"--attach http://127.0.0.1:{port}"
        # No --agent-file, no --api-keys: an attached TUI loads nothing locally.
        cmd = f"{binary} tui --keymap {keymap_path} {attach}"
        print(f"[e2e:{mode}] launching: {cmd}")

        env = dict(os.environ)
        env["TERM"] = "xterm-256color"
        env["COLUMNS"] = "120"
        env["LINES"] = "40"
        # Keep the user's own agents-exe config out of it.
        env["XDG_CONFIG_HOME"] = os.path.join(tui_dir, ".config")

        child = pexpect.spawn(cmd, cwd=tui_dir, env=env, dimensions=(40, 120), timeout=20)
        child.logfile = open(os.path.join(workdir, "pty.log"), "wb")

        child.expect("fake-agent", timeout=15)
        print(f"[e2e:{mode}] agent listed (from the server)")
        time.sleep(0.3)

        child.send("\x0e")  # Ctrl+N: new conversation
        time.sleep(2.5)
        child.send("\x1d")  # Ctrl+]: Chats tab
        time.sleep(1.0)
        child.send("\t")
        time.sleep(0.5)
        child.send("\r")  # open the conversation
        time.sleep(1.0)

        message = "hello from the attach e2e test"
        child.send(message)
        time.sleep(0.3)
        child.send("\r")
        child.send("\r")
        child.send("\r")
        print(f"[e2e:{mode}] message sent, waiting for the reply to render")

        child.expect(REPLY_TEXT, timeout=20)
        print(f"[e2e:{mode}] reply rendered")
        time.sleep(0.5)

        child.send("\x11")
        time.sleep(0.3)
        child.send("\x11")

        child.expect(pexpect.EOF, timeout=15)
        child.close()
        exit_code = child.exitstatus
        print(f"[e2e:{mode}] TUI exited with status {exit_code}")
        if exit_code != 0:
            print(f"[e2e:{mode}] FAIL: non-zero exit status")
            sys.exit(1)

        local_dbs = [p for p in glob.glob(os.path.join(tui_dir, "**", "*"), recursive=True) if p.endswith((".db", ".sqlite")) or "agents-server.db" in p]
        if local_dbs:
            print(f"[e2e:{mode}] FAIL: the attached TUI created local databases: {local_dbs}")
            sys.exit(1)
        print(f"[e2e:{mode}] no TUI-local database")

        # The server outlives the TUI and lists the session over HTTP.
        if server.poll() is not None:
            print(f"[e2e:{mode}] FAIL: the server exited with the TUI")
            sys.exit(1)
        listed = http_get(port, "/v1/sessions", TOKEN if mode == "token" else None)["sessions"]
        print(f"[e2e:{mode}] server lists {len(listed)} session(s): {[(s['agent'], s['status'], s.get('owner')) for s in listed]}")
        if len(listed) != 1:
            print(f"[e2e:{mode}] FAIL: expected 1 session over HTTP")
            sys.exit(1)
        if mode == "token" and listed[0].get("owner") != "e2e-owner":
            print(f"[e2e:{mode}] FAIL: expected the token's owner on the session")
            sys.exit(1)

        # And in the server's own database.
        conn = sqlite3.connect(db_path)
        cur = conn.cursor()
        cur.execute("SELECT COUNT(*) FROM sessions")
        (session_count,) = cur.fetchone()
        print(f"[e2e:{mode}] server db session count: {session_count}")
        if session_count != 1:
            print(f"[e2e:{mode}] FAIL: expected 1 session, got {session_count}")
            sys.exit(1)
        cur.execute("SELECT session_id, json FROM sessions LIMIT 1")
        row = cur.fetchone()
        sess = json.loads(row[1])
        turns = sess.get("turns", [])
        print(f"[e2e:{mode}] turn count: {len(turns)}")
        if len(turns) < 2:
            print(f"[e2e:{mode}] FAIL: expected at least 2 turns, got {len(turns)}")
            sys.exit(1)
        texts = json.dumps(sess)
        if message not in texts or REPLY_TEXT not in texts:
            print(f"[e2e:{mode}] FAIL: the stored session lacks the message or the reply")
            sys.exit(1)

        print(f"[e2e:{mode}] PASS")
    finally:
        if server is not None:
            server.terminate()
            try:
                server.wait(timeout=10)
            except Exception:
                server.kill()
        fake.terminate()
        try:
            fake.wait(timeout=5)
        except Exception:
            fake.kill()


if __name__ == "__main__":
    main()
