#!/usr/bin/env python3
"""Phase 3b-iv end-to-end TUI smoke test, no real LLM.

Drives `agents-exe tui` through a pty with pexpect: start the TUI, select
the (only) agent, open a new conversation, type a message, send it (the
triple-newline trigger, so no Ctrl+Enter/Meta+Enter is needed -- those
aren't reliably distinguishable from plain Enter over a raw pty), wait for
the fake model's reply to render, then Ctrl+Q twice to quit.

Asserts: the TUI exits 0, and the SQLite database it wrote to has exactly
one session with two turns (a user turn and the model's answer).
"""
import json
import os
import shutil
import socket
import sqlite3
import subprocess
import sys
import tempfile
import time

import pexpect

REPO = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
FAKE_SERVER = os.path.join(os.path.dirname(os.path.abspath(__file__)), "fake_openai.py")
REPLY_TEXT = "Hello from the fake model!"


def free_port():
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.bind(("127.0.0.1", 0))
    port = s.getsockname()[1]
    s.close()
    return port


def find_binary():
    out = subprocess.check_output(["cabal", "list-bin", "agents-exe"], cwd=REPO, text=True)
    return out.strip()


def main():
    workdir = tempfile.mkdtemp(prefix="agents-tui-e2e-")
    print(f"[e2e] workdir: {workdir}")

    port = free_port()
    server = subprocess.Popen(
        [sys.executable, FAKE_SERVER, str(port), REPLY_TEXT],
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True,
    )
    time.sleep(0.3)
    if server.poll() is not None:
        print("[e2e] fake OpenAI server failed to start")
        sys.exit(1)
    print(f"[e2e] fake OpenAI-compatible server on 127.0.0.1:{port}")

    try:
        agent_path = os.path.join(workdir, "agent.json")
        keys_path = os.path.join(workdir, "keys.json")
        keymap_path = os.path.join(workdir, "keymap.json")
        db_path = os.path.join(workdir, "tui.db")

        with open(agent_path, "w") as f:
            json.dump(
                {
                    "tag": "OpenAIAgentDescription",
                    "contents": {
                        "slug": "fake-agent",
                        "apiKeyId": "none",
                        "flavor": "OpenAIv1",
                        "modelUrl": f"http://127.0.0.1:{port}/v1",
                        "modelName": "fake-model",
                        "announce": "a fake test agent for the Phase 3b-iv e2e smoke test",
                        "systemPrompt": ["You are a test agent. Reply briefly."],
                        "builtinToolboxes": [],
                        "mcpServers": [],
                    },
                },
                f,
            )
        with open(keys_path, "w") as f:
            json.dump({}, f)
        # Triple-newline send trigger: driveable with plain Enter keypresses
        # over a raw pty (Ctrl+Enter/Meta+Enter, the default, is not
        # reliably distinguishable from plain Enter without a terminal that
        # speaks the kitty keyboard protocol, which a bare pty does not).
        with open(keymap_path, "w") as f:
            json.dump({"input": {"send_trigger": "triple_newline", "show_send_indicator": True}}, f)

        binary = find_binary()
        cmd = (
            f"{binary} --agent-file {agent_path} --api-keys {keys_path} "
            f"tui --keymap {keymap_path} --db {db_path}"
        )
        print(f"[e2e] launching: {cmd}")

        env = dict(os.environ)
        env["TERM"] = "xterm-256color"
        env["COLUMNS"] = "120"
        env["LINES"] = "40"

        child = pexpect.spawn(cmd, cwd=workdir, env=env, dimensions=(40, 120), timeout=20)
        child.logfile = open(os.path.join(workdir, "pty.log"), "wb")

        # The Agents tab shows the loaded agent's slug.
        child.expect("fake-agent", timeout=15)
        print("[e2e] agent listed")
        time.sleep(0.3)

        # Ctrl+N: new conversation with the selected agent (from the Agents
        # tab; the new conversation is added and selected in the Chats
        # tab's list, but the tab itself is not switched -- see documentation/tui.md).
        child.send("\x0e")
        time.sleep(2.5)  # let a heartbeat (1s) land the conversation in the list

        # Ctrl+]: switch to the Chats tab. Focus-preservation keeps whatever
        # widget was focused before if it also exists in the new tab's ring
        # (buildFocusRingForTabPreserving) -- AgentListWidget is in both the
        # Agents and Chats rings, so focus stays there, not on the
        # conversation list. One Tab cycles focus forward to
        # ConversationListWidget (first in the Chats ring).
        child.send("\x1d")
        time.sleep(1.0)
        child.send("\t")
        time.sleep(0.5)

        # Enter on the conversation list: EventOpenConversation, opens the
        # session and focuses the message editor.
        child.send("\r")
        time.sleep(1.0)

        # Type the message, then three Enters: the keymap file below sets
        # the triple-newline send trigger, which plain Enter keypresses can
        # drive reliably over a raw pty. The default trigger (Ctrl+Enter /
        # Meta+Enter) is not: a bare pty terminal has no way to send a
        # modified Enter distinct from a plain one without the kitty
        # keyboard protocol, which vty here does not request.
        message = "hello from the e2e test"
        child.send(message)
        time.sleep(0.3)
        child.send("\r")
        child.send("\r")
        child.send("\r")
        print("[e2e] message sent, waiting for the reply to render")

        # Wait for the fake model's fixed reply to show up in the conversation view.
        child.expect(REPLY_TEXT, timeout=20)
        print("[e2e] reply rendered")
        time.sleep(0.5)

        # Ctrl+Q twice: quit confirmation, then quit.
        child.send("\x11")
        time.sleep(0.3)
        child.send("\x11")

        child.expect(pexpect.EOF, timeout=15)
        child.close()
        exit_code = child.exitstatus
        print(f"[e2e] TUI exited with status {exit_code}")
        if exit_code != 0:
            print("[e2e] FAIL: non-zero exit status")
            sys.exit(1)

        # Assert the database: one session, two turns.
        conn = sqlite3.connect(db_path)
        cur = conn.cursor()
        cur.execute("SELECT name FROM sqlite_master WHERE type='table'")
        tables = [r[0] for r in cur.fetchall()]
        print(f"[e2e] tables: {tables}")

        cur.execute("SELECT COUNT(*) FROM sessions")
        (session_count,) = cur.fetchone()
        print(f"[e2e] session count: {session_count}")
        if session_count != 1:
            print(f"[e2e] FAIL: expected 1 session, got {session_count}")
            sys.exit(1)

        cur.execute("SELECT session_id, json FROM sessions LIMIT 1")
        row = cur.fetchone()
        sess = json.loads(row[1])
        turns = sess.get("turns", [])
        print(f"[e2e] turn count: {len(turns)}")
        if len(turns) < 2:
            print(f"[e2e] FAIL: expected at least 2 turns, got {len(turns)}")
            sys.exit(1)

        print("[e2e] PASS")
    finally:
        server.terminate()
        try:
            server.wait(timeout=5)
        except Exception:
            server.kill()


if __name__ == "__main__":
    main()
