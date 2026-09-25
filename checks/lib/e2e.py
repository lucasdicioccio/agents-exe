"""Shared plumbing for the pty end-to-end checks that use the scriptable
fake LLM (checks/lib/fake_llm.py): a fake server, agent files, the embedded
TUI driven through pexpect, and the database it leaves behind.

The keystrokes are the ones checks/phase3b-iv-e2e/run_e2e.py documents:
Ctrl+N new conversation, Ctrl+] to the Chats tab, Tab to the conversation
list, Enter to open it, then the message and three Enters (the
triple-newline send trigger, which a bare pty can produce).
"""
import json
import os
import socket
import sqlite3
import subprocess
import sys
import tempfile
import time

import pexpect

HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.dirname(os.path.dirname(HERE))
FAKE_LLM = os.path.join(HERE, "fake_llm.py")


def free_port():
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.bind(("127.0.0.1", 0))
    port = s.getsockname()[1]
    s.close()
    return port


def find_binary():
    return subprocess.check_output(["cabal", "list-bin", "agents-exe"], cwd=REPO, text=True).strip()


def fail(msg):
    print(f"[e2e] FAIL: {msg}")
    sys.exit(1)


class Check:
    """A workdir, a scripted fake LLM, and helpers to write agents and drive the TUI."""

    def __init__(self, name, script):
        self.workdir = tempfile.mkdtemp(prefix=f"agents-{name}-e2e-")
        self.log_path = os.path.join(self.workdir, "llm-requests.jsonl")
        self.script_path = os.path.join(self.workdir, "script.json")
        with open(self.script_path, "w") as f:
            json.dump(script, f)
        self.port = free_port()
        self.server = subprocess.Popen(
            [sys.executable, FAKE_LLM, str(self.port), self.script_path, "--log", self.log_path],
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=True,
        )
        time.sleep(0.4)
        if self.server.poll() is not None:
            fail("fake LLM failed to start")
        print(f"[e2e] workdir {self.workdir}; fake LLM on 127.0.0.1:{self.port}")
        self.db_path = os.path.join(self.workdir, "tui.db")
        self.child = None

    def agent(self, filename, slug, system_prompt, **extra):
        """Write an agent file; `extra` are more fields of the agent description."""
        contents = {
            "slug": slug,
            "apiKeyId": "none",
            "flavor": "OpenAIv1",
            "modelUrl": f"http://127.0.0.1:{self.port}/v1",
            "modelName": "fake-model",
            "announce": f"{slug}, a scripted test agent",
            "systemPrompt": [system_prompt],
            "builtinToolboxes": [],
            "mcpServers": [],
        }
        contents.update(extra)
        path = os.path.join(self.workdir, filename)
        with open(path, "w") as f:
            json.dump({"tag": "OpenAIAgentDescription", "contents": contents}, f)
        return path

    def start_tui(self, agent_path, first_screen):
        keys = os.path.join(self.workdir, "keys.json")
        keymap = os.path.join(self.workdir, "keymap.json")
        with open(keys, "w") as f:
            json.dump({}, f)
        with open(keymap, "w") as f:
            json.dump({"input": {"send_trigger": "triple_newline", "show_send_indicator": True}}, f)
        cmd = f"{find_binary()} --agent-file {agent_path} --api-keys {keys} tui --keymap {keymap} --db {self.db_path}"
        env = dict(os.environ, TERM="xterm-256color", COLUMNS="140", LINES="45")
        self.child = pexpect.spawn(cmd, cwd=self.workdir, env=env, dimensions=(45, 140), timeout=20)
        self.child.logfile = open(os.path.join(self.workdir, "pty.log"), "wb")
        self.child.expect(first_screen, timeout=15)
        time.sleep(0.3)
        return self.child

    def open_chat(self):
        c = self.child
        c.send("\x0e")  # Ctrl+N: new conversation
        time.sleep(2.5)
        c.send("\x1d")  # Ctrl+]: Chats tab
        time.sleep(1.0)
        c.send("\t")  # focus the conversation list
        time.sleep(0.5)
        c.send("\r")  # open it, focusing the message editor
        time.sleep(1.0)

    def send_message(self, text):
        c = self.child
        c.send(text)
        time.sleep(0.3)
        c.send("\r\r\r")

    def quit_tui(self):
        c = self.child
        time.sleep(0.5)
        c.send("\x11")
        time.sleep(0.3)
        c.send("\x11")
        c.expect(pexpect.EOF, timeout=15)
        c.close()
        if c.exitstatus != 0:
            fail(f"TUI exited with status {c.exitstatus}")
        print("[e2e] TUI exited 0")

    def sessions(self):
        """The stored sessions as parsed JSON, oldest first."""
        conn = sqlite3.connect(self.db_path)
        rows = conn.execute("SELECT json FROM sessions").fetchall()
        conn.close()
        return [json.loads(r[0]) for r in rows]

    def session_rows(self):
        """(session_id, agent_slug, parent_session_id) of every stored session, oldest first."""
        conn = sqlite3.connect(self.db_path)
        rows = conn.execute("SELECT session_id, agent_slug, parent_session_id FROM sessions ORDER BY created_at").fetchall()
        conn.close()
        return rows

    def requests(self):
        """What the fake LLM was asked: one dict per request."""
        if not os.path.exists(self.log_path):
            return []
        with open(self.log_path) as f:
            return [json.loads(line) for line in f if line.strip()]

    def close(self):
        if self.child is not None and self.child.isalive():
            self.child.terminate(force=True)
        self.server.terminate()
        try:
            self.server.wait(timeout=5)
        except Exception:
            self.server.kill()
