#!/usr/bin/env python3
"""Phase 5 end-to-end check: a sub-agent call through the TUI, no real LLM.

The root agent's scripted first answer is a `io_prompt_agent_helper` tool
call; the helper answers "helper says hi"; the root then answers with a
final text. Drives `agents-exe tui` through a pty (checks/lib/e2e.py).

Asserts: the TUI exits 0; the database holds two sessions, the helper's
one linked to the root's as its child (`prompt_agent_*` ran as a runner
session, not in-tool); the root's tool result carries the helper's text;
the fake LLM saw the helper's request and the root's follow-up.
"""
import os
import sys

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
from e2e import Check, fail  # noqa: E402

FINAL = "root final answer"
HELPER = "helper says hi"

SCRIPT = [
    {"match": {"system_contains": "ROOT AGENT", "last_role": "user"},
     "respond": {"tool_calls": [{"name": "io_prompt_agent_helper", "arguments": {"what": "please say hi"}}]}},
    {"match": {"system_contains": "HELPER AGENT"}, "respond": {"text": HELPER}},
    {"match": {"system_contains": "ROOT AGENT", "last_role": "tool"}, "respond": {"text": FINAL}},
]


def main():
    chk = Check("subagent", SCRIPT)
    try:
        chk.agent("helper.json", "helper", "You are the HELPER AGENT.")
        root = chk.agent(
            "agent.json", "root", "You are the ROOT AGENT.",
            extraAgents=[{"slug": "helper", "path": "helper.json"}],
        )
        chk.start_tui(root, "root")
        chk.open_chat()
        chk.send_message("delegate to the helper")
        chk.child.expect(FINAL, timeout=30)
        print("[e2e] root's final answer rendered")
        chk.quit_tui()

        sessions = chk.sessions()
        print(f"[e2e] sessions: {len(sessions)}")
        if len(sessions) != 2:
            fail(f"expected 2 sessions (root and helper), got {len(sessions)}")
        text = str(sessions)
        if HELPER not in text:
            fail("the helper's answer is not in any stored session")
        rows = {slug: (sid, parent) for sid, slug, parent in chk.session_rows()}
        print(f"[e2e] session rows: {rows}")
        if set(rows) != {"root", "helper"}:
            fail(f"expected a root and a helper session, got {sorted(rows)}")
        if rows["root"][1] is not None:
            fail("the root session should have no parent")
        if rows["helper"][1] != rows["root"][0]:
            fail("the helper session should be a child of the root session (prompt_agent_* must run as a runner session)")
        reqs = chk.requests()
        print(f"[e2e] fake LLM requests: {len(reqs)}, rules {[r['rule'] for r in reqs]}")
        if [r["rule"] for r in reqs] != [0, 1, 2]:
            fail("expected the root call, the helper call and the root follow-up, in that order")
        print("[e2e] PASS")
    finally:
        chk.close()


if __name__ == "__main__":
    main()
