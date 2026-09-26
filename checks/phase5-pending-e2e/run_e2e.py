#!/usr/bin/env python3
"""Phase 5 end-to-end check: deferred calls answered from the TUI's Pending
panel, no real LLM.

    run_e2e.py [answer|fail|select]

The agent's tool policy defers every call, so the scripted model's tool call
stops the run and the Pending panel lists it. Then, through a pty:

  answer  Ctrl+Y, type a result, send: the run resumes with that result.
  fail    type a reason, Ctrl+W: the call completes with "Error: <reason>".
  select  two calls pending; Ctrl+O selects the second, Ctrl+Y answers it
          (not the oldest); then the first is answered too.

Asserts on what the fake LLM was shown once the calls were completed (the
tool message of each call id in its last request) and that the TUI exits 0.
"""
import os
import sys
import time

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
from e2e import Check, fail  # noqa: E402

TOOL = "bash_lookup"  # the tool's name as the model sees it (checked below against the request log)
FINAL = "agent final answer"


def tool_script(n_calls):
    calls = [{"id": f"call_{i}", "name": TOOL, "arguments": {"key": f"k{i}"}} for i in range(n_calls)]
    return [
        {"match": {"system_contains": "PENDING AGENT", "last_role": "user"}, "respond": {"tool_calls": calls}},
        {"match": {"system_contains": "PENDING AGENT", "last_role": "tool"}, "respond": {"text": FINAL}},
    ]


def write_tool(workdir):
    tools = os.path.join(workdir, "tools")
    os.makedirs(tools, exist_ok=True)
    path = os.path.join(tools, "lookup")
    with open(path, "w") as f:
        f.write(
            "#!/bin/bash\n"
            'if [ "$1" == "describe" ]; then\n'
            '  echo \'{"slug":"lookup","description":"looks something up","args":[{"name":"key","description":"the key","type":"string","backing_type":"string","arity":"single","mode":"dashdashspace"}],"empty-result":{"tag":"AddMessage","contents":"nothing"}}\'\n'
            "  exit 0\nfi\n"
            'echo "looked up $2"\n'
        )
    os.chmod(path, 0o755)


def main():
    mode = sys.argv[1] if len(sys.argv) > 1 else "answer"
    if mode not in ("answer", "fail", "select"):
        fail(f"unknown mode {mode}")
    chk = Check(f"pending-{mode}", tool_script(2 if mode == "select" else 1))
    try:
        write_tool(chk.workdir)
        agent = chk.agent(
            "agent.json", "pending-agent", "You are the PENDING AGENT.",
            toolDirectory="tools",
            executionMode="asynchronous",
            toolCallPolicyConfig={"default": {"tag": "defer", "reason": "e2e"}, "rules": []},
        )
        chk.start_tui(agent, "pending-agent")
        chk.open_chat()
        chk.send_message("look things up")
        c = chk.child
        c.expect("Pending", timeout=30)
        print("[e2e] Pending panel shown")
        time.sleep(1.0)

        if mode == "answer":
            c.send("\x19")  # Ctrl+Y
            time.sleep(0.5)
            chk.send_message("the-answer-42")
        elif mode == "fail":
            c.send("no thanks")
            time.sleep(0.3)
            c.send("\x17")  # Ctrl+W
        else:
            c.send("\x0f")  # Ctrl+O: select the second call
            time.sleep(0.5)
            c.send("\x19")
            time.sleep(0.5)
            chk.send_message("answer-for-second")
            time.sleep(3.0)
            c.expect("Pending", timeout=30)  # the first is still pending
            c.send("\x19")
            time.sleep(0.5)
            chk.send_message("answer-for-first")

        c.expect(FINAL, timeout=30)
        print("[e2e] the run resumed to its final answer")
        chk.quit_tui()

        reqs = chk.requests()
        print(f"[e2e] fake LLM requests: {len(reqs)}: {[(r['rule'], r['last']['role']) for r in reqs]}")
        if TOOL not in reqs[0]["tools"]:
            fail(f"the model was not offered {TOOL}: {reqs[0]['tools']}")
        # What each call completed with, as the model saw it in its last request.
        seen = reqs[-1]["tool_messages"]
        print(f"[e2e] tool messages seen by the model: {seen}")
        expect = {
            "answer": {"call_0": "the-answer-42"},
            "fail": {"call_0": "Error: no thanks"},
            "select": {"call_1": "answer-for-second", "call_0": "answer-for-first"},
        }[mode]
        for call_id, want in expect.items():
            got = seen.get(call_id, "")
            if want not in got:
                fail(f"{call_id} should have completed with {want!r}, the model saw {got!r}")
        print("[e2e] PASS")
    finally:
        chk.close()


if __name__ == "__main__":
    main()
