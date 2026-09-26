# Scriptable fake LLM and pty end-to-end checks

`fake_llm.py` is an OpenAI-compatible chat-completions server driven by a
script of rules (match on the request index, the system prompt, the last
message, the tools offered; answer with text or tool calls; log every
request). `e2e.py` wraps it with agent files, the embedded TUI driven through
`pexpect`, and the SQLite database the TUI leaves behind. See the docstrings.

The older `checks/phase3b-iv-e2e` and `checks/phase4-attach-e2e` use the
fixed-reply `fake_openai.py` and are unchanged.

Checks built on it (each needs `pexpect` and a built `agents-exe`, found with
`cabal list-bin agents-exe`; none needs an API key):

```sh
python3 checks/phase5-subagent-e2e/run_e2e.py        # a prompt_agent_* call runs as a child session
python3 checks/phase5-pending-e2e/run_e2e.py answer  # Ctrl+Y answers a deferred call
python3 checks/phase5-pending-e2e/run_e2e.py fail    # Ctrl+W fails it: "Error: <reason>"
python3 checks/phase5-pending-e2e/run_e2e.py select  # Ctrl+O picks the second of two pending calls
```
