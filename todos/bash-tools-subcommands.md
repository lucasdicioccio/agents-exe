# Spec: bash tools with subcommands

Status: implemented, 2026-11-22 (projectz feature `d2726aa5`). Extends the v1
protocol (`documentation/binary-tool.md`) and makes §1.3 (`commands`) of
`todos/bash-tools-v2.md` concrete; `help` and `setup` from that section stay
out of scope.

## Goal

Some binaries do several things through subcommands (`kv get`, `kv set`,
`git log`, ...), each with its own arguments. A v1 tool has one flat `args`
list, so such a binary had to be wrapped once per subcommand, or expose one
tool whose arguments are the union of every subcommand's (the model then has
to guess which ones go together). This spec lets one `describe` declare the
subcommands and their own arguments.

## The `describe` document

A new optional top-level field `commands`, a list of:

| Field | Required | Meaning |
|---|---|---|
| `name` | yes | The subcommand, passed to `run`. ASCII letters, digits, `_`, `-` (it ends up in a tool name). Unique. |
| `description` | yes | What this subcommand does, for the model. |
| `args` | no (default `[]`) | Its own arguments, same shape as the top-level `args`. |
| `empty-result` | no | Overrides the top-level `empty-result` for this subcommand. |
| `output-media-type` | no | Overrides the top-level one for this subcommand. |

The top-level `args` stay, and are **shared**: every subcommand gets them
before its own. A subcommand may not redeclare a shared argument nor one of
its own twice. Any violation fails the load of the script (an
`InvalidDescriptionError`, as malformed JSON does).

```json
{
  "slug": "kv",
  "description": "a key-value store",
  "args": [ {"name": "store", "mode": "dashdashspace", ...} ],
  "commands": [
    { "name": "get", "description": "reads a key",
      "args": [ {"name": "key", "mode": "dashdashspace", ...} ] },
    { "name": "set", "description": "writes a key",
      "args": [ {"name": "key", "mode": "dashdashspace", ...},
                {"name": "value", "mode": "positional", ...} ],
      "empty-result": {"tag": "AddMessage", "contents": "done"} }
  ]
}
```

A `describe` without `commands` (or with an empty list) is a v1 tool,
unchanged.

## How it appears to the model: one tool per subcommand

Decision: a script with `commands` is **not** a tool itself; it exposes one
tool per subcommand, named `bash_<slug>_<name>` (`bash_kv_get`,
`bash_kv_set`). Each tool's schema is the shared args followed by the
subcommand's own; its description is the subcommand's description followed
by `(subcommand '<name>' of <slug>: <top-level description>)`.

Why not one tool with a discriminated `command` argument: our tool schemas
are flat lists of parameters (`ParamProperty`), so a discriminated union
would either need `oneOf` support across every LLM backend, or a flat union
of every argument where the model has to know which belong to which
subcommand. One tool per subcommand keeps each schema exact, needs nothing
from the backends, and composes with everything keyed by tool name
(bindings, activation, caching, narrowing). The separator is `_`, not `.` as
v2 §1.3 sketched, because `.` is not allowed in OpenAI tool names.

## How `run` passes the subcommand

```
./kv run get --store s1 --key k
./kv run set --store s1 --key k v
```

The subcommand name comes right after `run`, before every argument (all
modes: positional, dashdash, stdin and env are unchanged).

## Compatibility

* v1 scripts: nothing changes (no `commands`, no new argv).
* v2 (`todos/bash-tools-v2.md`): `commands` is the field §1.3 named;
  `process`/`runtime` are orthogonal and would apply to every subcommand.
* Skills scripts share `ScriptInfo` and parse `commands`, but the skills
  toolbox does not expand them yet: out of scope here.

## Implementation

* `System.Agents.Tools.ScriptTypes`: `ScriptCommand`, `ScriptInfo` gains
  `scriptCommands` (parsed) and `scriptSubcommand` (set by expansion only),
  `validateScriptCommands`, `expandScriptCommands`.
* `System.Agents.Tools.Bash`: `loadScript` validates; `loadScriptTools`
  returns the expanded tools; `loadDirectory` and the single-script toolbox
  source use it; `runValue` inserts the subcommand after `run`;
  `selectSubcommand` for the CLI helpers.
* CLI: `describe-tool` lists the subcommands; `check-tool-call --command
  NAME` validates against one; `replay-tool-call` picks the subcommand tool
  the recorded call named.
* Tests: `test/BashSubcommandsTests.hs`.
