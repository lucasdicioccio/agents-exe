# Revision history for agents

## 0.2.0.0 -- Unreleased

### New Features

#### Terminal UI screenshots and end-to-end tests
- `agents-tui-e2e`, an opt-in test-suite (`cabal test agents-tui-e2e -ftui-e2e`), drives the real `agents-exe tui` in a pseudo-terminal with [tuispec](https://github.com/Tritlo/tuispec) against a scripted OpenAI-compatible endpoint: launch, send a message and read the reply, answer a deferred call from the Pending panel
- Each step is compared with a baseline under `test/tui-snapshots/`; `AGENTS_TUI_E2E_PNG=1` renders the screenshots `documentation/tui.md` now shows
- See `documentation/tui.md` (Screenshots and end-to-end tests)

#### `agents-exe new agent` bootstrap defaults
- A new agent can read, edit and list files under `./` (a `workspace` file sandbox shared by its developer and system toolboxes) and has a read-write SQLite memory (`./{slug}-memory.sqlite`); before, its file sandbox denied everything
- After creating the agent, reports whether `agents-exe.cfg.json` loads it and offers to add it to `agentsFiles` (`--add-to-config`, `--no-add-to-config`)
- A `DirectoryRecursive` predicate written with a trailing slash (`./`, `./src/`) now allows the directory itself, not only its contents
- The system toolbox's `list-directory` now honours a configured `FileSandbox` even when `attach-file` is not enabled; without a sandbox it stays unrestricted
- `documentation/cli-commands.md` documents the `config` command

#### Named file sandboxes
- An agent may declare file sandboxes once, by name, in `fileSandboxes`; a builtin toolbox (System, Developer, Lua) refers to one with `"FileSandbox": {"ref": "<name>"}`
- The inline `FileSandbox` form is unchanged; an undeclared name is a loading error, also reported by `validate-agent`
- `sandbox-ref(name)` in the `agents` template library
- See `documentation/tools.md` (Named Sandboxes)

#### Agent templates
- An agent file may be a tramaj program (`.tramaj`), evaluated once at load to the JSON an agent file holds
- `$ctx` is the process parameters (`--set`, `--pin`, `--params-file`); a template may only read process-scope, non-secret parameters the agent declares
- `tramajLibraries` in `agents-exe.cfg.json` lists directories of shared libraries; a built-in `agents` library builds sandboxes and toolboxes
- `agents-exe check --show-config` prints the evaluated JSON
- See `documentation/agent-templates.md`

#### `agents-exe paths` Command
- Added new diagnostic command `agents-exe paths` to show all important configuration paths
- Displays config file location, agent files, API keys file, and session storage directory
- Supports `--json` flag for machine-readable output
- Helps users debug configuration issues when running outside a project

#### Session Storage Fix
- Fixed bug where conversation JSON files were empty when running outside a project
- Sessions are now properly stored in `~/.config/agents-exe/sessions/` when no project config is found
- The sessions directory is created automatically on first use

#### `--agent <slug>` Selection
- Added `--agent <agent-slug>` (or `-a <slug>`) command line argument
- Allows selecting agents by their slug instead of full file paths
- Example: `agents-exe run --agent openai-assistant -p "Hello"`
- Provides helpful error messages listing available agents when slug is not found

#### `--alias <name>` Prompts
- Added `--alias <name>` command line argument for well-known prompts
- Predefined aliases available:
  - `translate`: Translate text to English
  - `summarize`: Summarize text content
  - `code-review`: Review code for issues
  - `explain`: Explain code in plain English
- Aliases can be configured in `agents-exe.cfg.json`
- Supports template variables: `{{content}}`, `{{language}}`, `{{filename}}`
- Auto-detects programming language from file extension

### Bug Fixes

- The Lua toolbox's `MaxMemoryMB` is now enforced: a script that allocates past the limit fails with "Lua script exceeded the memory limit of N MB" instead of growing until the host runs out of memory

### Improvements

- Better experience when running `agents-exe` outside of a project directory
- Default configuration structure is automatically created in `~/.config/agents-exe/`
- Example agents (openai-assistant, mistral-assistant, ollama-assistant, orchestrator) created automatically

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.

