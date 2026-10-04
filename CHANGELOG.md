# Revision history for agents

## 0.2.0.0 -- Unreleased

### New Features

#### Tunable `spectate` display
- `agents-exe spectate --panels SPEC` chooses the panels shown, their places and sizes (`tree:60+tools:40/45,text/55` is the default screen; `tree,tools,text` is three columns); `--refresh SECONDS` sets the time between two refreshes
- Keys change the same things while running, as in `top`: `1` `2` `3` show or hide a panel, `Tab` picks one, `<` `>` move it, `[` `]` and `-` `+` resize it, `s` restacks it, `d` `D` change the refresh interval, `0` goes back to the layout at start, `?` lists the keys and the flags that reproduce the screen
- `W` saves the layout to `~/.config/agents-exe/spectate-layout` (`--layout-file` for another file), read at the next start; the flags go over it
- The screen is now redrawn once per refresh interval rather than on every event
- See `documentation/cli-commands.md` (spectate)

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

### Improvements

- Better experience when running `agents-exe` outside of a project directory
- Default configuration structure is automatically created in `~/.config/agents-exe/`
- Example agents (openai-assistant, mistral-assistant, ollama-assistant, orchestrator) created automatically

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.

