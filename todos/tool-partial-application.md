# Spec: partial application of tool arguments

Status: Phases 1-7 and §8.4 (`derive_agent`) done as of 2026-09-21. Phase 1 done
(`97961e0`). Phase 2 done
except MCP server `env` (G4); the tool-cache key (G8) is done too, see below (`03783fb`,
`b0c53a5`): parameters, `ctxParams` (now actually wired at runtime, not
just in tests), `--set`/`--set-json`/`--pin`/`--pin-json`/`--params-file`,
process-scope resolution, secret-argv-mode load-time guard (G7).
Phase 3 done (`8a41cd8`): OpenAPI/PostgREST argument bindings via the
generic combinator, `ParamSource` secrets resolved per request.

Phase 4 (session-level parameters in `agents-server`) done for the core
mechanics, live-verified end to end against a real LLM: `params` on
`POST /v1/sessions`, `/messages`, `/resume` and `POST /v1/continuations/:token`;
the validation table (`422 unknown_params`, `403 forbidden_params`,
`422 invalid_params`, `422 params_required`); a `params` column and
migration on both the SQLite and Postgres session backends (`SessionMeta.
smParams`, non-secret only); secret session values kept only in the
in-memory `LiveSession` (`Host/Runner.hs`'s new `lsParams`), never
persisted; message-scope values applied for one run only, never stored;
`agentView`'s `"parameters"` self-description (`bound`/`pinned`, never a
value); `agents-server --set`/`--set-json`/`--pin`/`--pin-json` (mirroring
`agents-exe`) so an operator can lock a container to one tenant. Not done
from Phase 4: the chat page's parameter form, and the generated OpenAPI
document's request/response schemas (`AgentsServer/Types.hs`/`Routes.hs`
are a separate hand-maintained layer used only for `/openapi.json` and
were not updated with the new fields — the real handlers in `Api.hs` are
fully wired and correct; this is a documentation-only gap).
`recoverOnStartup` marks the running calls of a session whose required
parameters are no longer bound as failed with a `params_required` message
(§5's documented recovery behaviour; done 2026-09-25, which also made the
persisted non-secret session values seed the live session back after a
restart or an eviction, as §5 always said they would). `seal` and session
tokens (§5.1) are done: `seal`/`session_token` on `POST /v1/sessions`, a
`security` column (sealed flag and token SHA-256 digest, never the token) on
both backends, session-token routing limited to its own session's read,
messages, events and cancel, and `DELETE /v1/sessions/:id/token`. Deviations:
the token is only honoured on `/v1/sessions/:id/...` paths of its own session
(anything else is a 401, so no probing); `seal` exempts the owner as §5.1 says,
so today it only matters against session-token holders, who never may set
parameters anyway; a fork or child does not inherit either. Since then:
`PUT /v1/sessions/:id/params`, `409 params_required` on a message or resume
to an existing session, and `params` on `POST /v1/sessions/:id/fork` (which
lacks §5.2's `fork` on session creation and the CLI's `--fork`; required
parameters are not enforced at fork time).

Phase 5 (sub-agents and MCP over HTTP) done, checked live with `agents-exe
check` against fixture agents and with an `agents-server` integration
test: `ExtraAgentRef` gains `with` (§7), a map from the *child's*
parameter names to `BindingValue`s in the *parent's* scope;
`AgentConfigNode.nodeExtraWith` carries it from discovery through to
`wireAgentTools`, which checks it at load time (every `with` key must be
a declared parameter of the child; every required session-scope parameter
of the child must be covered by `with` or by the child's own process
value/default) and reports failures through the existing
`LoadingError`/`OtherError` path. `Props.agentToTool` and
`turnAgentRuntimeIntoIOTool` gained a `Maybe (Map ParamName BindingValue)`
argument (`Nothing` for a child reached through a toolDirectory, which has
no reference site to hang a `with` on); `runSubAgent` resolves it against
the caller's `ctxParams` at call time (`OneShotTool.hs`'s new
`resolveWith`) and overlays the result onto the child's own resolved
params, so the child's own process values and defaults still apply
underneath. A `Literal` in `with` is never secret; a `Param` inherits
secrecy from the caller's value. MCP over HTTP (`AgentsServer/Mcp.hs`,
`Api.hs`) gained `Agents-Param-<name>` request headers (always strings)
and `_meta."agents-exe/params"` on a `tools/call` (any JSON, overrides the
headers by name); each call is one run, so session and message scope
coincide there, and both go through the same validation table as the REST
API.

Phase 6 (narrowing helpers down the call chain) done, checked with the
`agents-tests` suite (`NarrowingTests.hs`: the `AgentAddress` algebra, and
`describe_agent` end to end against a loaded three-level tree) and with
`agents-exe check` against fixture agents: `Agent` gains a top-level
`bindings` list (§8.1), applied in `loadAgentToolboxes` after every
toolbox's own bindings, matching by the LLM-visible tool name across
toolboxes (bash tools have no toolbox name of their own to hang a
toolbox-level binding on). `ExtraAgentRef` gains `narrowable` (default
`True`); `AgentConfigNode.nodeExtraNarrowable` carries it alongside
`nodeExtraWith`. `describe_agent` (§8.2) is registered by `wireAgentTools`
next to `prompt_agent_<slug>` for any agent with at least one helper; it
recurses over `nodeChildren`/`nodeExtraRefs`, cut at `maxDescribeDepth` and
at cycles, shows each declared parameter with a `bound` flag (already
resolved, or covered by the reference's `with`) and each tool's still-open
arguments (its current, already-bound-reduced schema minus anything the
caller's inherited bindings already cover), and reports only `announce`
for a `narrowable: false` reference. `prompt_agent_<slug>` gains optional
`bindings` and `with` arguments (§8.3): `AgentBinding`/`ScopedBinding`/
`AgentAddress` (`Bindings/Types.hs`) represent a binding addressed at the
prompted helper (`Here`), one of its own helpers (a slash-separated
`AgentPath`), or all of them (`"**"`/`Everywhere`); `descendAddress`/
`reRootBindings` re-root an address (or a whole list of `ScopedBinding`s)
one level down when a call descends into a named child. `runSubAgent`
resolves the call's own `bindings` against the caller's `ctxParams`
(`resolveCallBindings`; an unresolvable required `Param` fails the call
with a message the calling model can act on, per D8), combines them with
the caller's own `ctxInheritedBindings` re-rooted at the prompted helper,
applies whatever is addressed at the helper itself to a *fresh* copy of
its `osNodeTools` (`applyBindings`, scoped to this call only — the shared,
loaded toolboxes are never mutated) via a throwaway `OSAgentNode` passed to
`buildAgent`, and carries the rest into the sub-agent's own
`SessionBase.ctxInheritedBindings` for it to apply, in turn, when it
prompts its own helpers. `ToolExecutionContext`/`Session.Base.Agent` both
gained `ctxInheritedBindings :: [ScopedBinding]`, threaded through
`buildContext` like `ctxParams`; `ToolExecutionContextSnapshot` keeps only
its non-secret entries, same as `tecsParams`. A call-time `with` cannot
refill a parameter the reference's own static `with` (§7) already fills. A
`narrowable: false` reference refuses any `bindings`/`with` on the call
that reaches it, and any inherited binding addressed at it or below,
rather than applying them.

Phase 7 (`Expose`) done, checked with the `agents-tests` suite
(`BindingsTests.hs`'s new "Expose bindings (§7)" group) and live with
`agents-exe check` against fixture agents (an `Expose`-bound bash argument
stays in the schema when its parameter is a session-scope parameter that
`check` never resolves; a secret parameter bound with `whenUnbound:
"expose"` is a load-time `ExposedSecretParameter` error). A binding whose
`whenUnbound` is `"expose"` is left out of `applyBindings`'s *static*
schema reduction entirely (D3's opt-in exception): the shared registration
keeps the argument declared, so it is never hidden or shown by mistake for
every session at once. `Bindings.narrowExposedSchema :: Params -> [Binding]
-> ToolRegistration -> ToolRegistration` re-hides it when the parameter
happens to resolve in a given snapshot; `System.Agents.Combinators.
ProgressiveDisclosure.agentEvaluateActiveTools` calls it fresh on every
read of the tool list — the same "read fresh on every access" pattern it
already used for activation — against a new `IO Params` action
(`AgentFactory.AgentDeps.adLiveParams`) read only when the node has at
least one `Expose` binding. Outside `agents-server`, `adLiveParams`
defaults to `pure mempty`: an `Expose`d argument then behaves like a
process-scope-only binding, decided once from `osNodeParams`, since there
is no notion of a session distinct from the process. `agents-server`'s
`Host/Runner.newAgent` overrides it with `readTVarIO live.lsParams`, so the
tool list a session sees updates the moment `prepareParams` writes a new
session-scope value in — no agent rebuild, no registration mutation. A
message-scope value (this run only, never written to `lsParams`) is not
seen by this mechanism and so is treated as unbound for schema-visibility
purposes; the value itself is still merged correctly at call time
regardless, since that path (`wrapTool`, unchanged) already reads
`ctxParams` fresh per call. `AgentTree.loadAgentToolboxes` collects every
`Expose` binding for a node (toolbox-level, returned out of
`ToolLoader.loadAgentTools`/`loadBashTools`/`loadOpenAPIToolboxes`/
`loadPostgRESToolboxes`; and agent-level, from `Agent.bindings`) into a new
`OSAgentNode.osNodeExposeBindings` TVar, and rejects any of them bound to a
declared `secret` parameter (`Bindings.exposedSecretBindings`, a new
`ExposedSecretParameter` `LoadingError`): exposing a secret back to the
model would just let it retype the value in plain text.

§8.4 (`derive_agent`) done, checked with the `agents-tests` suite
(`BindingsTests.hs`'s `deriveAgentTable` group; `NarrowingTests.hs`'s
`derive_agent` group against the loaded three-level tree) and live with
`agents-exe check`. `derive_agent {from, slug, bindings, with}` is a new
tool, registered by `wireAgentTools` alongside `describe_agent` for any
agent with at least one helper: it checks `from` names a known, narrowable
direct helper and, if so, echoes back `{"stored": true, "from", "slug"}` —
nothing else happens, and nothing is stored beyond that call landing in the
session's own history. `prompt_agent_<from>` gains an optional `as`
argument; at call time, `System.Agents.Tools.Bindings.deriveAgentTable`
folds the *current* session's turns (matching successful `derive_agent`
calls by function name and response, the same "recomputed fresh, not
persisted" treatment as `ctxSessionToolCalls`) into a `Map (from, slug)
DerivedNarrowing`, exposed on a new `ToolExecutionContext.
ctxDerivedNarrowings` field set in `Session/Step.buildContext` — so this
works for every front-end without any wiring, no `includeFullSession` op-
in needed. `runSubAgent` looks up `(helper, as)`; an unknown name is an
error the calling model can retry (naming `derive_agent`, per D8). When
found, the derived `bindings` apply before the call's own explicit ones,
and the derived `with` fills what the call's own `with` and the
reference's static `with` (§7) do not. A `narrowable: false` helper refuses
`derive_agent` and `as` the same way it already refuses `bindings`/`with`.

## Goal

Let whoever *runs* an agent pin down some arguments of its tools, so that the
LLM neither sees nor chooses them. The motivating case: one bundled agent
(say, a docker image with `agents-server` and a bash toolbox) serves several
tenants, and each session carries its own `tenant-id` and API token.

The primitive is partial application, on purpose. A tool is a function from a
JSON object to a result:

```
query-invoices : { tenant_id, token, since } -> Result
```

Binding `tenant_id` and `token` gives a smaller function, and that smaller
function is the tool the LLM gets:

```
query-invoices' : { since } -> Result
```

Two things follow, and they are the whole feature:

1. **Schema**: a bound argument disappears from the tool's JSON schema. The
   model is not told that it ever existed. That is the point: with most
   arguments already given, the model has less to reason about locally.
2. **Call**: the bound value is merged into the LLM's argument object just
   before dispatch. The bound value always wins.

The deliverable is:

1. a generic combinator on `ToolRegistration` in `agents-lib`, so every kind
   of toolbox (bash, MCP, OpenAPI, PostgREST, builtin, sub-agents) gets the
   feature at once;
2. a way to *declare* an agent's parameters and to *bind* tool arguments to
   them in the agent file;
3. ways to *supply* parameter values: at process start (`agents-exe`,
   `agents-server`) and transiently per session (`agents-server`);
4. a changed interface for helper agents exposed as tools, so that a parent
   can *discover* the open tool arguments of a helper and *bind* some of them
   when it prompts it, all the way down a chain of sub-agents (§8).

## Scenarios

The design is checked against these three.

**S1. One container, one tenant.** A docker image holds `agents-server`, an
agent file and a bash toolbox. The operator starts it with
`-e BILLING_TOKEN=…` and `--pin tenant=acme`. Nobody who talks to the
container can change either. (§4)

**S2. A backend mints a credential and leads a chat.** A product API
receives "open an assistant on project 42" from a logged-in user. It mints a
credential that is valid for project 42 only, creates a session on
`agents-server` with `params: {project_id: "42", callback_token: "…"}` and
`seal: true`, and hands the browser a session token. The agent's tools are
callbacks into the product API, with `project_id` and the `Authorization`
header bound. Three defences stack up:

* the model cannot hallucinate a project id: the argument is not in any
  schema, and a bound value wins over anything the model sends;
* a prompt injection cannot retarget the tools, for the same reason;
* enumeration fails even if the two above were bypassed, because the minted
  credential opens one project. The end user, who only holds the session
  token, cannot re-parameterise the session either.

(§3.3, §5, §5.1)

**S3. A parent narrows its helpers, down the chain.** A root agent splits a
job and prompts a helper with extra bindings for that part (one repository,
one date range), routing secrets by parameter name without ever seeing them.
The helper may have helpers of its own: the root can bind arguments of tools
anywhere below it, each agent on the way can narrow further, and none can
undo what an ancestor bound. (§8)

## Non-goals (for this spec)

* Sandboxing. A bound token still reaches a bash script that the agent
  author wrote; we do not defend the value against the tool itself.
* A general templating language in agent files (`${…}` expansion in
  prompts, paths, URLs). Parameters bind tool arguments, nothing else.
  See [Decisions](#decisions), D7.
* Per-tenant *agent definitions*. The agent stays one static definition; only
  parameter values change between sessions.
* Encrypting values at rest. Secret values are simply never written.

---

## Current state

### What we can reuse as-is

| Piece | Where | Notes |
|---|---|---|
| One shape for all tools | `ToolRegistration{innerTool, declareTool, findTool, toolActivation}` (`ToolRegistration.hs:154`) | Schema is `declareTool.toolDescriptionParamProperties`; the call receives an `Aeson.Value`. A wrapper can rewrite both without knowing the toolbox kind. |
| A per-call context, already threaded everywhere | `ToolExecutionContext` (`Tools/Context.hs:238`), built per step in `Session/Step.hs:855` | The run function of every tool gets it. |
| A serializable part of that context | `ToolExecutionContextSnapshot` (`Tools/Context.hs:422`) | Persisted with deferred and async calls. |
| Value sources | `SecretSource = Given \| ApiKeySource \| FileSystem \| EnvVar \| Command` (`Tools/Secrets.hs:105`) and `resolveSecretSource` (`:343`) | Exactly what a parameter *default* needs. |
| Header/query serializers | `SecretSerializer` (`Secrets.hs:161`), `applySecretsToHeaders` (`:464`) | Applied per request already; only the *resolution* is load-time. |
| Bash argument marshalling | `translateArguments` (`ScriptTypes.hs:186`), `flattenArguments`/`flattenInput` (`Bash.hs:272`, `:284`) | Reads declared args from a JSON object. Merging bound values upstream needs no change here. |
| Single env funnel for bash tools | `buildToolEnvironment` (`Bash.hs:356`) | |
| Session metadata row with reserved columns | `SessionMeta` (`SessionStore.hs:192`) | Home for non-secret session parameters. |
| Per-session entry points | `Runner.createSessionAs` (`Host/Runner.hs:510`), `postMessage` (`:530`), `Api.createH` (`Api.hs:340`) | |
| Server self-description | `agentView` (`Api.hs:279`), `GET /openapi.json` | Where clients learn which parameters an agent wants. |

### Gaps

**G1. Nothing flows from the caller to a tool call.** `agents-server` reads
`agent`, `prompt`, `media`, `run` and the bearer token; none of it reaches
`ToolExecutionContext`. The CLI has no `--set`-like flag.

**G2. Toolboxes are frozen at tree load.** `osNodeTools :: TVar
[ToolRegistration]` is built once per agent tree and shared by all sessions.
Any per-session value must therefore arrive through the call context, not
through the registration.

**G3. HTTP toolbox secrets resolve once.** `OpenAPIToolbox.initializeToolbox`
(`OpenAPIToolbox.hs:323`) calls `resolveSecrets` at load. A per-session token
is impossible.

**G4. MCP servers cannot receive configuration.**
`McpSimpleBinaryConfiguration` has `name`, `executable`, `args` and no `env`.
`mcpTool` (`ToolRegistration.hs:1699`) ignores the context entirely.

**G5. Bash arguments are always `required`.** `mapArg`
(`ToolRegistration.hs:262`) sets `propertyRequired = True` and ignores
`argTypeArity`. The skills toolbox already honours arity
(`Skills/Toolbox.hs:333`). Not caused by this feature, but it makes "leave an
optional argument out" impossible to express, so it is fixed here.

**G6. Bash tools only take values through argv or stdin.** A token in argv
shows in `ps` and in traces. There is no calling mode that passes an argument
as an environment variable.

**G7. Traces record argv.** `RunCommandStart path args` (`Bash.hs:193`)
logs the full command line. A bound secret would land in log files.

**G8. The tool cache ignores everything but the LLM's arguments.**
`computeCacheKey` hashes tool name + LLM arguments (`Tools/Cache.hs:71`,
`Session/Async.hs:335`). With bindings, tenant A's cached result would answer
tenant B's identical call.

**G9. Sub-agents receive nothing from their caller** except the call stack
and the OS world (`OneShotTool.hs:191`). A child agent with its own tenant-aware
toolbox has no way to learn the tenant.

**G10. A parent cannot narrow a helper.** Helpers are the agents exposed to
a parent as `prompt_agent_<slug>` tools
(`OneShotTool.turnAgentRuntimeIntoIOTool`, wired by
`AgentTree.wireAgentTools`, `AgentTree.hs:724`). The tool takes one argument,
`what` (`OneShotTool.hs:95`). The parent cannot learn which tools a helper
has or what arguments they take, and cannot fix any of them for a call. With
a chain root → A → B, the root has even less say over B. One would expect
narrowing to compose along the chain naturally; today there is nothing to
compose. (The developer toolbox's `create-agent` is unrelated: it bootstraps
agent JSON *files* for a human and is left alone by this spec.)

---

## Design

### Overview

Three separate notions, kept apart deliberately:

```
  agent file                                  caller
  ──────────                                  ──────
  parameters:   declares names       ◄─────   supplies values
     tenant, api_token                         (process start, or per session)
        ▲
        │ refers to
  bindings:     tool argument := parameter | literal
     query-*.tenant_id := tenant
```

* A **parameter** is a named hole in the *agent*. The agent author declares
  it. An agent with parameters is a function `Params -> Agent`.
* A **binding** ties one *tool argument* to a parameter or to a literal. The
  agent author writes it, next to the toolbox.
* A **value** is supplied by the caller for a *parameter*, never directly
  for a tool argument.

The indirection matters for safety: a remote client can fill `tenant`, and
only `tenant`. It cannot decide to bind the `path` argument of some other
tool. What is bindable is the author's decision; what it is bound *to* is the
caller's.

### 1. Parameters

New optional field on `Agent` (`Base.hs:1595`):

```json
"parameters": [
  { "name": "tenant",
    "description": "Tenant identifier, as in the billing system",
    "scope": "session" },
  { "name": "api_token",
    "description": "Bearer token for the billing API",
    "secret": true,
    "scope": "session",
    "default": { "tag": "EnvVar", "contents": "BILLING_TOKEN" } },
  { "name": "region",
    "scope": "process",
    "default": { "tag": "Given", "contents": "eu-west-1" } }
]
```

```haskell
data ParameterDecl = ParameterDecl
    { paramName :: ParamName            -- [a-z][a-z0-9_]*
    , paramDescription :: Maybe Text
    , paramSecret :: Bool               -- default False
    , paramScope :: ParamScope          -- default ScopeProcess
    , paramRequired :: Bool             -- default True
    , paramDefault :: Maybe SecretSource
    }

data ParamScope = ScopeProcess | ScopeSession | ScopeMessage
```

* `scope: "process"`: only the operator can set it (CLI flag, environment,
  default). An HTTP client that tries gets `403`. This is the docker case
  where the operator pins a tenant for the whole container.
* `scope: "session"`: a client may also set it per session; the value lasts
  as long as the session. Process-level values still act as the fallback.
* `scope: "message"`: a client may set it with a request that starts a run
  (create, message, resume, continuation); the value lasts for that run only
  and is dropped when the run blocks or completes. Meant for short-lived
  tokens. It is never persisted, secret or not (§5).
* `secret: true`: the value must be a string; it is never persisted, never
  traced, never returned by the API (§6).
* `default` reuses `SecretSource`, so a default can be a literal, an env var,
  a file, a command, or an entry of the API-keys file.

Values are JSON (`Aeson.Value`), because MCP and OpenAPI arguments are not
always strings. Bash arguments need strings (§3.1).

Resolution order for one parameter, first hit wins:

1. pinned process value (`--pin`, §4): nothing below is looked at;
2. message value (only if `scope = message`);
3. session value (only if `scope = session`);
4. process value (`--set`, `--params-file`);
5. `default`, resolved once at tree load;
6. unbound.

An unbound `required` parameter is an error *before* any LLM call:
at startup for a process-scope parameter, at session creation or message post
for a session-scope or message-scope one (§5).

### 2. Bindings

Every toolbox description gains an optional `Bindings` list (same
prefix-stripping convention as `Activation`). Example for a bash toolbox:

```json
{ "tag": "FileSystemDirectory",
  "contents": {
    "Path": "tools",
    "Bindings": [
      { "arg": "tenant_id", "value": { "tag": "Param", "contents": "tenant" } },
      { "arg": "token",     "value": { "tag": "Param", "contents": "api_token" } },
      { "tool": "export-*", "arg": "format",
        "value": { "tag": "Literal", "contents": "csv" } }
    ] } }
```

```haskell
data Binding = Binding
    { bindTool :: Maybe ToolGlob     -- matched against the toolbox-local name; default: all
    , bindArg :: Text
    , bindValue :: BindingValue
    , bindWhenUnbound :: WhenUnbound -- default Fail
    }

data BindingValue = Param ParamName | Literal Aeson.Value

data WhenUnbound
    = Fail     -- the call fails with a clear error (cannot happen for required params)
    | Omit     -- the argument is left out; only for optional arguments
    | Expose   -- (later, Phase 7) the argument goes back to the LLM
```

`Literal` is the degenerate case and is useful on its own: specialise a
generic third-party tool without writing a wrapper script.

Load-time checks, all of them errors in `agents-exe check`:

* a `Param` names an undeclared parameter;
* a binding matches no tool, or no matched tool has that argument (typo
  guard). For MCP, whose tool list arrives late and may refresh, this is a
  warning trace at each `ToolsRefreshed`;
* two bindings hit the same (tool, argument);
* `Omit` on a required argument;
* a `secret` parameter bound to a bash argument whose mode is `positional`,
  `dashdashspace` or `dashdashequal` (it would show in `ps`): an error,
  pointing to the `env` mode of §3.1.

### 3. The combinator

In a new module `System.Agents.Tools.Bindings`:

```haskell
applyBindings :: [ResolvedBinding] -> ToolRegistration -> ToolRegistration
```

* `declareTool`: drop bound `ParamProperty`s from
  `toolDescriptionParamProperties`, and nothing else. No "pre-set" note, no
  trace of the argument: what the model gets is indistinguishable from a tool
  that never had it (D8). A tool whose prose description talks about an
  argument that the author then binds is the author's to reword; `check`
  warns when a bound argument's name appears in the tool description.
* `innerTool` / `findTool`: wrap the run function. At call time it reads
  parameter values from the context, builds the bound object, and calls the
  original with `bound <> llmArgs` (left-biased: bound wins). If the LLM sends
  a bound key anyway, it is dropped and a trace is emitted.

It is applied in `ToolLoader.loadAgentTools` (`ToolLoader.hs:127`), once per
toolbox, after the toolbox produced its registrations. Because values come
from the context at call time, the registrations stay shared across sessions
(G2) and the schema stays static.

The context gains one field:

```haskell
data ToolExecutionContext = ToolExecutionContext
    { …
    , ctxParams :: Params   -- Map ParamName ParamValue
    }

data ParamValue = ParamValue { pvValue :: Aeson.Value, pvSecret :: Bool }
```

`Show` and `ToJSON` of `ParamValue` print `"<secret>"` when `pvSecret`.
`Session/Step.buildContext` fills `ctxParams` from the agent runtime
(process values and defaults) overlaid with the session's values (§5).

The session itself is unchanged: it records the call as the LLM made it, so
bound values never enter the conversation history, never go back to the
model, and do not show in `session-print`.

#### 3.1 Bash tools

* **New calling mode `env`.** `ScriptArgCallingMode` gains `Env`. An argument
  `{"name": "token", "mode": "env"}` is passed as the environment variable
  `TOKEN` (upper-cased, `-` to `_`), not in argv. `flattenArguments` and
  `flattenInput` ignore it; a new `flattenEnv` feeds `buildToolEnvironment`.
  Partial application stays a single concept (everything is an argument);
  the mode only picks the transport. This closes G6 and is useful without
  bindings too.
* **Arity (G5).** `mapArg` sets `propertyRequired = (argTypeArity == Single)`.
* **Strings only.** A value bound to a bash argument must be a JSON string;
  numbers and booleans are rendered with their JSON text; objects and arrays
  are a load-time error for `Literal`, a call-time error for `Param`.
* **Tracing (G7).** `runValue` gets the set of secret argument names from the
  wrapper and replaces their values with `<secret>` in `RunCommandStart` /
  `RunCommandStopped`. With the load-time check of §2, secrets are env-only
  anyway; the redaction is the second line of defence.
* `documentation/binary-tool.md` and `agents-exe spec bash-tools` document the `env`
  mode.

#### 3.2 MCP toolboxes

* Argument bindings work through the generic combinator; `adaptSchema`
  (`ToolRegistration.hs:2600`) output is filtered like any other.
* Separately, close G4: `McpSimpleBinaryConfiguration` gains
  `env :: Maybe (Map Text BindingValue)`. The server process is started once
  per tree, so only `Literal` and **process-scope** `Param`s are accepted
  here; a session-scope parameter is a load-time error with a message that
  says why.

#### 3.3 OpenAPI and PostgREST toolboxes

* Argument bindings (path, query and body parameters) work through the
  generic combinator.
* Headers and tokens are not tool arguments, so they go through the existing
  secrets. `SecretSource` gains `ParamSource ParamName`. Secrets with that
  source are skipped by the load-time `resolveSecrets` and resolved per
  request from `ctxParams`, then serialized by the unchanged
  `applySecretsToHeaders` / `applySecretsToQueryString`. This closes G3.
* Fetching the spec at load time cannot use a session-scope secret. If the
  spec endpoint needs auth, use a process-scope parameter or a plain secret
  for it; the loader says so when it fails.

#### 3.4 Builtin toolboxes and sub-agents

Builtins need nothing special. For sub-agents see §7.

### 4. Supplying values: process level

Shared by `agents-exe` (all subcommands that load a tree) and
`agents-server`:

```
--set NAME=VALUE          string value; repeatable
--set-json NAME=JSON      any JSON value
--params-file FILE        {"tenant": "acme", "api_token": "…"}
--pin NAME=VALUE          like --set, and clients cannot override it
--pin-json NAME=JSON
```

* They feed a new `processParams :: Map ParamName ProcessValue` in
  `AgentTree.Props` (`AgentTree.hs:384`), where a `ProcessValue` is a value
  plus a `pinned` flag. `--params-file` entries may be written
  `{"value": …, "pinned": true}` to the same effect.
* `--set` on a session- or message-scope parameter gives a *fallback* that a
  client may override. `--pin` gives a value that a client may not: for the
  lifetime of the process the parameter behaves as if it were declared
  `scope: "process"`. This is how an operator takes a multi-tenant agent
  file and locks one container to one tenant without editing the file. On a
  process-scope parameter `--pin` and `--set` mean the same.
* A name that no loaded agent declares is an error (typo guard).
* For secrets, prefer a `default` with `EnvVar` / `FileSystem`, or
  `--params-file`: `--set` shows in `ps`. The docs say so.
* `agents-exe check` prints each agent's parameters and how each one
  resolved (`set`, `default:EnvVar`, `unbound`), never the values of secret
  ones.
* `agents-exe describe` and `describe-tool` show the *reduced* schema, with
  the bound arguments listed apart (this output is for the operator, not for
  the model).
* `agents-exe tool-call` and `replay-tool-call` go through the wrapped
  registration, so they apply bindings too.

In the docker case this is all that is needed: the image holds the agent
file, the operator runs it with `-e BILLING_TOKEN=… ` and `--pin tenant=acme`.

### 5. Supplying values: session level (`agents-server`)

**Create.** `POST /v1/sessions` accepts `params`:

```json
{ "agent": "billing", "prompt": "…",
  "params": { "tenant": "acme", "api_token": "s3cr3t" } }
```

**Follow-ups.** `POST /v1/sessions/:id/messages`, `/resume` and
`POST /v1/continuations/:token` accept the same field. It is merged over the
session's current values; `null` removes a value.

**Validation**, before the run starts:

| Case | Answer |
|---|---|
| name not declared by the agent | `422`, lists the unknown names |
| `scope: "process"` parameter, or one pinned with `--pin` | `403` |
| `scope: "message"` parameter on a request that starts no run | `422` |
| `secret` with a non-string value | `422` |
| a required session or message parameter still unbound after merging | `422`, lists the missing names |

**Storage.**

* Non-secret values: a new `params` JSON column on the sessions table (both
  SQLite and Postgres backends; `SessionMeta.smParams`), written through the
  same versioned store as the rest of the metadata. They survive restarts and
  live-session eviction.
* Secret values: only in the `LiveSession` (`Host/Runner.hs:178`), in memory.
  After a restart or after `--live-session-ttl` evicts the session, the next
  message must carry them again, or gets the `422` above. This is the
  "transient" in the goal, and it keeps tokens out of the database with no
  encryption story to design.
* Message-scope values, secret or not: only in the run's state inside the
  `LiveSession`, cleared when the run ends (blocked, completed, failed or
  cancelled). A required message-scope parameter must therefore come with
  *every* request that starts a run, including `/resume` and
  `POST /v1/continuations/:token` when it resumes. They are never in
  snapshots either.
* `ToolExecutionContextSnapshot` gains `tecsParams`, holding non-secret
  values only. A deferred or async call that is re-hydrated in another
  process sees non-secret parameters; if its tool needs a secret one, the
  re-hydrated call fails with `parameter api_token is not bound`.
  `recoverOnStartup` (`Runner.hs:700`) therefore marks such running calls as
  failed rather than retrying blind.

**Self-description.** `agentView` adds

```json
"parameters": [
  { "name": "tenant", "description": "…", "scope": "session",
    "secret": false, "required": true, "bound": false },
  { "name": "region", "scope": "process", "bound": true }
]
```

`bound` tells a client whether the process already supplies the value;
`"pinned": true` says that it cannot be overridden (and the chat page then
hides the field). No values are ever returned. `GET /v1/sessions/:id` returns the non-secret
session values and the *names* of the secret ones currently held.
`Routes.hs` and the generated OpenAPI document follow. The chat page gets a
small plain form for session parameters, shown when the chosen agent has
unbound ones (password inputs for secret ones).

**Stored agents.** `parameters` is a normal field for database agents
(`Host.putStoredAgent`). `Bindings` lives inside toolbox descriptions, so it
follows whatever `fileBasedFields` already allows.

**MCP over HTTP** (`POST /mcp`, the `ask_<slug>` tools). MCP clients are
usually configured with static headers, which is exactly the pin-down case:

* `Agents-Param-<name>: <value>` request headers, string values;
* `_meta: {"agents-exe/params": {…}}` on `tools/call`, any JSON, wins over
  headers.

Same validation as above, reported as a JSON-RPC error. Each `tools/call` is
one run, so session and message scope coincide here.

#### 5.1 Sealed sessions and delegated chat (S2)

In S2 two different principals talk to the same session: the backend, which
is trusted to choose `project_id`, and the end user's browser, which is not.
Today the server knows one kind of caller, the bearer-token owner
(`AgentsServer/Auth.hs`), and the backend cannot give its token to a
browser. Without more, the backend has to proxy every chat message and SSE
stream. Three additions remove that need:

* **`seal: true`** on `POST /v1/sessions`. After creation, `params` on
  `/messages`, `/resume` and continuations is refused with `403`, for every
  caller but the owner. The parameters of a sealed session are those its
  creator chose.
* **Session tokens.** `POST /v1/sessions` with `"session_token": true`
  returns a random token bound to that session. `Authorization: Bearer
  <session token>` allows exactly: post a message, read the session and its
  events, cancel a run, on that one session. It cannot set or read `params`,
  list sessions, complete continuations, or touch agents. Tokens are stored
  hashed, next to the session, and die with it; the owner can revoke one with
  `DELETE /v1/sessions/:id/token`.
* **`PUT /v1/sessions/:id/params`**, owner only, sets session-scope values
  without starting a run. The backend uses it to rotate a minted credential
  before it expires, and to re-supply secrets after a restart or an eviction
  (secrets are memory-only, §5). When a message arrives on a session whose
  required parameters are no longer all bound, the answer is
  `409 params_required` with the missing names; a browser holding a session
  token relays that to its backend, which calls this endpoint.

#### 5.2 Forking a session with new values

A credential that expires in the middle of a long run is not handled: the
tool calls fail, the model reports the failure, the run ends. No blocking,
no refresh protocol (D14). The way forward is a new session that starts from
the old one's history with a fresh value pinned:

```json
POST /v1/sessions
{ "fork": "<session id>",
  "params": { "callback_token": "…fresh…" },
  "seal": true, "session_token": true,
  "prompt": "The credential was renewed, please carry on." }
```

* The new session gets a copy of the turns of the source, a new session id,
  and `forkedFromSessionId` set (the field exists already,
  `Session/Types.hs:1385`). `agent` is taken from the source; `prompt` is
  optional, and without it the session is created idle.
* Parameters are **not** shared with the source at run time. The new
  session starts from the source's persisted (non-secret) values, overlaid
  with the request's `params`, and is validated like any creation; secrets
  always have to be given again. `seal` and session tokens are not inherited
  either.
* This is sound because bound values never enter the history (§3): the
  copied turns hold no trace of the old token or of any bound argument, so
  nothing in them contradicts the new values. Changing `project_id` in a
  fork is therefore *possible* and is the owner's call; it is refused to
  session-token holders, who cannot fork at all.
* Pending deferred calls and running async calls of the source are not
  copied. The fork's last turn is closed the way a cancelled run closes it,
  so the history stays well-formed for the LLM.
* Narrowed helper calls (§8) need nothing: their bindings are arguments of
  `prompt_agent_*` calls in the copied history, and a `Param` in a future
  call resolves against the fork's values, which is the intent.
* Only the owner of the source (or an admin owner) may fork it.
* On the CLI the same thing is `agents-exe session start --fork <id>` with
  the usual `--set` / `--pin` flags.

The callback tools themselves need nothing new. With an OpenAPI toolbox
pointing at the product API:

```json
{ "tag": "OpenAPIServer",
  "contents": {
    "SpecUrl": "https://api.example.com/agent-callbacks/openapi.json",
    "BaseUrl": "https://api.example.com/agent-callbacks",
    "Secrets": [
      { "source": { "tag": "ParamSource", "contents": "callback_token" },
        "decoder": { "tag": "Clear", "contents": true },
        "serializer": { "tag": "Header",
                        "contents": ["Authorization", "Bearer {{secret}}"] } } ],
    "Bindings": [
      { "arg": "project_id",
        "value": { "tag": "Param", "contents": "project_id" } } ] } }
```

Every operation that has a `project_id` path or query parameter loses it;
the model sees `list_tasks {status}` instead of `list_tasks {project_id,
status}`. The same works with bash tools that `curl` the API, with `token`
in `env` mode.

One thing stays with the product API: its error bodies go back to the model
verbatim. An API that answers "project 43 belongs to another account" leaks
through any amount of binding.

### 6. Secrecy rules

One list, because they are scattered over the sections above:

1. A secret value is never written: not in the session JSON, not in the
   metadata, not in continuation snapshots, not in the tool cache.
2. A secret value is never traced. `ParamValue` redacts itself; bash argv is
   redacted (§3.1); the HTTP toolboxes already keep secret headers out of
   `--log-http` and that path is reused.
3. A secret value is never returned by the API nor shown by `check` /
   `describe`.
4. A secret value never reaches bash argv (§2 load-time check).
5. None of this protects the value from the tool that receives it. A tool
   that echoes its token sends it to the LLM. That is the tool author's
   responsibility and the docs say so.

**Tool cache (G8), done.** Implemented as `CacheScope`/`cacheScopeOf` in
`Tools/Cache.hs`: a call's key also depends on the digest of the agent's
non-secret parameter values and inherited narrowing bindings (the context's
`ctxParams`/`ctxInheritedBindings`, at every cache call site), and a call
with any secret one is not cached. Deviation: this looks at *all* the agent's
parameters, not only those a tool binds (the registrations' bindings are closed
over in the tools and invisible where the cache is consulted), so it can only
cost hits, never share a result across bound values. Argument hashes are now
SHA-256 (the old length + 60-character prefix could collide). As designed:
`computeCacheKey` takes the bound values of the call:
non-secret values are hashed into `ckArgumentsHash` together with the LLM
arguments. A call with any secret binding is **not cached at all**: hashing a
token into a persisted key is a needless oracle, and results obtained with
one credential should not be replayed for another.

### 7. Sub-agents

Lexical, not dynamic: a child sees only what the parent passes explicitly.
`ExtraAgentRef` (`Base.hs:93`) gains `with`:

```json
"extraAgents": [
  { "slug": "invoice-expert", "path": "invoice-expert.json",
    "with": { "tenant": { "tag": "Param", "contents": "tenant" },
              "mode":   { "tag": "Literal", "contents": "read-only" } } }
]
```

* Keys are the *child's* parameter names; values are `BindingValue`s in the
  *parent's* scope.
* `runSubAgent` (`OneShotTool.hs:191`) evaluates `with` against the caller's
  `ctxParams`, at call time, so a message-scope value of the parent is passed
  like any other, and hands the result to the child as its session-level values;
  the child's own process values and defaults still apply underneath.
  Secrecy is inherited: a value that is secret in the parent is secret in
  the child, whatever the child declares.
* Children discovered through a tool directory have no reference site, so
  they get process values and defaults only. List them in `extraAgents` to
  pass something.
* The check phase verifies that `with` keys exist in the child and that the
  child's required session parameters are covered.

### 8. Narrowing helpers, down the call chain

The scenario (S3): a parent splits a job and prompts a helper whose tools are
already pinned to one part of it, so that the helper's model has nothing left
to choose. This is partial application done by a model instead of an
operator, and it composes: the operator binds `tenant`, the root binds
`repo`, helper A binds `branch`, helper B only picks `path`.

This concerns helper agents exposed as tools, and only them. Their interface
changes in two ways: (a) a parent can discover the open arguments below a
helper, (b) a parent can bind some of them when it prompts the helper.

#### 8.1 Agent-level bindings

Toolbox-level bindings (§2) need a toolbox to hang on, and bash toolboxes
have no name. So `Agent` also gets a top-level `bindings` list, where `tool`
globs match the LLM-visible names (`bash_query-*`, `mcp_billing_*`). They are
applied after the toolbox-level ones. §8.3 uses the same shape at run time.

Successive application is monotone by construction: a later binding can only
name an argument that is *still open*. There is no "unbind" and no override.

#### 8.2 (a) Discovery: `describe_agent`

One new tool next to the `prompt_agent_<slug>` family, registered by
`wireAgentTools` for any agent that has helpers:

```json
describe_agent { "slug": "repo-worker" }
```

```json
{ "slug": "repo-worker",
  "announce": "works on one git repository",
  "parameters": [ { "name": "gh_token", "description": "…", "bound": false } ],
  "tools": [
    { "name": "bash_git-log", "description": "…",
      "open": [ { "arg": "repo", "type": "string", "description": "…" },
                { "arg": "since", "type": "string", "description": "…" } ] } ],
  "helpers": [
    { "slug": "diff-reviewer",
      "tools": [ { "name": "bash_git-diff",
                   "open": [ { "arg": "repo", … }, { "arg": "rev", … } ] } ],
      "helpers": [] } ] }
```

* It shows **open** arguments only, as seen from the caller's position in
  the chain: what the agent file bound, what the operator bound, and what any
  ancestor of the caller bound for this call are all absent. A parent
  discovers exactly what is left for it to decide, in keeping with D8.
* It is recursive over `helpers`, cut at the configured max depth and at
  cycles (`isAgentInCallStack`, `Tools/Context.hs:767`).
* Parameters are listed by name and description, never by value.
* It is a read of the loaded tree, not an LLM call. The answer is the same
  for the whole run, so it caches well.
* An author can opt a helper out with `"narrowable": false` on its
  `extraAgents` entry: `describe_agent` then shows only its announce, and
  bindings that target it or anything below it are refused.

#### 8.3 (b) Binding: `prompt_agent_<slug>` takes `bindings`

```json
prompt_agent_repo-worker
{ "what": "Summarise last week's changes to the session runner.",
  "bindings": [
    { "tool": "bash_git-*", "arg": "repo",
      "value": { "tag": "Literal", "contents": "lucasdicioccio/agents-exe" } },
    { "agent": "diff-reviewer", "tool": "bash_git-diff", "arg": "repo",
      "value": { "tag": "Literal", "contents": "lucasdicioccio/agents-exe" } },
    { "agent": "**", "tool": "bash_gh-*", "arg": "token",
      "value": { "tag": "Param", "contents": "gh_token" } } ],
  "with": { "tenant": { "tag": "Param", "contents": "tenant" } } }
```

`bindings` and `with` are optional; the schema of the tool is static (D3)
and a parent that ignores them sees today's behaviour.

* **Addressing.** `agent` is a path of slugs *below the prompted helper*:
  absent means the helper itself, `diff-reviewer` its helper of that name,
  `a/b` a grandchild, `**` the helper and everything below. `tool` and `arg`
  are as in §2.
* **Values are resolved at the call site.** `Literal` is what the parent's
  model wrote. `Param` names a parameter of the *calling* agent and is
  resolved against its `ctxParams` when the call is made (lexical, as in §7);
  secrecy travels with the value. The parent's model routes a token it has
  never seen. `with` fills the helper's parameters as in §7, and cannot
  refill one that the `extraAgents` entry already fills.
* **How they travel.** `ToolExecutionContext` gains
  `ctxInheritedBindings :: [ScopedBinding]`, a resolved binding plus the
  remaining agent path. `runSubAgent` (`OneShotTool.hs:191`):
  1. takes the caller's inherited bindings, re-rooted at the helper, and adds
     the ones from this call;
  2. applies those addressed to the helper itself, wrapping the helper's
     registrations with `applyBindings` for this call only. The loaded
     toolboxes stay shared: no script is re-described, no MCP server
     restarted;
  3. puts the rest in the helper's context, from where step 1 picks them up
     when the helper prompts *its* helpers.
  The non-secret part goes in `ToolExecutionContextSnapshot`, like
  parameters.
* **The chain composes, and only narrows.** Every agent on the way may add
  bindings for what is below it. It can only name arguments that are still
  open from its position, which is all `describe_agent` shows it. If a
  model names an argument that an ancestor already bound (it cannot know
  about it, so this is a hallucination), the ancestor's value stays, the new
  binding is dropped, and a trace is emitted; the call goes on (D8).
* **Errors the parent's model can fix are told to it**: unknown agent path,
  no tool matching the glob, no such open argument, wrong value type, a
  `narrowable: false` target. The call fails before the helper runs, with
  a message that names the open arguments, so one retry suffices.
* **Literals bound to `env`-mode arguments are allowed.** Nothing makes a
  model-chosen literal secret.
* **Knowing parameter names is not knowing arguments.** D8 is about the
  surface of the tools an agent *calls*. An agent that narrows other agents
  works one level up and sees configuration: names and descriptions, never
  values. An operator who does not want even that pre-binds the argument in
  the helper's file: bound arguments are not shown.
* **Recovery.** The bindings of a call are in the parent's history, as the
  arguments of its `prompt_agent_*` call, so a resumed or forked (§5.2)
  parent re-issues them as they were; `Param` values resolve against the
  current values, which is the intent after a fork.
* **Tool cache.** Inherited non-secret bindings enter the helper's cache
  keys as in §6; any secret one disables caching for the calls it touches.

#### 8.4 Naming a narrowing (optional convenience)

Repeating the same `bindings` on ten prompts costs tokens and invites
inconsistency. `derive_agent {from, slug, bindings, with}` stores a
narrowing under a slug for the session, and `prompt_agent_<from>` accepts
`"as": "<slug>"` to use it. It is sugar over §8.3 and adds no power: the
stored thing is the same data, the table is a fold over the session's
successful `derive_agent` calls (in the manner of `ToolboxSessionState`,
`Tools/Activation.hs:75`), so nothing new is stored. To be built only if
§8.3 proves verbose in practice.

---

## Phases

Each phase is shippable and leaves `agents-exe check` and the test suite
green.

### Phase 1: the combinator, literals, bash fixes

* `System.Agents.Tools.Bindings`: `Binding`, `BindingValue`, `applyBindings`,
  schema reduction, argument merge.
* `Bindings` field on all toolbox descriptions; `Literal` only.
* Bash: arity in `mapArg` (G5), `env` calling mode (G6).
* Load-time checks of §2 that do not involve parameters.
* Tests: schema reduction; bound wins over an LLM-sent key; glob matching;
  `env` mode end to end with a fixture script.

### Phase 2: parameters at process level

* `ParameterDecl`, `parameters` on `Agent`, `Param` binding values.
* `ctxParams`, `ParamValue` with redaction, `buildContext` wiring.
* `--set`, `--set-json`, `--params-file`, `--pin`, `--pin-json`;
  `Props.processParams`; defaults
  through `resolveSecretSource`.
* `check`, `describe`, `describe-tool`, `tool-call` output.
* Bash argv redaction (G7) and the secret-in-argv check.
* Cache key (G8).
* MCP `env` (G4).

After this phase the docker use case works with one tenant per container.

### Phase 3: HTTP toolboxes

* `ParamSource` in `SecretSource`, per-request resolution (G3).
* Same for PostgREST.

### Phase 4: session-level parameters in `agents-server`

* `params` on create / messages / resume / continuations, validation table.
* `params` column and migration (SQLite and Postgres), `smParams`.
* Secrets in `LiveSession` only; `tecsParams`; recovery behaviour.
* Message scope: per-run values, cleared at run end.
* `seal`, session tokens, `PUT /v1/sessions/:id/params`,
  `409 params_required` (§5.1).
* `fork` on session creation and `session start --fork` (§5.2).
* Pinned parameters refused with `403`; `pinned` in `agentView`.
* `agentView`, session view, OpenAPI document, `documentation/agents-server.md`.
* Chat page form.

After this phase one container serves many tenants.

### Phase 5: sub-agents and MCP over HTTP

* `with` on `ExtraAgentRef`, evaluation in `runSubAgent`, checks (G9).
* `Agents-Param-*` headers and `_meta` on `/mcp`.

### Phase 6: narrowing helpers

* Agent-level `bindings` (§8.1), with the still-open check.
* `describe_agent` (§8.2), recursive, position-aware; `narrowable`.
* `bindings` and `with` on `prompt_agent_<slug>`; `ctxInheritedBindings`;
  per-call wrapping in `runSubAgent`; snapshot; cache keys (§8.3).
* Tests on a three-level chain: root binds for the grandchild; the middle
  agent narrows further; the middle agent cannot rebind; `describe_agent`
  from the middle does not show what the root bound; resume and fork.
* Later, if needed: `derive_agent` (§8.4).

Depends on Phases 1, 2 and 5 (`with`).

### Phase 7 (optional): `Expose`

`whenUnbound: "expose"` puts the argument back in the schema when the
parameter is unbound for this session. It makes the tool list a function of
the session, which the progressive-disclosure machinery
(`Combinators/ProgressiveDisclosure.hs`) already does for activation, so the
hook exists. Deferred until a real use shows up.

---

## Decisions

**D1. Callers fill parameters, not tool arguments.** The author decides what
is bindable. Otherwise any client of `agents-server` could pin any argument
of any tool, and every toolbox would need its own allow-list.

**D2. Values travel in the call context, not in the registration.** Toolboxes
load once per tree and are shared (G2). Rebuilding a toolbox per session
would mean re-running `describe` on every script and restarting MCP servers.

**D3. The schema is static.** A bound argument is hidden whether or not a
value is present; a missing value is an error, not a fallback to the LLM.
It keeps the tool list identical across sessions (prompt caching, simpler
reasoning about what the model can do). `Expose` is the opt-in exception,
postponed.

**D4. `env` is a calling mode, not a second kind of binding.** One concept
(arguments) with a transport choice, rather than "argument bindings" plus
"environment bindings" with different rules. The exception is the MCP server
process environment, which is not a tool argument at all.

**D5. Secrets are never persisted.** The cost is that clients re-send them
after eviction or restart. The alternative needs key management, and an
`agents-server` database that holds customer tokens is a much more sensitive
asset than one that does not.

**D6. Sub-agent passing is explicit.** Dynamic scoping ("the child inherits
every parameter with the same name") is shorter to write and leaks a token
to any child that happens to declare the same name.

**D7. No templating.** `${tenant}` in a system prompt or a path is tempting
and is a different feature: it changes the agent definition per session and
puts values in front of the model. Parameters here are deliberately
invisible to the LLM.

**D8. The model does not know.** A partially applied argument leaves no
mark in what the model sees: not in the schema, not as a "pre-set" note in
the description, not in error messages. The purpose of the feature is to
shrink the local reasoning a call requires; telling the model about a key it
cannot use, or a tenant it cannot choose, adds reasoning back and invites it
to try. Consequently, if the LLM sends a bound key anyway, the call proceeds
with the bound value and only a trace is emitted; and the error for an
unbound parameter at call time goes to the operator's trace, while the model
gets a generic "tool unavailable" failure.

**D9. Pinning is an operator flag, not an agent-file field.** The agent file
says what *may* vary per session; the deployment says what *does*. The same
file then serves both the shared server and the one-tenant container.

**D10. Message scope means one run.** Not one HTTP request, not one LLM
turn: a run is the unit after which control goes back to the client, which
is the only moment a client can supply a fresh token.

**D11. Narrowing composes along the chain, and only narrows.** Each agent
may bind what is still open below it; none may rebind. Whatever a narrowed
helper can do, the un-narrowed one could, so giving this power to a model
cannot widen anything, which is why it is on by default (`narrowable: false`
opts out).

**D12. Bindings ride on the call, not on a new kind of agent.** The helper
interface gains discovery and a `bindings` argument, rather than a registry
of derived agents. A call is self-describing, lives in the parent's history,
and needs no lifetime rules. Naming a narrowing (§8.4) is sugar.

**D13. Session tokens belong to this spec.** They look like a separate
authentication feature, but without them S2 either proxies everything
through the backend or hands the browser a token that can re-parameterise
the session, which undoes the point of binding.

**D14. An expired credential is a failed run, and the remedy is a fork.**
No mid-run refresh, no "blocked on credential" state. `PUT …/params` covers
rotation *between* runs, and a fork with a fresh value covers a run that
died. Forking is cheap to specify precisely because bound values are absent
from the history.

**D15. No saved narrowings for now.** It is kept possible, not planned: a
narrowing is plain serializable data (helper slug, scoped `bindings`,
`with`) and has the shape of an agent-level `bindings` list (§8.1), so a
later "save this helper" writes that data to the agent store and resolves
the reference at load. Nothing here should tie a narrowing to the in-memory
identity of a session. It stays secondary to a good experience for the user
and for the model.

## Nice to have, later

* Saving a narrowed helper as a stored agent (D15).
* `derive_agent`, naming a narrowing within a session (§8.4).
* `whenUnbound: "expose"` (Phase 7).

## Related docs

* `documentation/binary-tool.md`: bash tool protocol, to be extended with `env` mode.
* `documentation/tools.md`, `documentation/advanced-configuration.md`: toolbox configuration.
* `documentation/agents-server.md`: HTTP API.
* `todos/web-server-embedding.md`: the host, the runner, session metadata.
* `todo`: "partial-application of tool values".
