# Spec: optional buckette integration (a fleet of agents under one budget)

Status: proposal, 2026-11-23, asked for by the owner in the project chat;
revised 2026-11-24 with the owner's answers (see "Decisions (2026-11-24)").
Nothing here is implemented. The whole mechanism is **optional and off by
default**: an agent description without a `budget` field, run by a process
without buckette configuration, behaves exactly as today (§8).

## Goal

Run a **fleet of agents-exe agents** (one-shot runs from cron, TUI sessions,
`agents-exe-server` sessions, their helper agents and their tools) under
buckette budgets, using the three mechanisms buckette describes for
token-maxxers and for workflow systems:

1. **Reporting.** Every agent tells buckette what it used (turns, tokens,
   money, tool resources), with enough metadata to break the usage down by
   agent, session, model or tool. Nothing is refused.
2. **Reservation.** A run holds what it expects to use *before* it starts,
   keeps and grows the hold while it runs, and settles what it really used
   when it ends. A run that does not fit is told before it starts, not
   halfway through.
3. **Protected classes.** Each agent spends as a class (`interactive`,
   `batch`...). The bucket keeps a floor under the share that lower classes
   must not take, so a batch fleet cannot use up what chat needs.

The three things the owner listed are framed in those terms:

| Asked for | Here |
|---|---|
| (a) monitor or gate turns and tokens | reporting per completion (§3); the run reservation, its grows and its refusals (§4); under a class (§5) |
| (b) tools report, reserve, consume resources, declared in the tool description | the same reporting and reservation, per call, declared in `describe`, in an MCP tool's `_meta` (`buckette/resources`), or by the agent (§6) |
| (c) sub-agents with a delegated budget | a share carved out of the caller's hold; a reservation of its own for a child of a lower class; or a real child bucket for a child that runs elsewhere (§7) |

Adoption follows buckette's own three steps for workflows: **monitored**
(`mode: "report"`, every dimension in reporting mode), **controlled**
(`mode: "reserve"`, dimensions switched to locking, classes declared),
**scheduled** (refills and floors that release over the period: configured
in buckette, agents-exe only has to wait well, §4.4). Each step keeps what
the previous one gave, and an agent description changes by one field between
the first two.

Non-goals: agents-exe never creates credit, never changes a dimension's mode
or a class declaration during a run, never receives webhooks (no public
callback URL: no `Async` mode), and does not use claims, auctions, transfers
or receipts.

## References

* **The tenant guide** is the reference for every integration point in this
  document. Each tenant serves it at `/docs/llm.md` on its own host (the one
  read for this spec: `https://euw1-tpm52cdn-tomme.api.buckette.co/docs/llm.md`),
  with `/openapi.json` and `/help/<topic>` (`reservations`, `classes`,
  `delegation`) beside it. Field names below were checked against that
  `/openapi.json` on 2026-11-23.
* **The console guide**, `https://console-api.buckette.co/docs/llm.md`, is
  used for one thing only: setting the integration up and checking it
  (§2.4). agents-exe never calls the console API during a run.
* buckette's pages for the two audiences this spec follows:
  `https://www.buckette.co/for-token-maxxers.html` (the pacer: hold, grow,
  settle; a floor under the share you must not lose) and
  `https://www.buckette.co/for-workflows.html` (monitored, controlled,
  scheduled), and the post
  `https://www.buckette.co/blog-monitoring-to-strategy.html`.
* In this repository: `System.Agents.Base` (`Agent`, `ToolCallPolicyConfig`,
  `WrapperMatch`), `System.Agents.Session.Step` (the conversation loop),
  `System.Agents.LLMs.OpenAI` (`TokenUsage`), `System.Agents.Session.Types`
  (`Decorator`, `ToolCallDisposition`, `StepByteUsage`, `SessionStatus`),
  `System.Agents.Tools.ScriptTypes`/`ScriptRuntime` (bash tools v2,
  `todos/bash-tools-v2.md`, `documentation/binary-tool.md`),
  `System.Agents.Tools.Secrets` (`SecretSource`), `System.Agents.AgentTree`
  (helper agents as tools), `documentation/agents-server.md`.

## 1. The buckette surface used

* A **bucket** has named **dimensions** (`turns`, `tokens`, `usd`,
  `browser_slots`...). A dimension is `Reporting` (never refuses, may go
  negative), `Locking` (a use that does not fit is refused) or
  `MeteredOverage`. **The mode belongs to the dimension, not to the
  request:** going from monitored to controlled is a change in buckette.
* **Report:** `POST /buckets/:id/consume` `{"crAmounts", "crMode",
  "crClass", "crIdempotencyKey", "crMetadata"}` answers `ConsumeSuccess`
  (`coConsumed`, `coRemaining`) or `ConsumeShortfall` (`coShortfall`,
  `coAvailable`, `coEta`, `coProtection`). `crMode` `Partial` never fails;
  `Strict` is all or nothing. `POST /buckets/:id/check` asks without
  spending.
* **Reserve:** `POST /buckets/:id/reservations` `{"rrvAmounts", "rrvClass",
  "rrvExpiresAt", "rrvTransactionId", "rrvMetadata"}` answers
  `ReserveSuccess` (`rsReservation.rvId`, `rvExpiresAt`) or
  `ReserveShortfall` (`roShortfall`, `roAvailable`, `roProtection`). Then
  `.../reservations/:rid/grow` `{"grqAmounts", "grqTransactionId"}` (checked
  like a new reservation, all or nothing), `.../refresh` `{"rfrExpiresAt"}`,
  `.../commit` `{"crqAmounts", "crqMetadata"}` (spends at most what is held,
  frees the rest; zero gives a dimension back whole) and `.../release`. A
  hold with an expiry frees itself when its holder dies; a bucket's
  reservation policy can cap that lifetime.
* **Classes:** a bucket declares classes with a floor per dimension
  (`PUT /buckets/:id/class-config`, an admin's job). A consume, check or
  reserve of a class succeeds only if what it leaves available stays at or
  above the floor; the floor can release (`linear`, `step`) as the next
  refill approaches. The outcome carries `coProtection`/`roProtection`
  `{"cpClass", "cpFloor", "cpProtected"}`: `cpProtected: true` means the
  balance would have served the request and the floor held it back. A key
  can be limited to classes (`cakreClasses`), which is what makes a class
  binding. Errors: `400 unknown_class`, `400 class_required`,
  `403 class_not_allowed`. **Classes are not inherited by a split child, a
  split applies no floor, and a class-limited key cannot split**
  (`403 class_limited_key`).
* **Delegation:** `POST /buckets/:id/split` `{"srAllocation",
  "srIdempotencyKey", "srMetadata", "srExpiresAt"}` carves a child bucket
  (`srChildId`); `POST /buckets/:child/reclaim` `{"rrMode", "rrCascade"}`
  returns it; `POST /api-keys` with `cakreBucketId` mints a key confined to
  the child, deactivated by the reclaim.
* **Reading back:** `GET /buckets/pacing`, `GET
  /buckets/:id/consume-breakdown?key=<metadata key>`, `POST
  /buckets/:id/eta`, the per-period ledger, the event log.
* Errors are `{"errCode", "errMessage", "errDetails"}`; some are about the
  tenant, not the call (`403 tenant_on_hold`, `410 tenant_moved`,
  `429 rate_limited`), and `wrong_key_kind` says a console key (`cpk_...`)
  was sent to a tenant, or a tenant key (`bk_...`) to the console.

## 2. The fleet, and its configuration

### 2.1 One allowance, many agents

The model this spec is built for:

```
tenant
└── fleet bucket  "agents: weekly allowance"       dimensions: turns, tokens, usd
      classes: interactive {}                      (no floor: may spend everything)
               batch       {tokens: floor 400000, release linear}
               backfill    {tokens: floor 800000, release step}
      refilled weekly (buckette's schedule)
      ├── consumed by  tui sessions          class interactive
      ├── consumed by  agents-exe-server     class interactive
      ├── consumed by  nightly one-shots     class batch
      └── consumed by  re-indexing agent     class backfill
```

Every agent of the fleet spends from the **same bucket**, each as its class,
each run under its own reservation. That is what lets the floors work: they
compare one class's request with what the whole allowance has left. A fleet
may of course have several such buckets (one per team, per provider
allowance, per customer); an agent names the one it spends from.

### 2.2 Process level: connection and credentials

In `agents-exe.cfg.json`, or as flags that win over it:

```json
{ "agentsDirectories": ["./agents/"],
  "buckette": {
    "url": "https://euw1-tpm52cdn-tomme.api.buckette.co",
    "key": {"tag": "ApiKeySource", "contents": "buckette"},
    "bucket": "5f0c...",
    "class": "batch",
    "agentClasses": {"re-index": "backfill", "web-search": "batch"},
    "labels": {"fleet": "nightly", "host": "worker-3"},
    "onUnreachable": "closed",
    "timeoutMs": 2000,
    "retries": 1
  }
}
```

Flags: `--buckette-url URL`, `--buckette-key ID` (an id in the `--api-keys`
file), `--budget-bucket BUCKET`, `--budget-class CLASS`,
`--budget-on-unreachable open|closed`, `--budget-dry-run` (§8.3),
`--no-budget` (§8.1), `--agent-class SLUG=CLASS` (repeatable, as
`agentClasses`). They are global to the process, like `--envdir-root`,
and shared by `agents-exe-server`, which adds `--owner-budget
OWNER=BUCKET[:CLASS]` (repeatable, needs `--auth-tokens`) in the spirit of
`--owner-api-keys`.

**The class is process configuration only** (decision Q12). It is a
deployment fact, not agent behaviour: the same agent file is `interactive`
in a TUI and `batch` under cron. Agent files carry no class, and a `class`
key in a `budget` field is a load error that points here. The
**class-limited key is the single source of truth**: it says which classes
this machine may spend as, and buckette enforces it (`403
class_not_allowed`). The settings above only choose among the classes the
key allows:

* A key limited to exactly one class needs nothing else: `class` may be
  left out, no class is sent, and buckette applies the key's class. If
  `class` is set it must be that class.
* A key that allows several classes (a server with sessions of several
  grades, or children of a lower class, §7.2) needs `class` (the default
  for every run) and `agentClasses` (a class per agent slug, for the agents
  that differ). Each named class must be one the key allows. A slug has
  one class per process, wherever that agent runs: a helper that should
  spend as its caller in one place and as `batch` in another is given a
  second slug (decision Q16).
* A key with no class limit (one trust domain) works the same way, but
  nothing outside the process checks the choice (§5).

`check --probe` compares every configured class with the key's
`akiClasses` before any run (§2.4); at run time a mismatch is a
configuration error, never a shortfall.

**Credentials.** `key` is a `SecretSource`: `ApiKeySource` (an entry of the
`--api-keys` file, the default way), `FileSystem`, `EnvVar` or `Command`.
**`Given` (an inline value) is refused at load**, and no flag takes the key
itself. An agent description never contains or names a key. The key is a
tenant key (`bk_...`) with `ScopeConsume` and `ScopeRead`; nothing at run
time needs `ScopeAdmin` or `ScopeRefill`. The recommended fleet key is
confined to the fleet bucket (`cakreBucketId`) and, for a batch fleet,
**limited to its class** (`cakreClasses: ["batch"]`): the machine then
cannot present itself as `interactive` whatever its agent files say. The key
never appears in logs, traces, session files, tool environments or tool
results; the `Authorization` header is redacted by the HTTP logger.

### 2.3 Agent level: the `budget` field

One new optional field on `Agent`:

```json
{ "slug": "nightly-digest",
  "budget": {
    "mode": "reserve",
    "report": { "...": "§3" },
    "run":    { "...": "§4" },
    "tools":  { "...": "§6" },
    "delegation": { "...": "§7" }
  }
}
```

| Field | Meaning |
|---|---|
| `mode` | `"report"` (default): reporting only, nothing is ever refused. `"reserve"`: runs and gated tool calls take reservations. |
| `bucket` | Optional: `{"id": "..."}` or `{"param": "name"}` (an agent parameter, `todos/tool-partial-application.md`). Absent: the process's bucket. A bucket id is not a secret. |

There is no `class` field (§2.2, decision Q12).

Precedence for the bucket: a helper agent called under §7 uses what its
caller gives it; else the agent's own; else `--owner-budget`; else the
process's. An agent that needs a bucket and finds none fails to load (§8.1).

Precedence for the class of a run, all of it process configuration: a
helper carved from its caller's hold spends as its caller (§7.1), and a
helper given a lower class of its own uses that class (§7.2); otherwise, on
the server, the owner's `--owner-budget` class; else `agentClasses[slug]`;
else `class`; else none is sent, and buckette applies the key's single
class or the bucket's `default_class`.

Dimension names in `budget` are dimension names of that bucket.

### 2.4 Setting up and checking

**Setting up** is an operator's task, done once per fleet, and it is where
the console guide applies. The documentation page that ships with slice 1
gives it as a runbook:

1. On the console (`https://console-api.buckette.co`, a `cpk_...` key from
   the Console's Account tab or the device flow: `POST /auth/device/start`,
   a person approves the code, `POST /auth/device/token`): `GET /me` is
   `active`, `GET /tenants/plan-options` has a plan with `poiRemaining > 0`,
   `POST /tenants` returns `ctrExternalUrl` (the `buckette.url` above) and
   `ctrAdminApiKey` (shown once).
2. On the tenant, with the admin key: create the fleet bucket with its
   dimensions **in `Reporting` mode first**, a title and labels
   (`buckette/bucket-title`, `buckette/labels`); later its refill schedule,
   its reservation policy (a default and a maximum hold lifetime) and its
   `class-config`.
3. Mint the fleet's run-time key: `POST /api-keys` with `ScopeConsume`,
   `ScopeRead`, `cakreBucketId`, and `cakreClasses` where wanted. Put it in
   the `--api-keys` file under the id `buckette.key` names. The admin key
   is not given to agents-exe.

`agents-exe budget init` (the last slice, optional, Q9) automates steps 2
and 3 from an admin key given as a `SecretSource`. Step 1 stays with a
person: creating tenants and accepting plans is an account owner's act.

**Checking.** Plain `agents-exe check` validates syntax and references
(decorators, maps, delegation slugs, `Given` refused). `agents-exe check
--probe` also asks the tenant, spending nothing (`GET`, `/check` and `/eta`
only):

| Probe | Failure it names |
|---|---|
| `GET /health`, then an authenticated read | unreachable; `wrong_key_kind` ("a console key was configured where a tenant key is needed"); `401`; `tenant_on_hold` with its reason; `tenant_moved` |
| the key's scopes and limits | missing `ScopeConsume`; a key that is not confined to a bucket (warning); a key with no class limit (warning: classes are then whatever the process says); every configured class (`class`, `agentClasses`, `--owner-budget`) against the key's `akiClasses` (`class_not_allowed` before any run) |
| each agent's bucket | not found; a named dimension missing |
| modes | a warning for `mode: "reserve"` on a `Reporting` dimension (it will never refuse) and for `mode: "report"` on a `Locking` one (reports may be clamped) |
| classes | a configured class is not declared on the bucket (`unknown_class`); a class with a `linear`/`step` floor on a bucket without a schedule; a helper given a class of its own that is not lower than the class of every caller that may delegate to it (§7.2) |
| reservation policy | none on the bucket: a crashed run's hold relies only on the expiry agents-exe sets (warning) |

`check --probe --json` prints the same as data, for a fleet's deployment
pipeline. The console is consulted only by a person following the messages
(`tenant_on_hold: overdue_invoice` is settled in the Console).

## 3. Reporting

A **turn** is one LLM completion request. After each one, agents-exe
reports what the provider says it cost:

```json
"report": {
  "turns":  "turns",
  "tokens": {"total": "tokens"},
  "cost":   { "dimension": "usd",
              "perMillion": {"prompt": 3.0, "completion": 15.0, "cached": 0.3} },
  "metadata": {"project": "digest"}
}
```

`tokens` maps what `TokenUsage` carries (`prompt`, `completion`, `total`,
`cached`, `thinking`) to dimensions; `{"prompt": "tokens_in", "completion":
"tokens_out"}` is as valid. `cost` derives money from the same usage. Every
part is optional. When a provider returns no `usage`, the amount is
estimated from byte counts (`StepByteUsage`, bytes / 4) and flagged (Q4).

In `mode: "report"`, one request per completion:

```json
POST /buckets/<b>/consume
{ "crAmounts": {"turns": 1, "tokens": 5321, "usd": 0.021},
  "crMode": {"tag": "Partial"},
  "crClass": "batch",
  "crIdempotencyKey": "<sessionId>:<turnId>:llm",
  "crMetadata": {"agent": "nightly-digest", "session": "...", "model": "...",
                 "kind": "llm", "fleet": "nightly", "host": "worker-3",
                 "project": "digest"} }
```

* `Partial`, because a report must never fail. On a `Reporting` dimension
  the amount always lands in full. On a `Locking` dimension that is short,
  or held back by the class's floor, buckette takes what it may and the
  difference is traced as `budget.overrun` (with `protected: true` when
  `coProtection.cpProtected` says the floor did it). The work was already
  done; reporting does not pretend otherwise.
* The metadata is the fleet's breakdown: `GET
  /buckets/<b>/consume-breakdown?key=agent` (or `session`, `model`, `host`,
  `kind`, `tool`, or any key of `report.metadata` and `buckette.labels`)
  works with nothing declared first. It stays under buckette's 1 KiB cap;
  agents-exe truncates its own values, never the caller's keys.
* Reports are sent off the loop's path, in order per session, flushed
  before the process exits. A session never slows down or stops because of
  reporting, and never notices buckette being away (§8.2).

This alone is the **monitored** stage: dashboards, per-period ledger,
overflow alerts and pacing are all buckette's, fed by these calls.

## 4. Reservation: the run as a paced unit

In `mode: "reserve"` the unit is the **run**: from the moment a session
becomes runnable (a user message, a one-shot start, a resume, a helper call)
until it next idles (final answer, pause, stop, failure).

```json
"run": {
  "hold":  {"turns": 20, "tokens": 200000},
  "grow":  {"turns": 10, "tokens": 100000},
  "max":   {"turns": 60, "tokens": 800000},
  "ttlSeconds": 900,
  "wrapUp": {"turns": 1, "tokens": 6000},
  "onRefused": {"atStart": "stop", "midRun": "degrade", "waitSeconds": 0}
}
```

### 4.1 Hold before the run

Before the first completion: `POST /buckets/<b>/reservations` with
`rrvAmounts = hold`, `rrvClass`, `rrvExpiresAt = now + ttlSeconds`,
`rrvTransactionId = "<sessionId>:run:<n>"` and the metadata of §3.

* `ReserveSuccess`: the run starts. The reservation id is stored in the
  session (a new optional `budget` object in the session file), so a
  restarted process settles the same hold instead of taking another.
* `ReserveShortfall`: the run does not start, and nothing was spent
  (§4.4). This is the point of the pacer: a run that does not fit is told
  before it starts.

Only dimensions named in `hold` are held. Dimensions that `report` maps but
`hold` does not are reported per completion exactly as in §3: buckette's
"lock one dimension, report another" (hold `tokens`, report `usd`).

### 4.2 Keep and grow while it runs

* **Local account.** Each completion is debited from the hold in memory: no
  request per turn. A run costs one reserve, a few refreshes and grows, and
  one commit, whatever its length.
* **Refresh.** At half the hold's lifetime, `.../refresh` pushes
  `rfrExpiresAt` to `now + ttlSeconds` (buckette clamps it to the bucket's
  policy). A process that dies stops refreshing and the hold frees itself.
* **Grow.** Before a completion, if what is left of the hold (minus
  `wrapUp`) is less than the next turn's estimate (the previous completion's
  total tokens plus a margin, since a prompt only grows; one turn),
  `.../grow` asks for `grow` more, with `grqTransactionId =
  "<sessionId>:run:<n>:grow:<k>"`. A grow is checked like a new reservation,
  under the class's floor **of that moment**, and is all or nothing. `max`
  bounds what a run may ever hold; reaching it is treated as a refused grow.
* A refused grow is §4.4, `midRun`.

### 4.3 Settle what was used

When the run ends, **however it ends**: one `.../commit` with `crqAmounts`
= what the run really used per held dimension, and `crqMetadata` (agent,
session, model, turns, the end reason). The rest of the hold returns to the
allowance; a run that used nothing commits zero. If real usage exceeded the
hold (the last turn's estimate was low), the hold is committed in full and
the excess is reported `Partial` as in §3, a `budget.overrun` if it does not
fit: a reservation bounds a run to its hold plus one turn's estimate error.
On a `MeteredOverage` dimension the commit names the real amount, as
buckette allows.

A long interactive session is many runs, each settled when the model
answers, so its usage reaches the ledger as it goes. A single very long run
is settled only at its end, and a crash loses its unsettled usage with its
hold (the hold expires, uncommitted). `"settleEvery": N` (turns) commits and
re-reserves on the way for runs where that matters (Q6).

### 4.4 When buckette says no

A refusal carries the short dimensions, what is available, an ETA when
buckette can compute one (`coEta`, or `POST /buckets/:id/eta` with
`etqClass`), and whether a floor did it. agents-exe keeps the two cases
apart everywhere (traces, messages, pause reasons):

* `exhausted`: the allowance does not have it.
* `protected` (`roProtection.cpProtected: true`): the allowance has it, and
  it is being kept for a higher class. **This is the system working**, not
  an incident; the ETA is when the floor releases or the bucket refills.

`onRefused.waitSeconds` applies first in both cases: agents-exe retries the
reserve or grow until then, sleeping until the ETA when there is one. That
is all the **scheduled** stage asks of agents-exe: a batch run started
early waits for the floor's release instead of failing. Then:

| Action | `atStart` | `midRun` |
|---|---|---|
| `stop` (default) | The run does not start. The session is saved `StatusPaused` with reason `budget_exhausted` or `budget_protected`, the dimensions and the ETA. A later resume asks again. One-shot commands exit 75 (`EX_TEMPFAIL`, Q7) and print the ETA: a cron job or a workflow engine can reschedule on it. | The run stops before the next completion and settles (§4.3); same status and reason. Nothing is lost: the session resumes where it was. |
| `ask` | As `stop`, plus a question to whoever drives the session (TUI prompt, server event with `needsDecision`): **retry**, **run unreserved for N turns** (reporting only, the decision and its author recorded in the session), or **stop**. Where nobody can answer, `stop`. | Same. |
| `degrade` | Not available (nothing is held to wrap up with): `stop`. | One last **wrap-up** completion, paid by the `wrapUp` part of the hold that ordinary turns never touch: no tools offered, a system note saying the budget is exhausted and asking for a final answer from what is known. Then settle and `stop`. |

agents-exe never tops a bucket up. A cheaper-model fallback is not in the
first version (Q5).

## 5. Protected classes across the fleet

What classes ask of agents-exe is small, and it is everywhere:

* **Name the class on every request** that can be refused or clamped:
  `crClass` on consume and check, `rrvClass` on reserve (a grow and a commit
  inherit the hold's class). §2.2 and §2.3 say where the class comes from:
  the process, never the agent file.
* **Treat `protected` as its own outcome** (§4.4), with its own trace, pause
  reason and exit message, so that a fleet operator reading "batch refused"
  does not conclude the allowance is too small. buckette's shortfall
  statistics make the same distinction (`reason: "protected"`).
* **Fail loudly on class errors.** `unknown_class`, `class_required` and
  `class_not_allowed` are configuration errors, caught by `check --probe`
  and reported as such at run time; they are never treated as shortfalls,
  never waited on.
* **Keep the fleet on the bucket that declares the classes.** A split child
  has no classes and a split applies no floor, so a per-run or per-sub-agent
  child bucket would take the protected share out from under the floor.
  This is why the run's budget is a **reservation on the fleet bucket**
  (§4), and why delegation carves from the caller's hold, or, for a child
  of a lower class, takes a reservation of its own on the same bucket (§7),
  rather than splitting.
* **Let the key bind it.** The process chooses the class among those its
  key allows (§2.2); an agent file cannot name one at all. A process
  misconfigured as `interactive` on a batch machine is refused by buckette
  (`403 class_not_allowed`). Without a class-limited key, the class is what
  the process says it is, which is enough inside one trust domain and not
  across teams.

Declaring classes and floors is the bucket owner's job, in buckette, with
buckette's own help (`GET /buckets/:id/class-floor-suggestion`, fed by the
per-class ledger these reports fill). A typical fleet: `interactive` for
sessions a person waits on (TUI, server), `batch` for scheduled one-shots,
`backfill` for work that may take only what would otherwise be lost at the
reset.

## 6. Tools: the same two mechanisms, per call

### 6.1 What a tool declares: `resources` in `describe`

A bash tool (protocol 2) may add `resources` to its `describe` document. The
names are **the tool's own**; a tool knows nothing about buckette, buckets,
classes or keys.

```json
{ "protocol": 2, "slug": "browse",
  "resources": [
    { "name": "wall_seconds",  "kind": "report",  "actual": "measured" },
    { "name": "page_loads",    "kind": "consume", "amount": {"arg": "max_pages", "default": 1} },
    { "name": "usd",           "kind": "reserve", "amount": 0.50, "actual": "reported",
      "on_failure": "commit" },
    { "name": "browser_slots", "kind": "hold",    "amount": 1 }
  ]
}
```

| `kind` | Mechanism | Before the call | After the call |
|---|---|---|---|
| `report` | reporting | nothing | the actual amount is reported (`Partial`); never refuses |
| `reserve` | reservation | the declared amount (an upper bound) is held | the actual amount is committed, at most the bound; the rest is freed. `on_failure`: `release` (default: a failed call costs nothing) or `commit` |
| `consume` | reservation, committed in full | the declared amount is held | all of it is committed, whatever the outcome |
| `hold` | reservation, released | the declared amount is held | all of it is freed: a concurrency slot, a licence seat |

`amount` is a number, or `{"arg": NAME, "scale": K, "default": D}` to take
it from the call's arguments (the view hooks have). A missing argument
without `default`, or a non-numeric or negative value, fails the call
before anything is asked.

`actual` says where the real amount comes from: `"declared"` (the
`amount`); `"measured"` by the runtime, for the built-in names `calls`,
`wall_seconds`, `stdout_bytes`, `stdin_bytes`; or `"reported"` by the tool:
the runtime sets `AGENT_RESOURCE_REPORT` to the path of a fresh private
file and the tool writes one JSON object there before exiting (`{"usd":
0.12}`). A missing report falls back to the declared amount for `reserve`
(the bound, the safe side) and to nothing for `report`, with a
`budget.report_missing` trace. `stream`/`service` tools will instead carry
`"resources": {...}` in output events (slice 12).

A v1 tool, or a v2 tool without `resources`, is unchanged.
`agents-exe describe-tool` shows the parsed `resources`.

### 6.2 What an MCP tool declares: `_meta["buckette/resources"]`

An MCP server declares the same list in a tool's `_meta`, in its
`tools/list` answer, under the key **`buckette/resources`** (decision Q10).
The key is named after buckette, not after agents-exe, so that any agent
harness that integrates buckette can read the same declaration from the
same server:

```json
{ "name": "render_pdf",
  "inputSchema": { "...": "..." },
  "_meta": { "buckette/resources": [
      { "name": "render_seconds", "kind": "reserve",
        "amount": {"arg": "pages", "scale": 2, "default": 10}, "actual": "reported" },
      { "name": "renderer_slots", "kind": "hold", "amount": 1 } ] } }
```

* Entries have the shape of §6.1, with `"arg"` read from the call's
  `arguments`. The names are the server's own, mapped by `tools.map` like
  any tool's (§6.3).
* `actual: "reported"` reads the call result's
  `_meta["buckette/resources"]`, an object of actual amounts
  (`{"render_seconds": 7}`): the MCP counterpart of
  `AGENT_RESOURCE_REPORT`. A result without it falls back as in §6.1.
  `actual: "measured"` offers `calls` and `wall_seconds` only.
* This section is the reference for the convention: there is no separate
  page in buckette's documentation for now (decision Q17).
* The convention is only a declaration and a report. It says nothing of
  buckets, classes or keys, and a server that uses it learns none of them.
* A declaration that does not parse is ignored for that tool, with a
  load-time warning naming the server and the tool: an MCP server is
  outside the agent author's control, so this is not a `LoadingError`.
  Agent-side wrappers (§6.3) still apply.
* agents-exe's MCP client has to keep `_meta` from `tools/list` and from
  `tools/call` results; today it drops both.

### 6.3 What the agent declares: mapping, and rules for any tool

```json
"tools": {
  "map": { "usd": "usd", "page_loads": "api_calls", "wall_seconds": null },
  "unmapped": "ignore",
  "onRefused": "tell",
  "holdTtlSeconds": 600
}
```

`map` sends a tool's resource name to a dimension of the agent's bucket
(absent: the same name; `null`: dropped). A resource whose dimension the
bucket lacks is ignored with a load-time warning, or is a `LoadingError`
under `"unmapped": "error"`. Under `mode: "report"` every kind behaves as
`report`: amounts are reported, nothing is held or refused.

Tools without a declaration of their own (builtin toolboxes, OpenAPI,
PostgREST, Lua, helper agents, bash tools whose author declared nothing,
MCP tools without `_meta`), and any tool whose declaration the agent wants
to change, get resources from the agent through a **new decorator** in the existing
`toolCallPolicyConfig.wrappers`, matched by the existing `WrapperMatch`:

```json
"wrappers": [
  { "match": {"tool": "http_*"},
    "decorators": [ {"tag": "budget", "resources": [
        {"name": "api_calls", "kind": "consume", "amount": 1} ]} ] }
]
```

Entries have the shape of §6.1 (`actual: "reported"` for bash and MCP
tools only). For MCP tools both sources apply (decision Q10): the server's
`_meta` declaration first, then the agent's wrappers. When a tool's own
declaration (`describe` or `_meta`) and wrappers name the same resource,
the last matching wrapper wins for that name; `"kind": "none"` silences
one. The agent author has the last word.

### 6.4 The lifecycle around one call

1. **Before.** After before-hooks (a denied call costs nothing) and after
   the cache (a hit costs nothing), all held amounts of the call go in
   **one** reservation: atomic across dimensions, with the run's
   `rrvClass`, `rrvExpiresAt = now + max(holdTtlSeconds, the call's
   timeout)`, `rrvTransactionId = "<sessionId>:<toolCallId>"`, and metadata
   adding `tool` and `kind: "tool"` to §3's.
2. **Run.** As today. Retries (`WithRetries`) happen inside the one
   reservation (Q8). A call that outlives its hold (async, later `service`)
   has it refreshed like a run's.
3. **Settle.** One commit with per-dimension amounts (`consume` in full,
   `hold` zero, `reserve` the actual amount capped at the hold, or zero on
   failure with `on_failure: "release"`). A cancelled or timed-out call
   settles as a failure. `report` resources then go in one `Partial`
   consume keyed `"<sessionId>:<toolCallId>:report"`.

The reservation id is kept on the tracked call, so a durable session
restarted mid-call settles the right hold.

A tool resource that is a dimension the **run** already holds (a tool that
spends `tokens`) is debited from the run's hold in memory instead of taking
a hold of its own, and grows it when needed (§4.2).

### 6.5 Refusal

A refused reservation means the call **does not run**. `tools.onRefused`
(overridable per decorator):

| Value | Effect |
|---|---|
| `tell` (default) | The model gets a tool error result: `budget refused: api_calls short by 1 (0 available); retry possible after 2026-11-23T12:00:00Z`, or `...: held back for higher-priority work until ...` when protected. It can do something else. Not a failure for `WithRetries`. |
| `wait` | Retry for up to `waitSeconds`, sleeping until the ETA, then `tell`. |
| `defer` | The call takes the existing `Defer` disposition with the refusal as its reason. |
| `stop` | As §4.4 `midRun: "stop"` for the run. |

Amounts and availability are told to the model; bucket ids, class names of
others and anything about the key are not.

## 7. Sub-agents: a share of the caller's budget

A helper agent (a child under `toolDirectory`, or an `extraAgents` entry) is
called as a tool; its run is a run (§4) with a budget given by its caller.

```json
"delegation": {
  "default": { "allocation": {"turns": 10, "tokens": 60000} },
  "agents": {
    "summarizer": { "allocation": {"turns": 3, "tokens": 20000} },
    "web-search": { "fraction": 0.25, "max": {"tokens": 100000} },
    "linter":     "inherit",
    "sandboxed":  { "allocation": {"turns": 5, "tokens": 30000},
                    "carve": "split", "scopedKey": true }
  },
  "onRefused": "tell"
}
```

Per child slug, or `default`: `allocation` (amounts per dimension), or
`fraction` of what the caller has left (optionally capped by `max`), or
`"inherit"` (the child draws on the caller's budget without a share of its
own; also what happens when `delegation` is absent, so a budgeted caller is
never escaped through a child), or `"none"` (explicitly unbudgeted).

How the share is taken depends on where the child runs and on its class,
and the class comes from the process (§2.2), never from either agent file:

| The child | `carve` | § |
|---|---|---|
| runs in the process, spends as its caller (no `agentClasses` entry, or the caller's own class) | `"hold"` (default) | 7.1 |
| runs in the process, and the process gives it a **lower class** of its own | `"reserve"` (implied by that class) | 7.2 |
| runs outside the process, or must be its own bucket | `"split"` (explicit) | 7.3 |

### 7.1 `carve: "hold"` (default): a share of the run's reservation

The child's allocation is taken **out of the caller's hold**, in the
process: the caller's local account is debited by the allocation (growing
the hold first if it must, §4.2, under the caller's class), the child run
gets a local account of exactly that size, and what the child did not use
is credited back to the caller's account when the call ends, however it
ends. No request to buckette is made for the child at all; its usage is
settled with the caller's commit, and reported with its own metadata
(`agent: <child>`, `parent: <caller>`) for dimensions that are reported per
completion.

* The child cannot grow: its allocation is its limit. Exhausted, it stops
  (or wraps up, if it declares `wrapUp`), and the caller gets a tool result
  saying so with whatever the child produced: `sub-agent stopped: budget
  exhausted (turns)`.
* Grandchildren carve from the child's share: budgets nest along the call
  stack.
* The child spends as the **caller's class**: it is the caller's
  reservation. A child that must spend as another class takes §7.2.
* A child with no `budget` field of its own is still held to its share:
  **a delegated budget is always enforced.**
* This works with class-limited keys and keeps everything on the bucket
  that declares the classes (§5).

Under `mode: "report"` there is nothing to carve: the child reports on the
caller's bucket and class, and `allocation` is ignored with a load-time
note.

### 7.2 `carve: "reserve"`: a lower class, a reservation of its own

An interactive agent may hand a side task to a helper that does not need
interactive priority: a re-indexing pass, a long web search, a digest
nobody waits on (decision Q13). The process gives that helper a lower class
(`"agentClasses": {"web-search": "batch"}`, §2.2), and the helper then
**takes a reservation of its own** on the same fleet bucket, under its own
class, instead of a carve from the caller's hold. A carve cannot do it: a
carve is the caller's reservation and spends as the caller's class.

* **Size.** The delegation entry sizes it exactly as for a carve
  (`allocation`, or `fraction` of what the caller has left, capped by
  `max`). That is the child's `hold`, and also its limit: like a carved
  child it does not grow, and stops or wraps up when it runs out (§7.1).
* **Request.** `POST /buckets/<b>/reservations` with `rrvAmounts` = the
  share, `rrvClass` = the child's class, `rrvExpiresAt` as in §4.1,
  `rrvTransactionId = "<callerSessionId>:<toolCallId>:child"` (a retried
  call finds the same hold) and metadata `agent: <child>`, `parent:
  <caller>`. It is checked under the **lower class's floor**: the child can
  only take what that class may take, so the protected share of the
  caller's class is never used by its side tasks.
* **The caller's hold is untouched.** Nothing is debited from the caller's
  local account and nothing is credited back. The child's usage goes to the
  ledger under its own class, which is what makes the per-class breakdown
  and buckette's floor suggestions right.
* **Settle.** The child commits its own reservation when the call ends,
  however it ends (§4.3). A crash leaves it to the hold's expiry, like any
  run.
* **Refusal.** A refused reservation is a refused helper call:
  `delegation.onRefused`, with the values of §6.5 (default `tell`:
  `sub-agent not started: held back for higher-priority work until ...`).
  `protected` is the expected outcome for a batch child at a busy hour, and
  is told apart from `exhausted` as everywhere (§4.4).
* **Only lower, never higher.** A helper is never a way up: a class that is
  not lower than its caller's is a configuration error at load and in
  `check --probe`. "Lower" is read from the class floors (decision Q15):
  class C is lower than class P when, on every dimension the child's
  reservation names, C's base floor is at least P's (C leaves at least as
  much behind), read from the bucket's `class-config` at start and in
  `check --probe`; equal floors are not "lower", and there is no separate
  class order in the process configuration. The same class means
  a carve (§7.1). An explicit `"carve": "hold"` on a child the process gives
  a lower class is a load error too: the two cannot both hold.
* **The key.** The child's class must be one the process's key allows, so
  a fleet that uses this mints its key with both classes
  (`cakreClasses: ["interactive", "batch"]`). The key stays the single
  source of truth for what the machine may spend as.
* **Nesting.** The child's own helpers carve from the child's reservation
  (§7.1, at the child's class) or take a still lower class of their own.
* Under `mode: "report"` nothing is held: the child reports per completion
  under its own class.

### 7.3 `carve: "split"`: a real child bucket

For a child that runs **outside the process** (`runIsolated`, a remote
deployment) or that must be visible as its own bucket in the Console:

* `POST /buckets/<caller-bucket>/split` with `srAllocation`,
  `srIdempotencyKey = "<callerSessionId>:<toolCallId>"` (a retried call gets
  the same child, never a second debit), `srExpiresAt` as a backstop, and
  `srMetadata` with the title and labels buckette asks of every bucket an
  agent creates (`"<child slug>: call <toolCallId>"`, `agent:<slug>`,
  `parent:<caller>`, `session:<id>`).
* The child run uses that bucket as its own (§2.3), with `"scopedKey":
  true` a key minted for it (`POST /api-keys`, `cakreBucketId`,
  `ScopeConsume` + `ScopeRead`) and handed to the isolated runner: the only
  place in this spec where a credential reaches another process.
* When the call ends, however it ends: `POST /buckets/<child>/reclaim`
  `{"rrMode": "ReclaimAll", "rrCascade": true}`. The leftover returns to the
  caller's bucket, the key stops working, the bucket stays readable as
  `archived`. A crash is covered by `srExpiresAt`.

Limits, stated by buckette and therefore refused at load here: a
class-limited key cannot split, and a split child has no classes and is
carved without regard to floors. `carve: "split"` is for fleets that do not
use classes on that bucket, or whose caller is of the top class. A child
that needs a lower class and must also run outside the process is not
supported: a split child has no classes to spend as.

## 8. Off by default, and failure

### 8.1 Zero behaviour change when unconfigured

* No `budget` in the agent, no `budget` decorator, no `resources` in any
  tool: no buckette code runs, no HTTP client is built, session files and
  traces are what they are today. A `buckette` block that no agent uses
  does nothing either.
* `resources` in a tool's `describe` while the agent has no `budget`:
  parsed, shown by `describe-tool`, otherwise ignored.
* An agent with `budget` on a process without buckette url, key or bucket:
  a `LoadingError` naming what is missing; silently running a budgeted
  agent unbudgeted is the failure this prevents. `--no-budget` is the
  explicit way to ignore every budget rule (traced once as
  `budget.disabled`). See Q2.

### 8.2 buckette unreachable

"Unreachable" is: connection or TLS failure, timeout, `5xx`,
`429 rate_limited`, `403 tenant_on_hold`, `410 tenant_moved`, after
`retries` attempts. `401`, `wrong_key_kind`, a `404` bucket and the class
errors are configuration errors: reported as such, and following the same
policy at run time.

| Step | `open` | `closed` |
|---|---|---|
| report (always `open`) | dropped and counted (`budget.report_dropped`) | not available |
| reserve at run start, or for a tool call | the run or call proceeds unreserved, traced `unreserved`; its usage is reported afterwards if buckette is back | as a refusal with reason `budget_unreachable`: §4.4 / §6.5 |
| grow | the run continues on what it holds, then unreserved | as a refused grow |
| refresh | retried; the hold may lapse, the settle then reports instead | same |
| settle | retried in the background, then reported `Partial`, then left to the hold's expiry; always traced | same: work done is never undone |
| a lower-class child's reserve (§7.2) | the helper runs unreserved, under its own class, traced `unreserved` | the helper call is refused |
| split / reclaim | the child inherits the caller's budget / left to `srExpiresAt` | the helper call is refused / same |

Default: **`closed` for reservations** (a gate that opens when the network
fails is not a gate), reporting always open (Q3). `onUnreachable` is set
for the process and can be overridden in `run`, `tools` and `delegation`.
Idempotency keys (`crIdempotencyKey`, `rrvTransactionId`,
`grqTransactionId`, `srIdempotencyKey`) make every retry safe; a reserve
repeated under a used transaction id is refused by buckette, and the client
then finds the existing hold by that id in `GET /buckets/:id/reservations`.

### 8.3 Dry run

`--budget-dry-run`: every reservation becomes a `POST /buckets/:id/check`
with the same amounts and class, nothing is held or spent beyond reporting,
and what would have been refused is traced (`budget.would_refuse`, with
`protected` when a floor would have done it). For moving a fleet from
monitored to controlled without surprises.

## 9. Observability

**Traces.** A `BudgetTrace` family beside the LLM and tool traces, in the
JSON log (`agentsLogs`), with session, turn and tool-call ids and the class:

| Event | Fields |
|---|---|
| `budget.reported` | bucket, amounts, `estimated` |
| `budget.overrun` | dimension, amount not booked, `protected` |
| `budget.reserved`, `budget.grown`, `budget.refreshed` | bucket, amounts, reservation id, expiry, latency |
| `budget.refused` | `exhausted` or `protected`, requested, shortfall, available, floor, eta, the action taken |
| `budget.settled`, `budget.released` | reservation id, committed, freed |
| `budget.carved`, `budget.returned` | caller, child, allocation, returned |
| `budget.delegated_reserved` | caller and child with their classes, reservation id, allocation (§7.2; the settle is an ordinary `budget.settled`) |
| `budget.meta_ignored` | MCP server and tool, why its `buckette/resources` did not parse (§6.2) |
| `budget.split`, `budget.reclaimed` | parent and child buckets, allocation, returned |
| `budget.unreachable`, `budget.report_dropped` | error class, policy applied |
| `budget.would_refuse`, `budget.disabled`, `budget.report_missing` | as above |

No event carries the key or a tool's arguments.

**In buckette.** Every consume, reserve and commit carries the metadata of
§3, so the event log, the consumption breakdown and the pacing read are the
fleet's view with nothing else to build. Thresholds, alerts, forwarders and
monitors are configured in buckette and the Console.

**For the fleet operator.** `agents-exe budget status [--json]` reads, for
the configured bucket: each dimension's balance, held amount and mode; the
class floors of this instant; the open reservations with their agent and
session; the period's pacing; the breakdown by `agent` and by class.
Read-only (`ScopeRead`), and reporting reads may lag by a moment, as
buckette says.

**Server.** The same events on `GET /v1/events` and the per-session stream
as `budget.*`; a session's JSON gains `budget` (class, held and used per
dimension, the last refusal).

**To the model**, optionally: `"run": {"announce": {"below": 0.2}}` appends
one system note when what the run may still hold falls under that fraction
(`[budget] about 7 turns and 61000 tokens left`); a `budget` capability of
the system toolbox gives a read-only `budget_status` tool (amounts only).

## 10. Implementation slices

Each is one PR. "→" is a dependency.

1. **Configuration and types.** `budget` on `Agent` (mode, report, run,
   tools, delegation; a `class` key refused with a message pointing to the
   process configuration), the `budget` decorator, the `buckette` block
   and flags (`class`, `agentClasses`, `--agent-class`), the key as a
   `SecretSource` with `Given` refused, plain `check` validation, `documentation/budgets.md` with the setup runbook
   (§2.4). Parsed and validated, never acted on.
2. **Ledger and client.** A `Ledger` record (check, consume, reserve, grow,
   refresh, commit, release, eta, split, reclaim) with the buckette HTTP
   client (class on every request, error classification of §8.2,
   idempotency keys, redaction) and an in-memory implementation with floors
   for tests. `BudgetTrace`. → 1
3. **`check --probe`.** The table of §2.4, including every configured
   class against the key's `akiClasses`. → 2
4. **Reporting.** §3 in both step functions, class and fleet metadata,
   estimated usage, the background sender. The monitored stage is usable
   from here. → 2
5. **Run reservation.** §4.1 to §4.3: hold, local account, refresh, grow,
   settle, the session's `budget` object; `atStart`/`midRun: "stop"` with
   the `exhausted`/`protected` distinction, exit code, `onUnreachable`,
   `--budget-dry-run`, `--no-budget`. The controlled stage. → 4
6. **Waiting and the other actions.** `waitSeconds` with ETA, `ask` (TUI
   and server), `degrade` with the wrap-up. → 5
7. **Tool budgets from the agent.** The `budget` decorator for every tool
   kind: §6.4 and §6.5, `declared` and `measured` actuals. → 2 (and 5 for
   resources shared with the run's hold)
8. **Tool budgets from `describe`.** `resources`, `tools.map`,
   `AGENT_RESOURCE_REPORT`, merge with wrappers, `describe-tool`,
   `documentation/binary-tool.md`. → 7
9. **Tool budgets from MCP `_meta`.** §6.2: keep `_meta` in the MCP client
   (`tools/list` and `tools/call`), read `buckette/resources` from both,
   merge with wrappers, `budget.meta_ignored`; §6.2 is the reference for
   MCP server authors (no separate page, Q17). → 7
10. **Delegation by carving.** §7.1, nesting, the enforced share for a child
    without `budget`. → 5
11. **Delegation to a lower class.** §7.2: the child's own reservation, the
    "lower than its caller" check at load and in `check --probe`,
    `budget.delegated_reserved`. → 10, 3
12. **Long calls.** Refresh for async calls, settle on durable restart,
    resources of `stream`/`service` tools (waits for those bash-tools-v2
    modes), `settleEvery`. → 5, 7
13. **Fleet surface.** `agents-exe budget status`, server events and
    session JSON, `--owner-budget`, `announce`, `budget_status`. → 4
14. **Delegation by split.** §7.3 with scoped keys for isolated children.
    → 10
15. **`agents-exe budget init`.** Tenant-side setup from an admin key
    (bucket, dimensions, reservation policy, class-config, the run-time
    key). → 3

Slices 1 to 4 put a fleet under monitoring; 5 and 6 pace it; 7 to 11 are
the tool and sub-agent halves and can go in parallel after 5.

## Design principles

* **D1. Three mechanisms, nothing else:** reporting (`consume`, `Partial`),
  reservation (reserve, grow, refresh, commit), class named on every
  request. buckette is the only ledger; agents-exe keeps only the local
  account of a hold it owns.
* **D2. The paced unit is the run,** not the turn: one reservation per run,
  turns debited locally, so a gated run costs a handful of requests.
* **D3. The fleet spends from the bucket that declares the classes.**
  Per-run budgets are reservations, sub-agent budgets are carved from the
  hold or, for a child of a lower class, reserved on the same bucket under
  that class; a split is the exception, for children outside the process.
* **D4. `protected` is not `exhausted`,** in traces, statuses, messages and
  exit output.
* **D5. Tools speak their own resource names;** the agent maps them. A tool
  never sees buckette, a bucket id, a class or a key. MCP servers declare
  them under `_meta["buckette/resources"]`, a key any harness can read.
* **D6. Tool budgets are a decorator** of the existing wrapper rules, one
  code path for every tool kind; `describe.resources` and MCP `_meta`
  produce the same decorator, and the agent's wrappers have the last word.
* **D7. A delegated budget is always enforced,** and absent `delegation`
  means `inherit`, never "unbudgeted".
* **D8. Reservations fail closed, reporting fails open.**
* **D9. The key is process configuration:** a tenant key, never inline,
  never in an agent description, never passed to a tool. The console API is
  for setup by a person, not for the run time.
* **D10. The class is process configuration too,** chosen among the
  classes the key allows; the class-limited key is the single source of
  truth. Agent files carry no class, and a helper may only go down a class,
  never up.

## Decisions (2026-11-24)

The owner answered the fourteen open questions of the first draft (chat,
2026-11-24). Eleven confirm the proposal; Q10, Q12 and Q13 changed the
design, and the sections they touch were revised. Those three decisions
raised Q15 to Q17, which the owner answered the same day (chat,
2026-11-24). No question is open.

| # | Question | Decision | Where |
|---|---|---|---|
| Q1 | The paced unit | One reservation per run, with local per-turn accounting (as proposed). | §4 |
| Q2 | A budgeted agent on a process without buckette | Load error unless `--no-budget` (as proposed). | §8.1 |
| Q3 | buckette unreachable under `reserve` | `closed` by default; reporting always open (as proposed). | §8.2 |
| Q4 | A provider that returns no `usage` | Estimate from bytes and flag it (as proposed). | §3 |
| Q5 | `degrade` | The wrap-up turn; no cheaper-model fallback in the first version (as proposed). | §4.4 |
| Q6 | Unsettled usage of a crashed long run | Accepted: the hold expires and the usage is not booked; `settleEvery` stays available (as proposed). | §4.3 |
| Q7 | Status and exit code on refusal | `StatusPaused` with a reason, exit 75 (as proposed). | §4.4 |
| Q8 | Retries | A tool call is paid once, whatever the number of attempts (as proposed). | §6.4 |
| Q9 | Setup | The runbook plus an optional `agents-exe budget init`, tenant side only (as proposed). | §2.4 |
| Q10 | MCP tools | **Both**: `resources` read from the tool's `_meta` under **`buckette/resources`** (not `agents-exe/...`, so other agent harnesses can share the convention), and agent-side wrappers as well. | §6.2, §6.3 |
| Q11 | Server tenancy | One tenant key with `--owner-budget OWNER=BUCKET[:CLASS]` (as proposed). | §2.2 |
| Q12 | Where the class lives | **Process only.** The class-limited key is the single source of truth; agent files carry no class. | §2.2, §2.3, §5 |
| Q13 | Sub-agents and classes | **A child may have a lower class of its own**, which takes a reservation of its own instead of a carve. | §7, §7.2 |
| Q14 | Naming | `budget` (agent field, CLI command) and `buckette` (connection), with `report.cost.perMillion` in the agent description (as proposed). | §2, §3 |
| Q15 | What "lower class" means for a child (from Q13) | From the class floors: C is lower than P when C's base floor is at least P's on every dimension the child's reservation names, read from `class-config`; equal floors are not lower; no `classOrder` in the process configuration (as proposed). | §7.2 |
| Q16 | Where a child's lower class is named (from Q12, Q13) | Per agent slug in the process's `agentClasses`: one class per helper per process; a second slug is the workaround (as proposed). | §2.2, §7.2 |
| Q17 | A standalone page for the `buckette/resources` `_meta` convention (from Q10) | No: this spec (§6.2) is the reference for now; no page in buckette's documentation. | §6.2, slice 9 |
