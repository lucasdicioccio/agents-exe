# Spec: a salmon-driven preview deployment of agents-server

Status: proposal, 2026-10-03. Nothing implemented. A design pass with a
recommendation; the points marked **Decide** need the owner before any
phase starts.

## Goal

Run the *built* `agents-server` as it would run in production (its own
process, its files on disk, a real database, later a real init system),
check it over the network, and tear it down. The same recipe, left up
instead of torn down, is a preview of a branch.

[salmon](https://github.com/lucasdicioccio/salmon) is the tool that brings
the deployment up and down: it expresses the deployment as a graph of
idempotent ops with `up`/`down`/`check`, and it already has the pieces this
needs (podman containers, systemd units, postgres, qemu VMs, files).

## Where things stand

What is tested today, and how:

| What | Where | How real |
|---|---|---|
| Runtime, sessions, tools | `agents-tests` (`test/`) | in-process, in-memory persistence |
| HTTP API, auth, events, mail | `agents-server-tests` (`examples/agents-server/test`) | in-process WAI application on a free port, the LLM replaced by a Haskell function, a temporary SQLite file |
| Postgres store, leases, takeover | `agents-postgres-tests` (`postgres/test`) | a real cluster from `initdb`/`pg_ctl` (or `AGENTS_TEST_POSTGRES_URL`); skipped when neither is there |
| TUI against a real binary | `checks/*-e2e` | the built `agents-exe` under a pty, with `checks/lib/fake_llm.py` as an OpenAI-compatible LLM |

What none of them runs:

* the `agents-server` executable itself: its option parser, its startup
  order, its exit codes, its JSON log lines;
* the units in `bundling/systemd/` (`User=`, `StateDirectory=`,
  `TimeoutStopSec` against `--shutdown-grace`, `Restart=on-failure`);
* `SIGTERM` and `SIGKILL` on a real process, and what a restart finds;
* a database written by one version and opened by the next;
* two server processes on one Postgres, one of them killed;
* a client on another machine: `--bind`, `--auth-tokens`, `--cors-origin`,
  `agents-exe tui --attach URL`;
* the binary on a machine other than the one that built it.

salmon is not used by this repository. The two mentions are incidental
(`dbs/README.md`, `website/README.md`).

## What salmon provides

Read from salmon's README and `resources/howto-ops.md` (section 10); none of
it has been run for this spec.

* Builtin nodes for `Podman` (and quadlets), `Systemd`, `Postgres`, `Qemu`,
  `Filesystem`, `Cabal`, `Daemon` (a process salmon keeps running where
  there is no systemd), `Certificates`.
* A binary built on salmon speaks `config <seed> | run up`, `run down`,
  `run tree`. Teardown is the graph walked backwards.
* Its own tests are in four layers by cost: graph shape only (0), temporary
  directories (1), disposable podman containers (2), a qemu VM for systemd
  as PID 1 and real interfaces (3). Layers 2 and 3 skip loudly when podman
  or the VM privileges are missing.
* `SreBox.Gcp.PreviewEnvironment` groups several Cloud Run deploys into one
  node per branch. It is the precedent for "one thing to point `run up` and
  `run down` at", on a host this spec does not propose.
* `salmon-core`, `salmon-ops` and `salmon-ops-recipes` are on Hackage. The
  test harness (`Test.Harness`: `withContainer`, `withVm`, `sshToVm`, ...)
  is in salmon's test tree, not in a library, so it cannot be imported from
  here as it is.

## Recommendation

Build one salmon recipe, in this repository, that describes an
agents-server deployment, and use it two ways: a test driver runs `up`,
checks, `down`; an operator runs `up` and keeps it. Start with a container
and SQLite, and add Postgres and a VM as later phases. Do not make it a
tier of `cabal test`.

### One graph, two uses

```
agents-preview config --name NAME --host container|vm --db sqlite|postgres ...
    | agents-preview run up        # or: run down, run tree
```

The graph, from the leaves:

1. the `agents-server` binary for the target (see "The binary" below);
2. the host: a podman container, or a qemu VM;
3. files on the host: agent file, keys file, token file, and for the VM the
   unit from `bundling/systemd/`, copied as it is shipped;
4. the LLM: `checks/lib/fake_llm.py` with a script, listening next to the
   server; the agent file's `modelUrl` points at it, as `checks/lib/e2e.py`
   already does. No API key, so no secret is involved;
5. optionally Postgres and its database;
6. the server process (container command or `Daemon` in the container,
   the systemd unit in the VM);
7. a last node whose `check` is `GET /healthz` answering `200`.

`run up` prints where the server listens and a token. A preview is this,
left standing; `run down` removes it.

### Where it lives

A separate cabal package, `preview/`, with its own project file
(`cabal.preview.project`), depending on `salmon-ops` and
`salmon-ops-recipes` from Hackage and on `agents-lib` for the HTTP client.
The repository has no `cabal.project` today, only a freeze file, so the
default build and `cabal test` are untouched and nobody needs salmon, podman
or qemu to work on agents-exe. This is what salmon does for its own
experimental packages (`cabal.perso.project`).

Not recommended:

* *A new test-suite in `agents.cabal`*. It would put salmon and all its
  dependencies in the freeze file of every build, for tests that most
  machines skip.
* *The recipe in the salmon repository*. agents-server's flags and files
  change here; the recipe must change in the same commit.
* *A shell script around `podman`*. It would work for phase 1. It gives no
  `down` that matches `up`, no `check`, and nothing to reuse for the VM.

### The checks

Written in Haskell in the same package, against the address `run up`
printed, with `System.Agents.Host.Client.Http` (the client the TUI attaches
with). They take a URL and a token and nothing else, so they also run
against a server deployed by other means.

Phase 1, container and SQLite:

1. `/healthz` answers; the `server.started` log line says
   `authentication: bearer`.
2. A request without a token is refused; one owner cannot read another's
   session.
3. A scripted run: create a session with a prompt, wait, read the answer
   the fake LLM was scripted to give, with one tool call on the way.
4. The events stream of that run carries `run.started` ... `run.stopped`.
5. `SIGTERM` during a run: the process exits within `--shutdown-grace`, the
   session is stored.
6. `SIGKILL` during a run, then start: `sessions.recovered` names the
   session, its status is the one its turns imply, `resume` continues it.
7. `agents-exe tui --attach URL` from outside the container (the existing
   `checks/phase4-attach-e2e`, given a URL).
8. Upgrade: bring up the binary built from `main`, run check 3, replace the
   binary with the branch's, start on the same database, read the old
   session and run check 3 again.

Phase 2, Postgres:

9. The server creates its tables in an empty database.
10. Two servers on one database: a message sent to one reaches a run on the
    other; kill the owner, and the other logs `sessions.taken_over` after
    the lease expires.
11. Check 8 with Postgres.

Phase 3, a VM with systemd:

12. `bundling/systemd/agents-server.service`, installed as the
    documentation says, starts as the `agents-server` user and creates its
    state directory.
13. `systemctl stop` during a run ends cleanly, before `TimeoutStopSec`.
14. A killed server is restarted by systemd and recovers its sessions.
15. `journalctl -u agents-server -o cat` is one JSON object per line.
16. The VM reboots; the service comes back with its sessions.

### The binary

A binary built on the developer's machine is linked against that machine's
glibc and libraries, and may not start in a `debian:bookworm` container.
Two ways:

* build inside a container of the target's distribution and copy the binary
  out (`bundling/Containerfile.build` does this for `agents-exe`, from a
  clone of GitHub rather than the working tree, and on `haskell:9.8.4`
  while `agents.cabal` asks for `base >=4.20`: it looks out of date, and
  was not built for this spec);
* choose the container image to match the host that builds.

The first is right for a preview that must be like production and slow
(a full build, once per image, cached after). The second is enough for
phase 1 on the owner's machine. Phase 1 takes the path of a binary as a
seed argument, so the choice can be made later.

### Phases

Each is useful alone and can stop there.

| Phase | Adds | Needs on the machine |
|---|---|---|
| 0 | the `preview/` package, the graph with a container and no server (`run tree`, `up`, `down`), a graph-shape test | podman |
| 1 | server, fake LLM, SQLite, checks 1 to 8 | podman, python3 |
| 2 | Postgres, a second server, checks 9 to 11 | podman |
| 3 | VM host, the shipped unit, checks 12 to 16 | qemu, root or the `setcap` grants salmon's harness documents |
| 4 | a preview on a host other than this machine | **Decide**: see below |

Phase 3 depends on salmon's VM harness being usable from outside salmon's
test tree. That is a change in salmon and is not part of this work: ask for
it there, or copy the few functions needed here until it exists.

## The other reading: salmon as a feature

The request also said "or integrate as a new feature". Two candidates were
considered, and neither is recommended now.

* *Agents that operate salmon*. `salmon run serve --http` has an HTTP API
  with an OpenAPI description (`salmon-ops/openapi/serve-api.openapi.json`),
  and agents-exe has an OpenAPI toolbox. An agent that lists, converges and
  inspects a salmon world may need no code at all, only an agent file and a
  page of documentation. Worth one experiment after phase 1, when there is
  a salmon graph in this repository to point it at.
* *salmon as the way to install agents-server*. The units in
  `bundling/systemd/` and a page of documentation do this today. A salmon
  recipe for production installs is the phase 1 graph with a real host, and
  should wait until the preview has shown the graph is right.

## Not in scope

* Deploying anywhere but the machine that runs the recipe (until phase 4 is
  decided).
* TLS, a reverse proxy, DNS. The server binds a plain port; the
  documentation puts a proxy in front. A later check, not a first one.
* A real LLM. Every check uses the fake one. A preview an operator keeps
  may be given real keys by hand; the recipe does not handle them.
* CI. The repository has no CI configuration; this spec adds none.
* Changes to the salmon repository.

## Decide

1. **Scope of the first step.** Recommended: phases 0 and 1 (container,
   SQLite), then stop and look. The alternative is to go straight to
   Postgres and systemd, which is closer to production and much more
   work before the first result.
2. **Host.** Recommended: podman container first, qemu VM in phase 3 for
   systemd. The alternative is the VM from the start (one host kind, but
   every run needs VM privileges and a rootfs).
3. **Package location.** Recommended: `preview/` with its own project
   file. Is a second cabal project in this repository acceptable?
4. **The checks' language.** Recommended: Haskell with the existing HTTP
   client. The alternative is Python next to `checks/lib`, which reuses
   `fake_llm.py`'s conventions and adds no Haskell dependency, but
   duplicates the client.
5. **Phase 4.** Is a preview on another host wanted at all (a URL to open
   per branch), and if so on what: a machine of the owner's over ssh, or
   Cloud Run as salmon's `PreviewEnvironment` does? This one involves real
   hosts, credentials and cost, and nothing is proposed until it is
   answered.
6. **The salmon harness.** May the VM helpers move from salmon's test tree
   to a library, or should phase 3 copy them?
7. **`bundling/Containerfile.build`.** Fix it as part of phase 1 (build
   from the working tree, a GHC that satisfies `base >=4.20`, also produce
   `agents-server`), or leave it?
