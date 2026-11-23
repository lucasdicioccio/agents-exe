# Spec: tramaj in the serve UI, and a tool that teaches the served agent tramaj

Status: proposal, 2026-11-23, asked for by the owner in the project chat.
Nothing here is implemented. Four decisions were made by the owner before
this spec and are not reopened here:

1. Render **client-side**, by bundling tramaj-js into the serve UI.
2. Render **```` ```tramaj ```` fenced blocks** in agent replies.
3. The reference tool is a new **`TramajToolbox`** builtin with
   `list_sections` / `read_section`, split on the reference's numbered
   sections, plus the Node JSON format document.
4. Opt-in is **both** a per-agent `builtinToolboxes` entry and a `serve`
   flag that adds it to every served agent.

## Goal

A person chatting with an agent through the page `agents-exe serve` (and
plain `agents-server`) serves at `/` sees a tramaj program in the agent's
reply rendered as a document (tables, lists, headings) instead of as source.
An agent that has the tool can read the tramaj language reference when it
needs it, so it writes programs that the page can render.

Everything degrades to today's behaviour: a block that does not render is
shown as its source, a client other than the page shows the reply text as
it always did, and an agent without the toolbox is unchanged.

Non-goals: Markdown rendering of the rest of a reply; tramaj actions doing
anything in the page (§4.5); rendering tramaj server-side or in the TUI;
`import` of libraries from a reply (§4.3); editing the reference text (it
belongs to the tramaj repository).

## The split with the tramaj side

| Owned by the tramaj repository | Owned by agents-exe (this spec) |
|---|---|
| The text of the language reference and of the Node JSON document | Vendoring both files, at the commit agents-exe pins (§1) |
| A stable, versioned path for both files | The bump procedure and the check that everything moved together (§1.3) |
| Stable `## N. Title` section headings | Splitting on them, and failing the build when they break (§5.2) |
| Which tramaj-hs version (and tramaj-js version) each reference matches | Recording that in the vendored manifest and showing it to the model and the person (§1.2, §6) |
| tramaj-js: parser, evaluator, `Node` | The DOM renderer, the sanitizer, the bundle (§2, §4) |

Until the tramaj side publishes its stable path, this spec uses today's
paths, `specs/reference.md` and `specs/node-json.md`, and names them in one
place (the bump script, §1.3), so changing them is a one-line edit.

## References

* tramaj repository, `specs/reference.md` (sections `## 1. Shape of the
  language` to `## 14. Open`), `specs/node-json.md` (the normative output
  format), `tramaj-js/src/index.ts` (`tryParseProgram`, `runProgramWith`,
  `Node`), `tramaj-react/src/fold.tsx` (the React fold, read for how scalars
  and `$sym` values are shown, not used).
* In this repository: `examples/agents-server/src/AgentsServer/UI.hs` (the
  page: one `Text`, no build step, vanilla JS, `embedFile` for the
  favicon), `AgentsServer/Api.hs` (routes `/`, `/favicon.*`),
  `AgentsServer/Server.hs` (`ServerOptions`, `ServerFlags`, `--no-ui`),
  `System.Agents.Base` (`BuiltinToolboxDescription`,
  `DeveloperToolboxDescription` as the model for a new kind),
  `System.Agents.AgentTree.ToolLoader` (`loadBuiltinToolbox`),
  `System.Agents.ToolRegistration` (builtin tool names),
  `System.Agents.Tools.DeveloperToolbox.Spec` (an embedded document served by
  a tool, `embedStringFile`), `cabal.project` (the tramaj-hs pin,
  `9f2a6212844f539581f743cdeb7574986150dc3f`, about 151 commits behind
  tramaj `main` on 2026-11-23), `documentation/agents-server.md`.

## 1. Vendoring the reference, and one pin for everything tramaj

### 1.1 Files

```
vendor/tramaj/
  MANIFEST.json       what was vendored, from where, and its digests
  reference.md        copy of the tramaj reference at the pinned commit
  node-json.md        copy of the Node JSON document at the pinned commit
  LICENSE             tramaj's licence (MIT), carried with the copies
```

Both documents are embedded at compile time with `embedStringFile`, in a new
module `System.Agents.Tools.TramajToolbox.Reference`, and listed in
`extra-source-files` (as `documentation/binary-tool.md` already is). A
binary carries its reference: nothing is read from disk at run time, and the
reference a served agent reads is the one matching the binary's evaluator.

`MANIFEST.json`:

```json
{
  "tramajCommit": "9f2a6212844f539581f743cdeb7574986150dc3f",
  "referenceVersion": "<what the tramaj side names this reference>",
  "tramajHsVersion": "0.x.y.z",
  "tramajJsVersion": "0.1.0",
  "files": {
    "reference.md":  {"source": "specs/reference.md", "sha256": "..."},
    "node-json.md":  {"source": "specs/node-json.md", "sha256": "..."},
    "tramaj-ui.js":  {"sha256": "..."},
    "tramaj-worker.js": {"sha256": "..."}
  },
  "bundler": {"esbuild": "0.x.y", "node": "<image digest>"}
}
```

`referenceVersion` is whatever identifier the tramaj side gives each
reference; until it gives one, it is the commit's short hash. The manifest
is embedded too, so the toolbox and the page can say which version they
speak (§6).

### 1.2 One pin

The tramaj commit is pinned **once**: `cabal.project`'s
`source-repository-package` tag. The vendored documents, the tramaj-js
inside the UI bundle, and tramaj-hs (which evaluates `.tramaj` agent files)
all come from that commit. So the model reads the reference of the
evaluator the page runs, and the evaluator the page runs is the one the
server uses for agent files. A page that renders a program the server
would refuse, or the reverse, is a bug this rule prevents.

### 1.3 Bumping

One script, `scripts/bump-tramaj.sh <commit>`, does the whole bump in one
working-tree change, to be landed as one PR:

1. rewrites the tag in `cabal.project` (and the freeze file, if it names
   tramaj-hs);
2. `git archive`s the two documents and the licence from that commit into
   `vendor/tramaj/`;
3. rebuilds the UI bundle from that commit (§2.2);
4. rewrites `MANIFEST.json` (commit, versions read from that commit's
   `tramaj-hs/*.cabal` and `tramaj-js/package.json`, digests).

The first bump is the catch-up from `9f2a621` to tramaj `main`, and it is
its own slice (§9, slice 0): 151 commits include the integer/float split,
the arithmetic builtins, sorting and the `?shape` traverse form, which
change what `.tramaj` agent files evaluate to. It goes through the existing
`TemplateTests` before anything in this spec depends on it.

A Haskell test (`TramajVendorTests`) keeps the pin honest without needing
node or the network: the tag in `cabal.project` equals
`MANIFEST.json`'s `tramajCommit`, and the sha256 of every embedded file
equals its manifest entry. A bump that forgets a step fails `cabal test`.

## 2. The UI bundle

### 2.1 What is bundled

The page stays a hand-written `Text` in `UI.hs`; nothing about it gets a
build step. Next to it there are two **pre-built, committed** scripts:

* `tramaj-ui.js`: the renderer the page calls (fence splitting, the DOM
  fold, the sanitizer, the fallback, worker management). No tramaj-js in
  it.
* `tramaj-worker.js`: tramaj-js plus a 30-line message handler. It
  receives `{id, source}`, parses and evaluates, and posts back either
  `{id, ok: "node" | "value", json}` or `{id, error: {kind, message}}`.

Evaluation runs in a worker because a reply is untrusted input and tramaj,
though it has no recursion and always terminates, can build an output
exponential in the program's size (nested `map` over ranges). The page
terminates a worker that overruns its time budget and starts a new one
(§4.4); the tab never freezes.

Sources live in `ui/tramaj-render/` (TypeScript: `src/ui.ts`,
`src/worker.ts`, `src/fold.ts`, `src/fences.ts`, tests), with their own
`package.json` and `package-lock.json`. The outputs are committed at
`examples/agents-server/assets/tramaj-ui.js` and `.../tramaj-worker.js`.

### 2.2 How it is built

`scripts/build-tramaj-ui.sh`, run by the bump script and by hand after
editing `ui/tramaj-render/`:

1. In a container from `bundling/Containerfile.ui` (a node image pinned by
   digest, so the toolchain is fixed): `git clone` tramaj, `git checkout`
   the pinned commit, `npm ci && npm run build` in `tramaj-js/` (its own
   lockfile pins its toolchain).
2. `npm ci` in `ui/tramaj-render/` (esbuild and the test runner, exact
   versions, from the lockfile).
3. `esbuild` bundles each entry into one ES2020 IIFE, `--minify`,
   `--legal-comments=inline` (tramaj's MIT notice stays in the file),
   resolving `@lucasdicioccio/tramaj-js` to the step 1 checkout by alias.
   No source maps in the committed output.
4. Runs the renderer's tests (§8.2) against the bundle.
5. Writes the two files and their digests in `MANIFEST.json`.

tramaj-js is not on npm yet, which is why step 1 builds it from git. When
it is published, step 1 becomes a dependency on the exact npm version in
the lockfile (with its integrity hash), provided that version is built from
the pinned commit; the manifest keeps the commit either way.

### 2.3 Reproducible, and checked

esbuild's output is a function of its version, its flags and its inputs,
all of which are pinned (node image digest, two lockfiles, tramaj commit).
`checks/tramaj-ui-bundle` rebuilds in the container and fails if either
output differs byte for byte from what is committed. It needs network and
podman, so it is a check run before merging a bump or a renderer change
(and in CI if CI grows one), not part of `cabal test`. `cabal build` never
needs node: it embeds what is committed.

### 2.4 Embedding and serving

`UI.hs` embeds both files with `embedFile` (they join
`extra-source-files`). `Api.hs` serves them at
`/ui/tramaj-ui.js` and `/ui/tramaj-worker.js`, public like the favicon, with
`Content-Type: text/javascript; charset=utf-8`,
`X-Content-Type-Options: nosniff`, an `ETag` equal to the manifest digest,
and `Cache-Control: no-cache` (revalidate; they are small and change with
the binary). The page loads `tramaj-ui.js` with a `<script defer>` placed
before its own inline script. Expected size: tens of kB for the renderer,
on the order of 100 to 200 kB minified for the worker; it is fetched only
by the page.

The routes exist only when the page does (`uiEnabled`, not `--no-ui`).

## 3. Finding the blocks

`fences.ts` splits a reply's `responseText` into a list of text runs and
tramaj blocks:

* A block opens on a line that is exactly ```` ```tramaj ```` (three or more
  backticks, then `tramaj`, then optionally spaces and `key=value` words,
  then the end of the line), and closes on the first later line consisting
  only of at least as many backticks. This is CommonMark's fence rule,
  restricted to the info string `tramaj`.
* Info words: `version=<v>` (§4.6). Unknown words are ignored.
* An unclosed fence is not a block: the whole rest is a text run, as today.
* Fences with any other info string, and inline backticks, are text, as
  today. The page does not render Markdown; this spec does not change that.

Text runs are appended with `createTextNode`, exactly as the page does now.
So a reply without a tramaj block renders as it does today, byte for byte.

**Streaming.** While `text.delta` events arrive the reply is plain text, as
now. The full render happens on `session.updated`, which already re-renders
the whole log from the session. Re-rendering a log evaluates each block once
per render; the page caches results by source text so a re-render does not
re-evaluate, and does not flash.

## 4. Rendering a block

### 4.1 The steps

For each block, in order of appearance:

1. Insert a host `<div class="tramaj">` holding the source in a `<pre>`
   (what the person sees until the result arrives, and the fallback).
2. Send the source to the worker. The worker runs `tryParseProgram`, then
   `runProgramWith({mode: "concrete", arithmetic: true}, emptyLibs, {},
   program)` (or what the pinned tramaj-js names these).
3. A `node` result goes through the fold (§4.2) into a closed shadow root
   attached to the host; the `<pre>` stays in the host, hidden, behind a
   small "source" toggle. A `value` result (an expression program) is shown
   as pretty-printed JSON in a `<pre>`, labelled "value".
4. Any failure is the fallback (§4.4).

The context `$ctx` is the empty object, and the library table is empty: a
program in a reply stands alone. `import(...)` fails with `UnknownLibrary`,
which is the fallback, and the rendering note (§6.2) tells the model so.

### 4.2 The fold: what DOM a model may cause

The model's output is untrusted (a tool result can carry a prompt
injection), and the fold is the security boundary. It is written for this
page, not reused from `tramaj-react`, and it follows these rules:

* **Only** `document.createElement`, `createTextNode`, `setAttribute` and
  `appendChild`. No `innerHTML`, `outerHTML`, `insertAdjacentHTML`,
  `document.write`, `DOMParser`, `Range.createContextualFragment` anywhere
  in `tramaj-ui.js` (a test greps the bundle for them).
* **Tags: an allow-list.** `div span p br hr h1 h2 h3 h4 h5 h6 ul ol li dl
  dt dd table caption thead tbody tfoot tr th td pre code blockquote em
  strong b i u s small sub sup mark abbr kbd details summary section
  article header footer figure figcaption a`. The tag is compared
  lowercased. Any other tag (`script`, `style`, `img`, `iframe`, `svg`,
  `math`, `form`, `input`, `button`, `link`, `meta`, `base`, `object`,
  `video`, unknown names) is not created: its children are folded into a
  `<span>` (inline) carrying no attributes, and the block's footer counts
  "N elements not shown".
* **Attributes: an allow-list per tag.** `title`, `lang`, `dir`, `class` on
  every allowed tag; `colspan`, `rowspan`, `scope`, `headers` on cells;
  `start`, `reversed` on `ol`; `open` on `details`; `href` on `a`. Names
  are compared lowercased; everything else is dropped, including every
  `on*` name, `style`, `id`, `src`, `srcset`, `action`, `formaction`,
  `xlink:*`. A name the DOM would reject is dropped before `setAttribute`
  can throw.
* **Links.** `href` is kept only if, after trimming and removing ASCII
  control characters, it parses with `new URL(value)` (no base, so a
  relative URL is refused) to a protocol of `https:`, `http:` or
  `mailto:`. The link gets `rel="noopener noreferrer nofollow"`,
  `target="_blank"`, `referrerpolicy="no-referrer"`, and the fold appends
  the URL's host after the link text in small dim type, so the text cannot
  pass for another destination. A refused `href` leaves a plain `<span>`.
* **Values.** Text leaves, attribute values and the element value slot are
  JSON values; they become characters as `tramaj-react` writes them
  (integer as digits, float with a fraction or exponent, strings as is,
  other JSON compactly stringified, a `{"$sym": ...}` value as a dim
  placeholder). They only ever reach the DOM through `createTextNode` or
  `setAttribute` on an allowed name.
* **Actions** (`Action` attributes) are dropped: the page attaches no
  handler, and the element is rendered as if it had none (§4.5).
* **Annotations** are ignored.
* **Bounds.** At most 5,000 elements and text nodes and a depth of 64 per
  block, checked in the worker before the result is posted, and again in
  the fold. Over the bound is the fallback, not a truncated render.
* **Isolation.** The result lives in a **closed shadow root** with its own
  fixed stylesheet (readable table borders, the page's colours, a max
  height with scrolling, `contain: content`). Classes from the program style
  nothing outside the block and the page's classes do not leak in, so a
  program cannot dress itself as the page (a fake "Send result" card over
  the deferred-call card, for instance). With no `style` attribute and no
  `style` element, it cannot position anything outside its box either.

### 4.3 Content Security Policy

As defence in depth, `/` gets a CSP header from now on (it has none today):

```
default-src 'none'; script-src 'self' 'sha256-<inline script>';
worker-src 'self'; connect-src 'self'; img-src 'self' data:;
style-src 'self' 'unsafe-inline'; base-uri 'none'; form-action 'none';
frame-ancestors 'none'
```

The inline script's hash is computed once from the `uiPage` constant (in
Haskell, at startup or by Template Haskell). With `img-src` and
`connect-src` at `'self'`, even a fold bug cannot make the page fetch an
attacker's URL by itself (the Markdown-image exfiltration pattern); only a
click on a link the person can read leaves the page. `style-src` keeps
`'unsafe-inline'` because the page's own `<style>` and the shadow roots'
stylesheets are inline; that is acceptable since no model-controlled text
ever becomes a style. The CSP must not break today's features (the
`EventSource` with `access_token`, uploads, deferred-call cards): the UI
route test (§8.1) and a manual run of the page check them.

### 4.4 Errors and the fallback

A block falls back to **its source, shown as today's text would be**, in a
`<pre>`, with one dim line under it. The line says what happened and which
tramaj the page speaks:

| What happened | The line |
|---|---|
| parse error | `tramaj: parse error at line L, column C: <message> (renderer: tramaj <referenceVersion>)` |
| evaluation error | `tramaj: <Kind>: <message>`; kinds are stable (reference §12), messages are not |
| `UnknownLibrary` | `tramaj: imports are not available in chat replies` |
| over the time budget (1 s per block; the worker is terminated and replaced) | `tramaj: took too long to render` |
| over the size or depth bound | `tramaj: output too large to render` |
| more than 16 blocks in a reply, or a block over 32 KiB of source | the extra or oversized blocks are not evaluated: `tramaj: not rendered (limit)` |
| `tramaj-ui.js` or the worker failed to load (old cache, CSP mistake, `--no-ui`) | no line: the reply is plain text, as today |
| the fold throws anyway | the block falls back; the error goes to `console.error`; other blocks are unaffected |

Each block is independent: one failing block never stops the others or the
rest of the page. The fallback is never an empty box.

### 4.5 Actions

tramaj actions (`action("on-click", key, payload)`) have no meaning in the
page in this spec: they are dropped and the program renders read-only. A
later spec may map a clicked action to a new user message (for example
`[action <key>] <payload>`), which would need its own design for
confirmation and for what an injected program could make the person send.
Listed in the open questions.

### 4.6 Versions

A reply may say which reference it was written against: ```` ```tramaj
version=<v> ````. The page always tries to render with the tramaj it has.
If the block falls back **and** its `version` differs from the bundle's
`referenceVersion`, the fallback line adds `(written for tramaj <v>; this
page renders tramaj <referenceVersion>)`. A block without `version` is
taken to be written for the bundled one. The page never refuses a block
for its version alone: tramaj is mostly additive, and a working render is
better than a principled refusal.

The model learns the version from the toolbox (§5.3), so in practice a
block names the version the binary renders; the mismatch case is an old
session's history viewed after an upgrade, or an agent that wrote tramaj
from its training rather than from the tool.

## 5. `TramajToolbox`

### 5.1 Configuration

A new `BuiltinToolboxDescription` constructor, `TramajToolbox
TramajToolboxDescription`, with the same JSON shape as the others:

```json
{"tag": "TramajToolbox", "contents": {
  "Name": "tramaj",
  "Description": "The tramaj language reference",
  "Activation": null
}}
```

No sandbox, no capabilities list: the toolbox reads only text embedded in
the binary, does no IO and has nothing to restrict. `builtinToolboxFileSandbox`
is `Nothing` for it. Tool names follow the builtin convention in
`ToolRegistration` (`tramaj_<Name>_list_sections`,
`tramaj_<Name>_read_section`).

### 5.2 Sections

The splitter (a pure function, `splitReference :: Text -> [Section]`, run
once at load time over the embedded text) cuts the reference at each line
matching `^## ([0-9]+)\. (.+)$`. Each section runs to the next such line;
`###` subsections stay inside their section. The text before `## 1.` is
section `intro`. Ids are the numbers as strings (`"1"` to `"14"` today).

Two more sections are added after the numbered ones:

* `node-json`: the whole of `node-json.md` (about 190 lines, not split).
* `rendering`: a short document written in **this** repository,
  `documentation/tramaj-rendering.md`, also embedded, saying what the chat
  page does with a tramaj block (§6.2).

The vendor test (§8.1) asserts the split of the vendored text: `intro` plus
a contiguous run `1..N` with N at least 10, non-empty titles, and the
concatenation of the sections equals the original byte for byte. If the
tramaj side ever changes its headings, the bump fails its tests rather than
serving a one-section reference.

### 5.3 Tools

**`list_sections`**, no arguments. Returns:

```
tramaj reference <referenceVersion> (tramaj-hs <v>, tramaj-js <v>, commit <short>)
Fenced ```tramaj blocks in your replies are rendered by the agents-exe chat
page; see section "rendering" first.

id         title                               size
rendering  Answering in tramaj in this chat    2 KB
intro      Tramaj - Language Reference         3 KB
1          Shape of the language               2 KB
...
14         Open                                1 KB
node-json  Normative Node JSON representation  7 KB
```

**`read_section`**, `{"id": "11", "offset": 0}` (`offset` optional, in
characters). Returns the section's text from `offset`, at most 24,000
characters, cut at a line end. When more is left, it ends with the same kind
of line `read-file-range` writes after a capped read: `[section 11
continues: call read_section with id "11" and offset 24113]`. An unknown id
answers with the list of valid ids, not a bare error, so a ~25B model
corrects itself in one step.

The tool descriptions are written for a small model: one sentence on what
the tool returns, the example call, and (on `list_sections`) "call this
before writing a ```tramaj block".

### 5.4 Loading

`loadBuiltinToolbox` gets a `TramajToolbox` case building two tools over the
pre-split sections. Construction cannot fail (the text is in the binary and
its shape was checked by the test suite), so there is no
`BuiltinToolboxInitError` path beyond the generic one.

## 6. How the agent learns it may answer in tramaj

### 6.1 Through the tool, not the system prompt

The served agent learns it from the toolbox: the tool descriptions say the
chat page renders ```` ```tramaj ```` blocks, and `list_sections` puts the
`rendering` section first. Tool descriptions are sent on every request, so
an agent that has the toolbox always knows; an agent that does not, never
gets told, and its replies stay as they are. No system prompt is changed by
agents-exe: a system prompt belongs to the agent's author, who can add a
line ("prefer a tramaj table for tabular answers") if they want more.

The `rendering` section's wording does not claim every client renders: it
says the agents-exe chat page does, and that other clients of the same
session (the API, `agents-exe` TUI, a worker) show the source. That way an
agent served to several kinds of client is told the truth, and a source
block is still readable.

### 6.2 What `rendering` says

In about a page, for the model:

* when a block helps (a table, a list of records, a small report) and when
  plain text is better;
* the fence: ```` ```tramaj version=<referenceVersion> ````, one program
  per block, at most 16 blocks and 32 KiB per block;
* a document program (`.table(...)`) renders; an expression program shows
  its value as JSON;
* `$ctx` is `{}` and imports are not available: put the data in the program;
* the tags and attributes that render (§4.2's lists), that links need an
  absolute `https:`/`http:`/`mailto:` URL, that actions do nothing;
* one complete worked example (a small table built with `map`), checked by
  a test to render with the bundled renderer (§8.2).

## 7. Opt-in

### 7.1 Per agent

An agent lists the toolbox in `builtinToolboxes` (§5.1). This works under
every runner (one-shot, TUI, `serve`), since the reference is useful to an
agent writing `.tramaj` files too, wherever it runs.

### 7.2 For every served agent: `--tramaj-reference`

A new server flag, in `ServerFlags` so both `agents-exe serve` and plain
`agents-server` get it:

```
--tramaj-reference   Give every served agent the tramaj reference toolbox
```

When set, the server adds `TramajToolbox` with `Name: "tramaj"` to each
**root** agent it serves (the `--agent-file`/`--agent` ones and agents
stored through the API), at load time, before sandbox resolution:

* an agent that already has a `TramajToolbox` keeps its own entry (its name
  and activation win), and gets no second one;
* an agent that has another builtin toolbox named `tramaj` keeps it, and
  the server's entry is named `tramaj_reference` instead; a further clash is
  reported at startup like any toolbox name clash;
* helper agents (agents reached as tools) are not changed: they answer
  their caller, not the person, and their output is never shown as a reply
  in the page.

The flag changes the agents' tool lists, so it is logged once at startup
with the count of agents it touched. Without the flag nothing changes.

### 7.3 Rendering is not opt-in

The page renders tramaj blocks whenever it is served, with no flag: a reply
without a block is unchanged (§3), so there is nothing to turn off for an
agent that does not write tramaj. `--no-ui` still removes the page and its
scripts.

## 8. Tests

### 8.1 Haskell (`cabal test`)

* `TramajVendorTests`: pin equality (`cabal.project` tag vs manifest), the
  sha256 of each embedded file vs manifest, the split of the vendored
  reference (§5.2), `node-json` present and non-empty.
* `TramajToolboxTests`: JSON round trip of `TramajToolboxDescription` and of
  the `BuiltinToolboxDescription` tag; `list_sections` lists `rendering`,
  `intro`, `1..N`, `node-json` with the manifest's versions; `read_section`
  of each id; paging (`offset`, the continuation line, the last page has
  none, an offset past the end); unknown id lists the valid ids; tool names.
* Server tests (`examples/agents-server/test`): `--tramaj-reference` adds
  the toolbox to a root agent, keeps an agent's own, renames on a name
  clash, leaves helpers alone, is off by default; `GET /ui/tramaj-ui.js` and
  `/ui/tramaj-worker.js` answer 200 with the JS content type, `nosniff` and
  the manifest `ETag`, and 404 with `--no-ui`; `GET /` carries the CSP
  header and its `sha256-` source matches the inline script in `uiPage`.

### 8.2 The renderer (`ui/tramaj-render`, vitest with a DOM emulation, run by `build-tramaj-ui.sh` and the bundle check)

* Fences: a reply with none is one text run; several blocks; a longer
  closing fence; an unclosed fence; ```` ```tramajx ````, ```` ```Tramaj ````
  and other info strings are text; `version=` parsed; CRLF line ends.
* Rendering: each allowed tag; each allowed attribute; scalars (integer,
  float, `$sym`); an expression program shows JSON; the `rendering`
  section's worked example renders.
* A hostile corpus, each case asserting the resulting DOM contains no
  `script`, `iframe`, `img`, `svg`, `style`, no `on*` or `style` attribute,
  no `href` outside the three schemes, and no node outside the shadow root:
  `.script(...)`, `.img` with `src`, `.a` with `href` as `javascript:`,
  ` JaVaScRiPt:`, `java\tscript:`, `data:`, `vbscript:`, a relative URL;
  `onclick` and `onClick` attributes; `.svg` with `onload`; `style` and
  `class` attempts at overlaying; a tag name the DOM rejects; an attribute
  name the DOM rejects; actions; a text value holding `<script>` (stays
  text).
* Bounds: an exponential program hits the time budget and the worker is
  replaced and still works for the next block; 5,001 nodes; depth 65; a
  33 KiB block; a 17th block.
* Fallback: parse error, each evaluation error kind, `UnknownLibrary`,
  version mismatch line; one failing block among good ones.
* A grep of the built `tramaj-ui.js` for the forbidden DOM sinks (§4.2).

### 8.3 Manually, before merging the UI slice

Run `agents-exe serve --tramaj-reference` against the LAN test agent, ask
for a table, check it renders, that a deferred-call card, uploads and
streaming still work under the CSP, and that the hostile corpus pasted as a
reply (through a stub LLM) renders inert. A headless-browser e2e under
`checks/` is possible later; not required by this spec.

## 9. Implementation slices

Each slice is one PR and leaves `main` working.

0. **Catch up the pin.** Bump tramaj-hs from `9f2a621` to the tramaj commit
   that publishes the stable reference paths; fix what the 151 commits
   change in `.tramaj` agent file evaluation (`TemplateTests`). No UI, no
   toolbox. Needs the tramaj side's stable paths first, or uses today's.
1. **Vendor and the toolbox.** `vendor/tramaj/`, `bump-tramaj.sh` (docs
   and manifest only), `TramajToolbox`, the `rendering` document,
   `TramajVendorTests`, `TramajToolboxTests`. Usable on its own: an agent
   can read the reference under any runner.
2. **`--tramaj-reference`.** The flag and its tests;
   `documentation/agents-server.md`.
3. **The bundle.** `ui/tramaj-render/` with the fence splitter, fold,
   worker and their tests; `build-tramaj-ui.sh`, `Containerfile.ui`,
   `checks/tramaj-ui-bundle`; committed outputs and manifest digests;
   `bump-tramaj.sh` calls the build. Not loaded by the page yet.
4. **The page.** Routes, embedding, the `<script defer>`, the call from
   `render()`, the CSP header and its tests; the manual check of §8.3;
   `documentation/agents-server.md`.

## Decisions (proposed)

* One tramaj commit for tramaj-hs, the bundled tramaj-js and the vendored
  docs, pinned in `cabal.project` and checked by a test (§1.2).
* Committed, reproducible bundle outputs; `cabal build` never needs node
  (§2).
* Evaluation in a worker with a time budget; DOM only through an
  allow-list fold into a closed shadow root; CSP on `/` (§4).
* Fallback is the source plus one line; never an empty box, never a page
  error (§4.4).
* No system prompt change; the toolbox tells the agent, including that
  other clients show the source (§6).
* Rendering always on with the page; the toolbox opt-in per agent or with
  `--tramaj-reference` for root agents (§7).

## Open questions for the owner

1. **Actions.** Keep them inert for now (§4.5), or plan a follow-up where a
   click sends a message back to the session?
2. **Images and inline style.** Both are refused (exfiltration and page
   spoofing). Is a table without `style` and without images enough for
   what you want agents to show, or should a later slice allow `img` with
   `data:` URLs only, or a small set of style properties?
3. **Helper agents** are not given the toolbox by `--tramaj-reference`
   (§7.2). Should a helper that writes `.tramaj` files get it too, or is
   the per-agent entry enough for them?
4. **Slice 0 timing.** Catch the pin up to tramaj `main` now, as its own
   PR, or wait for the tramaj side's stable reference paths and do it once?
5. **`$ctx` and libraries.** A reply's program sees `{}` and no imports.
   Should the tramaj-hs standard library the server already loads for agent
   files also be available in the page (it would have to be bundled too)?
