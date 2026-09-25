# The agents-exe website

A [Kitchen-Sink](https://kitchensink-tech.github.io/) site, the same shape as
[tramaj's](https://github.com/lucasdicioccio/tramaj/tree/main/website) and
[salmon's](https://github.com/lucasdicioccio/salmon/tree/master/website). A
page is a `.cmark` file split into *sections* (content, metadata, CSS);
Kitchen-Sink assembles them into a static site.

- `src/` — the source: `kitchen-sink.json` (site config), the hand-written
  pages (`index.cmark`, `getting-started.cmark`, `docs.cmark`, `llms.txt`),
  the layout pages (`topics`, `hashtags`, `glossary`) and the CSS/JS they reference.
- `scripts/` — `sync-repo-docs.sh` regenerates the mirrored pages
  (`docs-*.cmark` from `docs/`, `specs-*.cmark` and `specs.cmark` from
  `todos/`, `commands.cmark` from the README's command reference);
  `mirror.py` and `md_tables_to_html.py` are what it runs. Those outputs are
  gitignored: the repository's markdown is the source of truth, the site is
  a view of it.
- `www/` — the output directory (gitignored).

## Regenerate and preview

```sh
./website/scripts/sync-repo-docs.sh
kitchen-sink serve --srcDir website/src --outDir website/www --servMode DEV --httpPort 7655
```

Then open http://localhost:7655/. The dev server rebuilds on file changes
under `src/`; re-run the sync script after editing anything under `docs/`,
`todos/` or the README.

To produce the site once:

```sh
./website/scripts/sync-repo-docs.sh
kitchen-sink produce --srcDir website/src --outDir website/www
```

`kitchen-sink.json`'s `basePath` is `/agents-exe`, so every absolute `/x.html`
link and every CSS import (through `$ctx.pathPrefix`) resolves under
`https://lucasdicioccio.github.io/agents-exe/`.

## Learn more

- [Features](https://kitchensink-tech.github.io/features.html) — what
  Kitchen-Sink can do.
- [Sections](https://kitchensink-tech.github.io/sections.html) — the
  section format used inside each `.cmark` file.
