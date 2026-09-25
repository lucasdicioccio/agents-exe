#!/usr/bin/env bash
# Regenerates the mirrored pages of website/src from the repository:
#
#   documentation/<name>.md     -> website/src/docs-<name>.cmark
#   todos/<name>.md             -> website/src/specs-<name>.cmark
#   README.md's command reference -> website/src/commands.cmark
#   the specs' Status: lines    -> website/src/specs.cmark  (one line per spec)
#
# so the site never needs hand-copying when a doc changes. Everything this
# writes is mechanical output, not a source of truth: gitignored (see
# .gitignore) and regenerated fresh each run, including the "generated" date.
# docs/ is what GitHub Pages serves: publish.sh runs this, then produces there.
set -euo pipefail
cd "$(dirname "$0")/../.."  # repo root

OUT=website/src
# the output skeleton `kitchen-sink produce` writes into and does not create
mkdir -p website/www/{audios,css,docs,gen,hashtags,images,js,json,raw,text,topics,videos}
MIRROR="python3 website/scripts/mirror.py"
GITHUB="https://github.com/lucasdicioccio/agents-exe/blob/main"
DATE="$(date -u +%Y-%m-%dT%H:%M:%SZ)"

title_of() { sed -n '1s/^# //p' "$1"; }

# the sentence after "# Title": the first non-empty paragraph, joined
summary_of() {
  awk 'NR>1 && NF {print; next} NR>1 && !NF && seen {exit} {if (NR>1 && NF) seen=1}' "$1" \
    | tr '\n' ' ' | sed -e 's/  */ /g' -e 's/ $//' | cut -c1-240
}

status_of() {
  local s
  s="$(grep -m1 -i '^status:' "$1" | sed 's/^[Ss]tatus: *//' | cut -c1-240 || true)"
  printf '%s' "${s:-no status line yet}"
}

# documentation/: the guides and references
for src in documentation/*.md; do
  name="$(basename "$src" .md)"
  $MIRROR --kind docs --source "$src" --title "$(title_of "$src")" --topic docs \
    --keywords "guide, documentation" --summary "$(summary_of "$src")" \
    --github "$GITHUB/$src" --date "$DATE" > "$OUT/docs-$name.cmark"
done

# todos/: the specs (design documents and their progress notes), most headed by a Status: line
for src in todos/*.md; do
  name="$(basename "$src" .md)"
  $MIRROR --kind specs --source "$src" --title "$(title_of "$src")" --topic specs \
    --keywords "spec, design" --summary "Status: $(status_of "$src")" \
    --github "$GITHUB/$src" --date "$DATE" > "$OUT/specs-$name.cmark"
done

# README's command reference, as one page
$MIRROR --kind readme-section --source README.md --title "Command reference" \
  --topic reference --keywords "cli, commands, options, check, init, tui, run, serve, mcp-server" \
  --summary "The global options and every agents-exe command, as the README lists them." \
  --github "$GITHUB/README.md" --date "$DATE" \
  --section-from "## Command Reference" --section-to "## Using as a Tool" \
  --intro "# Command reference" > "$OUT/commands.cmark"

# the specs index: title, status, link, one entry per spec
{
  cat <<EOF
=base:build-info.json
{"layout":"article"
,"publicationStatus":"Public"
}

=base:preamble.json
{"author": "Lucas DiCioccio"
,"date": "$DATE"
,"title": "Specs"
}

=base:topic.json
{"topics":["specs"]
,"keywords":["design", "roadmap", "status"]
}

=base:social.json
{"twitter": "lucasdicioccio"
,"linkedin": "lucasdicioccio"
,"github": "lucasdicioccio"
,"mastodon": "https://fosstodon.org/@lucasdicioccio"
}

=base:summary.cmark
The design documents under todos/, each headed by a Status line saying what of it has shipped.

=base:main-content.cmark

# Specs

The \`todos/\` directory holds the design documents behind the larger
features (durable workflows, asynchronous tool calls, the session mailbox,
partial application of tool arguments, the standalone server) and their
progress notes. Most start with a **Status** line saying what of it has
shipped, which is reproduced here. These pages are generated from the
[agents-exe repository]($GITHUB/../tree/main/todos): the repository is the
canonical source, and may be ahead of what is published here.

EOF
  for src in todos/*.md; do
    name="$(basename "$src" .md)"
    printf -- '- [**%s**](/specs-%s.html), *Status:* %s\n' "$(title_of "$src")" "$name" "$(status_of "$src")"
  done
  cat <<'EOF'

=base:main-css.tramaj-json
{ "format": "css"
, "contents":
  [ ""
  , "@import \"`$ctx.pathPrefix`/css/dev.css\";"
  , "@import \"`$ctx.pathPrefix`/css/colors.css\";"
  , "@import \"`$ctx.pathPrefix`/css/article.css\";"
  , "@import \"`$ctx.pathPrefix`/css/navigation.css\";"
  ]
}
EOF
} > "$OUT/specs.cmark"

echo "synced $(ls $OUT/docs-*.cmark | wc -l) docs, $(ls $OUT/specs-*.cmark | wc -l) specs, commands.cmark, specs.cmark"
