#!/usr/bin/env bash
# Regenerates the mirrored pages, then produces the site into docs/, the
# directory GitHub Pages serves from main (tramaj's arrangement). Commit the
# result: docs/ is the published site, website/src/ is its source, and the
# markdown documentation lives in documentation/.
set -euo pipefail
cd "$(dirname "$0")/../.."  # repo root
./website/scripts/sync-repo-docs.sh
mkdir -p docs/{audios,css,docs,gen/images,gen/out,hashtags,images,js,json,raw,text,topics,videos}
kitchen-sink produce --srcDir website/src --outDir docs
echo "produced $(ls docs/*.html | wc -l) pages into docs/"
