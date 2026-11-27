#!/bin/bash
# Run the 5 tasks for each rep, grading as it goes. Runs are sequential
# (run1.sh picks up the newest saved session).
# usage: sweep.sh <label> <reps...>
# env: as run1.sh; FIXTURE_REV (default 4471a94, the fixture sweep #1 used).
set -u
HERE=$(cd "$(dirname "$0")" && pwd)
L=$1; shift
OUT=${SWEEP_OUT:?set SWEEP_OUT}
mkdir -p "$OUT"
[ -f "$OUT/fixture.hs" ] || git -C "$HERE" show "${FIXTURE_REV:-4471a94}:src/System/Agents/ToolRegistration.hs" > "$OUT/fixture.hs"
MODEL_URL=${MODEL_URL:-http://mac.home:8000/v1}
for R in "$@"; do
  for T in 1 2 3 4 5; do
    curl -s -m 10 -o /dev/null "$MODEL_URL/models" || { echo "MODEL DOWN at $L t$T r$R"; exit 3; }
    "$HERE/run1.sh" "$L" "$T" "$R"
    python3 "$HERE/grade.py" "$OUT/runs/$L-t$T-r$R"
  done
done
