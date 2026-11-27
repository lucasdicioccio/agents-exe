#!/bin/bash
# One task, one fresh session.
# usage: run1.sh <label> <task 1..5> <rep>
# env: AGENTS_EXE (binary), SWEEP_OUT (output dir holding fixture.hs),
#      BOXNAME (toolbox Name, default fs), API_KEYS (secret-keys file),
#      MODEL_URL / MODEL_NAME / API_KEY_ID (see mkagent.py).
set -u
HERE=$(cd "$(dirname "$0")" && pwd)
L=$1; T=$2; R=$3
OUT=${SWEEP_OUT:?set SWEEP_OUT}
W=$OUT/runs/$L-t$T-r$R
rm -rf "$W"; mkdir -p "$W/work"
cp "$OUT/fixture.hs" "$W/work/ToolRegistration.hs"
python3 "$HERE/mkagent.py" "$W/work" "$W/agent.json" "${BOXNAME:-fs}"
F="$W/work/ToolRegistration.hs"
case $T in
1) P="In $F, rename the local \`hasCapability\` in \`buildDeveloperToolParams\` to \`hasCap\` and update its uses.";;
2) P="In $F, add an import of \`Data.Maybe (fromMaybe)\` after the last import.";;
3) P="In $F, delete the function \`devCapabilityToText\` and its type signature.";;
4) P="In $F, change the default of \`recursive\` in list-directory's description to say 'default: true'. Use patch-file.";;
5) P="Create $W/work/notes/TODO.md with three bullet points.";;
esac
echo "$P" > "$W/prompt.txt"
cd "$W"
timeout 900 "${AGENTS_EXE:?set AGENTS_EXE}" --api-keys "${API_KEYS:-$HOME/.config/agents-exe/secret-keys}" \
  --agent-file "$W/agent.json" --log-json-file "$W/log.jsonl" --log-file "$W/raw.log" \
  run --prompt "$P" > "$W/stdout.txt" 2> "$W/stderr.txt"
echo "exit=$?" > "$W/exit.txt"
# agents-exe run saves the session under ~/.config/agents-exe/sessions; keep it
# with the run (runs are sequential, so the newest one is ours).
for f in $(find ~/.config/agents-exe/sessions -name 'conv.*.json' -newer "$W/prompt.txt"); do mv "$f" "$W/session.json"; done
