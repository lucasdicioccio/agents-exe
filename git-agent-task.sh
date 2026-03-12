#!/usr/bin/env bash

set -Eeuo pipefail
set -x

# -----------------------------
# Help
# -----------------------------
usage() {
  cat <<EOF
Usage:
  $0 <command> [label] [name] [instruction-file]

Description:
  A wrapper script to run git-agent-task.sh in multiple project directories.

Commands:
  prepare     Sets up the environment.
  preview     Runs a preview/check.

Options:
  -h, --help                 Show this help message and exit
EOF
  exit 0
}

# -----------------------------
# Parse arguments
# -----------------------------
if [[ $# -eq 0 ]]; then
  usage
fi

if [[ "$1" == "-h" || "$1" == "--help" ]]; then
  usage
fi

command="$1"
label="${2:-}"
name="${3:-}"
instruction_rel="${4:-}"

case "$command" in
  prepare)
    echo "======================="
    cat "${instruction_rel}"
    echo "======================="
    echo ""
    echo "Nothing to prepare."
    ;;
  static-check)
    echo "Formatting."
    if [[ -x `which fourmolu` ]]; then
       find ./src ./app ./agq -name "*.hs"  -exec fourmolu -i '{}' \;
    fi
    ;;
  check)
    echo "Building."
    ./qa-agent/tools/cabal-build run

    echo "Testing."
    ./qa-agent/tools/cabal-test run

    echo "General verifications."
    ./qa-agent/tools/run-agents-exe run --args "check --tools openai"
    ./qa-agent/tools/run-agents-exe run --args "describe"
    ./qa-agent/tools/run-agents-exe run --args "cowsay 'we want types and we want it now'"
    ./qa-agent/tools/run-agents-exe run --args "echo-prompt -S '#' -p 'types types types'"
    ./qa-agent/tools/run-agents-exe run --args "session-print ./test/data/turn-v0.001.json"

    echo "Verifying check agents behavior."
    paths_json=$(agents-exe paths --json)
    while read -r agent_path; do
      if [[ -n "$agent_path" ]]; then
        echo "Running checks for agent: $agent_path"
        ./qa-agent/tools/run-agents-exe run --args "--agent-file $agent_path run -p 'run checks'"
      fi
    done < <(echo "$paths_json" | jq -r '.agents[] | select(test("\\./qa-agents?/check-.*\\.json"))' || true)

    ;;
  preview)
    echo "Skipping."
    ;;
  *)
    echo "Error: Unknown command '$command'"
    usage
    exit 1
    ;;
esac
