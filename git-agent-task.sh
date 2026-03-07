#!/usr/bin/env bash

set -Eeuo pipefail

# -----------------------------
# Help
# -----------------------------
usage() {
  cat <<EOF
Usage:
  $0 <command>

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

case "$command" in
  prepare)
    echo "Nothing to prepare."
    ;;
  preview)
    set -x
    echo "Checking configs"
    cabal run -- agents-exe check

    echo "Running checks"
    mkdir -p checks
    cabal run -- agents-exe --session-json-file-prefix checks/conv. --agent-file demo-agents/ollama-01.json run -p "hi, test the three tools"
    
    latest_check=$(ls -t checks/conv.*.json 2>/dev/null | head -n 1 || true)
    if [[ -n "$latest_check" ]]; then
      if command -v markdown-eye >/dev/null 2>&1; then
        cabal run -- agents-exe session-print "$latest_check" | markdown-eye
      else
        cabal run -- agents-exe session-print "$latest_check"
      fi
    else
      echo "No check files found in checks/"
    fi
    ;;
  *)
    echo "Error: Unknown command '$command'"
    usage
    exit 1
    ;;
esac
