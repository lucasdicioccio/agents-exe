#!/usr/bin/env bash

set -Eeuo pipefail

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
  preview)
    echo "Skipping."
    ;;
  *)
    echo "Error: Unknown command '$command'"
    usage
    exit 1
    ;;
esac
