#!/bin/bash
# display-large-text.bash - Display large markdown-formatted text using markdown-eye
# Required tools: bash, markdown-eye
#
# This tool takes markdown-formatted text via stdin and displays it using
# the markdown-eye binary. The viewer blocks until the user finishes reading.

set -euo pipefail

# Handle the 'describe' command to advertise tool interface
if [ "${1:-}" == "describe" ]; then
    cat <<'EOF'
{
  "slug": "display_large_text",
  "description": "Displays large markdown-formatted text using markdown-eye viewer. The text is passed via stdin and the tool blocks until the user closes the viewer. Note: images are not supported.",
  "args": [
    {
      "name": "text",
      "description": "The markdown-formatted text to display",
      "type": "string",
      "backing_type": "string",
      "arity": "single",
      "mode": "stdin"
    }
  ],
  "empty-result": { "tag": "DoNothing" }
}
EOF
    exit 0
fi

# Check for markdown-eye availability
if ! command -v markdown-eye &> /dev/null; then
    echo "Error: markdown-eye is not installed. Please install markdown-eye to use this tool." >&2
    exit 1
fi

# For 'run' command, we pass stdin directly to markdown-eye
# The binary reads from /dev/stdin and blocks until user closes the viewer
markdown-eye /dev/stdin

# Exit with the same status as markdown-eye
exit $?

