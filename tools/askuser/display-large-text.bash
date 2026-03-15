#!/bin/bash
# display-large-text.bash - Display large markdown-formatted text using markdown-eye
# Required tools: bash, markdown-eye
#
# This tool takes markdown-formatted text via stdin and displays it using
# the markdown-eye binary. The viewer blocks until the user finishes reading.

# Check for markdown-eye availability
if ! command -v markdown-eye &> /dev/null; then
    echo "Error: markdown-eye is not installed. Please install markdown-eye to use this tool." >&2
    exit 1
fi

markdown-eye "$@"
