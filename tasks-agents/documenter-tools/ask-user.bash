#!/bin/bash
# ask-user.bash - Interactive user dialog tool using zenity
# Required tools: bash, zenity

set -euo pipefail

# Handle the 'describe' command to advertise tool interface
if [ "${1:-}" == "describe" ]; then
    cat <<'EOF'
{
  "slug": "ask_user",
  "description": "Displays interactive dialogs to ask the user for input using zenity. Supports text entry, yes/no questions, list selection, and file selection dialogs.",
  "args": [
    {
      "name": "type",
      "description": "Type of dialog: 'entry' for text input, 'question' for yes/no, 'list' for selection, 'file' for file picker",
      "type": "string",
      "backing_type": "string",
      "arity": "single",
      "mode": "dashdashspace"
    },
    {
      "name": "text",
      "description": "The prompt or question text to display in the dialog",
      "type": "string",
      "backing_type": "string",
      "arity": "single",
      "mode": "dashdashspace"
    },
    {
      "name": "title",
      "description": "Optional dialog window title",
      "type": "string",
      "backing_type": "string",
      "arity": "optional",
      "mode": "dashdashspace"
    },
    {
      "name": "default",
      "description": "Default value for entry dialogs",
      "type": "string",
      "backing_type": "string",
      "arity": "optional",
      "mode": "dashdashspace"
    },
    {
      "name": "options",
      "description": "For list dialogs: pipe-separated options (e.g., 'Option A|Option B|Option C')",
      "type": "string",
      "backing_type": "string",
      "arity": "optional",
      "mode": "dashdashspace"
    },
    {
      "name": "multiple",
      "description": "For list dialogs: allow multiple selections (true/false)",
      "type": "boolean",
      "backing_type": "string",
      "arity": "optional",
      "mode": "dashdashspace"
    }
  ],
  "empty-result": { "tag": "AddMessage", "contents": "User cancelled the dialog or no input was provided" }
}
EOF
    exit 0
fi

# Check for zenity availability
if ! command -v zenity &> /dev/null; then
    echo "Error: zenity is not installed. Please install zenity to use this tool." >&2
    exit 1
fi

# Initialize variables
DIALOG_TYPE=""
TEXT=""
TITLE=""
DEFAULT=""
OPTIONS=""
MULTIPLE="false"

# Parse arguments (skipping 'run')
while [[ $# -gt 0 ]]; do
    case $1 in
        --type)
            DIALOG_TYPE="$2"
            shift 2
            ;;
        --text)
            TEXT="$2"
            shift 2
            ;;
        --title)
            TITLE="$2"
            shift 2
            ;;
        --default)
            DEFAULT="$2"
            shift 2
            ;;
        --options)
            OPTIONS="$2"
            shift 2
            ;;
        --multiple)
            MULTIPLE="$2"
            shift 2
            ;;
        run)
            shift
            ;;
        *)
            shift
            ;;
    esac
done

# Validate required arguments
if [ -z "$DIALOG_TYPE" ]; then
    echo "Error: --type is required" >&2
    exit 1
fi

if [ -z "$TEXT" ]; then
    echo "Error: --text is required" >&2
    exit 1
fi

# Build zenity command based on dialog type
case "$DIALOG_TYPE" in
    entry)
        # Text entry dialog
        ZENITY_CMD=(zenity --entry --text="$TEXT")
        [ -n "$TITLE" ] && ZENITY_CMD+=(--title="$TITLE")
        [ -n "$DEFAULT" ] && ZENITY_CMD+=(--entry-text="$DEFAULT")
        
        # Execute and capture output
        if OUTPUT=$("${ZENITY_CMD[@]}" 2>/dev/null); then
            echo "$OUTPUT"
            exit 0
        else
            exit 1
        fi
        ;;
        
    question)
        # Yes/No question dialog
        ZENITY_CMD=(zenity --question --text="$TEXT")
        [ -n "$TITLE" ] && ZENITY_CMD+=(--title="$TITLE")
        
        # Execute and return yes/no based on exit code
        if "${ZENITY_CMD[@]}" 2>/dev/null; then
            echo "yes"
            exit 0
        else
            echo "no"
            exit 0
        fi
        ;;
        
    list)
        # List selection dialog
        if [ -z "$OPTIONS" ]; then
            echo "Error: --options is required for list dialogs" >&2
            exit 1
        fi
        
        ZENITY_CMD=(zenity --list --text="$TEXT" --column="Options")
        [ -n "$TITLE" ] && ZENITY_CMD+=(--title="$TITLE")
        
        if [ "$MULTIPLE" == "true" ]; then
            ZENITY_CMD+=(--multiple --separator="|")
        fi
        
        # Parse pipe-separated options into array
        IFS='|' read -ra OPT_ARRAY <<< "$OPTIONS"
        for opt in "${OPT_ARRAY[@]}"; do
            ZENITY_CMD+=("$opt")
        done
        
        # Execute and capture output
        if OUTPUT=$("${ZENITY_CMD[@]}" 2>/dev/null); then
            echo "$OUTPUT"
            exit 0
        else
            exit 1
        fi
        ;;
        
    file)
        # File selection dialog
        ZENITY_CMD=(zenity --file-selection)
        [ -n "$TEXT" ] && ZENITY_CMD+=(--text="$TEXT")
        [ -n "$TITLE" ] && ZENITY_CMD+=(--title="$TITLE")
        
        # Execute and capture output
        if OUTPUT=$("${ZENITY_CMD[@]}" 2>/dev/null); then
            echo "$OUTPUT"
            exit 0
        else
            exit 1
        fi
        ;;
        
    *)
        echo "Error: Unknown dialog type '$DIALOG_TYPE'. Supported types: entry, question, list, file" >&2
        exit 1
        ;;
esac

