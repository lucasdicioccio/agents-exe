#!/bin/bash
set -euo pipefail

# PDF Text Extraction Script
# Usage: extract-text.sh run --pdf_path <pdf_path>

if [ "$#" -eq 1 ] && [ "$1" == "describe" ]; then
    echo '{
        "slug": "extract-text",
        "description": "Extract plain text from a PDF file",
        "args": [
            {"name": "pdf_path", "description": "Path to PDF file", "type": "string", "backing_type": "string", "arity": "single", "mode": "dashdashspace"}
        ]
    }'
    exit 0
fi

# Parse arguments for run command
pdf_path=""
while [[ $# -gt 0 ]]; do
    case $1 in
        --pdf_path)
            pdf_path="$2"
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

if [ -z "$pdf_path" ]; then
    echo "Usage: $0 run --pdf_path <pdf_path>" >&2
    exit 1
fi

if [ ! -f "$pdf_path" ]; then
    echo "Error: File not found: $pdf_path" >&2
    exit 1
fi

pdftotext "$pdf_path" -

