#!/bin/bash
set -euo pipefail

# PDF Text Extraction Script
# Usage: extract-text.sh <pdf_path>

if [ "$#" -eq 1 ] && [ "$1" == "describe" ]; then
    echo '{
        "slug": "extract-text",
        "description": "Extract plain text from a PDF file",
        "args": [
            {"name": "pdf_path", "description": "Path to PDF file", "type": "string", "required": true}
        ]
    }'
    exit 0
fi

if [ "$#" -lt 1 ]; then
    echo "Usage: $0 <pdf_path>" >&2
    exit 1
fi

pdf_path="$1"

if [ ! -f "$pdf_path" ]; then
    echo "Error: File not found: $pdf_path" >&2
    exit 1
fi

pdftotext "$pdf_path" -

