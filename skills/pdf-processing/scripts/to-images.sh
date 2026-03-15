#!/bin/bash
set -euo pipefail

# PDF to Images Conversion Script
# Usage: to-images.sh <pdf_path> <output_dir>

if [ "$#" -eq 1 ] && [ "$1" == "describe" ]; then
    echo '{
        "slug": "to-images",
        "description": "Convert PDF pages to PNG images",
        "args": [
            {"name": "pdf_path", "description": "Path to PDF file", "type": "string", "required": true},
            {"name": "output_dir", "description": "Output directory for images", "type": "string", "required": true}
        ]
    }'
    exit 0
fi

if [ "$#" -lt 2 ]; then
    echo "Usage: $0 <pdf_path> <output_dir>" >&2
    exit 1
fi

pdf_path="$1"
output_dir="$2"

if [ ! -f "$pdf_path" ]; then
    echo "Error: PDF file not found: $pdf_path" >&2
    exit 1
fi

mkdir -p "$output_dir"
pdftoppm -png "$pdf_path" "$output_dir/page"

echo "Images saved to: $output_dir"

