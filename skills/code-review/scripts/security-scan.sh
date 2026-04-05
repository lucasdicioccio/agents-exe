#!/bin/bash
set -euo pipefail

# Security Scan Script
# Usage: security-scan.sh <file_path>

if [ "$#" -eq 1 ] && [ "$1" == "describe" ]; then
    echo '{
        "slug": "security-scan",
        "description": "Scan code for common security issues",
        "args": [
            {"name": "file_path", "description": "Path to file to scan", "type": "string", "required": true}
        ]
    }'
    exit 0
fi

if [ "$#" -lt 1 ]; then
    echo "Usage: $0 <file_path>" >&2
    exit 1
fi

file_path="$1"

if [ ! -f "$file_path" ]; then
    echo "Error: File not found: $file_path" >&2
    exit 1
fi

echo "Security scan of: $file_path"
echo ""

# Check for potential secrets
issues=0

if grep -iE "(password|secret|token|key)\s*=" "$file_path" 2>/dev/null | grep -v "#" | head -5; then
    echo "WARNING: Potential hardcoded secrets detected"
    issues=$((issues + 1))
fi

# Check for SQL injection patterns
if grep -iE "(SELECT|INSERT|UPDATE|DELETE).*%s" "$file_path" 2>/dev/null; then
    echo "WARNING: Potential SQL injection vulnerability"
    issues=$((issues + 1))
fi

if [ "$issues" -eq 0 ]; then
    echo "No obvious security issues found"
else
    echo ""
    echo "Found $issues potential issue(s)"
fi

