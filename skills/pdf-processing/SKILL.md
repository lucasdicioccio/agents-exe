---
name: pdf-processing
description: Extract text and tables from PDF files, fill PDF forms, and merge multiple PDFs. Use when working with PDF documents or when the user mentions PDFs, forms, or document extraction.
license: Apache-2.0
compatibility: Requires poppler-utils (pdftotext, pdftoppm) and optionally qpdf
metadata:
  author: agents-exe
  version: "1.0"
---

# PDF Processing

This skill helps you work with PDF documents efficiently.

## Common Tasks

### Extract Text
Use the `extract-text` script to extract plain text from PDFs:
```bash
./scripts/extract-text.sh document.pdf
```

### Convert to Images
For visual analysis or OCR, convert pages to images:
```bash
./scripts/to-images.sh document.pdf output/
```

### Merge PDFs
Combine multiple PDFs into one:
```bash
./scripts/merge.sh output.pdf input1.pdf input2.pdf ...
```

## References
- [PDF Formats Guide](references/formats.md) - Common PDF formats explained
- [Troubleshooting](references/troubleshooting.md) - Common issues and solutions

