---
name: code-review
description: Perform structured code reviews following best practices. Use when reviewing pull requests, auditing code, or evaluating implementation quality.
license: MIT
metadata:
  category: development
  version: "1.0"
---

# Code Review

Perform comprehensive code reviews with consistent criteria.

## Review Checklist

### Security
- [ ] No hardcoded secrets or credentials
- [ ] Input validation on all external data
- [ ] Proper error handling (no info leakage)

### Maintainability
- [ ] Clear naming conventions
- [ ] Adequate documentation/comments
- [ ] No code duplication (DRY principle)

### Performance
- [ ] No obvious bottlenecks
- [ ] Efficient algorithms for data size
- [ ] Resource cleanup (files, connections)

## Using This Skill

1. First, analyze the changes:
   ```bash
   ./scripts/analyze-changes.sh <file_path>
   ```

2. Check for common issues:
   ```bash
   ./scripts/security-scan.sh <file_path>
   ```

3. Generate review summary

