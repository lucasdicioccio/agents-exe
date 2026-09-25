#!/usr/bin/env python3
"""Converts GFM-style pipe tables in markdown (read from stdin) into raw
HTML <table> blocks, leaving everything else untouched.

Kitchen-Sink's cmark renderer has no table support (tables aren't in vanilla
CommonMark), so a mirrored spec that contains pipe tables needs them turned
into HTML explicitly. A raw HTML block skips inline-markdown processing
entirely, so cell inline formatting (code spans, bold) is converted by hand
here rather than left as markdown for the renderer to skip.
"""
import re
import sys


def esc(s):
    return s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")


_CODE_SPAN = re.compile(r"``\s?(.+?)\s?``|`([^`]+)`")


def inline(s):
    s = esc(s)
    s = re.sub(r"\*\*(.+?)\*\*", r"<strong>\1</strong>", s)
    s = re.sub(r"\*(.+?)\*", r"<em>\1</em>", s)
    # One pass, preferring the double-backtick alternative first, so a span
    # like ``"n: `$x`"`` (whose content has a literal backtick) isn't split
    # apart and re-processed by a separate single-backtick pass.
    s = _CODE_SPAN.sub(lambda m: f"<code>{m.group(1) or m.group(2)}</code>", s)
    return s


def split_row(line):
    line = line.strip()
    if line.startswith("|"):
        line = line[1:]
    if line.endswith("|"):
        line = line[:-1]
    return [c.strip() for c in re.split(r"(?<!\\)\|", line)]


def is_delimiter_row(line):
    line = line.strip()
    if "-" not in line or "|" not in line:
        return False
    cells = split_row(line)
    return bool(cells) and all(re.fullmatch(r":?-{1,}:?", c) for c in cells)


def convert(text):
    lines = text.split("\n")
    out = []
    i, n = 0, len(lines)
    while i < n:
        line = lines[i]
        if line.strip().startswith("|") and i + 1 < n and is_delimiter_row(lines[i + 1]):
            header = split_row(line)
            i += 2
            rows = []
            while i < n and lines[i].strip().startswith("|"):
                rows.append(split_row(lines[i]))
                i += 1
            out.append("<table>")
            out.append("<tr>" + "".join(f"<th>{inline(h)}</th>" for h in header) + "</tr>")
            for row in rows:
                out.append("<tr>" + "".join(f"<td>{inline(c)}</td>" for c in row) + "</tr>")
            out.append("</table>")
            out.append("")
        else:
            out.append(line)
            i += 1
    return "\n".join(out)


if __name__ == "__main__":
    sys.stdout.write(convert(sys.stdin.read()))
