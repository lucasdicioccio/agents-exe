#!/usr/bin/env python3
"""Draws one terminal screen as a PNG: the screenshots of the TUI documentation.

    render_png.py CELLS.json OUT.png

CELLS.json is what tuispec serialises a snapshot to: {"rows", "cols",
"defaultFg", "defaultBg", "cells": [[[codepoint, [r,g,b], [r,g,b]], ...], ...]}.
The agents-tui-e2e suite calls this with AGENTS_TUI_E2E_PNG set.

Three things a terminal does and a plain "draw each character" loop does not:

- the grid is the font's advance and line height, so neighbouring cells touch;
- box-drawing characters are drawn as lines from edge to edge of their cell,
  whatever the font, so borders are continuous;
- a character the font lacks (the status icons of the conversation list) is
  taken from the first fallback font that has it, scaled to fit the cell.

The font is TUISPEC_FONT_PATH when set, else the first of FONTS found. The
colours are the terminal's sixteen, mapped to PALETTE where tuispec's own
(xterm's) are hard to read on a dark background.
"""
import json
import os
import sys

from PIL import Image, ImageDraw, ImageFont

SIZE = 16
PADDING = 12

FONTS = [
    "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf",
    "/usr/share/fonts/dejavu/DejaVuSansMono.ttf",
    "/usr/share/fonts/TTF/DejaVuSansMono.ttf",
    "/usr/share/fonts/truetype/liberation/LiberationMono-Regular.ttf",
    "/usr/share/fonts/truetype/liberation2/LiberationMono-Regular.ttf",
    "/usr/share/fonts/liberation/LiberationMono-Regular.ttf",
    "/System/Library/Fonts/Menlo.ttc",
]

# Tried in order for a character the main font has no glyph for.
FALLBACK_FONTS = [
    "/usr/share/fonts/truetype/noto/NotoSansSymbols2-Regular.ttf",
    "/usr/share/fonts/noto/NotoSansSymbols2-Regular.ttf",
    "/usr/share/fonts/truetype/noto/NotoSansMath-Regular.ttf",
    "/usr/share/fonts/noto/NotoSansMath-Regular.ttf",
    "/usr/share/fonts/truetype/noto/NotoSansSymbols-Regular.ttf",
    "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf",
    "/usr/share/fonts/dejavu/DejaVuSans.ttf",
    "/usr/share/fonts/TTF/DejaVuSans.ttf",
]

# xterm's colours, as tuispec's dark theme emits them, to ones that read well
# on the background below. Anything else is kept as it is.
PALETTE = {
    (0, 0, 0): (13, 17, 23),
    (205, 0, 0): (248, 113, 113),
    (0, 205, 0): (86, 211, 100),
    (205, 205, 0): (227, 179, 65),
    (0, 0, 238): (56, 110, 220),
    (205, 0, 205): (210, 130, 240),
    (0, 205, 205): (86, 212, 221),
    (229, 229, 229): (220, 224, 230),
    (127, 127, 127): (139, 148, 158),
    (255, 0, 0): (255, 123, 114),
    (0, 255, 0): (126, 231, 135),
    (255, 255, 0): (240, 200, 90),
    (92, 92, 255): (121, 170, 255),
    (255, 0, 255): (226, 160, 255),
    (0, 255, 255): (150, 230, 240),
}

# Box-drawing characters as the arms they have: (left, right, up, down).
BOX = {
    0x2500: (1, 1, 0, 0), 0x2502: (0, 0, 1, 1),
    0x250C: (0, 1, 0, 1), 0x2510: (1, 0, 0, 1),
    0x2514: (0, 1, 1, 0), 0x2518: (1, 0, 1, 0),
    0x251C: (0, 1, 1, 1), 0x2524: (1, 0, 1, 1),
    0x252C: (1, 1, 0, 1), 0x2534: (1, 1, 1, 0),
    0x253C: (1, 1, 1, 1),
    0x256D: (0, 1, 0, 1), 0x256E: (1, 0, 0, 1),
    0x2570: (0, 1, 1, 0), 0x256F: (1, 0, 1, 0),
}


def first_existing(paths):
    return next((p for p in paths if os.path.isfile(p)), None)


def has_glyph(font, ch):
    """Whether the font draws ch as something else than its missing-glyph box."""
    try:
        return font.getmask(ch).getbbox() is not None and bytes(font.getmask(ch)) != bytes(font.getmask("￿"))
    except Exception:
        return False


def colour(rgb):
    rgb = tuple(rgb)
    return PALETTE.get(rgb, rgb)


def main():
    inp, out = sys.argv[1], sys.argv[2]
    font_path = os.environ.get("TUISPEC_FONT_PATH") or first_existing(FONTS)
    if not font_path or not os.path.isfile(font_path):
        sys.exit("no monospace font found: set TUISPEC_FONT_PATH to a .ttf")
    font = ImageFont.truetype(font_path, SIZE)
    fallbacks = [p for p in FALLBACK_FONTS if os.path.isfile(p)]

    with open(inp, encoding="utf-8") as f:
        payload = json.load(f)
    rows, cols, cells = payload["rows"], payload["cols"], payload["cells"]
    default_fg = payload.get("defaultFg", [229, 229, 229])
    default_bg = payload.get("defaultBg", [0, 0, 0])

    ascent, descent = font.getmetrics()
    cell_w = max(1, round(font.getlength("M")))
    cell_h = ascent + descent
    stroke = max(1, SIZE // 12)

    img = Image.new("RGB", (cols * cell_w + 2 * PADDING, rows * cell_h + 2 * PADDING), colour(default_bg))
    draw = ImageDraw.Draw(img)

    known = {}  # character -> (font, x offset, y offset), None when nothing draws it

    def glyph(ch):
        if ch not in known:
            known[ch] = None
            if has_glyph(font, ch):
                known[ch] = (font, 0, 0)
            else:
                for path in fallbacks:
                    size = SIZE
                    candidate = ImageFont.truetype(path, size)
                    if not has_glyph(candidate, ch):
                        continue
                    # shrink until the ink about fits the cell (symbols are
                    # wider than letters, and readable only if left a little
                    # room over the edges), then centre it
                    while size > 6:
                        box = candidate.getbbox(ch)
                        if box[2] - box[0] <= cell_w * 1.3 and box[3] - box[1] <= cell_h - 2:
                            break
                        size -= 1
                        candidate = ImageFont.truetype(path, size)
                    box = candidate.getbbox(ch)
                    dx = (cell_w - (box[2] - box[0])) // 2 - box[0]
                    dy = (cell_h - (box[3] - box[1])) // 2 - box[1]
                    known[ch] = (candidate, dx, dy)
                    break
        return known[ch]

    def each_cell():
        for r in range(rows):
            row = cells[r] if r < len(cells) else []
            for c in range(cols):
                code, fg, bg = row[c] if c < len(row) and len(row[c]) == 3 else (32, default_fg, default_bg)
                yield PADDING + c * cell_w, PADDING + r * cell_h, code, colour(fg), colour(bg)

    # every background first: a glyph may reach a little over its neighbours
    for x, y, _, _, bg in each_cell():
        draw.rectangle((x, y, x + cell_w - 1, y + cell_h - 1), fill=bg)
    for x, y, code, fg, _ in each_cell():
        if code == 32:
            continue
        if code in BOX:
            left, right, up, down = BOX[code]
            cx, cy = x + (cell_w - stroke) // 2, y + (cell_h - stroke) // 2
            if left:
                draw.rectangle((x, cy, cx + stroke - 1, cy + stroke - 1), fill=fg)
            if right:
                draw.rectangle((cx, cy, x + cell_w - 1, cy + stroke - 1), fill=fg)
            if up:
                draw.rectangle((cx, y, cx + stroke - 1, cy + stroke - 1), fill=fg)
            if down:
                draw.rectangle((cx, cy, cx + stroke - 1, y + cell_h - 1), fill=fg)
            continue
        found = glyph(chr(code))
        if found is None:
            draw.text((x, y), chr(code), fill=fg, font=font)
        else:
            use, dx, dy = found
            draw.text((x + dx, y + dy), chr(code), fill=fg, font=use)

    os.makedirs(os.path.dirname(os.path.abspath(out)), exist_ok=True)
    img.save(out, "PNG", optimize=True)


if __name__ == "__main__":
    main()
