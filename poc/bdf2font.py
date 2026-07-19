#!/usr/bin/env python3
"""bdf2font.py — convert a BDF bitmap font into a tiny .lolf blob for canvas.lisp.

Why this exists (doc/code-browser.org §6.2, doc/fonts.org): we bake a permissive
bitmap font into the tool like alien.rgba, and we want to control the
codepoint->glyph index ourselves — PSF's implicit glyph ordering is what caused
the "·"->"Ö" class of bug. So this reads a BDF (honoring each glyph's advance
width, so the font renders TIGHT) and writes glyphs indexed by their real Unicode
codepoint.

The .lolf format (read by load-font in poc/canvas.lisp):
    byte 0-1 : magic  0x4C 0x46  ("LF")
    byte 2   : glyph WIDTH  in pixels (1..8 for the current 1-byte-per-row blitter)
    byte 3   : glyph HEIGHT in pixels
    byte 4.. : 256 glyphs, each HEIGHT bytes (1 byte per row, MSB = leftmost pixel),
               indexed by codepoint 0..255 — so #\· (U+00B7 = 183) IS the middle dot.

Usage:  python3 poc/bdf2font.py cozette.bdf poc/cozette.lolf
(Fetch a BDF, e.g. Cozette:
   curl -fsSL -o cozette.bdf \\
     https://github.com/slavfox/Cozette/releases/latest/download/cozette.bdf )
"""
import sys, struct

def parse_bdf(path):
    text = open(path, encoding="latin-1").read().splitlines()
    fbb = next(l.split() for l in text if l.startswith("FONTBOUNDINGBOX"))
    fbbh, fbbyo = int(fbb[2]), int(fbb[4])
    cell_h, ascent = fbbh, fbbh + fbbyo            # baseline row, from the top
    chars, dwidths = {}, []
    i, N = 0, len(text)
    while i < N:
        if text[i].startswith("STARTCHAR"):
            code, dw, bbx, rows = None, None, None, []
            i += 1
            while i < N and not text[i].startswith("ENDCHAR"):
                t = text[i]
                if   t.startswith("ENCODING"): code = int(t.split()[1])
                elif t.startswith("DWIDTH"):   dw   = int(t.split()[1])
                elif t.startswith("BBX"):
                    p = t.split(); bbx = tuple(int(x) for x in p[1:5])
                elif t == "BITMAP":
                    i += 1
                    while i < N and not text[i].startswith("ENDCHAR") and text[i].strip():
                        try: rows.append(int(text[i].strip(), 16))
                        except ValueError: pass
                        i += 1
                    continue
                i += 1
            if code is not None and 0 <= code and bbx:
                chars[code] = (dw, bbx, rows)
                if 0x20 <= code <= 0x7e and dw: dwidths.append(dw)
        i += 1
    cell_w = max(set(dwidths), key=dwidths.count) if dwidths else int(fbb[1])
    return cell_w, cell_h, ascent, chars

def glyph_cell(cell_w, cell_h, ascent, entry):
    "Render one BDF glyph into a cell_h x cell_w grid of 0/1, baseline-aligned."
    _dw, (bbw, bbh, bxoff, byoff), rows = entry
    cell = [[0]*cell_w for _ in range(cell_h)]
    top, bits = ascent - (bbh + byoff), ((bbw + 7)//8)*8
    for r in range(min(bbh, len(rows))):
        val = rows[r]
        for c in range(bbw):
            if (val >> (bits-1-c)) & 1:
                y, x = top + r, bxoff + c
                if 0 <= y < cell_h and 0 <= x < cell_w:
                    cell[y][x] = 1
    return cell

def main(bdf_path, out_path):
    cell_w, cell_h, ascent, chars = parse_bdf(bdf_path)
    if cell_w > 8:
        sys.exit(f"width {cell_w} > 8: the 1-byte-per-row blitter can't render it yet "
                 f"(see doc/fonts.org §7 Path B).")
    blank = [[0]*cell_w for _ in range(cell_h)]
    out = bytearray([0x4C, 0x46, cell_w, cell_h])
    for cp in range(256):
        cell = glyph_cell(cell_w, cell_h, ascent, chars[cp]) if cp in chars else blank
        for row in cell:                            # pack: MSB = leftmost pixel
            out.append(sum(bit << (7 - c) for c, bit in enumerate(row)))
    open(out_path, "wb").write(out)
    n = sum(1 for cp in range(256) if cp in chars)
    print(f"wrote {out_path}: {cell_w}x{cell_h}, {n}/256 codepoints, {len(out)} bytes")

if __name__ == "__main__":
    if len(sys.argv) != 3:
        sys.exit("usage: bdf2font.py <in.bdf> <out.lolf>")
    main(sys.argv[1], sys.argv[2])
