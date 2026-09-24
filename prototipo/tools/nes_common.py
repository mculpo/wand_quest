"""
Funções comuns das ferramentas do protótipo (desenho de tiles para a NES).

- Os desenhos são listas de strings: '.' = cor 0 (transparente/fundo),
  '1', '2', '3' = cores 1-3 da paleta. Nos sprites também dá para usar
  'o' (= 1), 'b' (= 2) e 's' (= 3), como no Wand Quest.
- encode() transforma um tile 8x8 no formato da NES (2 planos de 8 bytes).
- NES_RGB é a paleta de cores da NES, usada nas prévias em PNG.
"""

import os

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

CHAR_COLOR = {".": 0, "0": 0, "1": 1, "2": 2, "3": 3, "o": 1, "b": 2, "s": 3}


def parse(art):
    """Desenho em strings -> matriz de índices de cor."""
    return [[CHAR_COLOR[ch] for ch in row] for row in art]


def blank(w, h):
    return [[0] * w for _ in range(h)]


def put(canvas, art, x0, y0, transparent=True):
    """Desenha `art` (strings) em `canvas` com o canto em (x0, y0)."""
    for dy, row in enumerate(art):
        for dx, ch in enumerate(row):
            c = CHAR_COLOR[ch]
            if c == 0 and transparent:
                continue
            x, y = x0 + dx, y0 + dy
            if 0 <= y < len(canvas) and 0 <= x < len(canvas[0]):
                canvas[y][x] = c


def dot(canvas, x, y, c):
    if 0 <= y < len(canvas) and 0 <= x < len(canvas[0]):
        canvas[y][x] = CHAR_COLOR[c] if isinstance(c, str) else c


def tile_at(canvas, tx, ty):
    """Tile 8x8 (tupla de tuplas) na posição de tile (tx, ty) do canvas."""
    return tuple(tuple(canvas[ty * 8 + y][tx * 8 + x] for x in range(8)) for y in range(8))


def mirror(px):
    return tuple(tuple(reversed(row)) for row in px)


def encode(px):
    """Tile 8x8 de índices de cor -> 16 bytes no formato da NES."""
    p0 = bytearray(8)
    p1 = bytearray(8)
    for y in range(8):
        for x in range(8):
            c = px[y][x]
            if c & 1:
                p0[y] |= 0x80 >> x
            if c & 2:
                p1[y] |= 0x80 >> x
    return bytes(p0 + p1)


def write_asm(path, lines):
    with open(path, "w", encoding="utf-8", newline="\n") as f:
        f.write("\n".join(lines) + "\n")


# Paleta RGB da NES (2C02), índice = cor da NES ($00-$3F)
NES_RGB = [
    (84, 84, 84), (0, 30, 116), (8, 16, 144), (48, 0, 136), (68, 0, 100), (92, 0, 48), (84, 4, 0), (60, 24, 0),
    (32, 42, 0), (8, 58, 0), (0, 64, 0), (0, 60, 0), (0, 50, 60), (0, 0, 0), (0, 0, 0), (0, 0, 0),
    (152, 150, 152), (8, 76, 196), (48, 50, 236), (92, 30, 228), (136, 20, 176), (160, 20, 100), (152, 34, 32), (120, 60, 0),
    (84, 90, 0), (40, 114, 0), (8, 124, 0), (0, 118, 40), (0, 102, 120), (0, 0, 0), (0, 0, 0), (0, 0, 0),
    (236, 238, 236), (76, 154, 236), (120, 124, 236), (176, 98, 236), (228, 84, 236), (236, 88, 180), (236, 106, 100), (212, 136, 32),
    (160, 170, 0), (116, 196, 0), (76, 208, 32), (56, 204, 108), (56, 180, 204), (60, 60, 60), (0, 0, 0), (0, 0, 0),
    (236, 238, 236), (168, 204, 236), (188, 188, 236), (212, 178, 236), (236, 174, 236), (236, 174, 212), (236, 180, 176), (228, 196, 144),
    (204, 210, 120), (180, 222, 120), (168, 226, 144), (152, 226, 180), (160, 214, 228), (160, 162, 160), (0, 0, 0), (0, 0, 0),
]


def rgb(palette4, index):
    """Cor RGB do índice 0-3 numa paleta de 4 cores da NES."""
    return NES_RGB[palette4[index] & 0x3F]
