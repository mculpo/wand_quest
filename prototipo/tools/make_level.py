#!/usr/bin/env python3
"""
Converte levels/level1.txt (ASCII) nos dados da fase para o jogo.

Gera:
  - src/data/level1.asm : mapa de 64x48 metatiles (1 byte cada), posição
                          inicial do agente e lista de inimigos
  - tools/level1_preview.png : a fase inteira desenhada (1024x768), com os
                          inimigos marcados; também é usada pelo teste de
                          scroll (tests/scroll_test.lua) para conferir a tela

Regras:
  - '#' vira MT_WALL_TOP quando a célula de cima não é sólida (piso aceso),
    senão MT_WALL;
  - P, r, t, d e X ficam em cima de fundo (MT_BACK); X vira MT_EXIT.

Uso: python tools/make_level.py
"""

import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from nes_common import ROOT, parse, rgb, write_asm  # noqa: E402
import draw_tiles  # noqa: E402

SRC = os.path.join(ROOT, "levels", "level1.txt")
ASM = os.path.join(ROOT, "src", "data", "level1.asm")
PREVIEW = os.path.join(ROOT, "tools", "level1_preview.png")

W, H = 64, 48
MT = {name: i for i, (name, *_rest) in enumerate(draw_tiles.METATILES)}
CHAR_TO_MT = {c: MT[name] for name, c, *_ in draw_tiles.METATILES if c}
ENEMY_TYPES = {"r": 1, "t": 2, "d": 3}          # Iguais a EnemyType em inc/consts.inc
ENEMY_NAMES = {1: "soldado", 2: "torreta", 3: "drone"}
SOLID_CHARS = set("#^")


def load():
    rows = [line.rstrip("\n") for line in open(SRC, encoding="utf-8") if not line.startswith(";")]
    rows = [r for r in rows if r]
    if len(rows) != H or any(len(r) != W for r in rows):
        raise SystemExit(f"{SRC}: precisa ter {H} linhas de {W} caracteres (tem {len(rows)})")
    return rows


def convert(rows):
    grid = [[0] * W for _ in range(H)]
    enemies, start = [], None
    for y, row in enumerate(rows):
        for x, ch in enumerate(row):
            if ch == "#":
                above = rows[y - 1][x] if y > 0 else "#"
                grid[y][x] = MT["WALL"] if above in SOLID_CHARS else MT["WALL_TOP"]
            elif ch in CHAR_TO_MT:
                grid[y][x] = CHAR_TO_MT[ch]
            elif ch == "P":
                start = (x, y)
                grid[y][x] = MT["BACK"]
            elif ch in ENEMY_TYPES:
                enemies.append((x, y, ENEMY_TYPES[ch]))
                grid[y][x] = MT["BACK"]
            else:
                raise SystemExit(f"caractere desconhecido {ch!r} em ({x}, {y})")
    if start is None:
        raise SystemExit("a fase não tem 'P' (início do agente)")
    return grid, enemies, start


def write(grid, enemies, start):
    sx, sy = start
    out = [
        ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;",
        ";; Fase 1. Gerado por tools/make_level.py a partir de levels/level1.txt",
        ";; (não edite à mão). O mapa tem LEVEL_W x LEVEL_H metatiles, linha por linha.",
        ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;",
        "",
        f"LEVEL_W = {W}",
        f"LEVEL_H = {H}",
        "",
        ";; Início do agente em pixels (canto superior esquerdo da caixa 16x24:",
        ";; os pés ficam no chão da célula marcada com P)",
        f"PLAYER_START_X = {sx * 16}",
        f"PLAYER_START_Y = {sy * 16 + 16 - 24}",
        "",
        f"ENEMY_LIST_COUNT = {len(enemies)}",
        ";; Inimigos da fase: coluna e linha (em metatiles) e tipo (EnemyType)",
        "EnemyListCol:  .byte " + ", ".join(str(x) for x, _, _ in enemies),
        "EnemyListRow:  .byte " + ", ".join(str(y) for _, y, _ in enemies),
        "EnemyListType: .byte " + ", ".join(str(t) for _, _, t in enemies),
        "",
        "LevelMap:",
    ]
    for y, row in enumerate(grid):
        out.append("    .byte " + ",".join(str(v) for v in row) + f"   ; {y}")
    write_asm(ASM, out)


def preview(grid, enemies, start):
    try:
        from PIL import Image, ImageDraw
    except ImportError:
        return
    img = Image.new("RGB", (W * 16, H * 16))
    px = img.load()
    arts = [(parse(art), draw_tiles.BG_PALETTES[pal]) for _, _, pal, _, art in draw_tiles.METATILES]
    for my in range(H):
        for mx in range(W):
            art, pal = arts[grid[my][mx]]
            for y in range(16):
                for x in range(16):
                    px[mx * 16 + x, my * 16 + y] = rgb(pal, art[y][x])
    draw = ImageDraw.Draw(img)
    for x, y, t in enemies:
        draw.rectangle([x * 16 + 2, y * 16 + 2, x * 16 + 13, y * 16 + 13], outline=(255, 64, 64))
        draw.text((x * 16 + 4, y * 16 + 2), "rtd"[t - 1], fill=(255, 128, 128))
    sx, sy = start
    draw.rectangle([sx * 16 + 2, sy * 16 - 8, sx * 16 + 13, sy * 16 + 15], outline=(64, 255, 255))
    img.save(PREVIEW)


def main():
    rows = load()
    grid, enemies, start = convert(rows)
    write(grid, enemies, start)
    preview(grid, enemies, start)
    counts = {}
    for _, _, t in enemies:
        counts[ENEMY_NAMES[t]] = counts.get(ENEMY_NAMES[t], 0) + 1
    print(f"Fase 1: {W}x{H} metatiles, início em {start}, inimigos: {counts}")


if __name__ == "__main__":
    main()
