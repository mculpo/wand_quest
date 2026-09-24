#!/usr/bin/env python3
"""
Gera o BackgroundData.asm: o nametable 0 com a moldura da tela.

Layout (em tiles de 8x8; a tela tem 32 x 30):
  - Parede de tijolos em aparelho corrido em toda a volta.
  - Área de jogo (colunas 4-27, linhas 4-25) vazia: cada fase é desenhada
    nela pelo efeito de transição (src/effects.asm).
  - Friso em volta da área de jogo (colunas 3 e 28, linhas 3 e 26).
  - "WAND QUEST" numa faixa preta na linha 1 e o número da fase na linha 28
    (a faixa de baixo é reescrita pelo jogo a cada fase: HUD_ROW).
  - Rebites redondos nos quatro cantos e no meio das laterais.
  - Atributos: área de jogo na paleta de fundo 1, moldura na paleta 0.
    As duas paletas mudam a cada dificuldade (TierPalettes em src/effects.asm).

Os tiles vêm do tools/add_tiles.py. Uso:
  python tools/make_background.py
"""

import os

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
OUT = os.path.join(ROOT, "BackgroundData.asm")

BRICK_A, BRICK_B = 0x60, 0x61
BAND_TOP, BAND_BOTTOM, BAND_LEFT, BAND_RIGHT = 0x62, 0x63, 0x64, 0x65
CORNER_TL, CORNER_TR, CORNER_BL, CORNER_BR = 0x66, 0x67, 0x68, 0x69
STUD = 0x6A                     # 4 tiles: $6A $6B / $6C $6D
EMPTY = 0x00

TITLE = "WAND QUEST"
TITLE_ROW = 1
HUD_ROW = 28                    # Tem que bater com HUD_ROW em inc/consts.inc
HUD_TEXT = "FASE 01"            # Reescrito pelo jogo


def build():
    nt = [[0] * 32 for _ in range(30)]

    # Tijolos: cada fileira desloca meio tijolo (A nas colunas pares ou ímpares)
    for r in range(30):
        for c in range(32):
            nt[r][c] = BRICK_A if (r + c) % 2 == 0 else BRICK_B

    # Área de jogo vazia
    for r in range(4, 26):
        for c in range(4, 28):
            nt[r][c] = EMPTY

    # Friso
    for c in range(4, 28):
        nt[3][c] = BAND_TOP
        nt[26][c] = BAND_BOTTOM
    for r in range(4, 26):
        nt[r][3] = BAND_LEFT
        nt[r][28] = BAND_RIGHT
    nt[3][3], nt[3][28] = CORNER_TL, CORNER_TR
    nt[26][3], nt[26][28] = CORNER_BL, CORNER_BR

    # Faixas pretas com texto (um tile vazio de margem de cada lado)
    def strip(row, text):
        col = (32 - len(text)) // 2
        for c in range(col - 1, col + len(text) + 1):
            nt[row][c] = EMPTY
        for i, ch in enumerate(text):
            nt[row][col + i] = ord(ch)
    strip(TITLE_ROW, TITLE)
    strip(HUD_ROW, HUD_TEXT)

    # Rebites 16x16
    def stud(row, col):
        nt[row][col], nt[row][col + 1] = STUD, STUD + 1
        nt[row + 1][col], nt[row + 1][col + 1] = STUD + 2, STUD + 3
    for row in (1, 14, 27):
        stud(row, 1)
        stud(row, 29)

    # Atributos: um par de bits por metatile 16x16 (4 por byte)
    attr = [0] * 64
    for mr in range(2, 13):
        for mc in range(2, 14):
            byte = (mr // 2) * 8 + (mc // 2)
            shift = ((mr % 2) * 2 + (mc % 2)) * 2
            attr[byte] |= 1 << shift

    data = [b for row in nt for b in row] + attr
    assert len(data) == 1024
    return data


def main():
    data = build()
    lines = [
        ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;",
        ";; Nametable 0 (32x30 tiles + 64 bytes de atributos): a moldura da tela.",
        ";; Gerado por tools/make_background.py (não edite à mão).",
        ";; A área de jogo (tiles 4-27 x 4-25) fica vazia: cada fase é desenhada nela",
        ";; pelo efeito de transição (src/effects.asm) a partir de src/levels.asm.",
        ";; Atributos: a área de jogo usa a paleta de fundo 1 e a moldura a paleta 0;",
        ";; as duas mudam a cada dificuldade (TierPalettes em src/effects.asm).",
        ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;",
        "BackgroundData:",
    ]
    for i in range(0, 1024, 16):
        lines.append("  .byte " + ",".join("$%02X" % b for b in data[i:i + 16]))
    with open(OUT, "w", encoding="utf-8", newline="\n") as f:
        f.write("\n".join(lines) + "\n")
    print("Gerado:", os.path.relpath(OUT, ROOT))


if __name__ == "__main__":
    main()
