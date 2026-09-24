#!/usr/bin/env python3
"""
Desenha no wand_quest_bg.chr os tiles que o código precisa e que não
existiam na arte original:

- Fonte: dígitos e letras nas posições ASCII ($30-$39, $41-$5A, '!' em $21),
  então no assembly basta escrever  .byte "FASE 01"  e cada caractere já é
  o número do tile certo. O espaço ($20) é um tile vazio.
  Cores: 3 = letra (branco), 1 = sombra.
- Brilho do efeito de transição: metatile 16x16 nos tiles $05-$08.
- Pilares (as paredes de dentro da fase), um modelo por dificuldade:
    $09-$0C  fases  1-5   pedra
    $0D-$10  fases  6-10  cristal
    $11-$14  fases 11-15  caixa de madeira
    $15-$18  fases 16-20  metal com runa
- Moldura da tela (usada pelo tools/make_background.py):
    $60, $61  tijolos (A tem a junta vertical, B não; alternados = aparelho corrido)
    $62-$65   friso em volta da área de jogo: cima, baixo, esquerda, direita
    $66-$69   cantos do friso: sup. esq., sup. dir., inf. esq., inf. dir.
    $6A-$6D   rebite redondo 16x16 (enfeite dos cantos e das laterais)
- Cada metatile 16x16 usa 4 tiles na ordem: superior esquerdo, superior
  direito, inferior esquerdo, inferior direito.
- A área de jogo usa a paleta de fundo 1, que muda a cada dificuldade
  (TierPalettes em src/effects.asm); a cor 3 é sempre branca.

O script só escreve em tiles VAZIOS (ou que já têm exatamente o mesmo
desenho), então rodar de novo não estraga nada, e ele se recusa a apagar
um desenho seu. Rode depois de reexportar o CHR pelo NAW:
  python tools/add_tiles.py
"""

import os
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
CHR = os.path.join(ROOT, "wand_quest_bg.chr")

# Fonte 5x7: cada string é uma linha, '1' = pixel aceso
FONT = {
    "0": "01110 10001 10011 10101 11001 10001 01110",
    "1": "00100 01100 00100 00100 00100 00100 01110",
    "2": "01110 10001 00001 00010 00100 01000 11111",
    "3": "11111 00010 00100 00010 00001 10001 01110",
    "4": "00010 00110 01010 10010 11111 00010 00010",
    "5": "11111 10000 11110 00001 00001 10001 01110",
    "6": "00110 01000 10000 11110 10001 10001 01110",
    "7": "11111 00001 00010 00100 01000 01000 01000",
    "8": "01110 10001 10001 01110 10001 10001 01110",
    "9": "01110 10001 10001 01111 00001 00010 01100",
    "A": "01110 10001 10001 11111 10001 10001 10001",
    "B": "11110 10001 10001 11110 10001 10001 11110",
    "C": "01110 10001 10000 10000 10000 10001 01110",
    "D": "11100 10010 10001 10001 10001 10010 11100",
    "E": "11111 10000 10000 11110 10000 10000 11111",
    "F": "11111 10000 10000 11110 10000 10000 10000",
    "G": "01110 10001 10000 10111 10001 10001 01111",
    "H": "10001 10001 10001 11111 10001 10001 10001",
    "I": "01110 00100 00100 00100 00100 00100 01110",
    "J": "00111 00010 00010 00010 00010 10010 01100",
    "K": "10001 10010 10100 11000 10100 10010 10001",
    "L": "10000 10000 10000 10000 10000 10000 11111",
    "M": "10001 11011 10101 10101 10001 10001 10001",
    "N": "10001 10001 11001 10101 10011 10001 10001",
    "O": "01110 10001 10001 10001 10001 10001 01110",
    "P": "11110 10001 10001 11110 10000 10000 10000",
    "Q": "01110 10001 10001 10001 10101 10010 01101",
    "R": "11110 10001 10001 11110 10100 10010 10001",
    "S": "01111 10000 10000 01110 00001 00001 11110",
    "T": "11111 00100 00100 00100 00100 00100 00100",
    "U": "10001 10001 10001 10001 10001 10001 01110",
    "V": "10001 10001 10001 10001 10001 01010 00100",
    "W": "10001 10001 10001 10101 10101 10101 01010",
    "X": "10001 10001 01010 00100 01010 10001 10001",
    "Y": "10001 10001 10001 01010 00100 00100 00100",
    "Z": "11111 00001 00010 00100 01000 10000 11111",
    "!": "00100 00100 00100 00100 00100 00000 00100",
}

# Brilho 16x16 (0 = transparente/preto, 1-3 = cores da paleta)
SPARKLE = [
    "................",
    ".......33.......",
    ".......33.......",
    "..2....22....2..",
    "...2...11...2...",
    "....1..11..1....",
    "................",
    ".3321......1233.",
    ".3321......1233.",
    "................",
    "....1..11..1....",
    "...2...11...2...",
    "..2....22....2..",
    ".......33.......",
    ".......33.......",
    "................",
]
SPARKLE_TILES = (0x05, 0x06, 0x07, 0x08)

# Pilares 16x16, um por dificuldade: (primeiro tile, desenho)
PILLARS = [
    (0x09, [                        # Pedra: tijolos com junta escura
        ".33333333333332.",
        "3222222212222221",
        "3222222212222221",
        "3222222212222221",
        "1111111111111111",
        "3222122222221222",
        "3222122222221222",
        "3222122222221222",
        "1111111111111111",
        "3222222212222221",
        "3222222212222221",
        "3222222212222221",
        "1111111111111111",
        "3222122222221222",
        "3222122222221222",
        ".11111111111111.",
    ]),
    (0x0D, [                        # Cristal lapidado
        "................",
        ".......33.......",
        "......3323......",
        ".....332223.....",
        "....33222221....",
        "...3322222221...",
        "..332222222221..",
        ".33333333333321.",
        ".12222222222221.",
        "..122222222221..",
        "...1222222221...",
        "....12222221....",
        ".....122221.....",
        "......1221......",
        ".......11.......",
        "................",
    ]),
    (0x11, [                        # Caixa de madeira com X
        "3333333333333331",
        "3122222222222211",
        "3212222222222121",
        "3221222222221221",
        "3222122222212221",
        "3222212222122221",
        "3222221221222221",
        "3222222112222221",
        "3222222112222221",
        "3222221221222221",
        "3222212222122221",
        "3222122222212221",
        "3221222222221221",
        "3212222222222121",
        "3122222222222211",
        "1111111111111111",
    ]),
    (0x15, [                        # Metal com rebites e runa no meio
        "1333333333333331",
        "3211222222221121",
        "3213122222213121",
        "3211222222221121",
        "3222222332222221",
        "3222223113222221",
        "3222231221322221",
        "3222312222132221",
        "3222312222132221",
        "3222231221322221",
        "3222223113222221",
        "3222222332222221",
        "3211222222221121",
        "3213122222213121",
        "3211222222221121",
        "1111111111111111",
    ]),
]


# Tijolos 8x8: 1 = junta, 2 = tijolo, 3 = brilho na aresta de cima
BRICK_A = [                             # Com a junta vertical na esquerda
    "13333333",
    "12222223",
    "12222222",
    "12222222",
    "12212222",
    "12222222",
    "12222222",
    "11111111",
]
BRICK_B = [                             # Continuação do tijolo (sem junta)
    "33333333",
    "22222222",
    "22222222",
    "22222122",
    "22222222",
    "22222222",
    "22222221",
    "11111111",
]

# Friso horizontal que fica ACIMA da área de jogo (a linha 7 é o vão preto
# encostado na área de jogo). Os outros lados são este desenho espelhado.
BAND = ["1", "3", "2", "2", "2", "1", "3", "."]  # de fora para dentro
BAND_STUDS = [False, False, False, True, False, False, False, False]  # Pontinhos na linha 3

# Rebite redondo 16x16 (brilho em cima à esquerda)
STUD = [
    "................",
    "......1111......",
    "....11333311....",
    "...1332222331...",
    "..133222222221..",
    "..132232222221..",
    ".13223322222221.",
    ".13222222222221.",
    ".13222222222221.",
    ".12222222222211.",
    "..122222222211..",
    "..112222222111..",
    "...1112222111...",
    "....11111111....",
    "......1111......",
    "................",
]


def band_value(depth, along):
    """Cor do friso na distância `depth` da borda de fora (0-7), na posição `along` ao longo dele."""
    ch = BAND[depth]
    if BAND_STUDS[depth] and along % 4 == 1:
        ch = "1"
    return 0 if ch == "." else int(ch)


def frame_tiles():
    """Tiles da moldura: tijolos, friso (4 lados) e cantos em meia-esquadria."""
    rows = lambda art: [[0 if ch == "." else int(ch) for ch in r] for r in art]
    top = [[band_value(y, x) for x in range(8)] for y in range(8)]            # Friso acima da área
    bottom = [[band_value(7 - y, x) for x in range(8)] for y in range(8)]     # Abaixo
    left = [[band_value(x, y) for x in range(8)] for y in range(8)]           # À esquerda
    right = [[band_value(7 - x, y) for x in range(8)] for y in range(8)]      # À direita

    def corner(horiz, vert, flip_x, flip_y):
        # Meia-esquadria: acima da diagonal vale o friso horizontal, abaixo o vertical
        px = []
        for y in range(8):
            line = []
            for x in range(8):
                dx = 7 - x if flip_x else x
                dy = 7 - y if flip_y else y
                line.append(horiz[y][x] if dy <= dx else vert[y][x])
            px.append(line)
        return px

    return {
        0x60: rows(BRICK_A),
        0x61: rows(BRICK_B),
        0x62: top, 0x63: bottom, 0x64: left, 0x65: right,
        0x66: corner(top, left, False, False),      # Canto superior esquerdo da área de jogo
        0x67: corner(top, right, True, False),      # Superior direito
        0x68: corner(bottom, left, False, True),    # Inferior esquerdo
        0x69: corner(bottom, right, True, True),    # Inferior direito
    }


def encode(pixels):
    """8x8 de índices de cor (0-3) -> 16 bytes no formato da NES (2 planos)."""
    plane0 = bytearray(8)
    plane1 = bytearray(8)
    for y in range(8):
        for x in range(8):
            c = pixels[y][x]
            if c & 1:
                plane0[y] |= 0x80 >> x
            if c & 2:
                plane1[y] |= 0x80 >> x
    return bytes(plane0 + plane1)


def glyph_pixels(rows):
    """Letra 5x7 na posição (1,0) com sombra de cor 1 deslocada (+1,+1)."""
    px = [[0] * 8 for _ in range(8)]
    lit = [(x + 1, y) for y, row in enumerate(rows.split()) for x, b in enumerate(row) if b == "1"]
    for x, y in lit:                    # Sombra primeiro...
        if x + 1 < 8 and y + 1 < 8:
            px[y + 1][x + 1] = 1
    for x, y in lit:                    # ...e a letra por cima
        px[y][x] = 3
    return px


def quarter_pixels(art, tx, ty):
    """Um dos 4 tiles 8x8 de um desenho 16x16 (tx, ty = 0 ou 1)."""
    rows = art[ty * 8:(ty + 1) * 8]
    return [[0 if ch == "." else int(ch) for ch in row[tx * 8:(tx + 1) * 8]] for row in rows]


def add_metatile(tiles, first, art):
    for i in range(4):
        tiles[first + i] = encode(quarter_pixels(art, i % 2, i // 2))


def main():
    data = bytearray(open(CHR, "rb").read())
    tiles = {}
    for ch, rows in FONT.items():
        tiles[ord(ch)] = encode(glyph_pixels(rows))
    add_metatile(tiles, SPARKLE_TILES[0], SPARKLE)
    for first, art in PILLARS:
        add_metatile(tiles, first, art)
    for index, pixels in frame_tiles().items():
        tiles[index] = encode(pixels)
    add_metatile(tiles, 0x6A, STUD)

    for index, tile in sorted(tiles.items()):
        current = bytes(data[index * 16:(index + 1) * 16])
        if any(current) and current != tile:
            print(f"ERRO: o tile ${index:02X} já tem um desenho seu; não vou sobrescrever.")
            sys.exit(1)

    for index, tile in tiles.items():
        data[index * 16:(index + 1) * 16] = tile
    open(CHR, "wb").write(data)
    print(f"{len(tiles)} tiles escritos em {os.path.basename(CHR)}")


if __name__ == "__main__":
    main()
