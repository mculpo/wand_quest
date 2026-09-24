#!/usr/bin/env python3
"""
Desenha o tileset do cenário (base futurista) e os metatiles da fase.

Cada metatile tem 16x16 pixels (4 tiles 8x8), uma paleta de fundo e
propriedades de colisão. Gera:
  - bg.chr                (pattern table do fundo, 4 KB, em $1000 na PPU)
  - src/data/metatiles.asm (tiles, paleta e propriedades de cada metatile,
                            e as 4 paletas de fundo)

O tile 0 fica vazio (preto), que é o que aparece no nametable ainda não
carregado. Tiles repetidos entre metatiles são reaproveitados.

Uso: python tools/draw_tiles.py
"""

import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from nes_common import ROOT, parse, tile_at, encode, write_asm  # noqa: E402

CHR = os.path.join(ROOT, "bg.chr")
ASM = os.path.join(ROOT, "src", "data", "metatiles.asm")

# Paletas de fundo (a cor 0 é a mesma para todas: preto $0F)
BG_PALETTES = [
    [0x0F, 0x2D, 0x00, 0x10],   # 0: estrutura metálica (cinza escuro, cinza, cinza claro)
    [0x0F, 0x01, 0x0C, 0x1C],   # 1: paredes do fundo (azul-noite, petróleo, azul-esverdeado)
    [0x0F, 0x06, 0x16, 0x27],   # 2: perigo e luzes (vermelho-escuro, vermelho, laranja)
    [0x0F, 0x09, 0x1A, 0x2A],   # 3: tecnologia (verdes: canos, telas, porta)
]

# Propriedades (bits) — iguais às constantes MT_* em inc/consts.inc
SOLID, ONEWAY, HURT, EXIT = 1, 2, 4, 8

# Metatiles: (nome da constante, caractere na fase, paleta, propriedades, desenho 16x16)
METATILES = [
    ("BACK", ".", 1, 0, [                       # Fundo escuro com juntas discretas
        "1...............",
        "1...............",
        "1...............",
        "1...............",
        "1...............",
        "1...............",
        "1...............",
        "1......1........",
        "1...............",
        "1...............",
        "1...............",
        "1...............",
        "1...............",
        "1...............",
        "1...............",
        "1111111111111111",
    ]),
    ("PANEL", ":", 1, 0, [                      # Painel do fundo com rebites e uma faixa acesa
        "1222222222222221",
        "2311111111111132",
        "2111111111111112",
        "2112211111122112",
        "2112211111122112",
        "2111111111111112",
        "2111111111111112",
        "2111133333311112",
        "2111122222211112",
        "2111111111111112",
        "2111111111111112",
        "2112211111122112",
        "2112211111122112",
        "2111111111111112",
        "2311111111111132",
        "1222222222222221",
    ]),
    ("WALL", "#", 0, SOLID, [                   # Bloco metálico (miolo da parede)
        "3333333333333331",
        "3222222222222211",
        "3232222222223211",
        "3222222222222211",
        "3222222222222211",
        "3222111111222211",
        "3222133332222211",
        "3222132222222211",
        "3222132222222211",
        "3222122222222211",
        "3222222222222211",
        "3222222222222211",
        "3232222222223211",
        "3222222222222211",
        "3111111111111111",
        "1111111111111111",
    ]),
    ("WALL_TOP", None, 0, SOLID, [              # Parede com piso gradeado em cima (automático)
        "3333333333333333",
        "2323232323232323",
        "1111111111111111",
        "3222222222222211",
        "3232222222223211",
        "3222222222222211",
        "3222111111222211",
        "3222133332222211",
        "3222132222222211",
        "3222132222222211",
        "3222122222222211",
        "3222222222222211",
        "3232222222223211",
        "3222222222222211",
        "3111111111111111",
        "1111111111111111",
    ]),
    ("PLATFORM", "=", 0, ONEWAY, [              # Passarela vazada: só segura por cima
        "3333333333333333",
        "2222222222222222",
        "1.1.1.1.1.1.1.1.",
        ".1.1.1.1.1.1.1.1",
        "1111111111111111",
        "................",
        "................",
        "................",
        "................",
        "................",
        "................",
        "................",
        "................",
        "................",
        "................",
        "................",
    ]),
    ("SPIKES", "^", 2, SOLID | HURT, [          # Espinhos no chão
        "................",
        "................",
        "................",
        "................",
        "...3.......3....",
        "...3.......3....",
        "..323.....323...",
        "..323.....323...",
        ".32223...32223..",
        ".32223...32223..",
        "3222223.3222223.",
        "3222223.3222223.",
        "1111111111111111",
        "2222222222222222",
        "1111111111111111",
        "1111111111111111",
    ]),
    ("PIPE", "|", 3, 0, [                       # Cano vertical com braçadeira
        "....12333221....",
        "....12333221....",
        "....12333221....",
        "....12333221....",
        "....12333221....",
        "...1122222211...",
        "...1233333321...",
        "...1122222211...",
        "....12333221....",
        "....12333221....",
        "....12333221....",
        "....12333221....",
        "....12333221....",
        "....12333221....",
        "....12333221....",
        "....12333221....",
    ]),
    ("LIGHT", "*", 2, 0, [                      # Luminária de parede
        "................",
        "......1111......",
        ".....122221.....",
        "....12333321....",
        "....13333331....",
        "....12333321....",
        ".....122221.....",
        "......1111......",
        ".......11.......",
        "................",
        "......2..2......",
        "................",
        ".....2....2.....",
        "................",
        "................",
        "................",
    ]),
    ("SCREEN", "$", 3, 0, [                     # Monitor com gráfico
        "1111111111111111",
        "1222222222222221",
        "12............21",
        "12.3..........21",
        "12.33.....3...21",
        "12..3....33...21",
        "12..33..33....21",
        "12...3333.....21",
        "12............21",
        "12.2222.2222..21",
        "12............21",
        "12.22.222.22..21",
        "12............21",
        "1222222222222221",
        "1111111111111111",
        ".....111111.....",
    ]),
    ("EXIT", "X", 3, EXIT, [                    # Porta de saída com luz verde
        "1111111111111111",
        "1222222222222221",
        "1233333333333321",
        "1222222222222221",
        "12............21",
        "12.1........1.21",
        "12.1........1.21",
        "12.1........1.21",
        "12.1........1.21",
        "12.1........1.21",
        "12.1........1.21",
        "12.1........1.21",
        "12.1........1.21",
        "12.1........1.21",
        "12.1........1.21",
        "1111111111111111",
    ]),
]


def main():
    tiles = {(tuple((0,) * 8 for _ in range(8))): 0}   # Tile 0 = vazio
    order = [tuple((0,) * 8 for _ in range(8))]
    table = []                                           # (TL, TR, BL, BR) de cada metatile
    for name, _, pal, props, art in METATILES:
        canvas = parse(art)
        ids = []
        for ty in (0, 1):
            for tx in (0, 1):
                px = tile_at(canvas, tx, ty)
                if px not in tiles:
                    tiles[px] = len(order)
                    order.append(px)
                ids.append(tiles[px])
        table.append(ids)

    data = bytearray(4096)
    for i, px in enumerate(order):
        data[i * 16:(i + 1) * 16] = encode(px)
    open(CHR, "wb").write(data)

    out = [
        ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;",
        ";; Metatiles 16x16 do cenário. Gerado por tools/draw_tiles.py (não edite à mão).",
        ";; Cada tabela é indexada pelo número do metatile (constantes MT_*).",
        ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;",
        "",
    ]
    for i, (name, _, _, _, _) in enumerate(METATILES):
        out.append(f"MT_{name:<10} = {i}")
    out.append(f"MT_COUNT        = {len(METATILES)}")
    out.append("")
    for label, k in (("MetaTL", 0), ("MetaTR", 1), ("MetaBL", 2), ("MetaBR", 3)):
        out.append(f"{label}:    .byte " + ", ".join(f"${t[k]:02X}" for t in table))
    out.append("MetaPal:   .byte " + ", ".join(str(m[2]) for m in METATILES) + "      ; Paleta de fundo (0-3)")
    out.append("MetaFlags: .byte " + ", ".join(f"%{m[3]:04b}" for m in METATILES) + "  ; MTF_SOLID/ONEWAY/HURT/EXIT")
    out.append("")
    out.append(";; Paletas de fundo (4 x 4 cores)")
    out.append("BgPalettes:")
    for i, p in enumerate(BG_PALETTES):
        out.append("    .byte " + ", ".join(f"${c:02X}" for c in p) + f"   ; {i}")
    write_asm(ASM, out)
    print(f"{len(order)} tiles de fundo, {len(METATILES)} metatiles")


if __name__ == "__main__":
    main()
