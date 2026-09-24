#!/usr/bin/env python3
"""
Compara os prints do tests/scroll_test.lua com a prévia da fase.

Para cada print, recorta tools/level1_preview.png na posição da câmera
(o FCEUX corta as 8 primeiras linhas da tela, então o recorte começa em
CamY + 8) e conta quantos pixels batem. Os sprites (agente e barra de vida)
ficam por cima do fundo, então 100% não é esperado: acima de ~95% o scroll
e os atributos estão certos. Gera tests/scroll_compare.png lado a lado.

Uso: python tests/compare_scroll.py
"""
import os
from PIL import Image, ImageChops

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
preview = Image.open(os.path.join(ROOT, "tools", "level1_preview.png")).convert("RGB")
shots = [l.split() for l in open(os.path.join(HERE, "scroll_shots.txt"))]

rows = []
worst = 100.0
for name, cx, cy in shots:
    cx, cy = int(cx), int(cy)
    shot = Image.open(os.path.join(HERE, name)).convert("RGB")
    w, h = shot.size
    ref = preview.crop((cx, cy + 8, cx + w, cy + 8 + h))
    # O FCEUX usa uma paleta RGB um pouco diferente da prévia. Então primeiro
    # descobre qual cor do emulador corresponde a qual cor da prévia (a mais
    # comum na mesma posição) e depois conta os pixels que batem. Com o
    # scroll desalinhado essa correspondência não se sustenta e a conta cai.
    sp, rp = shot.load(), ref.load()
    votes = {}
    pixels = []
    for y in range(h):
        for x in range(w):
            if 12 <= x < 28 and y < 64:
                continue                    # Barra de vida
            a, b = sp[x, y], rp[x, y]
            pixels.append((a, b))
            votes.setdefault(a, {}).setdefault(b, 0)
            votes[a][b] += 1
    mapping = {a: max(v, key=v.get) for a, v in votes.items()}
    same = sum(1 for a, b in pixels if mapping[a] == b)
    total = len(pixels)
    pct = 100.0 * same / total
    worst = min(worst, pct)
    print(f"{name}: câmera ({cx:4d}, {cy:3d})  {pct:5.1f}% igual")
    rows.append((shot, ref))

img = Image.new("RGB", (512 + 12, len(rows) * 230), (40, 40, 48))
for i, (a, b) in enumerate(rows):
    img.paste(a, (0, i * 230))
    img.paste(b, (268, i * 230))
img.save(os.path.join(HERE, "scroll_compare.png"))
print(f"pior: {worst:.1f}%")
