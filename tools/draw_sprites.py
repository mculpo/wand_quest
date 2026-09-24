#!/usr/bin/env python3
"""
Desenha todos os sprites do jogo: o mago (16x16) e as animações dele, a
bola de pedra e os 4 inimigos (gosma, morcego, aranha e fantasma).

Em vez de desenhar cada quadro à mão, o mago é montado com PARTES
(chapéu, rosto, túnica, barra da túnica, mãos, pés e varinha) e cada quadro
só muda alguns parâmetros dessas partes, calculados com ondas suaves (seno):
  - o corpo desce 1 px no passo;
  - os pés alternam (de frente o pé no ar some atrás da túnica);
  - os braços balançam em oposição;
  - a barra da túnica e as duas linhas da ponta do chapéu balançam com
    atrasos diferentes (como uma bandeira), o que cria poses intermediárias
    e deixa o movimento fluido.

Animações (quadros):
  - Idle (16): sempre de frente; respira, balança o chapéu, a varinha
    brilha e ele pisca.
  - WalkUp / WalkDown / WalkLeft (12 cada); WalkRight é a esquerda
    espelhada pelo hardware (flip do sprite), sem gastar tiles.
  - CastUp / CastDown / CastLeft (4 cada): estica a varinha com um clarão
    quando atira a bola; CastRight é espelhado.
  - Die (8): morte do mago (susto, o chapéu voa, ele vira brilhos).
  - Ball (4): bola de pedra 8x8 girando.   Puff (3): poeira do impacto.
  - Inimigos (4 quadros cada): Slime pula esticando/achatando, Bat bate as
    asas, Spider alterna as pernas, Ghost flutua ondulando o lençol.
    Poof (4): morte do inimigo.

Gera:
  - os tiles no wand_quest_spr.chr a partir de FIRST_TILE (tiles repetidos
    são reaproveitados; quadrantes vazios nem viram sprite);
  - src/player_anim.asm e src/enemy_anim.asm com os metasprites e as
    tabelas de animação;
  - tools/mage_preview.png com todos os quadros ampliados;
  - as paletas de sprite do mago (3), da pedra (2) e dos inimigos (1) no
    palettes_1.dat.
Tiles iguais (ou iguais espelhados) são reaproveitados: nos sprites
simétricos a metade direita é a esquerda com flip, sem gastar tile.

Uso:
  python tools/draw_sprites.py
"""

import math
import os

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
CHR = os.path.join(ROOT, "wand_quest_spr.chr")
ASM = os.path.join(ROOT, "src", "player_anim.asm")
ENEMY_ASM = os.path.join(ROOT, "src", "enemy_anim.asm")
PREVIEW = os.path.join(ROOT, "tools", "mage_preview.png")
PALETTE_FILE = os.path.join(ROOT, "palettes_1.dat")

FIRST_TILE = 0x10               # Os tiles $00-$08 são do protótipo e dos blocos
MAGE_PALETTE = [0x0F, 0x02, 0x22, 0x37]   # Transparente, contorno azul-escuro, túnica azul, pele
ROCK_PALETTE = [0x0F, 0x07, 0x27, 0x38]   # Pedra e bloco no encaixe: marrom escuro, laranja, areia
MAGE_PAL, ROCK_PAL, ENEMY_PAL = 3, 2, 1   # Paletas de sprite usadas (a 0 é dos blocos)

# Os inimigos usam a paleta de sprite 1, que muda a cada dificuldade
# (TierPalettes em src/effects.asm). Estas são só para a prévia e o
# palettes_1.dat inicial; mantenha iguais às de TierPalettes.
ENEMY_TIER_PALETTES = [
    [0x0F, 0x09, 0x2A, 0x30],   # 0: verde
    [0x0F, 0x04, 0x24, 0x30],   # 1: roxo
    [0x0F, 0x0C, 0x2C, 0x30],   # 2: azul-petróleo
    [0x0F, 0x00, 0x10, 0x30],   # 3: cinza fantasma
]
ENEMY_PREVIEW_PALETTE = ENEMY_TIER_PALETTES[0]

IDLE_FRAMES, WALK_FRAMES, CAST_FRAMES = 16, 12, 4
BALL_FRAMES, PUFF_FRAMES = 4, 3
DIE_FRAMES = 8                  # Morte do mago
ENEMY_FRAMES, POOF_FRAMES = 4, 4

COLOR = {".": 0, "o": 1, "e": 1, "b": 2, "s": 3}


def wave(i, n, amp=1.0, phase=0.0):
    """Seno arredondado: amp * sin(2*pi*(i/n - phase))."""
    return int(round(amp * math.sin(2 * math.pi * (i / n - phase))))


# --------------------------------------------------------------------------
# Partes do mago. Coordenadas em pixels no quadro 16x16; '.' é transparente.
# --------------------------------------------------------------------------

HAT_FRONT = [                   # Linhas 0-7 (0 e 1 são a ponta, que balança)
    ".........o......",
    "........oo......",
    ".......obo......",
    "......obbo......",
    "......obbbo.....",
    ".....obbbbo.....",
    "....oobbbboo....",
    "...obbbbbbbbo...",
]
FACE_FRONT = [                  # Linhas 8-10
    "....osssssso....",
    "....osesseso....",
    ".....ossssoo....",
]
FACE_FRONT_BLINK = "....ossooosso...."[:16]   # Olhos fechados (piscando)
FACE_BACK = [                   # Nuca: cabelo escuro sob a aba
    "....obbbbbbo....",
    "....oooooooo....",
    ".....oooooo.....",
]
ROBE_FRONT = [                  # Linhas 11-13 (a barra, linha 14, é separada)
    "...obbbssbbbo...",
    "..obbbbbbbbbbo..",
    "..obbbbbbbbbbo..",
]
ROBE_BACK = [
    "...obbbbbbbbo...",
    "..obbbbbbbbbbo..",
    "..obbbbbbbbbbo..",
]
HEM_FRONT = "...obbbbbbbbo..."    # Linha 14

HAT_SIDE = [                    # Olhando para a esquerda: ponta caída para trás
    "...........o....",
    "..........oo....",
    ".........obo....",
    "........obbo....",
    ".......obbbo....",
    "......obbbbbo...",
    "....oobbbbbbboo.",
    "..obbbbbbbbbbo..",
]
FACE_SIDE = [
    "...osssssooo....",
    "..ossesoooo.....",
    "...osssoo.......",
]
ROBE_SIDE = [
    "....obbbbbo.....",
    "...obbbbbbbo....",
    "...obbbbbbbo....",
]
HEM_SIDE = "....obbbbbo....."


def blank(size=16):
    return [[0] * size for _ in range(size)]


def put(canvas, rows, x0, y0):
    """Desenha as linhas `rows` com o canto em (x0, y0); '.' não pinta."""
    size = len(canvas)
    for dy, row in enumerate(rows):
        for dx, ch in enumerate(row):
            if ch == ".":
                continue
            x, y = x0 + dx, y0 + dy
            if 0 <= x < size and 0 <= y < size:
                canvas[y][x] = COLOR[ch]


def dot(canvas, x, y, ch):
    if 0 <= x < len(canvas) and 0 <= y < len(canvas):
        canvas[y][x] = COLOR[ch]


def sparkle_at(c, x, y, kind):
    """Clarão na ponta da varinha: 1 = cruz (+), 2 = xis (x), 3 = estrela grande (+ de braço 2)."""
    dot(c, x, y, "s")
    if kind == 1:
        for px, py in ((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)):
            dot(c, px, py, "s")
    elif kind == 2:
        for px, py in ((x - 1, y - 1), (x + 1, y - 1), (x - 1, y + 1), (x + 1, y + 1)):
            dot(c, px, py, "s")
    elif kind == 3:
        for k in (1, 2):
            for px, py in ((x - k, y), (x + k, y), (x, y - k), (x, y + k)):
                dot(c, px, py, "s")


def vertical_wand(c, hx, hy, sparkle=0, length=4):
    """Varinha em pé, ao lado da mão (hx, hy)."""
    for k in range(1, length + 1):
        dot(c, hx + 1, hy - k, "o")
    sparkle_at(c, hx + 1, hy - length - 1, sparkle)


def hat(c, rows, bob, tip0, tip1):
    """Chapéu com as duas linhas da ponta deslocadas (tip0 = linha 0, tip1 = linha 1)."""
    put(c, rows[2:], 0, 2 + bob)
    put(c, [rows[1]], tip1, 1 + bob)
    put(c, [rows[0]], tip0, bob)


def frame_vertical(view, bob=0, feet="both", hand_l=0, hand_r=0, tip0=0, tip1=0, hem=0,
                   blink=False, sparkle=0, cast=None):
    """Quadro de frente (view='down') ou de costas (view='up').
    cast = None ou (altura da mão da varinha, clarão)."""
    c = blank()
    # Pés (linha 15): o pé que está no ar some atrás da túnica
    if feet == "both":
        put(c, ["oo"], 4, 15)
        put(c, ["oo"], 10, 15)
    elif feet == "L":
        put(c, ["ooo"], 3, 15)
    elif feet == "R":
        put(c, ["ooo"], 10, 15)

    put(c, ROBE_FRONT if view == "down" else ROBE_BACK, 0, 11 + bob)
    put(c, [HEM_FRONT], hem, 14)
    face = list(FACE_FRONT if view == "down" else FACE_BACK)
    if blink and view == "down":
        face[1] = FACE_FRONT_BLINK
    put(c, face, 0, 8 + bob)
    hat(c, HAT_FRONT, bob, tip0, tip1)

    # De frente a varinha fica na mão da direita da tela; de costas, na da esquerda
    wand_is_right = view == "down"
    left = [2, 12 + bob + hand_l]
    right = [13, 12 + bob + hand_r]
    wand_hand = right if wand_is_right else left
    if cast is not None:
        wand_hand[1] = 12 + bob + cast[0]    # Mão da varinha levantada
    dot(c, *left, "s")
    dot(c, *right, "s")
    wx = wand_hand[0] if wand_is_right else wand_hand[0] - 2
    vertical_wand(c, wx, wand_hand[1], cast[1] if cast else sparkle)
    return c


def frame_side(bob=0, spread=0, hand=0, tip0=0, tip1=0, hem=0, cast=None):
    """Quadro de perfil olhando para a esquerda.
    spread = abertura das pernas (0-2). cast = None ou (x da mão da frente, clarão)."""
    c = blank()
    put(c, ["oo"], 8 + spread, 15)
    put(c, ["oo"], 6 - spread - (1 if spread == 2 else 0), 15)

    if cast is None:
        # Braço de trás ANTES do corpo: balança ao contrário do da frente e
        # leva a varinha por cima do ombro; o corpo esconde o que fica atrás.
        bx, by = 11 - hand, 12 + bob
        dot(c, bx, by, "s")
        dot(c, bx + 1, by - 1, "o")
        dot(c, bx + 2, by - 2, "o")
        dot(c, bx + 3, by - 3, "s")

    put(c, ROBE_SIDE, 0, 11 + bob)
    put(c, [HEM_SIDE], hem, 14)
    put(c, FACE_SIDE, 0, 8 + bob)
    hat(c, HAT_SIDE, bob, tip0, tip1)

    if cast is None:
        dot(c, 4 + hand, 12 + bob, "s")      # Mão da frente balançando
    else:
        # Lançando: a mão da frente estica a varinha para frente (esquerda)
        hx, flash = cast
        hy = 12 + bob
        dot(c, hx, hy, "s")
        dot(c, hx - 1, hy - 1, "o")
        sparkle_at(c, hx - 2, hy - 2, flash)
    return c


def build_animations():
    anims = {}

    # Idle: respiração lenta, ponta do chapéu balançando, a varinha brilha
    # no meio do ciclo e ele pisca perto do fim.
    n = IDLE_FRAMES
    idle = []
    for i in range(n):
        breath = math.sin(2 * math.pi * i / n)
        sparkle = {4: 1, 5: 2, 6: 3, 7: 2, 8: 1}.get(i, 0)
        idle.append(frame_vertical(
            "down",
            bob=1 if breath > 0.35 else 0,
            tip0=wave(i, n, 1.2, 0.30), tip1=wave(i, n, 0.8, 0.15),
            sparkle=sparkle,
            blink=(i == 13)))
    anims["Idle"] = idle

    # Andando de frente e de costas (2 passos por ciclo)
    n = WALK_FRAMES
    for view, name in (("down", "WalkDown"), ("up", "WalkUp")):
        frames = []
        for i in range(n):
            s = math.sin(2 * math.pi * i / n)          # Qual pé está na frente
            feet = "L" if s > 0.6 else "R" if s < -0.6 else "both"
            hand = int(round(s * 1.2))
            frames.append(frame_vertical(
                view, bob=1 if abs(s) > 0.6 else 0, feet=feet,
                hand_l=hand, hand_r=-hand,
                tip0=wave(i, n, 1.3, 0.25), tip1=wave(i, n, 0.8, 0.12),
                hem=wave(i, n, 0.9, 0.10)))
        anims[name] = frames

    # Andando de perfil: pernas abrem e fecham duas vezes por ciclo
    frames = []
    for i in range(n):
        c = math.cos(2 * math.pi * i / n)              # Pernas abertas em c = ±1
        spread = int(round(abs(c) * 2))
        frames.append(frame_side(
            bob=1 if spread == 2 else 0, spread=spread,
            hand=wave(i, n, 2.0),
            tip0=wave(i, n / 2, 1.2, 0.25), tip1=wave(i, n / 2, 0.7, 0.12),
            hem=wave(i, n / 2, 0.8, 0.15)))
    anims["WalkLeft"] = frames

    # Lançar: clarão grande, clarão menor, recolhendo, normal
    anims["CastDown"] = [frame_vertical("down", cast=(-3, 3)), frame_vertical("down", cast=(-3, 2)),
                         frame_vertical("down", cast=(-2, 1)), frame_vertical("down", cast=(-1, 0))]
    anims["CastUp"] = [frame_vertical("up", cast=(-3, 3)), frame_vertical("up", cast=(-3, 2)),
                       frame_vertical("up", cast=(-2, 1)), frame_vertical("up", cast=(-1, 0))]
    anims["CastLeft"] = [frame_side(cast=(2, 3)), frame_side(cast=(2, 2)),
                         frame_side(cast=(3, 1)), frame_side(cast=(4, 0))]
    return anims


# --------------------------------------------------------------------------
# Bola de pedra e poeira (8x8)
# --------------------------------------------------------------------------

ROCK = [
    "..oooo..",
    ".obbsbo.",
    "obbssbbo",
    "obbbbobo",
    "obobbbbo",
    "obbbbbbo",
    ".oboobo.",
    "..oooo..",
]
PUFF = [
    [
        "........",
        "...ss...",
        "..sbbs..",
        ".sbbbbs.",
        ".sbbbbs.",
        "..sbbs..",
        "...ss...",
        "........",
    ],
    [
        "..s..s..",
        ".b....b.",
        "s..bb..s",
        "..b..b..",
        "..b..b..",
        "s..bb..s",
        ".b....b.",
        "..s..s..",
    ],
    [
        "o......o",
        "...o....",
        "........",
        ".o....o.",
        "........",
        "....o...",
        "........",
        "o..o...o",
    ],
]


def rotate(art):
    """Gira um desenho quadrado 90 graus no sentido horário."""
    size = len(art)
    return ["".join(art[size - 1 - x][y] for x in range(size)) for y in range(size)]


def small_frames():
    ball = []
    art = ROCK
    for _ in range(BALL_FRAMES):
        c = blank(8)
        put(c, art, 0, 0)
        ball.append(c)
        art = rotate(art)
    puff = []
    for art in PUFF:
        c = blank(8)
        put(c, art, 0, 0)
        puff.append(c)
    return ball, puff


# --------------------------------------------------------------------------
# Morte do mago (8 quadros): leva o susto (olhos em X, pulinho), o chapéu
# voa e ele se desfaz em brilhos de baixo para cima.
# --------------------------------------------------------------------------

def mage_death_frames():
    frames = []
    base = frame_vertical("down")
    hurt = frame_vertical("down", hand_l=-2, hand_r=-2)
    for y, row in ((9, "....oosossoo...."),):         # Olhos em X (bem simples)
        put(hurt, [row], 0, y)
    frames.append(hurt)
    up = blank()
    for y in range(15):
        up[y] = list(hurt[y + 1])                       # Pulinho de 1 px
    frames.append(up)
    for k in range(6):
        c = blank()
        cut = 15 - k * 3                                # Linhas do corpo que ainda existem
        hat_dy = -min(k + 1, 3)                          # O chapéu sobe um pouco...
        for y in range(16):
            for x in range(16):
                if not base[y][x]:
                    continue
                if y < 8:
                    if k < 4:                           # ...e some no fim
                        ny = y + hat_dy
                        if 0 <= ny < 16:
                            c[ny][x] = base[y][x]
                elif y < cut:
                    c[y][x] = base[y][x]
        # Brilhos subindo de onde o corpo sumiu
        for i in range(5):
            sx = (2 + i * 3 + k) % 13 + 2
            sy = 14 - k * 2 - (i * 3) % 5
            if 0 <= sy < 16:
                sparkle_at(c, sx, sy, 1 if (i + k) % 2 else 2)
        frames.append(c)
    return frames


# --------------------------------------------------------------------------
# Inimigos (16x16, simétricos: a metade direita reaproveita o tile da
# esquerda espelhado). Cores: o = contorno, b = corpo, s = olhos/brilho.
# --------------------------------------------------------------------------

def ellipse(c, cx, cy, rx, ry, fill="b", edge="o"):
    """Elipse preenchida com contorno (centro em meios pixels permitido)."""
    for y in range(16):
        for x in range(16):
            dx = (x + 0.5 - cx) / rx
            dy = (y + 0.5 - cy) / ry
            d = dx * dx + dy * dy
            if d <= 1.0:
                c[y][x] = COLOR[fill]
            elif d <= 1.0 + 2.2 / max(rx, ry) and c[y][x] == 0:
                c[y][x] = COLOR[edge]


def slime_frames():
    """Gosma: pula esticando e achatando, sempre apoiada no chão (linha 14)."""
    shapes = [(6.0, 3.6), (6.8, 3.0), (6.0, 3.6), (5.2, 4.6)]   # (raio x, raio y)
    frames = []
    for rx, ry in shapes:
        c = blank()
        cy = 14.5 - ry
        ellipse(c, 8, cy, rx, ry)
        ey = int(cy - ry * 0.15)
        for ex in (6, 9):                        # Olhos brancos com pupila
            dot(c, ex, ey, "s")
            dot(c, ex, ey + 1, "o")
        dot(c, 5, int(cy - ry * 0.6), "s")       # Brilho
        for x in range(16):                      # Base achatada
            if c[15][x]:
                c[15][x] = COLOR["o"]
        frames.append(c)
    return frames


def bat_frames():
    """Morcego: bate as asas (cima, meio, baixo, meio) e sobe/desce no ar."""
    wings = {                                 # Asa esquerda (8 colunas); a direita é o espelho
        "up": ["o.......", "bo......", "bbo.....", "bbbo....", ".bbbo...", "..obbo..", "....obo."],
        "mid": ["........", "........", "oo......", "bbbo....", "bbbbbo..", ".obbbbo.", "...oobo."],
        "down": ["........", "........", "........", "....obo.", "..obbbo.", ".obbbbo.", "obbbo...", "bbo.....", "o......."],
    }
    body = [
        ".....o....o.....",
        ".....oo..oo.....",
        "......obbo......",
        ".....obsbso.....",
        ".....obbbbo.....",
        "......obbo......",
        ".......oo.......",
    ]
    frames = []
    for pose, bob in (("up", 0), ("mid", 1), ("down", 2), ("mid", 1)):
        c = blank()
        top = 3 + bob
        for dy, row in enumerate(wings[pose]):
            put(c, [row], 0, top + dy)
            put(c, [row[::-1]], 8, top + dy)
        put(c, body, 0, top + 1)
        frames.append(c)
    return frames


def spider_frames():
    """Aranha: corpo redondo e 4 pares de pernas que alternam o passo."""
    frames = []
    for phase in range(4):
        c = blank()
        ellipse(c, 8, 8.5, 4.2, 3.6)
        dot(c, 6, 7, "s")
        dot(c, 9, 7, "s")
        dot(c, 7, 11, "s")
        dot(c, 8, 11, "s")
        for leg in range(4):
            up = ((leg + phase) % 2 == 0)
            y0 = 5 + leg * 2
            lift = -1 if up else 1
            for side in (-1, 1):
                x_body = 8 + side * 4 if side > 0 else 8 - 5
                x1 = x_body + side * 1
                x2 = x_body + side * 2
                x3 = x_body + side * 3
                dot(c, x1, y0, "b")          # Pernas na cor do corpo (o contorno
                dot(c, x2, y0 + lift, "b")   # escuro some no chão preto)
                dot(c, x3, y0 + lift + (1 if leg >= 2 else 0), "b")
        frames.append(c)
    return frames


def ghost_frames():
    """Fantasma: flutua subindo e descendo e a barra do lençol ondula."""
    body = [
        "......oooo......",
        "....oobbbboo....",
        "...obbbbbbbbo...",
        "..obbbbbbbbbbo..",
        "..obsobbbbsobo..",
        "..obsobbbbsobo..",
        ".obbbbbbbbbbbbo.",
        ".obbbbboobbbbbo.",
        ".obbbbbbbbbbbbo.",
        ".obbbbbbbbbbbbo.",
    ]
    hems = [
        ".obbobbbobbbobo.",
        ".obbbobbbobbbbo.",
        ".obobbbobbbobbo.",
        ".obbbobbbobbbbo.",
    ]
    tails = [
        ".o.o..o.o..o..o.",
        ".o..o...o...o.o.",
        ".o.o..o...o..oo.",
        ".o..o...o...o.o.",
    ]
    frames = []
    for i in range(4):
        c = blank()
        top = 2 + (0, 1, 2, 1)[i]
        put(c, body, 0, top)
        put(c, [hems[i]], 0, top + len(body))
        put(c, [tails[i]], 0, top + len(body) + 1)
        frames.append(c)
    return frames


def poof_frames():
    """Poof da morte do inimigo: fumaça que abre e brilhos que se espalham."""
    frames = []
    for k in range(4):
        c = blank()
        r = 2 + k * 1.8
        for a in range(8):
            ang = a * math.pi / 4 + k * 0.3
            x = int(round(8 + math.cos(ang) * r))
            y = int(round(8 + math.sin(ang) * r))
            if k < 3:
                sparkle_at(c, x, y, 1 if (a + k) % 2 else 2)
            else:
                dot(c, x, y, "o")
        if k < 2:
            ellipse(c, 8, 8, 2.5 - k, 2.5 - k, fill="s", edge="b")
        frames.append(c)
    return frames


ENEMIES = ["Slime", "Bat", "Spider", "Ghost"]


# --------------------------------------------------------------------------
# Saída: tiles, metasprites, paletas e prévia
# --------------------------------------------------------------------------

def tiles_of(canvas):
    """Tiles 8x8 do quadro: (dx, dy, pixels)."""
    size = len(canvas)
    out = []
    for qy in range(0, size, 8):
        for qx in range(0, size, 8):
            out.append((qx, qy, tuple(tuple(canvas[qy + y][qx + x] for x in range(8)) for y in range(8))))
    return out


def mirror(px):
    return tuple(tuple(reversed(row)) for row in px)


def encode(pixels):
    p0 = bytearray(8)
    p1 = bytearray(8)
    for y in range(8):
        for x in range(8):
            c = pixels[y][x]
            if c & 1:
                p0[y] |= 0x80 >> x
            if c & 2:
                p1[y] |= 0x80 >> x
    return bytes(p0 + p1)


def main():
    anims = build_animations()
    anims["Die"] = mage_death_frames()
    ball, puff = small_frames()
    enemies = {"Slime": slime_frames(), "Bat": bat_frames(),
               "Spider": spider_frames(), "Ghost": ghost_frames()}
    poof = poof_frames()

    tiles = {}                  # pixels -> índice do tile
    order = []

    def tile_for(px):
        """(tile, flip): reaproveita o tile se ele já existe ou existe espelhado."""
        if px in tiles:
            return tiles[px], 0
        m = mirror(px)
        if m in tiles:
            return tiles[m], 0x40
        tiles[px] = FIRST_TILE + len(order)
        order.append(px)
        return tiles[px], 0

    def meta(canvas, pal):
        sprites = []
        for dx, dy, px in tiles_of(canvas):
            if any(any(r) for r in px):      # Quadrante vazio não vira sprite
                t, flip = tile_for(px)
                sprites.append((dy, t, pal | flip, dx))
        return sprites

    metas = {}
    for name, frames in anims.items():
        for i, canvas in enumerate(frames):
            sprites = meta(canvas, MAGE_PAL)
            metas[f"Mage{name}{i}"] = sprites
            if name.endswith("Left"):
                # Direita = esquerda espelhada: inverte o flip de cada tile e
                # troca os lados (dx 0 <-> 8)
                mirror_name = name.replace("Left", "Right")
                metas[f"Mage{mirror_name}{i}"] = [(dy, t, a ^ 0x40, 8 - dx) for dy, t, a, dx in sprites]
    for i, canvas in enumerate(ball):
        metas[f"Ball{i}"] = meta(canvas, ROCK_PAL)
    for i, canvas in enumerate(puff):
        metas[f"Puff{i}"] = meta(canvas, ROCK_PAL)
    for name, frames in enemies.items():
        for i, canvas in enumerate(frames):
            metas[f"{name}{i}"] = meta(canvas, ENEMY_PAL)
    for i, canvas in enumerate(poof):
        metas[f"Poof{i}"] = meta(canvas, ENEMY_PAL)

    if FIRST_TILE + len(order) > 0x100:
        raise SystemExit(f"tiles demais para o CHR de sprites: {len(order)}")

    data = bytearray(open(CHR, "rb").read())
    for index in range(FIRST_TILE, 0x100):            # Limpa sobras de versões antigas
        data[index * 16:(index + 1) * 16] = bytes(16)
    for px, index in tiles.items():
        data[index * 16:(index + 1) * 16] = encode(px)
    open(CHR, "wb").write(data)

    pal = bytearray(open(PALETTE_FILE, "rb").read())
    pal[16 + MAGE_PAL * 4:16 + MAGE_PAL * 4 + 4] = bytes(MAGE_PALETTE)
    pal[16 + ROCK_PAL * 4:16 + ROCK_PAL * 4 + 4] = bytes(ROCK_PALETTE)
    pal[16 + ENEMY_PAL * 4:16 + ENEMY_PAL * 4 + 4] = bytes(ENEMY_PREVIEW_PALETTE)
    open(PALETTE_FILE, "wb").write(pal)

    write_mage_asm(metas)
    write_enemy_asm(metas)
    write_preview(anims, ball, puff, enemies, poof)
    total = sum(len(v) for v in anims.values())
    print(f"{len(order)} tiles (${FIRST_TILE:02X}-${FIRST_TILE + len(order) - 1:02X}): "
          f"{total} quadros do mago, {BALL_FRAMES + PUFF_FRAMES} da bola, "
          f"{sum(len(v) for v in enemies.values()) + len(poof)} dos inimigos")


def meta_lines(metas, name):
    out = [f"{name}:"]
    for dy, t, a, dx in metas[name]:
        out.append(f"    .byte {dy:2d}, ${t:02X}, %{a:08b}, {dx:2d}")
    out.append("    .byte METASPRITE_END")
    return out


def write_mage_asm(metas):
    tables = [("MageIdle", "Idle", IDLE_FRAMES), ("MageDie", "Die", DIE_FRAMES)]
    for d in ("Up", "Down", "Left", "Right"):
        tables.append((f"MageWalk{d}", f"Walk{d}", WALK_FRAMES))
    for d in ("Up", "Down", "Left", "Right"):
        tables.append((f"MageCast{d}", f"Cast{d}", CAST_FRAMES))

    out = [
        ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;",
        ";; Animações do mago e da bola de pedra. Gerado por tools/draw_sprites.py",
        ";; (não edite à mão).",
        ";;",
        ";; Cada animação é uma tabela de endereços de metasprite (ver",
        ";; DrawMetasprite em src/utils.asm). As tabelas *BySide dão a animação",
        ";; para cada valor de Side (NONE, UP, DOWN, RIGHT, LEFT).",
        ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;",
        "",
        f"MAGE_IDLE_FRAMES = {IDLE_FRAMES}",
        f"MAGE_WALK_FRAMES = {WALK_FRAMES}",
        f"MAGE_CAST_FRAMES = {CAST_FRAMES}",
        f"MAGE_DIE_FRAMES  = {DIE_FRAMES}",
        f"BALL_FRAMES      = {BALL_FRAMES}",
        f"PUFF_FRAMES      = {PUFF_FRAMES}",
        "",
    ]
    for prefix in ("Walk", "Cast"):
        out.append(f"Mage{prefix}BySide:")
        out.append(f"    .word Mage{prefix}Down          ; Side::NONE (de frente)")
        out.append(f"    .word Mage{prefix}Up            ; Side::UP")
        out.append(f"    .word Mage{prefix}Down          ; Side::DOWN")
        out.append(f"    .word Mage{prefix}Right         ; Side::RIGHT")
        out.append(f"    .word Mage{prefix}Left          ; Side::LEFT")
        out.append("")
    for label, name, count in tables:
        out.append(f"{label}:")
        out += [f"    .word Mage{name}{i}" for i in range(count)]
        out.append("")
    out.append("BallFrames:")
    out += [f"    .word Ball{i}" for i in range(BALL_FRAMES)]
    out.append("")
    out.append("PuffFrames:")
    out += [f"    .word Puff{i}" for i in range(PUFF_FRAMES)]
    out.append("")
    out.append(";;     dy  tile  atributo    dx")
    for label, name, count in tables:
        for i in range(count):
            out += meta_lines(metas, f"Mage{name}{i}")
        out.append("")
    for name, count in (("Ball", BALL_FRAMES), ("Puff", PUFF_FRAMES)):
        for i in range(count):
            out += meta_lines(metas, f"{name}{i}")
        out.append("")
    with open(ASM, "w", encoding="utf-8", newline="\n") as f:
        f.write("\n".join(out))


def write_enemy_asm(metas):
    out = [
        ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;",
        ";; Animações dos inimigos. Gerado por tools/draw_sprites.py (não edite à mão).",
        ";;",
        ";; EnemyAnimByType dá a tabela de ENEMY_FRAMES quadros de cada tipo, na",
        ";; ordem do enum EnemyType (a partir de SLIME = 1). PoofFrames é a",
        ";; animação de morte, igual para todos.",
        ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;",
        "",
        f"ENEMY_FRAMES = {ENEMY_FRAMES}",
        f"POOF_FRAMES  = {POOF_FRAMES}",
        "",
        "EnemyAnimByType:",
    ]
    out += [f"    .word {name}Frames" for name in ENEMIES]
    out.append("")
    for name in ENEMIES:
        out.append(f"{name}Frames:")
        out += [f"    .word {name}{i}" for i in range(ENEMY_FRAMES)]
        out.append("")
    out.append("PoofFrames:")
    out += [f"    .word Poof{i}" for i in range(POOF_FRAMES)]
    out.append("")
    out.append(";;     dy  tile  atributo    dx")
    for name in ENEMIES:
        for i in range(ENEMY_FRAMES):
            out += meta_lines(metas, f"{name}{i}")
        out.append("")
    for i in range(POOF_FRAMES):
        out += meta_lines(metas, f"Poof{i}")
    out.append("")
    with open(ENEMY_ASM, "w", encoding="utf-8", newline="\n") as f:
        f.write("\n".join(out))


# Paleta RGB da NES (2C02) para a prévia
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


def write_preview(anims, ball, puff, enemies, poof, scale=5):
    try:
        from PIL import Image
    except ImportError:
        return
    rows = [(k, v, MAGE_PALETTE) for k, v in anims.items()]
    rows.append(("Rock", ball + puff, ROCK_PALETTE))
    for tier, pal in enumerate(ENEMY_TIER_PALETTES):
        # Uma linha por dificuldade, com os 4 inimigos e o poof nas cores dela
        frames = [f for name in ENEMIES for f in enemies[name]] + poof
        rows.append((f"Inimigos {tier}", frames, pal))
    cell = 16 * scale + 6
    width = max(len(v) for _, v, _ in rows)
    img = Image.new("RGB", (width * cell + 6, len(rows) * cell + 6), (24, 24, 32))
    for r, (_, frames, palette) in enumerate(rows):
        colors = [(0, 0, 0)] + [NES_RGB[c] for c in palette[1:]]
        for i, canvas in enumerate(frames):
            ox, oy = 6 + i * cell, 6 + r * cell
            for y in range(len(canvas)):
                for x in range(len(canvas)):
                    col = colors[canvas[y][x]]
                    for sy in range(scale):
                        for sx in range(scale):
                            img.putpixel((ox + x * scale + sx, oy + y * scale + sy), col)
    img.save(PREVIEW)


if __name__ == "__main__":
    main()
