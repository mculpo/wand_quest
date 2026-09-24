#!/usr/bin/env python3
"""
Desenha todos os sprites do protótipo: o agente de armadura vermelha, os
inimigos, os tiros, os efeitos, a barra de vida e as letras dos textos.

O agente (16x24, olhando para a direita) é montado com PARTES — capacete
com visor, tronco, braço com a arma e as duas pernas — e cada quadro só muda
alguns parâmetros (posição dos pés, joelhos, recuo da arma, brilho do
visor). As linhas de tiles casam com as partes (linha 0 = capacete, 1 =
tronco, 2 = pernas), então quadros diferentes reaproveitam muitos tiles.
Olhar para a esquerda é o mesmo quadro espelhado pelo hardware.

Gera:
  - spr.chr                (pattern table dos sprites, 4 KB, em $0000)
  - src/data/sprites.asm   (metasprites, tabelas de animação e paletas)
  - tools/sprites_preview.png

Cores: o/1, b/2, s/3 = cores 1-3 da paleta do sprite.
Uso: python tools/draw_sprites.py
"""

import math
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from nes_common import ROOT, blank, put, dot, tile_at, mirror, encode, write_asm, rgb  # noqa: E402

CHR = os.path.join(ROOT, "spr.chr")
ASM = os.path.join(ROOT, "src", "data", "sprites.asm")
PREVIEW = os.path.join(ROOT, "tools", "sprites_preview.png")

SPR_PALETTES = [
    [0x0F, 0x06, 0x16, 0x2C],   # 0: agente (vermelho-escuro, vermelho, ciano do visor)
    [0x0F, 0x27, 0x38, 0x30],   # 1: efeitos, barra de vida e textos (laranja, creme, branco)
    [0x0F, 0x00, 0x10, 0x16],   # 2: soldado-robô e torreta (cinza, cinza claro, olho vermelho)
    [0x0F, 0x03, 0x14, 0x34],   # 3: drone e tiros inimigos (roxo, magenta, rosa)
]
PAL_AGENT, PAL_FX, PAL_ROBOT, PAL_DRONE = 0, 1, 2, 3


# --------------------------------------------------------------------------
# Agente
# --------------------------------------------------------------------------

HEAD = [                        # Linhas 0-7: capacete com visor na frente
    "................",
    ".....oooo.......",
    "....obbbbo......",
    "...obbbbbbo.....",
    "...obbbsssso....",
    "...obbbsssso....",
    "...obbbbbbo.....",
    "....obbbbo......",
]
TORSO = [                       # Linhas 8-15: ombros, peito com luz, cinto
    "...oobbbboo.....",
    "..obbbbbbbbo....",
    "..obsbbbbbbo....",
    "..obbbbbbbbo....",
    "...obbbbbbo.....",
    "...obbbbbbo.....",
    "...oooooooo.....",
    "...obbbbbbo.....",
]
GUN_FORWARD = [                 # Braço e arma de energia apontados para frente (linhas 10-12)
    ".......oooooooo.",
    "......obbssssss.",
    ".......oo.o.....",
]
GUN_UP = [                      # Arma apontada para cima (colunas 8-10, linhas 0-9)
    ".........s......",
    ".........s......",
    "........oso.....",
    "........ooo.....",
    "........ooo.....",
    "........ooo.....",
    "........ooo.....",
    "........ooo.....",
    "........obo.....",
    ".......obbo.....",
]


def leg(c, hip_x, foot_x, lift, knee_bend=1):
    """Perna de 3 px (contorno, miolo, contorno) da cintura (linha 16) ao pé.
    lift = quanto o pé sobe; knee_bend = quanto o joelho vai para frente."""
    foot_y = 23 - lift
    knee_y = 16 + (foot_y - 16) // 2
    knee_x = (hip_x + foot_x) / 2 + knee_bend * (1 if lift > 0 else 0)
    for y in range(16, foot_y - 1):
        if y <= knee_y:
            t = (y - 16) / max(1, knee_y - 16)
            x = hip_x + (knee_x - hip_x) * t
        else:
            t = (y - knee_y) / max(1, foot_y - 1 - knee_y)
            x = knee_x + (foot_x - knee_x) * t
        x = int(round(x))
        dot(c, x - 1, y, "o")
        dot(c, x, y, "b")
        dot(c, x + 1, y, "o")
    put(c, ["obbo"], foot_x - 1, foot_y - 1)     # Bota
    put(c, ["oooo"], foot_x - 1, foot_y)


def agent(front=(8, 0), back=(5, 0), gun="forward", recoil=0, glint=None,
          chest=True, visor=True, crouch=False):
    """Um quadro do agente. front/back = (x do pé, altura do pé)."""
    c = blank(16, 24)
    if crouch:
        # Agachado: capacete desce 8 px (mesmos tiles), tronco e pernas dobradas
        put(c, HEAD, 0, 8)
        put(c, [
            "..oobbbbbboo....",
            "..obsbbbbbbo....",
            "..obbbbbbbbo....",
            "..oooooooooo....",
            ".obbbbobbbbbo...",
            ".obbbo..obbbo...",
            "obbbo...obbbbo..",
            "ooooo....ooooo..",
        ], 0, 16)
        if gun == "forward":
            put(c, GUN_FORWARD, -recoil, 17)
        return c
    leg(c, 5, back[0], back[1], knee_bend=-1)
    leg(c, 7, front[0], front[1], knee_bend=1)
    put(c, TORSO, -recoil, 8)
    head = [list(r) for r in HEAD]
    if not visor:
        for r in head:
            for i, ch in enumerate(r):
                if ch == "s":
                    r[i] = "o"
    if glint is not None:                       # Brilho correndo pelo visor
        gx, gy = glint
        head[gy][gx] = "b"
    put(c, ["".join(r) for r in head], -recoil, 0)
    if not chest:
        dot(c, 4 - recoil, 10, "b")
    if gun == "forward":
        put(c, GUN_FORWARD, -recoil, 10)
    elif gun == "up":
        put(c, GUN_UP, -recoil, 0)
    return c


def roll_frame(i):
    """Rolamento: bola de 16x16 (linhas 8-23) com o visor girando."""
    c = blank(16, 24)
    cx, cy, r = 7.5, 15.5, 7.4
    ang = i * math.pi / 3
    for y in range(8, 24):
        for x in range(16):
            dx, dy = x - cx, y - cy
            d = math.hypot(dx, dy)
            if d <= r:
                a = math.atan2(dy, dx) - ang
                a = (a + math.pi) % (2 * math.pi) - math.pi
                if d > r - 1.2:
                    c[y][x] = 1                       # Contorno
                elif abs(a) < 0.55 and d > r - 3.2:
                    c[y][x] = 3                       # Visor
                elif abs(abs(a) - math.pi / 2) < 0.2 or abs(a) > math.pi - 0.2:
                    c[y][x] = 1                       # Juntas da armadura
                else:
                    c[y][x] = 2
    return c


def build_agent():
    anims = {}
    # Parado: o brilho passa pelo visor e a luz do peito pisca
    glints = [None, (7, 4), (8, 4), (9, 5), (10, 5), None]
    anims["Idle"] = [agent(front=(8, 0), back=(4, 0), glint=g, chest=(i != 5)) for i, g in enumerate(glints)]
    # Correndo: pés em onda (um na frente, outro atrás), o que está voltando sobe
    run = []
    for i in range(8):
        p = 2 * math.pi * i / 8
        fx = 6 + int(round(3 * math.cos(p)))
        bx = 6 + int(round(3 * math.cos(p + math.pi)))
        flift = max(0, int(round(2.2 * math.sin(p))))
        blift = max(0, int(round(2.2 * math.sin(p + math.pi))))
        run.append(agent(front=(fx, flift), back=(bx, blift)))
    anims["Run"] = run
    anims["Jump"] = [agent(front=(9, 4), back=(4, 1)), agent(front=(9, 3), back=(5, 2))]
    anims["Fall"] = [agent(front=(8, 1), back=(3, 0))]
    anims["Crouch"] = [agent(crouch=True), agent(crouch=True, recoil=1)]
    anims["Roll"] = [roll_frame(i) for i in range(6)]
    anims["ShootStand"] = [agent(front=(8, 0), back=(4, 0), recoil=1)]
    anims["ShootUp"] = [agent(front=(8, 0), back=(4, 0), gun="up")]
    anims["Hurt"] = [agent(front=(9, 1), back=(3, 2), recoil=1, visor=False, gun="none")]
    return anims


# --------------------------------------------------------------------------
# Inimigos
# --------------------------------------------------------------------------

ROBOT_TOP = [                   # Linhas 0-15: cabeça quadrada com olho, antena, tronco
    "......o.........",
    "......o.........",
    "...oooooooo.....",
    "...obbbbbbo.....",
    "...obbbbsso.....",
    "...obbbbsso.....",
    "...obbbbbbo.....",
    "...oooooooo.....",
    "..oobbbbbboo....",
    ".obbbbbbbbbbo...",
    ".obboooooobbo...",
    ".obbbbbbbbbbo...",
    ".obbbbbbbbbbo...",
    "..obbbbbbbbo....",
    "...oooooooo.....",
    "...obbbbbbo.....",
]


def robot(step, shoot=False):
    c = blank(16, 24)
    put(c, ROBOT_TOP, 0, 0)
    # Pernas mecânicas: dois pistões que alternam
    offs = [(0, 2), (1, 1), (2, 0), (1, 1)][step]
    for base, lift in ((4, offs[0]), (8, offs[1])):
        for y in range(16, 22 - lift):
            put(c, ["obo"], base - 1, y)
        put(c, ["obbbo"], base - 2, 22 - lift)
        put(c, ["ooooo"], base - 2, 23 - lift)
    if shoot:
        put(c, ["......oooooooo..", ".....obbboooos3.".replace("3", "s")], 0, 10)
    else:
        put(c, [".........obo....", ".........obo...."], 0, 11)
    return c


def turret(aim):
    """Torreta de chão 16x16: base em cúpula e cano (0 = frente, 1 = diagonal, 2 = cima)."""
    c = blank(16, 16)
    put(c, [
        "................",
        "................",
        "................",
        "................",
        "................",
        "................",
        ".....oooooo.....",
        "....obbbbbbo....",
        "...obbsssbbbo...",
        "...obbsssbbbo...",
        "..obbbbbbbbbbo..",
        "..oooooooooooo..",
        ".obbbbbbbbbbbbo.",
        ".obobobobobobob.",
        "oooooooooooooooo",
        "obbbbbbbbbbbbbbo",
    ], 0, 0)
    if aim == 0:
        put(c, ["........oooooooo", "........obbbbbbs", "........oooooooo"], 0, 7)
    elif aim == 1:
        for k in range(6):
            put(c, ["ooo"], 8 + k, 7 - k)
            dot(c, 9 + k, 7 - k, "b")
        dot(c, 14, 2, "s")
        dot(c, 15, 1, "s")
    else:
        for y in range(0, 7):
            put(c, ["obo"], 7, y)
        dot(c, 8, 0, "s")
    return c


def drone(i):
    """Drone 16x16: corpo redondo com olho e hélice girando em cima."""
    c = blank(16, 16)
    rotor = [
        ["oooooooooooooooo", "......oooo......"],
        ["..oooooooooooo..", "......oooo......"],
        ["....oooooooo....", "......oooo......"],
        ["..oooooooooooo..", "......oooo......"],
    ][i]
    put(c, rotor, 0, 0)
    put(c, [
        ".......oo.......",
        "....oooooooo....",
        "...obbbbbbbbo...",
        "..obbbssssbbbo..",
        "..obbsobbosbbo..",
        "..obbsobbosbbo..",
        "..obbbssssbbbo..",
        "...obbbbbbbbo...",
        "....oobbbboo....",
        ".....o.oo.o.....",
        "....o..oo..o....",
        "................",
    ], 0, 2)
    if i % 2:
        dot(c, 7, 12, "s")
        dot(c, 8, 12, "s")
    return c


# --------------------------------------------------------------------------
# Tiros, efeitos, barra de vida e letras (8x8, a explosão tem 16x16)
# --------------------------------------------------------------------------

def art8(rows):
    c = blank(8, 8)
    put(c, rows, 0, 0)
    return c


SHOT_H = art8(["........", "........", ".obbbb..", "osssssso", "osssssso", ".obbbb..", "........", "........"])
SHOT_V = art8(["...oo...", "..osso..", "..bssb..", "..bssb..", "..bssb..", "..bssb..", "..osso..", "...oo..."])
ORB = [art8(["........", "..oooo..", ".obbbbo.", ".obssbo.", ".obssbo.", ".obbbbo.", "..oooo..", "........"]),
       art8(["...oo...", "..obbo..", ".obssbo.", "obsssbo.".replace(".", "o")[:8], ".obssbo.", ".obbbbo.", "..obbo..", "...oo..."])]
MUZZLE = art8(["........", "..o.....", "...o.o..", ".obsbbso", ".obsbbso", "...o.o..", "..o.....", "........"])
SPARK = [art8(["........", "........", "...s....", "..sss...", "...s....", "........", "........", "........"]),
         art8(["........", ".s...s..", "..s.s...", "...s....", "..s.s...", ".s...s..", "........", "........"]),
         art8(["b.....b.", "........", "........", "...o....", "........", "........", "b.....b.", "........"])]
HUD_CAP = art8(["oooooooo", "obbbbbbo", "ob.bb.bo", "obbbbbbo", "obbssbbo", "obbbbbbo", "oooooooo", "........"])
HUD_FULL = art8(["obbbbbbo", "osssssso", "obbbbbbo", "obbbbbbo", "osssssso", "obbbbbbo", "obbbbbbo", "oooooooo"])
HUD_HALF = art8(["o......o", "o......o", "o......o", "obbbbbbo", "osssssso", "obbbbbbo", "obbbbbbo", "oooooooo"])
HUD_EMPTY = art8(["o......o", "o......o", "o......o", "o......o", "o......o", "o......o", "o......o", "oooooooo"])


def explosion(k):
    c = blank(16, 16)
    r = 2.5 + k * 1.8
    for y in range(16):
        for x in range(16):
            d = math.hypot(x - 7.5, y - 7.5)
            n = (x * 7 + y * 13 + k * 5) % 5
            if d <= r:
                if k < 3:
                    c[y][x] = 3 if d < r * 0.45 else (2 if d < r * 0.8 else 1)
                    if n == 0 and d > r * 0.5:
                        c[y][x] = 0
                elif n < 2 and d > r - 3:
                    c[y][x] = 1
    return c


FONT = {
    "A": "01110 10001 10001 11111 10001 10001 10001",
    "C": "01110 10001 10000 10000 10000 10001 01110",
    "E": "11111 10000 10000 11110 10000 10000 11111",
    "I": "01110 00100 00100 00100 00100 00100 01110",
    "L": "10000 10000 10000 10000 10000 10000 11111",
    "M": "10001 11011 10101 10101 10001 10001 10001",
    "O": "01110 10001 10001 10001 10001 10001 01110",
    "P": "11110 10001 10001 11110 10000 10000 10000",
    "S": "01111 10000 10000 01110 00001 00001 11110",
    "T": "11111 00100 00100 00100 00100 00100 00100",
    "U": "10001 10001 10001 10001 10001 10001 01110",
}


def letter(ch):
    c = blank(8, 8)
    lit = [(x + 1, y) for y, row in enumerate(FONT[ch].split()) for x, b in enumerate(row) if b == "1"]
    for x, y in lit:
        dot(c, x + 1, y + 1, 1)                     # Sombra
    for x, y in lit:
        dot(c, x, y, 3)
    return c


# --------------------------------------------------------------------------
# Saída
# --------------------------------------------------------------------------

def main():
    tiles = {}
    order = []

    def tile_for(px):
        if px in tiles:
            return tiles[px], 0
        m = mirror(px)
        if m in tiles:
            return tiles[m], 0x40
        tiles[px] = len(order)
        order.append(px)
        return tiles[px], 0

    def meta(canvas, pal):
        sprites = []
        for ty in range(len(canvas) // 8):
            for tx in range(len(canvas[0]) // 8):
                px = tile_at(canvas, tx, ty)
                if any(any(r) for r in px):
                    t, flip = tile_for(px)
                    sprites.append((ty * 8, t, pal | flip, tx * 8))
        return sprites

    def flipped(sprites, width):
        """O mesmo metasprite olhando para o outro lado."""
        return [(dy, t, a ^ 0x40, width - 8 - dx) for dy, t, a, dx in sprites]

    groups = []                     # (nome da tabela, [(label, sprites)], paleta, quadros p/ prévia)
    agent = build_agent()
    for name, frames in agent.items():
        right = [(f"Agent{name}R{i}", meta(c, PAL_AGENT)) for i, c in enumerate(frames)]
        left = [(f"Agent{name}L{i}", flipped(s, 16)) for i, (_, s) in enumerate(right)]
        groups.append((f"Agent{name}R", right, PAL_AGENT, frames))
        groups.append((f"Agent{name}L", left, PAL_AGENT, None))

    robot_frames = [robot(i) for i in range(4)] + [robot(0, shoot=True)]
    right = [(f"RobotR{i}", meta(c, PAL_ROBOT)) for i, c in enumerate(robot_frames)]
    groups.append(("RobotR", right, PAL_ROBOT, robot_frames))
    groups.append(("RobotL", [(f"RobotL{i}", flipped(s, 16)) for i, (_, s) in enumerate(right)], PAL_ROBOT, None))

    turret_frames = [turret(a) for a in range(3)]
    right = [(f"TurretR{i}", meta(c, PAL_ROBOT)) for i, c in enumerate(turret_frames)]
    groups.append(("TurretR", right, PAL_ROBOT, turret_frames))
    groups.append(("TurretL", [(f"TurretL{i}", flipped(s, 16)) for i, (_, s) in enumerate(right)], PAL_ROBOT, None))

    drone_frames = [drone(i) for i in range(4)]
    groups.append(("Drone", [(f"Drone{i}", meta(c, PAL_DRONE)) for i, c in enumerate(drone_frames)], PAL_DRONE, drone_frames))

    small = [("ShotH", [SHOT_H], PAL_AGENT), ("ShotV", [SHOT_V], PAL_AGENT), ("Orb", ORB, PAL_DRONE),
             ("Muzzle", [MUZZLE], PAL_FX), ("Spark", SPARK, PAL_FX)]
    for name, frames, pal in small:
        groups.append((name, [(f"{name}{i}", meta(c, pal)) for i, c in enumerate(frames)], pal, frames))
    boom = [explosion(k) for k in range(4)]
    groups.append(("Explosion", [(f"Explosion{i}", meta(c, PAL_FX)) for i, c in enumerate(boom)], PAL_FX, boom))

    # Tiles soltos (desenhados direto, sem metasprite): barra de vida e letras
    hud = {"HUD_CAP": HUD_CAP, "HUD_FULL": HUD_FULL, "HUD_HALF": HUD_HALF, "HUD_EMPTY": HUD_EMPTY}
    hud_ids = {k: tile_for(tile_at(v, 0, 0))[0] for k, v in hud.items()}
    letters = {ch: tile_for(tile_at(letter(ch), 0, 0))[0] for ch in FONT}

    if len(order) > 256:
        raise SystemExit(f"tiles demais: {len(order)}")
    data = bytearray(4096)
    for i, px in enumerate(order):
        data[i * 16:(i + 1) * 16] = encode(px)
    open(CHR, "wb").write(data)

    out = [
        ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;",
        ";; Sprites do protótipo. Gerado por tools/draw_sprites.py (não edite à mão).",
        ";;",
        ";; Cada tabela <Nome> é uma lista de endereços de metasprite (um por quadro),",
        ";; e <NOME>_FRAMES é quantos quadros ela tem. Metasprite: .byte dy, tile,",
        ";; atributo, dx ... e METASPRITE_END. R/L = olhando para a direita/esquerda.",
        ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;",
        "",
    ]
    for k, v in hud_ids.items():
        out.append(f"TILE_{k:<10} = ${v:02X}")
    for ch, v in letters.items():
        out.append(f"TILE_LETTER_{ch} = ${v:02X}")
    out.append("")
    for name, frames, _, _ in groups:
        const = "".join("_" + ch if ch.isupper() and i else ch for i, ch in enumerate(name)).upper()
        out.append(f"{const}_FRAMES = {len(frames)}")
    out.append("")
    for name, frames, _, _ in groups:
        out.append(f"{name}:")
        out += [f"    .word {label}" for label, _ in frames]
    out.append("")
    out.append(";;     dy  tile  atributo    dx")
    for _, frames, _, _ in groups:
        for label, sprites in frames:
            out.append(f"{label}:")
            for dy, t, a, dx in sprites:
                out.append(f"    .byte {dy:2d}, ${t:02X}, %{a:08b}, {dx:2d}")
            out.append("    .byte METASPRITE_END")
    out.append("")
    out.append(";; Paletas de sprite (4 x 4 cores)")
    out.append("SprPalettes:")
    for i, p in enumerate(SPR_PALETTES):
        out.append("    .byte " + ", ".join(f"${c:02X}" for c in p) + f"   ; {i}")
    write_asm(ASM, out)

    preview(groups, hud, letters)
    print(f"{len(order)} tiles de sprite")


def preview(groups, hud, letters, scale=4):
    try:
        from PIL import Image
    except ImportError:
        return
    rows = [(frames, pal) for _, _, pal, frames in groups if frames]
    rows.append((list(hud.values()) + [letter(ch) for ch in letters], PAL_FX))
    cell_w, cell_h = 16 * scale + 6, 24 * scale + 6
    width = max(len(f) for f, _ in rows)
    img = Image.new("RGB", (width * cell_w + 6, len(rows) * cell_h + 6), (24, 24, 32))
    for r, (frames, pal) in enumerate(rows):
        palette = SPR_PALETTES[pal]
        for i, c in enumerate(frames):
            ox, oy = 6 + i * cell_w, 6 + r * cell_h
            for y in range(len(c)):
                for x in range(len(c[0])):
                    col = (0, 0, 0) if c[y][x] == 0 else rgb(palette, c[y][x])
                    for sy in range(scale):
                        for sx in range(scale):
                            img.putpixel((ox + x * scale + sx, oy + y * scale + sy), col)
    img.save(PREVIEW)


if __name__ == "__main__":
    main()
