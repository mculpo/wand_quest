#!/usr/bin/env python3
"""
Solver das fases do Wand Quest.

Lê as fases em ASCII de src/levels.asm, simula as mesmas regras do jogo e
encontra, por busca em largura (BFS), a solução com o MENOR número de
empurrões. Serve para três coisas:

  1. Provar que cada fase tem solução (--check).
  2. Gerar docs/SOLUCOES.md com o passo a passo de cada fase.
  3. Gerar tests/solutions.lua, usado pelo teste automático no FCEUX.

Regras simuladas (iguais às do jogo):
  - O mapa é um grid de metatiles de 16x16 px. A área de jogo tem 12 colunas
    x 11 linhas; em volta dela tudo é parede.
  - O player anda livre pelo chão e pelos encaixes, mas não atravessa
    paredes nem blocos. Como ele tem o mesmo tamanho de um metatile, os
    lugares que ele alcança são exatamente as células ligadas por um
    caminho de 4 vizinhos (flood fill).
  - O player empurra um bloco atirando a bola de pedra (B): a bola anda em
    linha reta e acerta o primeiro bloco no caminho. Então, para empurrar um
    bloco numa direção, o player precisa alcançar QUALQUER célula livre na
    mesma linha/coluna, atrás do bloco (do lado oposto à direção), sem parede
    nem outro bloco entre ele e o bloco.
  - O bloco atingido desliza até a próxima célula ser parede ou outro
    bloco. O player está sempre atrás, então nunca fica no caminho.
  - A fase termina quando todos os encaixes têm um bloco em cima.
  - Inimigos (g, m, a, f) não entram no puzzle: eles nunca param blocos
    (um bloco deslizando esmaga o inimigo) e o player pode matá-los com a
    bola, então para o solver a célula deles é chão.

Uso:
  python tools/solver.py            -> valida e gera os arquivos
  python tools/solver.py --check    -> só valida (não gera os arquivos)
  python tools/solver.py --no-cache -> ignora o cache e resolve tudo de novo
"""

import json
import os
import re
import sys
from collections import deque

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
LEVELS_ASM = os.path.join(ROOT, "src", "levels.asm")
SOLUTIONS_MD = os.path.join(ROOT, "docs", "SOLUCOES.md")
SOLUTIONS_LUA = os.path.join(ROOT, "tests", "solutions.lua")

# Tamanho da área de jogo e onde ela começa no mapa 16x16 do jogo
LEVEL_W, LEVEL_H = 12, 11
ORIGIN_COL, ORIGIN_ROW = 2, 2
MAX_BLOCKS = 5

WALL, FLOOR, SLOT = "#", ".", "o"
BLOCK, PLAYER = "B", "P"
ENEMIES = "gmaf"            # Gosma, morcego, aranha, fantasma (para o puzzle contam como chão)
VALID_CHARS = set("#.oBP") | set(ENEMIES)

# Direções: nome, (dx, dy), seta
DIRS = [
    ("up", (0, -1), "↑", "cima"),
    ("down", (0, 1), "↓", "baixo"),
    ("left", (-1, 0), "←", "esquerda"),
    ("right", (1, 0), "→", "direita"),
]


class Level:
    def __init__(self, name, rows):
        self.name = name
        self.rows = rows
        self.walls = set()
        self.slots = set()
        blocks = []
        self.player = None
        for y, row in enumerate(rows):
            for x, ch in enumerate(row):
                if ch == WALL:
                    self.walls.add((x, y))
                elif ch == SLOT:
                    self.slots.add((x, y))
                elif ch == BLOCK:
                    blocks.append((x, y))
                elif ch == PLAYER:
                    if self.player is not None:
                        raise ValueError(f"{name}: mais de um 'P'")
                    self.player = (x, y)
        self.blocks = tuple(blocks)  # na ordem de leitura = índice no jogo

    def is_wall(self, x, y):
        return not (0 <= x < LEVEL_W and 0 <= y < LEVEL_H) or (x, y) in self.walls


def parse_levels(path=LEVELS_ASM):
    """Lê as fases do levels.asm: um label LevelNN: seguido de 11 .byte "..."."""
    levels = []
    current = None
    with open(path, encoding="utf-8") as f:
        for lineno, line in enumerate(f, 1):
            code = line.split(";", 1)[0]
            m = re.match(r"\s*(Level\d+)\s*:", code)
            if m:
                current = (m.group(1), [], lineno)
                levels.append(current)
                continue
            m = re.match(r'\s*\.byte\s+"([^"]*)"', code)
            if m and current is not None:
                current[1].append(m.group(1))
    result = []
    for name, rows, lineno in levels:
        if len(rows) != LEVEL_H or any(len(r) != LEVEL_W for r in rows):
            raise ValueError(f"{name} (linha {lineno}): precisa de {LEVEL_H} linhas de {LEVEL_W} caracteres")
        bad = set("".join(rows)) - VALID_CHARS
        if bad:
            raise ValueError(f"{name}: caracteres inválidos {bad}")
        result.append(Level(name, rows))
    return result


def reachable(level, blocks, start):
    """Células que o player alcança a partir de start (flood fill)."""
    seen = {start}
    queue = deque([start])
    while queue:
        x, y = queue.popleft()
        for _, (dx, dy), _, _ in DIRS:
            n = (x + dx, y + dy)
            if n not in seen and not level.is_wall(*n) and n not in blocks:
                seen.add(n)
                queue.append(n)
    return seen


def slide(level, blocks, index, d):
    """Desliza o bloco `index` na direção d. Devolve a posição final."""
    x, y = blocks[index]
    dx, dy = d
    others = set(blocks) - {blocks[index]}
    while not level.is_wall(x + dx, y + dy) and (x + dx, y + dy) not in others:
        x, y = x + dx, y + dy
    return (x, y)


def moves(level, blocks, player, reach=None):
    """Todos os empurrões possíveis: (índice do bloco, direção, célula do player, nova posição)."""
    if reach is None:
        reach = reachable(level, set(blocks), player)
    block_set = set(blocks)
    for i, (bx, by) in enumerate(blocks):
        for name, (dx, dy), arrow, word in DIRS:
            # Procura, voltando a partir do bloco, a célula livre mais próxima
            # de onde o player alcança e pode atirar (a linha até o bloco é livre)
            behind = None
            cx, cy = bx - dx, by - dy
            while not level.is_wall(cx, cy) and (cx, cy) not in block_set:
                if (cx, cy) in reach:
                    behind = (cx, cy)
                    break
                cx, cy = cx - dx, cy - dy
            if behind is None:
                continue
            end = slide(level, blocks, i, (dx, dy))
            if end == (bx, by):
                continue  # Encostado em algo: o empurrão não faz nada
            yield i, (name, (dx, dy), arrow, word), behind, end


def is_solved(level, blocks):
    return level.slots <= set(blocks)


def state_key(blocks, reach):
    # Os blocos são iguais entre si, então a ordem não importa para o estado.
    # A posição do player vira "a menor célula alcançável" (a região inteira).
    return (tuple(sorted(blocks)), min(reach))


def solve(level, max_states=500000):
    """BFS pelo menor número de empurrões. Devolve (passos, estados visitados) ou (None, n).

    É a mesma busca de moves()/reachable(), mas otimizada para as fases de
    5 blocos:
      - as células viram bits de um inteiro (y * 13 + x; a coluna 12 é uma
        parede "fantasma" que impede o deslocamento de dar a volta na linha);
      - a região que o player alcança é calculada crescendo a máscara nas
        4 direções de uma vez (deslocamento de bits) até parar de crescer;
      - para cada arranjo de blocos guardamos as regiões já vistas: se a
        célula do player cai numa delas, o estado é repetido e nem é preciso
        calcular a região de novo.
    """
    if is_solved(level, level.blocks):
        return [], 1

    WP = LEVEL_W + 1                         # Largura com a coluna fantasma
    def bit(x, y):
        return 1 << (y * WP + x)

    free = 0                                 # Células livres de parede
    for y in range(LEVEL_H):
        for x in range(LEVEL_W):
            if (x, y) not in level.walls:
                free |= bit(x, y)
    DELTAS = [-WP, WP, -1, 1]                # Mesma ordem de DIRS: up, down, left, right

    def flood(blockmask, start):
        space = free & ~blockmask
        reach = 1 << start
        while True:
            grown = (reach | (reach << 1) | (reach >> 1) | (reach << WP) | (reach >> WP)) & space
            if grown == reach:
                return reach
            reach = grown

    def is_free(c):
        return c >= 0 and (free >> c) & 1

    slots_mask = 0
    for x, y in level.slots:
        slots_mask |= bit(x, y)

    start_blocks = tuple(y * WP + x for x, y in level.blocks)
    start_mask = sum(1 << c for c in start_blocks)
    px, py = level.player
    reach = flood(start_mask, py * WP + px)
    key = (tuple(sorted(start_blocks)), (reach & -reach).bit_length())
    parent = {key: None}                     # estado -> (estado anterior, passo)
    regions = {key[0]: [reach]}              # arranjo de blocos -> regiões já vistas
    queue = deque([(start_blocks, start_mask, reach, key)])
    while queue:
        blocks, bmask, reach, key = queue.popleft()
        for i, p in enumerate(blocks):
            for d in range(4):
                delta = DELTAS[d]
                # Célula de onde atirar: a mais próxima do bloco, voltando pela
                # linha livre atrás dele, que o player alcança
                behind = -1
                c = p - delta
                while is_free(c) and not (bmask >> c) & 1:
                    if (reach >> c) & 1:
                        behind = c
                        break
                    c -= delta
                if behind < 0:
                    continue
                q = p
                while True:
                    n = q + delta
                    if not is_free(n) or (bmask >> n) & 1:
                        break
                    q = n
                if q == p:
                    continue                 # Encostado em algo: não se mexe
                nb = blocks[:i] + (q,) + blocks[i + 1:]
                nmask = bmask & ~(1 << p) | (1 << q)
                sb = tuple(sorted(nb))
                known = regions.get(sb)
                if known and any((r >> behind) & 1 for r in known):
                    continue                 # Mesmo arranjo e mesma região: repetido
                nreach = flood(nmask, behind)
                nkey = (sb, (nreach & -nreach).bit_length())
                regions.setdefault(sb, []).append(nreach)
                parent[nkey] = (key, (i, d, behind, q))
                if nmask & slots_mask == slots_mask:
                    raw = []
                    k = nkey
                    while parent[k] is not None:
                        k, s = parent[k]
                        raw.append(s)
                    steps = []
                    for bi, dd, b, e in reversed(raw):
                        steps.append((bi, DIRS[dd], (b % WP, b // WP), (e % WP, e // WP)))
                    return steps, len(parent)
                if len(parent) > max_states:
                    return None, len(parent)
                queue.append((nb, nmask, nreach, nkey))
    return None, len(parent)


def validate(level):
    """Erros de formato da fase (lista vazia = ok)."""
    errors = []
    if level.player is None:
        errors.append("sem 'P'")
    if not level.blocks:
        errors.append("sem blocos")
    if len(level.blocks) > MAX_BLOCKS:
        errors.append(f"{len(level.blocks)} blocos (máximo {MAX_BLOCKS})")
    if len(level.blocks) != len(level.slots):
        errors.append(f"{len(level.blocks)} blocos e {len(level.slots)} encaixes (precisam ser iguais)")
    return errors


# --------------------------------------------------------------------------
# Saída
# --------------------------------------------------------------------------

def render(level, blocks, player=None):
    """Desenha o mapa em texto: blocos numerados 1..5, '*' = bloco no encaixe."""
    grid = [list(r.replace(BLOCK, FLOOR).replace(PLAYER, FLOOR)) for r in level.rows]
    for i, (x, y) in enumerate(blocks):
        grid[y][x] = str(i + 1)
    if player:
        grid[player[1]][player[0]] = PLAYER
    header = "    " + " ".join("%X" % (c + 1) for c in range(LEVEL_W))
    lines = [header, "   +" + "-" * (LEVEL_W * 2 - 1) + "+"]
    for y, row in enumerate(grid):
        lines.append("%2d |%s|" % (y + 1, " ".join(row)))
    lines.append("   +" + "-" * (LEVEL_W * 2 - 1) + "+")
    return "\n".join(lines)


def replay(level, steps):
    """Reaplica os passos a partir do início, devolvendo os estados intermediários."""
    blocks = list(level.blocks)
    states = []
    for i, d, behind, end in steps:
        start = blocks[i]
        blocks[i] = end
        states.append((i, d, behind, start, end, tuple(blocks)))
    return states


def cell(p):
    """Coordenada para humanos: (coluna, linha) começando em 1, coluna em hexa como no mapa."""
    return "coluna %X, linha %d" % (p[0] + 1, p[1] + 1)


TIER_NAMES = ["Pedra", "Cristal", "Madeira", "Metal"]   # A cada 5 fases (src/effects.asm)


def tier_name(n):
    return TIER_NAMES[min((n - 1) // 5, len(TIER_NAMES) - 1)]


def pushes(n):
    return f"{n} empurrão" if n == 1 else f"{n} empurrões"


def write_markdown(results):
    os.makedirs(os.path.dirname(SOLUTIONS_MD), exist_ok=True)
    out = []
    out.append("# Wand Quest — Soluções das fases\n")
    out.append("> Arquivo gerado por `tools/solver.py` a partir de `src/levels.asm`. "
               "Não edite à mão: mude a fase e rode `makefile solve`.\n")
    out.append("## Como ler\n")
    out.append("- `#` parede, `.` chão, `o` encaixe, `P` player, `1`–`5` blocos.")
    out.append("- Inimigos: `g` gosma, `m` morcego, `a` aranha, `f` fantasma. Eles andam sozinhos;")
    out.append("  mate com a bola (B) ou esmague com um bloco. Encostar neles reinicia a fase.")
    out.append("- As colunas vão de `1` a `C` (hexadecimal) e as linhas de `1` a `11`.")
    out.append("- Cada passo diz **de onde atirar** a bola de pedra (B) e **para onde** o bloco vai.")
    out.append("  Dá para atirar de qualquer ponto livre dessa mesma linha/coluna, atrás do bloco;")
    out.append("  o passo mostra o mais perto dele. O caminho até lá está sempre livre.")
    out.append("- As soluções usam o **menor número de empurrões** possível.")
    out.append("- Travou? Aperte **Select** para reiniciar a fase. "
               "Com `DEBUG_LEVEL_SELECT = 1`, **Select + →/←** pula de fase.\n")

    out.append("## Resumo\n")
    out.append("| Fase | Dificuldade | Blocos | Empurrões |")
    out.append("|---:|:---|---:|---:|")
    for n, (level, steps, _) in enumerate(results, 1):
        out.append(f"| {n:02d} | {tier_name(n)} | {len(level.blocks)} | {len(steps)} |")
    out.append("")

    for n, (level, steps, _) in enumerate(results, 1):
        out.append(f"## Fase {n:02d} — {tier_name(n)}\n")
        out.append("```")
        out.append(render(level, level.blocks, level.player))
        out.append("```\n")
        out.append(f"<details><summary>Solução ({pushes(len(steps))})</summary>\n")
        for k, (i, d, behind, start, end, blocks) in enumerate(replay(level, steps), 1):
            name, _, arrow, word = d
            out.append(f"**{k}.** De ({cell(behind)}), atire para **{word} {arrow}** no **bloco {i + 1}** "
                       f"→ ele para em ({cell(end)}).\n")
            out.append("```")
            out.append(render(level, blocks, behind))
            out.append("```\n")
        out.append("</details>\n")
    with open(SOLUTIONS_MD, "w", encoding="utf-8", newline="\n") as f:
        f.write("\n".join(out))


def write_lua(results):
    os.makedirs(os.path.dirname(SOLUTIONS_LUA), exist_ok=True)
    out = ["-- Gerado por tools/solver.py. Cada empurrão: célula do player no mapa",
           "-- 16x16 do jogo (col, row), direção, e onde o bloco deve parar.",
           "return {"]
    for n, (level, steps, _) in enumerate(results, 1):
        out.append(f"  -- Fase {n:02d}")
        out.append("  {")
        for i, d, behind, start, end, blocks in replay(level, steps):
            out.append("    {col=%d, row=%d, dir=\"%s\", block=%d, endcol=%d, endrow=%d}," % (
                behind[0] + ORIGIN_COL, behind[1] + ORIGIN_ROW, d[0], i,
                end[0] + ORIGIN_COL, end[1] + ORIGIN_ROW))
        out.append("  },")
    out.append("}")
    with open(SOLUTIONS_LUA, "w", encoding="utf-8", newline="\n") as f:
        f.write("\n".join(out) + "\n")


# --------------------------------------------------------------------------
# Cache: as fases difíceis de 5 blocos levam alguns segundos cada, então a
# solução de cada fase fica guardada, indexada pelo conteúdo da fase. Só as
# fases que mudaram são resolvidas de novo.
# --------------------------------------------------------------------------
CACHE = os.path.join(ROOT, "tools", "solver_cache.json")
DIR_BY_NAME = {d[0]: d for d in DIRS}


def cache_key(level):
    # Inimigos não mudam a solução, então não entram na chave
    return "|".join(re.sub(f"[{ENEMIES}]", FLOOR, r) for r in level.rows)


def load_cache():
    try:
        with open(CACHE, encoding="utf-8") as f:
            return json.load(f)
    except (OSError, ValueError):
        return {}


def save_cache(cache):
    with open(CACHE, "w", encoding="utf-8", newline="\n") as f:
        json.dump(cache, f, indent=1, sort_keys=True)


def solve_cached(level, cache, use_cache=True):
    key = cache_key(level)
    if use_cache and key in cache:
        entry = cache[key]
        steps = [(i, DIR_BY_NAME[d], tuple(b), tuple(e)) for i, d, b, e in entry["steps"]]
        return steps, entry["states"], True
    steps, explored = solve(level)
    if steps is not None:
        cache[key] = {"states": explored,
                      "steps": [[i, d[0], list(b), list(e)] for i, d, b, e in steps]}
    return steps, explored, False


def main():
    check_only = "--check" in sys.argv
    use_cache = "--no-cache" not in sys.argv
    levels = parse_levels()
    cache = load_cache()
    results = []
    failed = False
    for n, level in enumerate(levels, 1):
        errors = validate(level)
        if errors:
            print(f"Fase {n:02d} ({level.name}): ERRO: " + "; ".join(errors))
            failed = True
            continue
        steps, explored, cached = solve_cached(level, cache, use_cache)
        if steps is None:
            print(f"Fase {n:02d} ({level.name}): SEM SOLUÇÃO ({explored} estados)")
            failed = True
            continue
        print(f"Fase {n:02d}: {len(level.blocks)} bloco(s), {len(steps):2d} empurrões, "
              f"{explored:6d} estados{' (cache)' if cached else ''}")
        results.append((level, steps, explored))
    # Guarda só as fases atuais (o cache não cresce com versões antigas)
    save_cache({cache_key(l): cache[cache_key(l)] for l, _, _ in results})
    if failed:
        sys.exit(1)
    if not check_only:
        write_markdown(results)
        write_lua(results)
        print(f"Gerado: {os.path.relpath(SOLUTIONS_MD, ROOT)} e {os.path.relpath(SOLUTIONS_LUA, ROOT)}")


if __name__ == "__main__":
    main()
