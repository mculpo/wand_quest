#!/usr/bin/env python3
"""
Gerador de fases candidatas (ferramenta de desenvolvimento).

Sorteia paredes, blocos e player, e então JOGA a fase ao contrário do que
o jogador fará: faz uma sequência aleatória de empurrões válidos e coloca
os encaixes onde os blocos terminaram. Assim toda fase gerada tem solução
por construção. Depois o solver acha a solução MÍNIMA, e só ficam as fases
cuja solução mínima tem pelo menos MIN_EMPURROES.

A saída já sai no formato de src/levels.asm; as fases escolhidas são
revisadas e coladas à mão no levels.asm.

Uso:
  python tools/levelgen.py BLOCOS MIN_EMPURROES [TENTATIVAS] [SEMENTE] [MAX_ESTADOS]
Ex.:
  python tools/levelgen.py 3 8 2000 42
"""

import random
import sys

import solver
from solver import LEVEL_W, LEVEL_H, Level


def random_level(rng, n_blocks):
    grid = [["." for _ in range(LEVEL_W)] for _ in range(LEVEL_H)]

    # Paredes: pilares soltos + alguns trechos de parede, às vezes espelhados
    # na horizontal (fica mais bonito e mais "desenhado").
    mirror = rng.random() < 0.5
    n_walls = rng.randint(4 + n_blocks, 10 + 2 * n_blocks)   # Mais blocos, mais paredes
    for _ in range(n_walls):
        x, y = rng.randrange(LEVEL_W), rng.randrange(LEVEL_H)
        length = rng.choice([1, 1, 1, 2, 3])
        horizontal = rng.random() < 0.5
        for k in range(length):
            cx, cy = (x + k, y) if horizontal else (x, y + k)
            if 0 <= cx < LEVEL_W and 0 <= cy < LEVEL_H:
                grid[cy][cx] = "#"
                if mirror:
                    grid[cy][LEVEL_W - 1 - cx] = "#"

    free = [(x, y) for y in range(LEVEL_H) for x in range(LEVEL_W) if grid[y][x] == "."]
    rng.shuffle(free)
    if len(free) < n_blocks + 1:
        return None
    blocks = [free.pop() for _ in range(n_blocks)]
    player = free.pop()
    for x, y in blocks:
        grid[y][x] = "B"
    grid[player[1]][player[0]] = "P"
    base = Level("Gen", ["".join(r) for r in grid])

    # Joga uma sequência aleatória de empurrões a partir do início
    state = tuple(blocks)
    who = player
    moved = set()
    for _ in range(rng.randint(n_blocks * 3, n_blocks * 9)):
        options = list(solver.moves(base, state, who))
        if not options:
            break
        i, d, behind, end = rng.choice(options)
        nb = list(state)
        nb[i] = end
        state = tuple(nb)
        who = behind
        moved.add(i)

    # Os encaixes ficam onde os blocos pararam; todos precisam ter saído do lugar
    if len(moved) < n_blocks or set(state) & set(blocks):
        return None
    for x, y in state:
        grid[y][x] = "o"
    return Level("Gen", ["".join(r) for r in grid])


def main():
    n_blocks = int(sys.argv[1])
    min_pushes = int(sys.argv[2])
    tries_max = int(sys.argv[3]) if len(sys.argv) > 3 else 2000
    seed = int(sys.argv[4]) if len(sys.argv) > 4 else 1
    max_states = int(sys.argv[5]) if len(sys.argv) > 5 else 60000
    rng = random.Random(seed)
    for tries in range(1, tries_max + 1):
        level = random_level(rng, n_blocks)
        if level is None or solver.validate(level):
            continue
        # Nenhum bloco pode começar em cima de um encaixe
        if set(level.blocks) & level.slots:
            continue
        steps, explored = solver.solve(level, max_states=max_states)
        if steps is None or len(steps) < min_pushes:
            continue
        # Todos os blocos precisam ser empurrados pelo menos uma vez
        if len({s[0] for s in steps}) < n_blocks:
            continue
        print(f"; {len(steps)} empurrões, {explored} estados (semente {seed}, tentativa {tries})")
        print("LevelXX:")
        for row in level.rows:
            print(f'    .byte "{row}"')
        print(flush=True)


if __name__ == "__main__":
    main()
