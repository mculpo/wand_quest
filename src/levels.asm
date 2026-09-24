;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; As 20 fases do jogo, em ASCII.
;;
;; Cada fase tem 11 linhas de 12 caracteres (a área de jogo de 12x11
;; metatiles; a parede em volta já existe e não aparece aqui):
;;   #  parede (pilar)          .  chão
;;   o  encaixe                 B  bloco
;;   P  posição inicial do player
;;   g  gosma      m  morcego      a  aranha      f  fantasma   (inimigos, até 4)
;;
;; Regras para a fase ser válida (o tools/solver.py confere tudo):
;;   - exatamente um P;
;;   - de 1 a MAX_BLOCKS (4) blocos, e o mesmo número de encaixes;
;;   - ter solução.
;; Depois de editar uma fase, rode  makefile solve  para validar e
;; regenerar docs/SOLUCOES.md e tests/solutions.lua.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Endereço de cada fase (índice = número da fase - 1)
LevelTable:
    .word Level01
    .word Level02
    .word Level03
    .word Level04
    .word Level05
    .word Level06
    .word Level07
    .word Level08
    .word Level09
    .word Level10
    .word Level11
    .word Level12
    .word Level13
    .word Level14
    .word Level15
    .word Level16
    .word Level17
    .word Level18
    .word Level19
    .word Level20

.assert (* - LevelTable) / 2 = LEVEL_COUNT, error, "LevelTable precisa ter LEVEL_COUNT fases"

Level01:
    .byte "............"
    .byte "............"
    .byte "............"
    .byte "............"
    .byte "........#..."
    .byte "..B....oo..."
    .byte "............"
    .byte "............"
    .byte "........B..."
    .byte "........P..."
    .byte "............"

Level02:
    .byte ".....###o.##"
    .byte ".........g#."
    .byte "..........#."
    .byte ".###......#."
    .byte ".....#......"
    .byte "#.B......#.#"
    .byte "#..........#"
    .byte "#..#.......o"
    .byte "...#.....B.."
    .byte "............"
    .byte "..#.#.#..P.."

Level03:
    .byte "...#.....P.."
    .byte "#........#.."
    .byte "o....##...B."
    .byte "..##.#......"
    .byte ".....#.#...."
    .byte ".......###.."
    .byte ".#...g......"
    .byte ".#.B..#....."
    .byte ".#.........."
    .byte ".........##o"
    .byte "...........#"

Level04:
    .byte "..#......#.."
    .byte "........g..o"
    .byte "#..#..B.#..#"
    .byte "#....g.....#"
    .byte "#........B.#"
    .byte "............"
    .byte ".........P.."
    .byte ".#........#."
    .byte "....####...."
    .byte "#..........#"
    .byte "...........o"

Level05:
    .byte "##........##"
    .byte "...##..##..o"
    .byte "............"
    .byte "..#......#.."
    .byte "#.g........#"
    .byte "#...B.....P#"
    .byte "#..........#"
    .byte "....B......."
    .byte ".#........#."
    .byte "....#g.#...."
    .byte "o..........."

Level06:
    .byte "o.#........#"
    .byte "...P.......o"
    .byte "....#......."
    .byte ".#...Bg.####"
    .byte ".#.........."
    .byte ".....B....#."
    .byte ".#.......m.#"
    .byte ".##........."
    .byte ".##........."
    .byte "........B.#."
    .byte "o..........."

Level07:
    .byte "...###......"
    .byte "....m......."
    .byte ".......#...."
    .byte ".......#..#."
    .byte "....#......."
    .byte "............"
    .byte ".#.....B..B."
    .byte "............"
    .byte "#P.....B.m.."
    .byte ".###.......o"
    .byte ".#o....#...o"

Level08:
    .byte "...........o"
    .byte "..#..B.B..#."
    .byte "..#........#"
    .byte "..#o##......"
    .byte "...#...###.."
    .byte "...#.......#"
    .byte "....B.P....#"
    .byte "............"
    .byte "....#......."
    .byte "....#......."
    .byte "m......go###"

Level09:
    .byte "....#..#...."
    .byte "....#B.#.P.."
    .byte ".#........#."
    .byte ".....##....."
    .byte ".....##....."
    .byte ".###.##.###."
    .byte "......BB...."
    .byte "..........m."
    .byte "............"
    .byte "............"
    .byte ".#.m....oo#o"

Level10:
    .byte "m.#o.....#.."
    .byte ".B...##....."
    .byte ".....##.P..."
    .byte "o..######..o"
    .byte "##.g.B....##"
    .byte "...######..."
    .byte "............"
    .byte "#...#..#...#"
    .byte "##........##"
    .byte "##..B.....##"
    .byte ".#.#...m#.#."

Level11:
    .byte ".#.........."
    .byte ".#Bm........"
    .byte "....#...a###"
    .byte "....#......#"
    .byte ".........#.#"
    .byte ".........#.."
    .byte "#........#.."
    .byte "..B.B.#..##."
    .byte "o........B.."
    .byte "#.###......."
    .byte "oo...P.....o"

Level12:
    .byte "..........oo"
    .byte ".....B..P.B."
    .byte ".....##....."
    .byte ".....##....."
    .byte ".#...##...#."
    .byte ".#.######.#."
    .byte ".#......a.#."
    .byte ".#...##...#."
    .byte "..B..##....."
    .byte "...#B...#..."
    .byte "o.##.a..##.o"

Level13:
    .byte ".m..o###o#.o"
    .byte "#........#g."
    .byte "#B.B.....#.."
    .byte "#..........."
    .byte "...#........"
    .byte "..##........"
    .byte "..........#."
    .byte ".#.B...B...."
    .byte ".#........P#"
    .byte "a..........."
    .byte "o..........."

Level14:
    .byte "o.o#.......#"
    .byte "...........#"
    .byte "...........o"
    .byte "...B..#...#."
    .byte "#.....#..m.."
    .byte "....#.#.B..."
    .byte ".#.....a...."
    .byte ".....#....B."
    .byte "..#.#......."
    .byte "P..B##......"
    .byte "....#o.a...."

Level15:
    .byte "o#...#o...##"
    .byte "....B#....a#"
    .byte ".....#......"
    .byte "#......B..##"
    .byte ".........###"
    .byte "#...aB..#..P"
    .byte "..........B."
    .byte "..####m....."
    .byte ".....#......"
    .byte ".#...#....#."
    .byte ".##.....oo#."

Level16:
    .byte ".......#oo.."
    .byte "......#o..f."
    .byte "...####.#..."
    .byte ".#....#B..##"
    .byte "..B###......"
    .byte "......a..B.."
    .byte "#.P........."
    .byte ".........###"
    .byte "....B.#....#"
    .byte "#...B....###"
    .byte "..#.#o.....o"

Level17:
    .byte "..o######..."
    .byte "...#...o#..."
    .byte "#..##.B##..#"
    .byte "o#........#."
    .byte "...B..B..f.."
    .byte "..P.....BB.."
    .byte "............"
    .byte "..########oo"
    .byte "###......###"
    .byte "##..####..##"
    .byte ".#f..##...#."

Level18:
    .byte "............"
    .byte "..########.."
    .byte "P...o##....."
    .byte ".....##B...."
    .byte "#.BB.##...f#"
    .byte "#..#.##.#..#"
    .byte "#.#......#.#"
    .byte ".#.#....#o#."
    .byte "##..B..m..##"
    .byte "##..a.....##"
    .byte "ooB.####...o"

Level19:
    .byte ".#o....B..#."
    .byte "..#...B.P#.."
    .byte ".#of####Bo#."
    .byte "##......B.##"
    .byte ".#........#."
    .byte "...#.##.#..."
    .byte "##..#..#..##"
    .byte ".#.....B.a#o"
    .byte ".#........#."
    .byte ".....##....o"
    .byte "..##...f##.."

Level20:
    .byte "o.....P....."
    .byte "#..#B#......"
    .byte "#....#...#.."
    .byte ".#f..#...#.."
    .byte "#....#..B..#"
    .byte "...#.BB....."
    .byte ".m.........."
    .byte "....#.fB...."
    .byte "..#.#o##...a"
    .byte "....#o##...."
    .byte ".##..o#...#o"
