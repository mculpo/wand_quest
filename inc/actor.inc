;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Direção para onde um objeto está virado/andando.
;; O valor é usado como índice nas tabelas SideDeltaX/SideDeltaY e
;; SidePerpX/SidePerpY (src/player.asm), então a ordem aqui importa.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.enum Side
    NONE = 0                ; Parado / ainda não virou para lado nenhum
    UP = 1
    DOWN = 2
    RIGHT = 3
    LEFT = 4
.endenum

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tipos de objeto do jogo (NULL = slot livre no array)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.enum GameObjectType
    NULL = 0
    PLAYER = 1
    BLOCKS = 2
.endenum

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tipos de inimigo (EnemyType). NONE = slot livre. A ordem é a mesma das
;; tabelas EnemySpeed/EnemyAnimDelay (src/enemies.asm) e EnemyAnimByType
;; (src/enemy_anim.asm).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.enum EnemyType
    NONE = 0
    SLIME = 1               ; Gosma: devagar, para de vez em quando
    BAT = 2                 ; Morcego: rápido, muda muito de direção
    SPIDER = 3              ; Aranha: média, para nos cruzamentos
    GHOST = 4               ; Fantasma: devagar, atravessa blocos (não paredes)
.endenum

;; Os blocos não usam mais .struct: cada campo virou um array próprio
;; (Structure of Arrays) em src/variables.asm — BlockType, BlockX, BlockY...
;; Assim o índice do bloco é o próprio registrador X (0..MAX_BLOCKS-1) e
;; avançar para o próximo bloco é só um "inx".
