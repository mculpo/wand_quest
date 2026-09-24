;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constantes para os registradores da PPU (Picture Processing Unit)
;; Endereços mapeados de $2000 a $2007
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PPU_CTRL    = $2000  ; Controla a PPU (modo de rolagem, geração de NMI, etc.)
PPU_MASK    = $2001  ; Controla a renderização da PPU (sprites, fundo, cor, etc.)
PPU_STATUS  = $2002  ; Status da PPU (bit de vblank, colisão de sprites, etc.)
OAM_ADDR    = $2003  ; Define o endereço no buffer de sprites (OAM)
OAM_DATA    = $2004  ; Escreve ou lê dados do buffer de sprites (OAM)
PPU_SCROLL  = $2005  ; Define a posição de rolagem (scroll) horizontal e vertical
PPU_ADDR    = $2006  ; Define o endereço para acessar a VRAM (memória de vídeo)
PPU_DATA    = $2007  ; Lê ou escreve dados na VRAM (memória de vídeo)
PPU_OAM_DMA = $4014  ; Inicia uma transferência de DMA para o buffer de sprites

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuração da PPU usada durante o gameplay
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PPU_CTRL_GAME = %10010000     ; NMI ligada, sprites na pattern table $0000, fundo na $1000
PPU_MASK_GAME = %00011110     ; Mostra fundo e sprites, sem recorte nos 8px da esquerda

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constantes para o OAM (Object Attribute Memory) - Memória de sprites
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
OAM_BUFFER = $0200    ; Endereço onde os sprites são armazenados na RAM
OAM_SIZE   = 256      ; O OAM tem 256 bytes (64 sprites x 4 bytes por sprite)
OAM_OFFSET = $00      ; Variável usada para rastrear deslocamento dentro do OAM

METASPRITE_END = $80  ; Marca o fim de uma tabela de metasprite (no lugar do "dy")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Registradores da APU (Audio Processing Unit) - Som do NES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
APU_FLAGS   = $4015  ; Controla os canais de áudio (ativa/desativa sons)

;; Canal de Onda Quadrada 1 (Square Wave 1)
SQ1_ENV     = $4000  ; Controla volume e envelope do canal 1
SQ1_LO      = $4002  ; Define a frequência do som (parte baixa)
SQ1_HI      = $4003  ; Define a frequência do som (parte alta) e inicia a nota

;; Canal de Onda Quadrada 2 (Square Wave 2) - Segundo canal de som
SQ2_ENV     = $4004  ; Controla volume e envelope do canal 2
SQ2_LO      = $4006  ; Define a frequência do som (parte baixa)
SQ2_HI      = $4007  ; Define a frequência do som (parte alta) e inicia a nota

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Portas do controle e máscaras dos botões (bit a bit, como ficam em Buttons)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
JOYPAD1       = $4016
JOYPAD2       = $4017

BUTTON_A      = $80          ; 10000000
BUTTON_B      = $40          ; 01000000
BUTTON_SELECT = $20          ; 00100000
BUTTON_START  = $10          ; 00010000
BUTTON_UP     = $08          ; 00001000
BUTTON_DOWN   = $04          ; 00000100
BUTTON_LEFT   = $02          ; 00000010
BUTTON_RIGHT  = $01          ; 00000001

SIDE_UP    = %00  ; 0 em binário
SIDE_DOWN  = %01  ; 1 em binário
SIDE_RIGHT = %10  ; 2 em binário
SIDE_LEFT  = %11  ; 3 em binário

PLAYER_OFFSET_X = 4
PLAYER_OFFSET_Y = 4

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convenção de hitbox: todo objeto tem 16x16 e os limites são INCLUSIVOS,
;; ou seja, X2 = X + HITBOX_SIZE e Y2 = Y + HITBOX_SIZE (último pixel do objeto).
;; Dois objetos lado a lado (sem sobrepor nenhum pixel) NÃO estão colidindo.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
HITBOX_SIZE = 15

;; Bola de pedra (src/ball.asm)
BALL_SPEED        = 4         ; Pixels por frame (andando 1 por vez: nunca atravessa nada)
BALL_HITBOX       = 7         ; A bola tem 8x8 (limites inclusivos: X2 = X + 7)
IMPACT_ANIM_DELAY = 4         ; Frames de cada quadro da poeira do impacto

;; Inimigos (src/enemies.asm)
MAX_ENEMIES       = 4         ; Inimigos ao mesmo tempo
ENEMY_HIT_INSET   = 3         ; A hitbox do inimigo é 3 px menor de cada lado (encostar de raspão não mata)
POOF_ANIM_DELAY   = 4         ; Frames de cada quadro do poof (morte do inimigo)
ENEMY_KEEP_CHANCE = 150       ; Chance (em 256) de seguir reto num cruzamento
ENEMY_PAUSE_CHANCE = 40       ; Chance (em 256) de a gosma/aranha parar um pouco numa célula
DIE_ANIM_DELAY    = 5         ; Frames de cada quadro da morte do mago
DIE_EXTRA_TIME    = 30        ; Frames parado depois da animação de morte, antes de reiniciar

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Valores do mapa de colisão do cenário (bgcollision)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
TILE_EMPTY = $00              ; Chão livre
TILE_SOLID = $01              ; Parede (bloqueia player e blocos)
TILE_SLOT  = $02              ; Encaixe: o bloco que parar aqui muda de cor

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Blocos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
MAX_ACTORS = 10

MAX_BLOCKS     = 5            ; Quantidade máxima de blocos ao mesmo tempo
BLOCK_SPEED    = 3            ; Pixels por frame que um bloco empurrado anda
BLOCK_PALETTE  = 0            ; Paleta de sprite de um bloco fora do encaixe
SLOT_PALETTE   = 2            ; Paleta de sprite de um bloco parado no encaixe
NO_BLOCK       = $FF          ; Índice "nenhum bloco" (para IgnoreBlock)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Player
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CORNER_ASSIST  = 6            ; Até quantos pixels o player é "puxado" para entrar num corredor

;; Animação do mago (quadros em src/player_anim.asm, gerado por tools/draw_sprites.py)
WALK_ANIM_DELAY = 3           ; Andando: frames por quadro (12 quadros = 36 frames = 2 passos de 18 px)
IDLE_ANIM_DELAY = 6           ; Parado:  frames por quadro (16 quadros = pouco mais de 1,5 s)
CAST_ANIM_DELAY = 4           ; Lançando a bola: frames por quadro
FACE_HOLD       = 20          ; Frames virado para a direção depois de parar/lançar a bola

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fases
;;
;; Cada fase é a área de jogo de 12x11 metatiles escrita em ASCII em
;; src/levels.asm. No mapa 16x16 (LevelMap) ela começa na coluna 2, linha 2;
;; o resto do mapa é parede (a moldura desenhada no nametable).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
LEVEL_COUNT    = 20           ; Quantidade de fases
LEVEL_W        = 12           ; Largura da área de jogo (metatiles)
LEVEL_H        = 11           ; Altura da área de jogo (metatiles)
LEVEL_COL0     = 2            ; Coluna do mapa onde a área de jogo começa
LEVEL_ROW0     = 2            ; Linha do mapa onde a área de jogo começa
LEVEL_FIRST_CELL = LEVEL_ROW0 * 16 + LEVEL_COL0   ; Índice no mapa do canto superior esquerdo

;; Caracteres usados nas fases em ASCII
LCHAR_WALL     = '#'          ; Parede (pilar)
LCHAR_FLOOR    = '.'          ; Chão
LCHAR_SLOT     = 'o'          ; Encaixe
LCHAR_BLOCK    = 'B'          ; Bloco (em cima de chão)
LCHAR_PLAYER   = 'P'          ; Posição inicial do player (em cima de chão)
LCHAR_SLIME    = 'g'          ; Inimigo: gosma
LCHAR_BAT      = 'm'          ; Inimigo: morcego
LCHAR_SPIDER   = 'a'          ; Inimigo: aranha
LCHAR_GHOST    = 'f'          ; Inimigo: fantasma

DEBUG_LEVEL_SELECT = 1        ; 1 = Select + Direita/Esquerda pula de fase (desligue na versão final)

LEVEL_CLEAR_DELAY = 45        ; Frames de pausa depois de completar a fase, antes do efeito
LEVEL_TEXT_DELAY  = 70        ; Frames que o texto "FASE 00" fica na tela

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Efeitos (src/effects.asm)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
VRAM_BUFFER_SIZE    = 128     ; Bytes do buffer de escrita na VRAM (esvaziado a cada NMI)
DISSOLVE_PER_FRAME  = 4       ; Quantos metatiles o dissolve processa por frame

;; Tipos de metatile desenhados pelo efeito (índice nas tabelas Metatile*).
;; Os três primeiros são os mesmos valores do mapa de colisão (TILE_*).
MT_FLOOR       = TILE_EMPTY   ; Chão
MT_WALL        = TILE_SOLID   ; Pilar (o QueueMetatile troca pelo modelo da dificuldade)
MT_SLOT        = TILE_SLOT    ; Encaixe
MT_SPARKLE     = 3            ; Brilho (quadro intermediário do dissolve)
MT_PILLAR0     = 4            ; Pilares de cada dificuldade: MT_PILLAR0 + dificuldade

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dificuldades: a cada 5 fases muda o modelo do pilar, as cores da área de
;; jogo e as cores dos blocos (TierPalettes em src/effects.asm).
;;   0: fases  1-5  (2 blocos)    2: fases 11-15 (4 blocos)
;;   1: fases  6-10 (3 blocos)    3: fases 16-20 (5 blocos)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
LEVELS_PER_TIER = 5
TIER_COUNT      = 4

PAL_FRAME       = $3F01       ; Cores 1-3 da paleta de fundo 0 (moldura da tela)
PAL_PLAYFIELD   = $3F05       ; Cores 1-3 da paleta de fundo 1 (área de jogo)
PAL_BLOCKS      = $3F11       ; Cores 1-3 da paleta de sprite 0 (blocos fora do encaixe)
PAL_ENEMIES     = $3F15       ; Cores 1-3 da paleta de sprite 1 (inimigos)

;; Posição do texto no nametable (linha 14 de tiles, no meio da área de jogo)
TEXT_ROW       = 14
HUD_ROW        = 28           ; Faixa de baixo da moldura com "FASE 00" (tools/make_background.py)
