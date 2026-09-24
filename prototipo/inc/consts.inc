;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Registradores da PPU, APU e controle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PPU_CTRL    = $2000
PPU_MASK    = $2001
PPU_STATUS  = $2002
OAM_ADDR    = $2003
PPU_SCROLL  = $2005
PPU_ADDR    = $2006
PPU_DATA    = $2007
PPU_OAM_DMA = $4014
JOYPAD1     = $4016

OAM_BUFFER  = $0200

;; PPU_CTRL: NMI ligada, sprites 8x8 na pattern table $0000, fundo na $1000.
;; O bit 0 escolhe o nametable da esquerda/direita (vem do CamX) e o bit 2
;; liga o incremento de 32 (usado para escrever colunas).
PPU_CTRL_BASE  = %10010000
PPU_CTRL_INC32 = %00000100
PPU_MASK_ON    = %00011110      ; Fundo e sprites ligados, sem recorte na esquerda

;; Botões (bit a bit, como ficam em Buttons)
BUTTON_A      = $80
BUTTON_B      = $40
BUTTON_SELECT = $20
BUTTON_START  = $10
BUTTON_UP     = $08
BUTTON_DOWN   = $04
BUTTON_LEFT   = $02
BUTTON_RIGHT  = $01

METASPRITE_END = $80            ; Marca o fim de um metasprite (no lugar do dy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mapa e câmera
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Propriedades dos metatiles (MetaFlags em src/data/metatiles.asm)
MTF_SOLID  = %0001              ; Parede: bloqueia de todos os lados
MTF_ONEWAY = %0010              ; Passarela: só segura quem cai de cima
MTF_HURT   = %0100              ; Espinho: machuca quem pisa
MTF_EXIT   = %1000              ; Saída da fase

WORLD_W = 64 * 16               ; Largura do mundo em pixels (LEVEL_W metatiles)
WORLD_H = 48 * 16               ; Altura do mundo em pixels (LEVEL_H metatiles)
WORLD_TILES_W = 64 * 2          ; Largura em tiles de 8x8
WORLD_TILES_H = 48 * 2          ; Altura em tiles de 8x8
CAM_MAX_X = WORLD_W - 256       ; A câmera mostra 256x240
CAM_MAX_Y = WORLD_H - 240

;; Zona morta da câmera: posição do agente na tela em que a câmera começa a andar
CAM_LEFT   = 100
CAM_RIGHT  = 140
CAM_TOP    = 72
CAM_BOTTOM = 136
CAM_SPEED_X = 4                 ; Pixels por frame (menos de 8: no máximo 1 coluna nova por frame)
CAM_SPEED_Y = 6                 ; Pixels por frame (menos de 8: no máximo 1 linha nova por frame)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Agente (velocidades em 8.8: $0100 = 1 pixel por frame)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
RUN_SPEED     = $0180           ; 1,5 px/frame
RUN_ACCEL     = $0030
RUN_FRICTION  = $0040
JUMP_SPEED    = $0540           ; 5,25 px/frame para cima
GRAVITY_HOLD  = $0040           ; Subindo com o A apertado (pulo alto)
GRAVITY       = $0080           ; Caindo ou com o A solto (pulo baixo)
MAX_FALL      = $0500           ; 5 px/frame
ROLL_SPEED    = $0300           ; 3 px/frame
ROLL_TIME     = 22              ; Frames de rolamento
ROLL_INVULN_FROM = 21           ; Invencível enquanto o timer está entre estes dois valores
ROLL_INVULN_TO   = 4
HURT_TIME     = 18              ; Frames de recuo depois de levar dano
INVULN_TIME   = 70              ; Frames piscando invencível
KNOCK_X       = $0180
KNOCK_Y       = $0280
PLAYER_MAX_HP = 12
SHOOT_COOLDOWN = 10
SHOOT_POSE     = 12
DEAD_TIME      = 110            ; Frames entre a explosão e voltar ao começo
CLEAR_TIME     = 200            ; Frames mostrando "MISSAO COMPLETA"

;; Caixa de colisão do agente (em relação ao canto do sprite 16x24)
PBOX_X1 = 3
PBOX_X2 = 12
PBOX_Y1 = 2                     ; De pé
PBOX_Y1_LOW = 12                ; Agachado ou rolando
PBOX_Y2 = 23

;; Estados do agente
.enum PState
    NORMAL                      ; No chão ou no ar, controlável
    CROUCH                      ; Agachado
    ROLL                        ; Rolando (esquiva)
    HURT                        ; Recuo depois de levar dano
    DEAD                        ; Explodiu
.endenum

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tiros, inimigos e efeitos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
MAX_PBULLETS = 3                ; Tiros do agente ao mesmo tempo
MAX_EBULLETS = 6                ; Tiros dos inimigos ao mesmo tempo
PBULLET_SPEED = 5
EBULLET_SPEED = $0200           ; 2 px/frame (diagonal: $016A = 1,41)
EBULLET_DIAG  = $016A

MAX_ENEMIES = 6
MAX_LIST    = 32                ; Máximo de inimigos na lista da fase

.enum EnemyType
    NONE
    ROBOT                       ; Soldado-robô: patrulha e atira na horizontal
    TURRET                      ; Torreta: fixa, mira em 3 ângulos
    DRONE                       ; Drone: voa atrás do agente
.endenum

ROBOT_HP  = 3
ROBOT_SIGHT = 110              ; Distância (px) em que o soldado-robô vê o agente
TURRET_HP = 4
DRONE_HP  = 1
CONTACT_DAMAGE = 2
BULLET_DAMAGE  = 2
SPIKE_DAMAGE   = 3

MAX_FX = 6
.enum FxType
    NONE
    EXPLOSION                   ; 16x16, 4 quadros
    SPARK                       ; 8x8, 3 quadros (tiro batendo na parede)
    MUZZLE                      ; 8x8, clarão no cano
.endenum

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Estados do jogo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.enum GState
    PLAY
    PAUSE
    DEAD                        ; Agente explodiu: espera e recomeça
    CLEAR                       ; Chegou na saída
.endenum
