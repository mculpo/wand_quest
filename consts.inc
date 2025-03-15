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
;; Constantes para o OAM (Object Attribute Memory) - Memória de sprites
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
OAM_BUFFER = $0200    ; Endereço onde os sprites são armazenados na RAM
OAM_SIZE   = 256      ; O OAM tem 256 bytes (64 sprites x 4 bytes por sprite)
OAM_OFFSET = $00      ; Variável usada para rastrear deslocamento dentro do OAM

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
;; Controller ports and buttons
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Maximum number of actors that we can have
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
MAX_ACTORS = 10
