;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants for PPU registers mapped from addresses $2000 to $2007
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PPU_CTRL    = $2000
PPU_MASK    = $2001
PPU_STATUS  = $2002
OAM_ADDR    = $2003
OAM_DATA    = $2004
PPU_SCROLL  = $2005
PPU_ADDR    = $2006
PPU_DATA    = $2007
PPU_OAM_DMA = $4014


OAM_BUFFER = $0200      ; Endereço do OAM no NES
OAM_SIZE   = 256        ; OAM tem 256 bytes (64 sprites x 4 bytes)
OAM_OFFSET = $00        ; Variável para armazenar o deslocamento do buffer

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; APU ports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
APU_FLAGS   = $4015

SQ1_ENV     = $4000
SQ1_LO      = $4002
SQ1_HI      = $4003

SQ2_ENV     = $4004
SQ2_LO      = $4006
SQ2_HI      = $4007

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Maximum number of actors that we can have
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
MAX_ACTORS = 10
