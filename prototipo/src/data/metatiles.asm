;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Metatiles 16x16 do cenário. Gerado por tools/draw_tiles.py (não edite à mão).
;; Cada tabela é indexada pelo número do metatile (constantes MT_*).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

MT_BACK       = 0
MT_PANEL      = 1
MT_WALL       = 2
MT_WALL_TOP   = 3
MT_PLATFORM   = 4
MT_SPIKES     = 5
MT_PIPE       = 6
MT_LIGHT      = 7
MT_SCREEN     = 8
MT_EXIT       = 9
MT_COUNT        = 10

MetaTL:    .byte $01, $04, $08, $0C, $0F, $10, $12, $16, $1A, $1E
MetaTR:    .byte $00, $05, $09, $0D, $0F, $10, $13, $17, $1B, $1F
MetaBL:    .byte $02, $06, $0A, $0E, $00, $11, $14, $18, $1C, $20
MetaBR:    .byte $03, $07, $0B, $0B, $00, $11, $15, $19, $1D, $21
MetaPal:   .byte 1, 1, 0, 0, 0, 2, 3, 2, 3, 3      ; Paleta de fundo (0-3)
MetaFlags: .byte %0000, %0000, %0001, %0001, %0010, %0101, %0000, %0000, %0000, %1000  ; MTF_SOLID/ONEWAY/HURT/EXIT

;; Paletas de fundo (4 x 4 cores)
BgPalettes:
    .byte $0F, $2D, $00, $10   ; 0
    .byte $0F, $01, $0C, $1C   ; 1
    .byte $0F, $06, $16, $27   ; 2
    .byte $0F, $09, $1A, $2A   ; 3
