;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The iNES header (contains a total of 16 bytes with the header flags)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "HEADER"
.byte $4E, $45, $53, $1A        ; 'N', 'E', 'S', 0x1A - Identificação do formato iNES
.byte $02                        ; Quantidade de PRG-ROM em blocos de 16KB (2 * 16KB = 32KB)
.byte $01                        ; Quantidade de CHR-ROM em blocos de 8KB  (1 * 8KB)
.byte %00000000                  ; Flags 6 - Mapper 0 (NROM), mirroring horizontal
; Bit 7-4 - Nibble baixo do número do mapper (0000 = NROM)
; Bit 3   - 1 = Four-screen VRAM (ignora o bit 0)
; Bit 2   - 1 = Trainer de 512 bytes presente antes da PRG-ROM
; Bit 1   - 1 = PRG-RAM com bateria em $6000-$7FFF (saves)
; Bit 0   - Mirroring: 0 = horizontal, 1 = vertical

.byte %00000000                  ; Flags 7
; Bit 7-4 - Nibble alto do número do mapper (0000)
; Bit 3-2 - 10 = formato NES 2.0, 00 = iNES clássico
; Bit 1   - PlayChoice-10
; Bit 0   - VS Unisystem

.byte $00                        ; PRG-RAM extra (0 = sem memória RAM adicional)
.byte $00                        ; TV system (0 = NTSC, 1 = PAL)
.byte $00                        ; Flags extras (sem expansão especial)
.byte $00,$00,$00,$00,$00        ; Preenchimento para completar 16 bytes do cabeçalho
