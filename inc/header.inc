;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The iNES header (contains a total of 16 bytes with the header flags)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "HEADER"
.byte $4E, $45, $53, $1A        ; 'N', 'E', 'S', 0x1A - Identificação do formato iNES
.byte $02                        ; Quantidade de PRG-ROM (2 * 16KB = 32KB)
.byte $40                        ; Quantidade de CHR-ROM (64 * 8KB = 512KB)
.byte %11110000                  ; Configuração do Mapper (UNROM 512 - mapeador homebrew)
; Bit 7 - 1 = Ignorar controle de mirroring (mirroring definido pelo cartucho) ignora o bit 2
;        - 0 = Mirroring controlado pelo bit 2
; Bit 6 - 1 = PRG-ROM pode ser trocado via mapper (bancos de 16KB)
;        - 0 = PRG-ROM fixo (sem troca de bancos)
; Bit 5 - 1 = CHR-ROM pode ser trocado via mapper (bancos de 8KB)
;        - 0 = CHR-ROM fixo (sem troca de bancos)
; Bit 4 - 1 = Jogo usa UNROM 512 (sistema de troca de bancos avançado)
;        - 0 = Jogo usa UNROM normal
; Bit 3 - 1 = Usa CHR-RAM em vez de CHR-ROM
;        - 0 = Usa CHR-ROM (não pode ser modificado durante a execução)
; Bit 2 - 1 = Mirroring vertical (telas dispostas verticalmente)
;        - 0 = Mirroring horizontal (telas dispostas horizontalmente)
; Bit 1 - 1 = PRG-RAM com bateria (memória para saves)
;        - 0 = Sem PRG-RAM com bateria
; Bit 0 - 1 = Trainer presente (512 bytes extras no início da ROM)
;        - 0 = Sem Trainer

.byte %00000000                  ; Configuração extra do Mapper e formatação
; Bit 7-4 - 0000 = Continuação da configuração do Mapper (não afeta nada aqui)
; Bit 3 - 1 = Jogo compatível com PlayChoice-10 (sistema arcade da Nintendo)
;        - 0 = Jogo normal de NES
; Bit 2 - 1 = Usa formato NES 2.0 (expansão do iNES)
;        - 0 = Formato iNES clássico
; Bit 1 - 1 = Reservado (sem uso definido)
;        - 0 = Reservado (sem uso definido)
; Bit 0 - 1 = Reservado (sem uso definido)
;        - 0 = Reservado (sem uso definido)

.byte $00                        ; PRG-RAM extra (0 = sem memória RAM adicional)
.byte $00                        ; TV system (0 = NTSC, 1 = PAL)
.byte $00                        ; Flags extras (sem expansão especial)
.byte $00,$00,$00,$00,$00        ; Preenchimento para completar 16 bytes do cabeçalho

