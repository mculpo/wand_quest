;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macro para inicializar a NES: desliga renderização/interrupções,
;; espera a PPU estabilizar (2 VBlanks) e zera a RAM.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.macro INIT_NES
    sei                      ; Desliga as interrupções IRQ
    cld                      ; Desliga o modo decimal (a CPU da NES não tem BCD)
    ldx #$FF
    txs                      ; Inicializa o ponteiro da pilha em $01FF

    inx                      ; X = $FF + 1 = $00
    stx PPU_CTRL             ; Desliga a NMI
    stx PPU_MASK             ; Desliga a renderização (fundo e sprites)
    stx $4010                ; Desliga as IRQs do canal DMC

    lda #$40
    sta $4017                ; Desliga a IRQ do frame counter da APU

    bit PPU_STATUS           ; Lê PPU_STATUS para limpar a flag de VBlank
Wait1stVBlank:               ; Espera o primeiro VBlank da PPU
    bit PPU_STATUS           ; O bit 7 de PPU_STATUS vai para a flag N
    bpl Wait1stVBlank        ; Repete até o bit 7 ser 1 (dentro do VBlank)

    txa                      ; A = 0
ClearRAM:
    sta $0000,x              ; Zera a RAM de $0000 a $00FF
    sta $0100,x              ; Zera a RAM de $0100 a $01FF

    lda #$FF                 ; O buffer de OAM ($0200-$02FF) não pode ser zerado,
    sta $0200,x              ; senão todos os sprites apareceriam em (0,0): Y = $FF esconde

    lda #0                   ; E continua zerando o resto
    sta $0300,x              ; Zera a RAM de $0300 a $03FF
    sta $0400,x              ; Zera a RAM de $0400 a $04FF
    sta $0500,x              ; Zera a RAM de $0500 a $05FF
    sta $0600,x              ; Zera a RAM de $0600 a $06FF
    sta $0700,x              ; Zera a RAM de $0700 a $07FF
    inx
    bne ClearRAM

Wait2ndVBlank:               ; Espera o segundo VBlank (a PPU está pronta depois dele)
    bit PPU_STATUS
    bpl Wait2ndVBlank
.endmacro
