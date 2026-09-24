;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macros (a maioria veio do Wand Quest, com as de 16 bits novas)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Coloca a NES num estado conhecido: desliga tudo, espera a PPU e zera a RAM
.macro INIT_NES
    sei
    cld
    ldx #$FF
    txs
    inx
    stx PPU_CTRL
    stx PPU_MASK
    stx $4010
    lda #$40
    sta $4017
    bit PPU_STATUS
:   bit PPU_STATUS              ; 1º VBlank
    bpl :-
    txa
:   sta $0000,x
    sta $0100,x
    sta $0300,x
    sta $0400,x
    sta $0500,x
    sta $0600,x
    sta $0700,x
    lda #$FF
    sta $0200,x                 ; OAM com Y = $FF: todos os sprites escondidos
    lda #0
    inx
    bne :-
:   bit PPU_STATUS              ; 2º VBlank
    bpl :-
.endmacro

;; Endereço de 16 bits no PPU_ADDR
.macro PPU_SETADDR addr
    bit PPU_STATUS
    lda #>addr
    sta PPU_ADDR
    lda #<addr
    sta PPU_ADDR
.endmacro

.macro PUSH_REGS
    pha
    txa
    pha
    tya
    pha
.endmacro

.macro PULL_REGS
    pla
    tay
    pla
    tax
    pla
.endmacro

;; dest (16 bits) = valor imediato
.macro MOV16I dest, value
    lda #<(value)
    sta dest
    lda #>(value)
    sta dest+1
.endmacro

;; dest = src1 + src2 (16 bits, variáveis)
.macro ADD16 dest, src1, src2
    clc
    lda src1
    adc src2
    sta dest
    lda src1+1
    adc src2+1
    sta dest+1
.endmacro

;; dest = src1 - src2 (16 bits, variáveis)
.macro SUB16 dest, src1, src2
    sec
    lda src1
    sbc src2
    sta dest
    lda src1+1
    sbc src2+1
    sta dest+1
.endmacro

;; dest = src + imediato de 8 bits (sem sinal)
.macro ADD16_8 dest, src, value
    clc
    lda src
    adc #value
    sta dest
    lda src+1
    adc #0
    sta dest+1
.endmacro

;; dest (16 bits) = (arrayLo,x ; arrayHi,x) + imediato de 8 bits (para os arrays SoA)
.macro ADD16_8_X dest, arrLo, arrHi, value
    clc
    lda arrLo,x
    adc #value
    sta dest
    lda arrHi,x
    adc #0
    sta dest+1
.endmacro

;; Posição de 24 bits (sub, lo, hi) += velocidade 8.8 com sinal (vel = lo/fração, vel+1 = inteiro)
.macro ADD_VEL sub, pos, vel
    .local Positive
    clc
    lda sub
    adc vel
    sta sub
    lda pos
    adc vel+1
    sta pos
    lda vel+1                   ; Estende o sinal da velocidade no byte alto
    and #$80
    beq Positive
        lda #$FF
    Positive:
    adc pos+1
    sta pos+1
.endmacro
