;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilidades: controle, números aleatórios, paletas e limpeza da tela
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "CODE"

;; Lê o controle 1 em Buttons e calcula Pressed (botões que acabaram de ser
;; apertados neste frame).
.proc ReadControllers
    lda Buttons
    sta PrevButtons
    lda #1
    sta Buttons
    sta JOYPAD1
    lsr
    sta JOYPAD1
    :
        lda JOYPAD1
        lsr
        rol Buttons
        bcc :-
    lda PrevButtons
    eor #$FF
    and Buttons
    sta Pressed
    rts
.endproc

;; Número aleatório de 8 bits em A (LFSR de 16 bits). Destrói Y.
.proc GetRandomNumber
    ldy #8
    lda Seed
    :
        asl
        rol Seed+1
        bcc :+
        eor #$39
    :
        dey
        bne :--
    sta Seed
    rts
.endproc

;; Copia as 8 paletas (fundo e sprites) para a PPU. Tela desligada.
.proc LoadPalettes
    PPU_SETADDR $3F00
    ldx #0
    :
        lda BgPalettes,x
        sta PPU_DATA
        inx
        cpx #16
        bne :-
    ldx #0
    :
        lda SprPalettes,x
        sta PPU_DATA
        inx
        cpx #16
        bne :-
    rts
.endproc

;; Zera os 2 nametables (tiles e atributos) e a cópia dos atributos. Tela desligada.
.proc ClearNametables
    PPU_SETADDR $2000
    lda #0
    ldy #8                      ; 8 x 256 = 2 KB ($2000-$27FF)
    ldx #0
    :
        sta PPU_DATA
        inx
        bne :-
        dey
        bne :-
    ldx #0
    :
        sta AttrShadow,x
        inx
        cpx #128
        bne :-
    rts
.endproc

;; Espera a próxima NMI terminar (usado com a tela desligada, fora do loop)
.proc WaitNmi
    lda #0
    sta NmiDone
    :
        lda NmiDone
        beq :-
    rts
.endproc
