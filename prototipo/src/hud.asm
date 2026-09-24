;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HUD: barra de vida (sprites) e textos
;;
;; A barra fica na vertical, no estilo Mega Man: uma tampa e 6 segmentos,
;; cada um vale 2 pontos (cheio, metade ou vazio). Por ser vertical, usa só
;; 1 sprite por linha da tela (o limite da NES é 8), e por ser feita de
;; sprites não precisa dividir o scroll da tela.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "CODE"

HUD_X = 16
HUD_Y = 20

.proc RenderHud
    lda #HUD_X
    sta Temp
    lda #HUD_Y
    sta Temp2
    lda #PAL_FX_ATTR
    sta Temp3
    lda #TILE_HUD_CAP
    jsr DrawTileScreen

    ldx #5                      ; Segmentos de cima para baixo: o de cima é o 5
    Loop:
        lda Temp2
        clc
        adc #8
        sta Temp2
        txa                     ; Vida neste segmento = PHealth - 2 * índice
        asl
        sta Ptr
        lda PHealth
        sec
        sbc Ptr
        bcc Empty               ; Negativo
        beq Empty
        cmp #1
        beq Half
        lda #TILE_HUD_FULL
        jmp Draw
    Half:
        lda #TILE_HUD_HALF
        jmp Draw
    Empty:
        lda #TILE_HUD_EMPTY
    Draw:
        jsr DrawTileScreen
        dex
        bpl Loop
    rts
.endproc

;; Desenha o texto apontado por Ptr — lista de tiles terminada em $FF — a
;; partir da posição Temp = X, Temp2 = Y da tela. Destrói A, X, Y.
.proc DrawText
    lda #PAL_FX_ATTR
    sta Temp3
    ldx #0
    Loop:
        txa
        tay
        lda (Ptr),y
        cmp #$FF
        beq Done
        jsr DrawTileScreen      ; Preserva X
        lda Temp
        clc
        adc #8
        sta Temp
        inx
        jmp Loop
    Done:
    rts
.endproc

PAL_FX_ATTR = 1                 ; Paleta de sprite dos efeitos/HUD

TextPause:
    .byte TILE_LETTER_P, TILE_LETTER_A, TILE_LETTER_U, TILE_LETTER_S, TILE_LETTER_A, $FF
TextMission:
    .byte TILE_LETTER_M, TILE_LETTER_I, TILE_LETTER_S, TILE_LETTER_S, TILE_LETTER_A, TILE_LETTER_O, $FF
TextComplete:
    .byte TILE_LETTER_C, TILE_LETTER_O, TILE_LETTER_M, TILE_LETTER_P, TILE_LETTER_L, TILE_LETTER_E, TILE_LETTER_T, TILE_LETTER_A, $FF

;; Textos do estado atual (pausa / missão completa)
.proc RenderTexts
    lda GameState
    cmp #GState::PAUSE
    bne :+
        MOV16I Ptr, TextPause
        lda #108
        sta Temp
        lda #108
        sta Temp2
        jmp DrawText
    :
    cmp #GState::CLEAR
    bne :+
        MOV16I Ptr, TextMission
        lda #104
        sta Temp
        lda #100
        sta Temp2
        jsr DrawText
        MOV16I Ptr, TextComplete
        lda #96
        sta Temp
        lda #112
        sta Temp2
        jmp DrawText
    :
    rts
.endproc
