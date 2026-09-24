;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Desenho de sprites no buffer de OAM ($0200)
;;
;; Os objetos vivem em coordenadas do mundo (16 bits). DrawMetaWorld converte
;; para a tela subtraindo a câmera e descarta cada sprite 8x8 que fica fora
;; da tela — assim um inimigo pela metade na borda aparece só com a metade
;; que está dentro.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "CODE"

;; Desenha o metasprite MetaPtr com o canto em (MetaWX, MetaWY) do mundo.
;; Formato: .byte dy, tile, atributo, dx ... METASPRITE_END
;; Destrói: A, Y (preserva X)
.proc DrawMetaWorld
    txa
    pha

    sec                         ; BaseSX = MetaWX - CamX
    lda MetaWX
    sbc CamX
    sta BaseSX
    lda MetaWX+1
    sbc CamX+1
    sta BaseSX+1
    sec                         ; BaseSY = MetaWY - CamY - 1 (o sprite aparece 1 linha abaixo)
    lda MetaWY
    sbc CamY
    sta BaseSY
    lda MetaWY+1
    sbc CamY+1
    sta BaseSY+1
    lda BaseSY
    sec
    sbc #1
    sta BaseSY
    lda BaseSY+1
    sbc #0
    sta BaseSY+1

    ldx OamIndex
    ldy #0
    Loop:
        lda (MetaPtr),y         ; dy
        cmp #METASPRITE_END
        beq Done
        clc                     ; Y na tela = BaseSY + dy (16 bits)
        adc BaseSY
        sta Temp
        lda BaseSY+1
        adc #0
        bne SkipSprite          ; Fora da tela (acima ou abaixo)
        lda Temp
        cmp #$EF
        bcs SkipSprite          ; Linhas 239+ não aparecem

        iny                     ; tile
        iny                     ; atributo
        iny                     ; dx
        lda (MetaPtr),y
        clc
        adc BaseSX
        sta SprX
        lda BaseSX+1
        adc #0
        bne SkipX               ; Fora da tela na horizontal

        lda Temp
        sta OAM_BUFFER+0,x
        dey
        dey
        lda (MetaPtr),y
        sta OAM_BUFFER+1,x
        iny
        lda (MetaPtr),y
        sta OAM_BUFFER+2,x
        iny
        lda SprX
        sta OAM_BUFFER+3,x
        iny
        inx
        inx
        inx
        inx
        beq Full                ; OAM cheio (64 sprites)
        jmp Loop

    SkipSprite:
        iny
        iny
        iny
    SkipX:
        iny
        jmp Loop

    Full:
        ldx #$FC                ; Fica no último sprite
    Done:
        stx OamIndex
        pla
        tax
        rts
.endproc

;; Desenha um tile 8x8 direto na tela: A = tile, Temp = X, Temp2 = Y, Temp3 = atributo.
;; Destrói: A, Y (preserva X)
.proc DrawTileScreen
    ldy OamIndex
    sta OAM_BUFFER+1,y
    lda Temp2
    sta OAM_BUFFER+0,y
    lda Temp3
    sta OAM_BUFFER+2,y
    lda Temp
    sta OAM_BUFFER+3,y
    tya
    clc
    adc #4
    sta OamIndex
    rts
.endproc

;; Esconde (Y = $FF) todos os sprites depois do último desenhado.
.proc HideUnusedSprites
    ldx OamIndex
    lda #$FF
    :
        sta OAM_BUFFER,x
        inx
        inx
        inx
        inx
        bne :-
    rts
.endproc
