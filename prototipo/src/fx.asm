;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Efeitos visuais: explosão (16x16), faísca e clarão do cano (8x8)
;; Cada efeito toca a sua animação uma vez e some.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "CODE"

;; Duração (frames) de cada tipo (índice = FxType)
FxDuration: .byte 0, EXPLOSION_FRAMES * 5, SPARK_FRAMES * 3, 4

.proc ClearFx
    lda #FxType::NONE
    ldx #MAX_FX - 1
    :
        sta FxType,x
        dex
        bpl :-
    rts
.endproc

;; Cria o efeito A em (SpawnX, SpawnY). O clarão do cano segue o lado do agente.
;; Preserva X.
.proc SpawnFx
    sta Temp
    txa
    pha
    ldx #0
    :
        lda FxType,x
        beq Found
        inx
        cpx #MAX_FX
        bne :-
    jmp Done
    Found:
    lda Temp
    sta FxType,x
    tay
    lda FxDuration,y
    sta FxTimer,x
    lda SpawnX
    sta FxXL,x
    lda SpawnX+1
    sta FxXH,x
    lda SpawnY
    sta FxYL,x
    lda SpawnY+1
    sta FxYH,x
    lda #0
    ldy PFacing
    beq :+
        lda #$40
    :
    sta FxFlip,x
    Done:
    pla
    tax
    rts
.endproc

.proc UpdateFx
    ldx #0
    :
        lda FxType,x
        beq :+
        dec FxTimer,x
        bne :+
        lda #FxType::NONE
        sta FxType,x
    :
        inx
        cpx #MAX_FX
        bne :--
    rts
.endproc

;; Desenha os efeitos: o quadro sai do tempo que passou.
.proc RenderFx
    ldx #0
    Loop:
        lda FxType,x
        beq Next
        cmp #FxType::EXPLOSION
        bne NotExplosion
            lda #EXPLOSION_FRAMES * 5   ; Quadro = (duração - tempo) / 5
            sec
            sbc FxTimer,x
            ldy #0
            Div5:
                cmp #5
                bcc Div5Done
                sbc #5
                iny
                jmp Div5
            Div5Done:
            tya
            asl
            tay
            lda Explosion,y
            sta MetaPtr
            lda Explosion+1,y
            sta MetaPtr+1
            jmp Draw
        NotExplosion:
        cmp #FxType::SPARK
        bne Muzzle
            lda #SPARK_FRAMES * 3       ; Quadro = (duração - tempo) / 3
            sec
            sbc FxTimer,x
            ldy #0
            Div3:
                cmp #3
                bcc Div3Done
                sbc #3
                iny
                jmp Div3
            Div3Done:
            tya
            asl
            tay
            lda Spark,y
            sta MetaPtr
            lda Spark+1,y
            sta MetaPtr+1
            jmp Draw
        Muzzle:
        MOV16I MetaPtr, Muzzle0
        Draw:
        lda FxXL,x
        sta MetaWX
        lda FxXH,x
        sta MetaWX+1
        lda FxYL,x
        sta MetaWY
        lda FxYH,x
        sta MetaWY+1
        jsr DrawMetaWorld
    Next:
        inx
        cpx #MAX_FX
        bne Loop
    rts
.endproc
