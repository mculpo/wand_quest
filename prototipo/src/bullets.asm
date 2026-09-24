;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tiros
;;
;; Tiros do agente (PB*): 8x8, velocidade inteira em px/frame (5 para frente
;; ou para cima). Tiros dos inimigos (EB*): orbe 8x8 com velocidade 8.8, que
;; permite ir na diagonal. Os dois somem ao bater numa parede (com uma
;; faísca) ou ao sair da área da câmera.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "CODE"

;; Limpa todos os tiros
.proc ClearBullets
    lda #0
    ldx #MAX_PBULLETS - 1
    :
        sta PBActive,x
        dex
        bpl :-
    ldx #MAX_EBULLETS - 1
    :
        sta EBActive,x
        dex
        bpl :-
    rts
.endproc

;; O ponto (PointX, PointY) está a mais de 16 px para fora da tela? C = 1 se fora.
;; Dentro = X na tela entre -16 e 271, Y entre -16 e 255.
.proc IsOffCamera
    sec                         ; (X - CamX + 16) tem que ser < 288
    lda PointX
    sbc CamX
    sta Temp
    lda PointX+1
    sbc CamX+1
    sta Temp2
    clc
    lda Temp
    adc #16
    sta Temp
    lda Temp2
    adc #0
    bmi Off
    cmp #1
    bcc CheckY                  ; 0-255: dentro
    bne Off                     ; 512+: fora
    lda Temp
    cmp #288 - 256
    bcs Off
    CheckY:
    sec                         ; (Y - CamY + 16) tem que ser < 272
    lda PointY
    sbc CamY
    sta Temp
    lda PointY+1
    sbc CamY+1
    sta Temp2
    clc
    lda Temp
    adc #16
    sta Temp
    lda Temp2
    adc #0
    bmi Off
    cmp #1
    bcc In
    bne Off
    lda Temp
    cmp #272 - 256
    bcs Off
    In:
    clc
    rts
Off:
    sec
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tiros do agente: andam, batem na parede, saem da tela.
;; (O acerto nos inimigos fica em src/enemies.asm.)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc UpdatePBullets
    ldx #0
    Loop:
        lda PBActive,x
        beq Next
        lda PBVX,x              ; X += vx (com sinal)
        clc
        adc PBXL,x
        sta PBXL,x
        lda PBVX,x
        and #$80
        beq :+
            lda #$FF
        :
        adc PBXH,x
        sta PBXH,x
        lda PBVY,x              ; Y += vy
        clc
        adc PBYL,x
        sta PBYL,x
        lda PBVY,x
        and #$80
        beq :+
            lda #$FF
        :
        adc PBYH,x
        sta PBYH,x

        clc                     ; Centro do tiro
        lda PBXL,x
        adc #4
        sta PointX
        lda PBXH,x
        adc #0
        sta PointX+1
        clc
        lda PBYL,x
        adc #4
        sta PointY
        lda PBYH,x
        adc #0
        sta PointY+1
        jsr IsOffCamera
        bcs Remove
        jsr IsSolidAt
        bcc Next
        jsr SparkAtPoint
    Remove:
        lda #0
        sta PBActive,x
    Next:
        inx
        cpx #MAX_PBULLETS
        bne Loop
        rts
.endproc

;; Faísca centrada em (PointX, PointY). Preserva X.
.proc SparkAtPoint
    sec
    lda PointX
    sbc #4
    sta SpawnX
    lda PointX+1
    sbc #0
    sta SpawnX+1
    sec
    lda PointY
    sbc #4
    sta SpawnY
    lda PointY+1
    sbc #0
    sta SpawnY+1
    lda #FxType::SPARK
    jmp SpawnFx
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cria um tiro de inimigo em (SpawnX, SpawnY) com velocidade (SpawnVX, SpawnVY).
;; Preserva X.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc SpawnEBullet
    txa
    pha
    ldx #0
    :
        lda EBActive,x
        beq Found
        inx
        cpx #MAX_EBULLETS
        bne :-
    jmp Done
    Found:
    lda #1
    sta EBActive,x
    lda SpawnX
    sta EBXL,x
    lda SpawnX+1
    sta EBXH,x
    lda SpawnY
    sta EBYL,x
    lda SpawnY+1
    sta EBYH,x
    lda #0
    sta EBXS,x
    sta EBYS,x
    lda SpawnVX
    sta EBVXL,x
    lda SpawnVX+1
    sta EBVXH,x
    lda SpawnVY
    sta EBVYL,x
    lda SpawnVY+1
    sta EBVYH,x
    Done:
    pla
    tax
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tiros dos inimigos: andam (8.8), batem na parede, acertam o agente.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc UpdateEBullets
    ldx #0
    Loop:
        lda EBActive,x
        bne :+
            jmp Next
        :
        clc                     ; X += vx (8.8 com sinal)
        lda EBXS,x
        adc EBVXL,x
        sta EBXS,x
        lda EBXL,x
        adc EBVXH,x
        sta EBXL,x
        lda EBVXH,x
        and #$80
        beq :+
            lda #$FF
        :
        adc EBXH,x
        sta EBXH,x
        clc                     ; Y += vy
        lda EBYS,x
        adc EBVYL,x
        sta EBYS,x
        lda EBYL,x
        adc EBVYH,x
        sta EBYL,x
        lda EBVYH,x
        and #$80
        beq :+
            lda #$FF
        :
        adc EBYH,x
        sta EBYH,x

        clc                     ; Centro
        lda EBXL,x
        adc #4
        sta PointX
        lda EBXH,x
        adc #0
        sta PointX+1
        clc
        lda EBYL,x
        adc #4
        sta PointY
        lda EBYH,x
        adc #0
        sta PointY+1
        jsr IsOffCamera
        bcs Remove
        jsr IsSolidAt
        bcc :+
            jsr SparkAtPoint
            jmp Remove
        :
        ; Acertou o agente? Caixa 4x4 no meio do orbe contra a caixa do agente
        lda PState
        cmp #PState::DEAD
        beq Next
        jsr SetPlayerBoxA
        sec
        lda PointX
        sbc #2
        sta BX1
        lda PointX+1
        sbc #0
        sta BX1+1
        ADD16_8 BX2, PointX, 1
        sec
        lda PointY
        sbc #2
        sta BY1
        lda PointY+1
        sbc #0
        sta BY1+1
        ADD16_8 BY2, PointY, 1
        jsr BoxesOverlap
        bcc Next
        jsr IsPlayerInvulnerable
        bcs Next                ; Invencível: o tiro atravessa
        lda #BULLET_DAMAGE
        jsr DamagePlayerX
    Remove:
        lda #0
        sta EBActive,x
    Next:
        inx
        cpx #MAX_EBULLETS
        beq :+
        jmp Loop
    :
    rts
.endproc

;; DamagePlayer preservando X
.proc DamagePlayerX
    sta Temp
    txa
    pha
    lda Temp
    jsr DamagePlayer
    pla
    tax
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Desenha os tiros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc RenderBullets
    ldx #0
    PLoop:
        lda PBActive,x
        beq PNext
        lda PBVY,x              ; Tiro para cima usa o desenho vertical
        beq :+
            MOV16I MetaPtr, ShotV0
            jmp :++
        :
            MOV16I MetaPtr, ShotH0
        :
        lda PBXL,x
        sta MetaWX
        lda PBXH,x
        sta MetaWX+1
        lda PBYL,x
        sta MetaWY
        lda PBYH,x
        sta MetaWY+1
        jsr DrawMetaWorld
    PNext:
        inx
        cpx #MAX_PBULLETS
        bne PLoop

    ldx #0
    ELoop:
        lda EBActive,x
        beq ENext
        lda Frame               ; Orbe pulsando
        and #%00000100
        beq :+
            MOV16I MetaPtr, Orb1
            jmp :++
        :
            MOV16I MetaPtr, Orb0
        :
        lda EBXL,x
        sta MetaWX
        lda EBXH,x
        sta MetaWX+1
        lda EBYL,x
        sta MetaWY
        lda EBYH,x
        sta MetaWY+1
        jsr DrawMetaWorld
    ENext:
        inx
        cpx #MAX_EBULLETS
        bne ELoop
    rts
.endproc
