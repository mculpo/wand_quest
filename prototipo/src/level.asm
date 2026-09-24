;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mapa da fase: consultas de metatile e colisão com o cenário
;;
;; LevelMap (src/data/level1.asm) tem LEVEL_W x LEVEL_H metatiles de 16x16,
;; linha por linha. Como LEVEL_W = 64, o endereço da linha é base + linha*64.
;; Fora do mapa tudo conta como parede.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "CODE"

;; Endereço de cada linha do mapa
LevelRowLo:
    .repeat LEVEL_H, i
        .byte <(LevelMap + i * LEVEL_W)
    .endrepeat
LevelRowHi:
    .repeat LEVEL_H, i
        .byte >(LevelMap + i * LEVEL_W)
    .endrepeat

;; Metatile no ponto (PointX, PointY) do mundo.
;; Saída: A = metatile (MT_WALL fora do mapa)
;; Destrói: A, Y (preserva X)
.proc GetMetatileAt
    lda PointX+1                ; X >= 1024 ou negativo: fora
    cmp #>WORLD_W
    bcs Outside
    lda PointY+1
    cmp #>WORLD_H
    bcc :+
        bne Outside             ; Byte alto maior: fora
        lda PointY
        cmp #<WORLD_H
        bcs Outside
    :
    lda PointY+1                ; Linha = Y / 16 (0-47)
    sta Temp
    lda PointY
    lsr Temp
    ror
    lsr Temp
    ror
    lsr Temp
    ror
    lsr Temp
    ror
    tay
    lda LevelRowLo,y
    sta Ptr
    lda LevelRowHi,y
    sta Ptr+1

    lda PointX+1                ; Coluna = X / 16 (0-63)
    sta Temp
    lda PointX
    lsr Temp
    ror
    lsr Temp
    ror
    lsr Temp
    ror
    lsr Temp
    ror
    tay
    lda (Ptr),y
    rts
Outside:
    lda #MT_WALL
    rts
.endproc

;; Propriedades (MTF_*) do metatile no ponto (PointX, PointY).
;; Destrói: A, Y (preserva X)
.proc GetFlagsAt
    jsr GetMetatileAt
    tay
    lda MetaFlags,y
    rts
.endproc

;; O ponto (PointX, PointY) é parede? Saída: C = 1 se sólido. Destrói: A, Y
.proc IsSolidAt
    jsr GetFlagsAt
    lsr                         ; MTF_SOLID é o bit 0: vai direto para o carry
    rts
.endproc

;; As caixas A e B (16 bits, limites inclusivos) se sobrepõem? C = 1 se sim.
;; Destrói: A
.proc BoxesOverlap
    lda AX2                     ; A.x2 < B.x1 ?
    cmp BX1
    lda AX2+1
    sbc BX1+1
    bcc No
    lda BX2                     ; B.x2 < A.x1 ?
    cmp AX1
    lda BX2+1
    sbc AX1+1
    bcc No
    lda AY2                     ; A.y2 < B.y1 ?
    cmp BY1
    lda AY2+1
    sbc BY1+1
    bcc No
    lda BY2                     ; B.y2 < A.y1 ?
    cmp AY1
    lda BY2+1
    sbc AY1+1
    bcc No
    sec
    rts
No:
    clc
    rts
.endproc

;; Caixa A = caixa do agente (depende de estar de pé ou agachado/rolando).
;; Destrói: A
.proc SetPlayerBoxA
    clc
    lda PX
    adc #PBOX_X1
    sta AX1
    lda PX+1
    adc #0
    sta AX1+1
    clc
    lda PX
    adc #PBOX_X2
    sta AX2
    lda PX+1
    adc #0
    sta AX2+1
    clc
    lda PY
    adc PBoxTop
    sta AY1
    lda PY+1
    adc #0
    sta AY1+1
    clc
    lda PY
    adc #PBOX_Y2
    sta AY2
    lda PY+1
    adc #0
    sta AY2+1
    rts
.endproc
