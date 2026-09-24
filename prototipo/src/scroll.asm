;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Câmera livre e scroll em 8 direções
;;
;; Mirroring vertical: os nametables $2000 e $2400 ficam lado a lado (512 px
;; de largura) e $2800/$2C00 repetem os de cima. Então:
;;   - na horizontal há 256 px de sobra fora da tela: cada coluna nova de
;;     tiles é escrita antes de aparecer, sem nenhum defeito;
;;   - na vertical o nametable tem 240 px, a altura da tela: a linha de tiles
;;     do mundo `tr` fica na linha `tr mod 30` do nametable. A linha nova
;;     aparece na borda de cima/baixo, que a TV e o FCEUX escondem (overscan).
;;
;; Colunas e linhas carregadas (tc/tr = coluna/linha de tiles 8x8 do mundo):
;;   - colunas: CamTileX até CamTileX + 32   (33, a tela mais a parcial)
;;   - linhas:  CamTileY + 1 até CamTileY + 30 (30; a linha parcial de cima
;;              divide o espaço com a de baixo e fica no overscan)
;; Quando a câmera cruza 8 px, o loop monta UMA coluna (ColBuf) e/ou UMA linha
;; (RowBuf) e a NMI copia para a PPU (FlushColumn / FlushRow). Como a câmera
;; anda menos de 8 px por frame, nunca precisa de mais de uma de cada.
;;
;; Atributos (paleta de cada metatile 16x16): a cópia AttrShadow guarda as
;; duas tabelas de atributos; cada metatile carregado atualiza o seu
;; quadrante de 2 bits e a NMI copia os bytes afetados.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "CODE"

;; Máscara que apaga cada quadrante (0 = sup. esq., 1 = sup. dir., 2 = inf. esq., 3 = inf. dir.)
QuadMask:   .byte %11111100, %11110011, %11001111, %00111111
;; Paleta p no quadrante q: QuadPal[q*4 + p]
QuadPal:
    .repeat 4, q
        .repeat 4, p
            .byte p << (q * 2)
        .endrepeat
    .endrepeat

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Muda o quadrante AttrQuad do byte AttrIdx da AttrShadow para a paleta A.
;; Destrói: A, Y (preserva X)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc SetAttrQuad
    sta Temp3
    lda AttrQuad                ; Y = quadrante * 4 + paleta
    asl
    asl
    ora Temp3
    tay
    lda QuadPal,y
    sta Temp3
    ldy AttrQuad
    lda QuadMask,y
    ldy AttrIdx
    and AttrShadow,y
    ora Temp3
    sta AttrShadow,y
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TopTbl/BotTbl = tabelas de tiles do lado (esquerdo/direito) do metatile
;; da coluna de tiles A (par = esquerdo).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc SetSideTables
    and #1
    bne Right
        MOV16I TopTbl, MetaTL
        MOV16I BotTbl, MetaBL
        rts
    Right:
        MOV16I TopTbl, MetaTR
        MOV16I BotTbl, MetaBR
        rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monta a coluna de tiles A do mundo em ColBuf (ordem das linhas do
;; nametable) para as linhas CamTileY+1 .. CamTileY+30, e atualiza os
;; atributos dos metatiles da coluna na AttrShadow.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc BuildColumn
    sta BuildTC
    jsr SetSideTables

    lda BuildTC                 ; Endereço no nametable: $2000/$2400 + coluna
    and #%00100000
    lsr
    lsr
    lsr
    ora #$20
    sta ColAddrHi
    ora #$03
    sta ColAttrHi               ; $23 ou $27
    lda BuildTC
    and #%00011111
    sta ColAddrLo
    lsr                         ; Coluna de atributos = coluna / 4
    lsr
    sta Temp
    ora #$C0
    sta ColAttrLo
    lda BuildTC                 ; Posição na AttrShadow = nametable * 64 + coluna de atributos
    and #%00100000
    asl
    ora Temp
    sta ColAttrIndex

    ldx CamTileY                ; Primeira linha = CamTileY + 1
    inx
    stx BuildTR
    txa                         ; BuildQ = linha mod 30
    :
        cmp #30
        bcc :+
        sbc #30
        jmp :-
    :
    sta BuildQ

    lda BuildTR                 ; Ptr = linha do mapa da primeira linha + coluna do metatile
    lsr
    cmp #LEVEL_H
    bcc :+
        lda #LEVEL_H - 1
    :
    tay
    lda BuildTC
    lsr
    clc
    adc LevelRowLo,y
    sta Ptr
    lda LevelRowHi,y
    adc #0
    sta Ptr+1

    lda #30
    sta BuildCount
    Loop:
        lda BuildTR             ; Fora do mapa (embaixo ou à direita): parede
        cmp #WORLD_TILES_H
        bcs Wall
        lda BuildTC
        cmp #WORLD_TILES_W
        bcs Wall
        ldy #0
        lda (Ptr),y
        jmp HaveMeta
    Wall:
        lda #MT_WALL
    HaveMeta:
        tay                     ; Y = metatile
        lda BuildTR
        and #1
        bne Bottom
            lda (TopTbl),y
            jmp Store
        Bottom:
            lda (BotTbl),y
        Store:
        ldx BuildQ
        sta ColBuf,x

        ; Atributo do metatile: na linha de cima dele, ou se é a primeira linha
        lda BuildTR
        and #1
        beq SetAttr
        lda BuildCount
        cmp #30
        bne NoAttr
        SetAttr:
            lda MetaPal,y
            pha
            lda BuildQ          ; Linha de metatiles no nametable = linha mod 30 / 2
            lsr
            sta BuildLr
            lsr                 ; Byte = base + (linha de metatiles / 2) * 8
            asl
            asl
            asl
            ora ColAttrIndex
            sta AttrIdx
            lda BuildLr         ; Quadrante = (linha & 1) * 2 + (coluna do metatile & 1)
            and #1
            asl
            sta AttrQuad
            lda BuildTC
            lsr
            and #1
            ora AttrQuad
            sta AttrQuad
            pla
            jsr SetAttrQuad
        NoAttr:

        lda BuildTR             ; Depois da linha de baixo do metatile, desce uma linha no mapa
        and #1
        beq :+
            clc
            lda Ptr
            adc #LEVEL_W
            sta Ptr
            bcc :+
            inc Ptr+1
        :
        inc BuildTR
        ldx BuildQ
        inx
        cpx #30
        bcc :+
            ldx #0
        :
        stx BuildQ
        dec BuildCount
        bne Loop
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monta a linha de tiles A do mundo em RowBuf (colunas CamTileX .. +32) e
;; os dois pedaços de endereço (um em cada nametable).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc BuildRow
    sta BuildTR
    :                           ; Temp2 = linha no nametable (mod 30)
        cmp #30
        bcc :+
        sbc #30
        jmp :-
    :
    sta Temp2
    lsr                         ; Linha de metatiles no nametable
    sta BuildLr
    lsr                         ; RowAttrOff = (linha de atributos) * 8
    asl
    asl
    asl
    sta RowAttrOff

    ; Endereço: $2000 + nt*$400 + linha*32 + coluna
    lda Temp2                   ; Byte alto da linha = linha / 8
    lsr
    lsr
    lsr
    sta Temp3
    lda Temp2                   ; Byte baixo = (linha & 7) * 32
    and #%00000111
    asl
    asl
    asl
    asl
    asl
    sta Temp2
    lda CamTileX
    and #%00100000              ; Nametable da primeira coluna
    lsr
    lsr
    lsr
    ora #$20
    ora Temp3
    sta RowSeg1Hi
    eor #%00000100              ; O outro nametable
    sta RowSeg2Hi
    lda CamTileX
    and #%00011111
    ora Temp2
    sta RowSeg1Lo
    lda Temp2
    sta RowSeg2Lo
    lda CamTileX
    and #%00011111
    sta Temp
    lda #32
    sec
    sbc Temp
    sta RowSeg1Len              ; Até o fim do primeiro nametable
    lda #33
    sec
    sbc RowSeg1Len
    sta RowSeg2Len              ; O resto no segundo

    lda BuildTR                 ; Tabelas: linha de cima ou de baixo do metatile
    and #1
    bne :+
        MOV16I TopTbl, MetaTL   ; TopTbl = coluna par, BotTbl = coluna ímpar
        MOV16I BotTbl, MetaTR
        jmp :++
    :
        MOV16I TopTbl, MetaBL
        MOV16I BotTbl, MetaBR
    :

    lda BuildTR                 ; Ptr = linha do mapa + coluna do metatile
    lsr
    cmp #LEVEL_H
    bcc :+
        lda #LEVEL_H - 1
    :
    tay
    lda CamTileX
    lsr
    clc
    adc LevelRowLo,y
    sta Ptr
    lda LevelRowHi,y
    adc #0
    sta Ptr+1

    lda CamTileX
    sta BuildTC
    lda #0
    sta BuildQ
    Loop:
        lda BuildTR
        cmp #WORLD_TILES_H
        bcs Wall
        lda BuildTC
        cmp #WORLD_TILES_W
        bcs Wall
        ldy #0
        lda (Ptr),y
        jmp HaveMeta
    Wall:
        lda #MT_WALL
    HaveMeta:
        tay
        lda BuildTC
        and #1
        bne Odd
            lda (TopTbl),y
            jmp Store
        Odd:
            lda (BotTbl),y
        Store:
        ldx BuildQ
        sta RowBuf,x

        ; Atributo: na coluna esquerda do metatile, ou se é o primeiro tile
        lda BuildTC
        and #1
        beq SetAttr
        lda BuildQ
        bne NoAttr
        SetAttr:
            lda MetaPal,y
            pha
            lda BuildTC         ; Byte = nt*64 + (linha de atributos)*8 + coluna de atributos
            and #%00100000
            asl
            ora RowAttrOff
            sta AttrIdx
            lda BuildTC
            and #%00011100
            lsr
            lsr
            ora AttrIdx
            sta AttrIdx
            lda BuildLr         ; Quadrante = (linha & 1) * 2 + (coluna do metatile & 1)
            and #1
            asl
            sta AttrQuad
            lda BuildTC
            lsr
            and #1
            ora AttrQuad
            sta AttrQuad
            pla
            jsr SetAttrQuad
        NoAttr:

        lda BuildTC             ; Depois da coluna direita do metatile, anda no mapa
        and #1
        beq :+
            inc Ptr
            bne :+
            inc Ptr+1
        :
        inc BuildTC
        inc BuildQ
        lda BuildQ
        cmp #33
        bne Loop
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NMI: copia ColBuf (30 tiles, incremento de 32) e os 8 bytes de atributo
;; da coluna. Totalmente desenrolado para caber no VBlank.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc FlushColumn
    lda #PPU_CTRL_BASE | PPU_CTRL_INC32
    sta PPU_CTRL
    bit PPU_STATUS
    lda ColAddrHi
    sta PPU_ADDR
    lda ColAddrLo
    sta PPU_ADDR
    .repeat 30, i
        lda ColBuf + i
        sta PPU_DATA
    .endrepeat
    lda #PPU_CTRL_BASE
    sta PPU_CTRL

    ldx ColAttrIndex
    .repeat 8, i
        lda ColAttrHi
        sta PPU_ADDR
        lda ColAttrLo
        ora #i * 8
        sta PPU_ADDR
        lda AttrShadow + i * 8,x
        sta PPU_DATA
    .endrepeat
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NMI: copia RowBuf (33 tiles em 2 pedaços) e a linha de atributos dos 2
;; nametables.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc FlushRow
    bit PPU_STATUS
    lda RowSeg1Hi
    sta PPU_ADDR
    lda RowSeg1Lo
    sta PPU_ADDR
    ldx #0
    ldy RowSeg1Len
    :
        lda RowBuf,x
        sta PPU_DATA
        inx
        dey
        bne :-
    lda RowSeg2Hi
    sta PPU_ADDR
    lda RowSeg2Lo
    sta PPU_ADDR
    ldy RowSeg2Len
    :
        lda RowBuf,x
        sta PPU_DATA
        inx
        dey
        bne :-

    ldx RowAttrOff
    lda #$23
    sta PPU_ADDR
    txa
    ora #$C0
    sta PPU_ADDR
    .repeat 8, i
        lda AttrShadow + i,x
        sta PPU_DATA
    .endrepeat
    lda #$27
    sta PPU_ADDR
    txa
    ora #$C0
    sta PPU_ADDR
    .repeat 8, i
        lda AttrShadow + 64 + i,x
        sta PPU_DATA
    .endrepeat
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calcula CamTileX/Y, ScrollX/Y e ScrollNt a partir de CamX/CamY.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc CamTileOf
    ; A = CamX / 8 (cabe em 8 bits: CamX <= 768)
    lda CamX+1
    sta Temp
    lda CamX
    lsr Temp
    ror
    lsr Temp
    ror
    lsr Temp
    ror
    rts
.endproc

.proc CamRowOf
    lda CamY+1
    sta Temp
    lda CamY
    lsr Temp
    ror
    lsr Temp
    ror
    lsr Temp
    ror
    rts
.endproc

.proc UpdateScrollRegs
    lda CamX
    sta ScrollX
    lda CamX+1
    and #1
    sta ScrollNt
    lda CamY                    ; ScrollY = CamY mod 240
    sta Temp
    lda CamY+1
    sta Temp2
    :
        lda Temp2
        bne Sub
        lda Temp
        cmp #240
        bcc Done
    Sub:
        sec
        lda Temp
        sbc #240
        sta Temp
        lda Temp2
        sbc #0
        sta Temp2
        jmp :-
    Done:
    lda Temp
    sta ScrollY
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Põe a câmera centrada no agente (dentro dos limites do mundo).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc CenterCamera
    sec                         ; CamX = PX + 8 - 128
    lda PX
    sbc #120
    sta CamX
    lda PX+1
    sbc #0
    sta CamX+1
    sec                         ; CamY = PY + 12 - 120
    lda PY
    sbc #108
    sta CamY
    lda PY+1
    sbc #0
    sta CamY+1
    jmp ClampCamera
.endproc

;; Limita a câmera a 0..CAM_MAX_X e 0..CAM_MAX_Y
.proc ClampCamera
    lda CamX+1
    bpl :+
        lda #0                  ; Negativo: 0
        sta CamX
        sta CamX+1
    :
    lda CamX
    cmp #<CAM_MAX_X
    lda CamX+1
    sbc #>CAM_MAX_X
    bcc :+
        MOV16I CamX, CAM_MAX_X
    :
    lda CamY+1
    bpl :+
        lda #0
        sta CamY
        sta CamY+1
    :
    lda CamY
    cmp #<CAM_MAX_Y
    lda CamY+1
    sbc #>CAM_MAX_Y
    bcc :+
        MOV16I CamY, CAM_MAX_Y
    :
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Desenha a tela inteira em volta da câmera (tela desligada): as 33 colunas
;; visíveis e as 2 tabelas de atributos.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc LoadFullScreen
    jsr CamTileOf
    sta CamTileX
    jsr CamRowOf
    sta CamTileY
    jsr UpdateScrollRegs

    lda #0
    sta Temp2
    Columns:
        lda CamTileX
        clc
        adc Temp2
        pha
        jsr BuildColumn         ; Usa Temp/Temp3, então o contador fica na pilha também
        pla
        jsr FlushColumn
        inc Temp2
        lda Temp2
        cmp #33
        bne Columns

    PPU_SETADDR $23C0           ; Atributos dos 2 nametables
    ldx #0
    :
        lda AttrShadow,x
        sta PPU_DATA
        inx
        cpx #64
        bne :-
    PPU_SETADDR $27C0
    :
        lda AttrShadow,x
        sta PPU_DATA
        inx
        cpx #128
        bne :-
    lda #0
    sta ColReady
    sta RowReady
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Um frame da câmera: segue o agente com zona morta e velocidade máxima,
;; e prepara a coluna/linha nova se cruzou 8 px.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc UpdateCamera
    lda ColReady                ; A NMI ainda não copiou a anterior (frame atrasado):
    ora RowReady                ; espera, para não perder uma coluna/linha
    beq :+
        rts
    :

    ; ---- Horizontal: posição do agente na tela = PX - CamX
    sec
    lda PX
    sbc CamX
    sta Temp
    lda PX+1
    sbc CamX+1
    bmi MoveLeftMax             ; Muito à esquerda (negativo)
    bne MoveRightMax            ; Muito à direita (>= 256)
    lda Temp
    cmp #CAM_LEFT
    bcc MoveLeft
    cmp #CAM_RIGHT + 1
    bcs MoveRight
    jmp Vertical
    MoveLeft:
        lda #CAM_LEFT
        sec
        sbc Temp                ; Quanto passou
        cmp #CAM_SPEED_X
        bcc :+
    MoveLeftMax:
        lda #CAM_SPEED_X
        :
        sta Temp
        sec
        lda CamX
        sbc Temp
        sta CamX
        lda CamX+1
        sbc #0
        sta CamX+1
        jmp Vertical
    MoveRight:
        sec
        sbc #CAM_RIGHT
        cmp #CAM_SPEED_X
        bcc :+
    MoveRightMax:
        lda #CAM_SPEED_X
        :
        clc
        adc CamX
        sta CamX
        lda CamX+1
        adc #0
        sta CamX+1

    Vertical:
    ; ---- Vertical: posição do agente na tela = PY - CamY
    sec
    lda PY
    sbc CamY
    sta Temp
    lda PY+1
    sbc CamY+1
    bmi MoveUpMax
    bne MoveDownMax
    lda Temp
    cmp #CAM_TOP
    bcc MoveUp
    cmp #CAM_BOTTOM + 1
    bcs MoveDown
    jmp Clamp
    MoveUp:
        lda #CAM_TOP
        sec
        sbc Temp
        cmp #CAM_SPEED_Y
        bcc :+
    MoveUpMax:
        lda #CAM_SPEED_Y
        :
        sta Temp
        sec
        lda CamY
        sbc Temp
        sta CamY
        lda CamY+1
        sbc #0
        sta CamY+1
        jmp Clamp
    MoveDown:
        sec
        sbc #CAM_BOTTOM
        cmp #CAM_SPEED_Y
        bcc :+
    MoveDownMax:
        lda #CAM_SPEED_Y
        :
        clc
        adc CamY
        sta CamY
        lda CamY+1
        adc #0
        sta CamY+1

    Clamp:
    jsr ClampCamera
    jsr UpdateScrollRegs

    ; ---- Coluna/linha nova? Primeiro calcula as duas posições novas, porque
    ; a linha usa as colunas novas e a coluna usa as linhas novas.
    lda #$FF
    sta RowTarget               ; $FF = nada para carregar
    sta ColTarget
    jsr CamRowOf
    cmp CamTileY
    beq :++
        bcc :+
            sta CamTileY        ; Desceu: carrega a linha de baixo (CamTileY + 30)
            clc
            adc #30
            sta RowTarget
            jmp :++
        :
            sta CamTileY        ; Subiu: carrega a linha de cima (CamTileY + 1)
            clc
            adc #1
            sta RowTarget
    :
    jsr CamTileOf
    cmp CamTileX
    beq :++
        bcc :+
            sta CamTileX        ; Foi para a direita: coluna CamTileX + 32
            clc
            adc #32
            sta ColTarget
            jmp :++
        :
            sta CamTileX        ; Foi para a esquerda: coluna CamTileX
            sta ColTarget
    :

    lda RowTarget
    cmp #$FF
    beq :+
        jsr BuildRow
        lda #1
        sta RowReady
    :
    lda ColTarget
    cmp #$FF
    beq :+
        jsr BuildColumn
        lda #1
        sta ColReady
    :
    rts
.endproc
