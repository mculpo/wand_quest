.segment "CODE"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Os blocos ficam em Structure of Arrays (ver src/variables.asm):
;; o bloco de índice N (0..MAX_BLOCKS-1) é BlockType+N, BlockX+N, etc.
;; Nas rotinas abaixo o índice do bloco atual fica sempre em X.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adiciona um bloco no primeiro slot livre (BlockType = NULL).
;; Os blocos de cada fase são criados pelo efeito de entrada
;; (UpdateCellObjects em src/effects.asm), quando o quadrado deles aparece.
;; Se todos os MAX_BLOCKS slots estiverem ocupados, não faz nada.
;;
;; Entrada: ParamX, ParamY = posição do bloco em pixels
;; Saída:   C = 0 se adicionou, C = 1 se o array estava cheio
;; Destrói: A, X (preserva Y)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc AddNewBlock
    ldx #0
    FindFreeSlot:
        lda BlockType,x
        beq AddBlock                ; GameObjectType::NULL = 0: slot livre
        inx
        cpx #MAX_BLOCKS
        bne FindFreeSlot
        rts                         ; Cheio (o cpx igual deixou C = 1)

    AddBlock:
        lda #GameObjectType::BLOCKS
        sta BlockType,x
        lda ParamX
        sta BlockX,x
        lda ParamY
        sta BlockY,x
        lda #BLOCK_SPEED
        sta BlockSpeed,x
        lda #BLOCK_PALETTE
        sta BlockAttr,x
        lda #Side::NONE             ; Começa parado
        sta BlockSide,x
        clc
        rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Atualiza todos os blocos ativos: os que estão deslizando (BlockSide
;; diferente de NONE) andam até BlockSpeed pixels neste frame.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc UpdateBlock
    ldx #0
    Loop:
        lda BlockType,x
        beq Next                    ; Slot livre
        lda BlockSide,x
        beq Next                    ; Side::NONE: parado
        jsr MoveBlock               ; Preserva X
    Next:
        inx
        cpx #MAX_BLOCKS
        bne Loop
        rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Move o bloco X, um pixel por vez, BlockSpeed vezes na direção BlockSide.
;;
;; Andar de 1 em 1 pixel (em vez de somar a velocidade de uma vez) garante
;; que o bloco nunca atravessa uma parede, outro bloco ou o player.
;; Inimigos não param o bloco: são esmagados por ele.
;; Quando bate em algo, para (Side::NONE) e verifica se ficou no encaixe.
;;
;; Entrada: X = índice do bloco
;; Destrói: A, Y (preserva X)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc MoveBlock
    lda BlockSpeed,x
    sta StepCount
    stx IgnoreBlock                 ; O bloco não pode colidir consigo mesmo

    StepLoop:
        ldy BlockSide,x             ; Y = direção, índice nas tabelas de deslocamento
        lda BlockX,x
        clc
        adc SideDeltaX,y            ; Posição X depois de andar 1 pixel
        sta BoxX1
        lda BlockY,x
        clc
        adc SideDeltaY,y            ; Posição Y depois de andar 1 pixel
        sta BoxY1
        jsr SetBoxSize

        jsr CheckBoxVsWorld         ; Parede ou outro bloco?
        bcs Blocked
        jsr CheckBoxVsPlayer        ; Player?
        bcs Blocked

        lda BoxX1                   ; Livre: confirma o passo
        sta BlockX,x
        lda BoxY1
        sta BlockY,x
        jsr CrushEnemiesInBox       ; Esmaga os inimigos no caminho (preserva X)

        dec StepCount
        bne StepLoop
        rts

    Blocked:
        lda #Side::NONE
        sta BlockSide,x             ; Para de deslizar
        jsr UpdateBlockPalette      ; Ajusta a cor conforme o encaixe
        jmp CheckLevelComplete      ; Tail call: todos os encaixes ocupados?
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ajusta a paleta do bloco X conforme o lugar onde ele parou:
;; SLOT_PALETTE se o centro dele está sobre um TILE_SLOT, senão BLOCK_PALETTE
;; (assim, empurrar um bloco para fora do encaixe devolve a cor original).
;;
;; Entrada: X = índice do bloco
;; Destrói: A, Y (preserva X)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc UpdateBlockPalette
    lda BlockX,x
    clc
    adc #8
    sta PointX                      ; Centro do bloco (16x16)
    lda BlockY,x
    clc
    adc #8
    sta PointY
    lda #TILE_SLOT
    sta ParamTile
    jsr IsPointOnTile               ; C = 1 se o centro está no encaixe

    lda #BLOCK_PALETTE
    bcc :+
        lda #SLOT_PALETTE
    :
    sta ParamAttrIn
    lda BlockAttr,x
    sta ParamAttrOut
    jsr SetSpritePalette            ; Troca só os bits de paleta do atributo
    lda ParamAttrOut
    sta BlockAttr,x
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Desenha todos os blocos ativos no OAM usando o metasprite MetaBlock.
;; A paleta de cada bloco (BlockAttr) entra como MetaAttr.
;;
;; A NES só mostra 8 sprites por linha; os que vêm depois no OAM somem.
;; Por isso a ordem de desenho gira a cada frame (começa por um bloco
;; diferente): se passar do limite, os sprites piscam em vez de sumir.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc RenderBlocks
    lda #<MetaBlock                 ; Todos os blocos usam a mesma tabela
    sta MetaPtr
    lda #>MetaBlock
    sta MetaPtr+1

    ldx RenderStart                 ; Gira o bloco inicial: 0, 1, ..., MAX_BLOCKS-1, 0...
    inx
    cpx #MAX_BLOCKS
    bcc :+
        ldx #0
    :
    stx RenderStart                 ; X = primeiro bloco a desenhar neste frame

    lda #MAX_BLOCKS
    sta RenderCount
    Loop:
        lda BlockType,x
        beq Next                    ; Slot livre

        lda BlockX,x
        sta MetaX
        lda BlockY,x
        sta MetaY
        lda BlockAttr,x
        sta MetaAttr
        jsr DrawMetasprite          ; Preserva X
    Next:
        inx                         ; Próximo bloco, dando a volta no fim do array
        cpx #MAX_BLOCKS
        bcc :+
            ldx #0
        :
        dec RenderCount
        bne Loop
        rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A hitbox Box bate em uma parede ou em algum bloco?
;; (Usada tanto pelo player quanto pelos blocos.)
;;
;; Entrada: BoxX1..BoxY2, IgnoreBlock
;; Saída:   C = 1 se colidiu
;; Destrói: A, Y, Temp, PointX, PointY, ParamTile, RectX1..RectY2 (preserva X)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc CheckBoxVsWorld
    lda #TILE_SOLID
    sta ParamTile
    jsr CheckBoxOnTile              ; Parede?
    bcs Done
    jsr CheckBoxVsBlocks            ; Outro bloco?
Done:
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A hitbox Box bate em algum bloco ativo (exceto o bloco IgnoreBlock)?
;;
;; Entrada: BoxX1..BoxY2, IgnoreBlock (NO_BLOCK para testar todos)
;; Saída:   C = 1 se colidiu, e então Y = índice do bloco atingido
;; Destrói: A, Y, RectX1..RectY2 (preserva X)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc CheckBoxVsBlocks
    ldy #0
    Loop:
        cpy IgnoreBlock
        beq Next                    ; Não testa contra si mesmo
        lda BlockType,y
        beq Next                    ; Slot livre

        lda BlockX,y                ; Rect = hitbox do bloco Y
        sta RectX1
        clc
        adc #HITBOX_SIZE
        sta RectX2
        lda BlockY,y
        sta RectY1
        clc
        adc #HITBOX_SIZE
        sta RectY2

        jsr IsBoxColliding          ; Preserva Y
        bcs Done                    ; Colidiu: devolve C = 1 e Y = índice
    Next:
        iny
        cpy #MAX_BLOCKS
        bne Loop
        clc                         ; Nenhum bloco (o cpy igual tinha deixado C = 1)
Done:
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A hitbox Box bate no player?
;;
;; Entrada: BoxX1..BoxY2
;; Saída:   C = 1 se colidiu
;; Destrói: A, RectX1..RectY2 (preserva X e Y)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc CheckBoxVsPlayer
    lda PlayerX                     ; Rect = hitbox do player
    sta RectX1
    clc
    adc #HITBOX_SIZE
    sta RectX2
    lda PlayerY
    sta RectY1
    clc
    adc #HITBOX_SIZE
    sta RectY2
    jmp IsBoxColliding              ; Tail call: devolve o carry direto
.endproc
