;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fases: carregar, checar vitória, reiniciar e avançar.
;;
;; As fases estão em ASCII em src/levels.asm (ver consts.inc, LCHAR_*).
;; O LoadLevel converte uma fase para:
;;   - LevelMap: mapa de colisão 16x16 (TILE_*), com parede em volta
;;   - LevelStartCell: células onde os blocos começam (eles só são criados
;;     pelo efeito de entrada, quando o quadrado deles aparece)
;;   - LevelEnemyCell/LevelEnemyType: onde e quais inimigos começam (também
;;     criados pelo efeito de entrada)
;;   - PlayerStartCell / PlayerX / PlayerY
;;   - SlotCount: quantos encaixes precisam ser ocupados para vencer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "CODE"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Carrega a fase A (0 = fase 1).
;; Remove todos os blocos atuais; os novos aparecem durante o dissolve.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc LoadLevel
    sta CurrentLevel
    jsr GetLevelTier            ; Modelo de pilar da fase (src/effects.asm)
    sta LevelTier

    lda CurrentLevel
    asl                         ; Cada entrada de LevelTable tem 2 bytes
    tay
    lda LevelTable,y
    sta LevelPtr
    lda LevelTable+1,y
    sta LevelPtr+1

    ldx #0                      ; Nenhum bloco ativo
    lda #GameObjectType::NULL
    ClearBlocks:
        sta BlockType,x
        inx
        cpx #MAX_BLOCKS
        bne ClearBlocks

    jsr ClearEnemies            ; Nenhum inimigo ativo

    lda #TILE_SOLID             ; Mapa inteiro como parede; o interior é escrito abaixo
    ldx #0
    FillMap:
        sta LevelMap,x
        inx
        bne FillMap

    lda #0
    sta SlotCount
    sta LevelStartCount
    sta LevelEnemyCount
    lda #LEVEL_FIRST_CELL       ; Se a fase esquecer o 'P', o player nasce no canto
    sta PlayerStartCell

    ldy #0                      ; Y = posição no texto da fase
    ldx #LEVEL_FIRST_CELL       ; X = célula do mapa
    lda #LEVEL_H
    sta LoadRow
    RowLoop:
        lda #LEVEL_W
        sta LoadCol
    ColLoop:
        lda (LevelPtr),y
        jsr StoreLevelChar      ; Preserva X e Y
        iny
        inx
        dec LoadCol
        bne ColLoop

        txa                     ; Pula as colunas de parede até a próxima linha
        clc
        adc #16 - LEVEL_W
        tax
        dec LoadRow
        bne RowLoop

    lda PlayerStartCell         ; Célula -> pixels
    and #$0F
    asl
    asl
    asl
    asl
    sta PlayerX
    lda PlayerStartCell
    and #$F0
    sta PlayerY
    lda #Side::NONE
    sta PlayerSide
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Converte um caractere da fase e grava na célula X do LevelMap.
;;
;; Entrada: A = caractere, X = célula
;; Destrói: A (preserva X e Y)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc StoreLevelChar
    sty LoadTextIndex           ; Guarda Y (posição no texto): os inimigos usam Y
    jsr StoreLevelCharInner
    ldy LoadTextIndex
    rts
.endproc

.proc StoreLevelCharInner
    cmp #LCHAR_WALL
    bne NotWall
        lda #TILE_SOLID
        sta LevelMap,x
        rts
    NotWall:

    cmp #LCHAR_SLOT
    bne NotSlot
        lda #TILE_SLOT
        sta LevelMap,x
        inc SlotCount
        rts
    NotSlot:

    cmp #LCHAR_BLOCK
    bne NotBlock
        lda LevelStartCount
        cmp #MAX_BLOCKS
        bcs Floor               ; Blocos demais: ignora (o solver já avisa)
        tya
        pha                     ; Salva Y (posição no texto)
        ldy LevelStartCount
        txa
        sta LevelStartCell,y
        inc LevelStartCount
        pla
        tay
        jmp Floor
    NotBlock:

    cmp #LCHAR_PLAYER
    bne NotPlayer
        stx PlayerStartCell
        jmp Floor
    NotPlayer:

    ldy #EnemyType::SLIME       ; Inimigos: guarda a célula e o tipo
    cmp #LCHAR_SLIME
    beq Enemy
    ldy #EnemyType::BAT
    cmp #LCHAR_BAT
    beq Enemy
    ldy #EnemyType::SPIDER
    cmp #LCHAR_SPIDER
    beq Enemy
    ldy #EnemyType::GHOST
    cmp #LCHAR_GHOST
    bne Floor
    Enemy:
        lda LevelEnemyCount
        cmp #MAX_ENEMIES
        bcs Floor               ; Inimigos demais: ignora
        sty Temp                ; Temp = tipo
        ldy LevelEnemyCount
        txa
        sta LevelEnemyCell,y
        lda Temp
        sta LevelEnemyType,y
        inc LevelEnemyCount

    Floor:                      ; Chão (também embaixo do bloco e do player)
        lda #TILE_EMPTY
        sta LevelMap,x
        rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chamada quando um bloco para. Se todos os encaixes estão ocupados, a fase
;; está completa. Um bloco está no encaixe quando a paleta dele é
;; SLOT_PALETTE (o UpdateBlockPalette acabou de calcular isso).
;;
;; Destrói: A, Y (preserva X)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc CheckLevelComplete
    lda GameState
    cmp #State::PLAYING
    bne Done

    lda #0
    sta OnSlotCount
    ldy #0
    Loop:
        lda BlockType,y
        beq Next
        lda BlockAttr,y
        and #%00000011
        cmp #SLOT_PALETTE
        bne Next
        inc OnSlotCount
    Next:
        iny
        cpy #MAX_BLOCKS
        bne Loop

    lda OnSlotCount
    cmp SlotCount
    bne Done
    lda #State::LEVEL_CLEAR     ; Todos os encaixes ocupados!
    sta GameState
    lda #LEVEL_CLEAR_DELAY
    sta StateTimer
Done:
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Estado LEVEL_CLEAR: espera um pouco e começa a transição para a próxima
;; fase (ou para a tela final, depois da última).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc UpdateLevelClear
    dec StateTimer
    bne Done
    lda CurrentLevel
    clc
    adc #1                      ; Depois da última: LEVEL_COUNT = tela final
    jmp StartTransition
Done:
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Botão Select durante o jogo:
;;   - soltar o Select (sem ter feito combo) reinicia a fase;
;;   - com DEBUG_LEVEL_SELECT = 1, Select + Direita/Esquerda pula de fase.
;; O reinício é no SOLTAR para não disparar quando você aperta o Select
;; só para começar o combo.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc HandleSelect
    lda Buttons
    and #BUTTON_SELECT
    beq Released

    lda PrevButtons             ; Select apertado agora? Começa um combo novo
    and #BUTTON_SELECT
    bne Held
    lda #0
    sta SelectComboUsed
    Held:
.if ::DEBUG_LEVEL_SELECT
    lda Buttons                 ; Direita apertada agora: próxima fase
    and #BUTTON_RIGHT
    beq CheckLeft
    lda PrevButtons
    and #BUTTON_RIGHT
    bne CheckLeft
        lda #1
        sta SelectComboUsed
        ldx CurrentLevel
        inx
        cpx #LEVEL_COUNT
        bne :+
            ldx #0              ; Depois da última, volta para a primeira
        :
        txa
        jmp StartTransition

    CheckLeft:                  ; Esquerda apertada agora: fase anterior
    lda Buttons
    and #BUTTON_LEFT
    beq Done
    lda PrevButtons
    and #BUTTON_LEFT
    bne Done
        lda #1
        sta SelectComboUsed
        ldx CurrentLevel
        dex
        bpl :+
            ldx #LEVEL_COUNT - 1    ; Antes da primeira, vai para a última
        :
        txa
        jmp StartTransition
.endif
    Done:
        rts

    Released:
        lda PrevButtons         ; Acabou de soltar o Select?
        and #BUTTON_SELECT
        beq Done
        lda SelectComboUsed
        bne Done                ; Foi usado num combo: não reinicia
        lda CurrentLevel
        jmp StartTransition     ; Reinicia a fase atual
.endproc
