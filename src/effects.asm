;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Efeitos visuais e transição entre fases
;;
;; Tudo que muda o nametable com a tela LIGADA passa por aqui. A PPU só
;; aceita escritas na VRAM durante o VBlank, então o jogo não escreve direto:
;; ele enfileira as escritas no VramBuffer durante o frame e a NMI copia
;; tudo de uma vez (FlushVramBuffer), logo no começo do VBlank.
;;
;; Formato do VramBuffer (várias entradas em sequência):
;;   .byte endereço_alto, endereço_baixo, tamanho, byte1, byte2, ...
;;   .byte 0                              ; fim da fila
;; O endereço alto nunca é 0 (o nametable fica em $2000+), então 0 serve
;; como marcador de fim.
;;
;; === Transição "dissolver" ===
;; Um LFSR de 8 bits (gerador pseudo-aleatório com período 255) visita
;; todos os números de 1 a 255 exatamente uma vez, em ordem embaralhada.
;; Cada número é tratado como uma célula do mapa 16x16 (linha*16 + coluna);
;; as que não fazem parte da área de jogo são puladas. Cada célula sorteada
;; vira "brilho" e, no frame seguinte, vira o desenho final:
;;   - saindo:  brilho -> chão   (e o bloco daquela célula some)
;;   - entrando: brilho -> metatile da fase nova (e o bloco aparece)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "CODE"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tiles de cada tipo de metatile (índice = MT_*): superior esquerdo,
;; superior direito, inferior esquerdo e inferior direito.
;; O MT_WALL (pilar original) nunca é desenhado: o QueueMetatile troca pelo
;; pilar da dificuldade atual (MT_PILLAR0 + LevelTier).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;             Chão  Pilar Encaixe Brilho Pedra Cristal Caixa Metal
MetatileTL: .byte $00,  $EE,   $02,   $05,  $09,  $0D,   $11,  $15
MetatileTR: .byte $00,  $EF,   $01,   $06,  $0A,  $0E,   $12,  $16
MetatileBL: .byte $00,  $FE,   $03,   $07,  $0B,  $0F,   $13,  $17
MetatileBR: .byte $00,  $FF,   $04,   $08,  $0C,  $10,   $14,  $18

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cores de cada dificuldade (12 bytes por dificuldade), cores 1-3 de:
;;   paleta de fundo 0   (moldura: junta, tijolo, brilho/letras)
;;   paleta de fundo 1   (área de jogo: pilar, encaixe, texto, brilho)
;;   paleta de sprite 0  (blocos fora do encaixe)
;;   paleta de sprite 1  (inimigos: contorno, corpo, olhos)
;; As cores dos inimigos têm que bater com ENEMY_TIER_PALETTES em
;; tools/draw_sprites.py (lá só são usadas na prévia).
;; A cor 3 das paletas de fundo fica clara porque é a cor das letras.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
TIER_PALETTE_SIZE = 12
TierPalettes:
;;      moldura           área de jogo      blocos            inimigos
.byte   $0F, $00, $10,    $0A, $19, $30,    $00, $10, $30,    $09, $2A, $30   ; 0: pedra - blocos cinza, inimigos verdes
.byte   $0F, $01, $21,    $01, $21, $30,    $06, $16, $36,    $04, $24, $30   ; 1: cristal - blocos vermelhos, inimigos roxos
.byte   $0F, $09, $29,    $07, $17, $38,    $04, $14, $34,    $0C, $2C, $30   ; 2: madeira - blocos roxos, inimigos azul-petróleo
.byte   $0F, $03, $23,    $06, $16, $30,    $0B, $1B, $3B,    $00, $10, $30   ; 3: metal - blocos verdes, inimigos cinza

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dificuldade de uma fase: fase / LEVELS_PER_TIER, limitada à última
;; (a tela final, fase = LEVEL_COUNT, usa as cores da última dificuldade).
;;
;; Entrada: A = fase (0 = fase 1)
;; Saída:   A = dificuldade (0 a TIER_COUNT - 1)
;; Destrói: A, Y
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc GetLevelTier
    ldy #0
    Loop:
        cmp #LEVELS_PER_TIER
        bcc Done
        sbc #LEVELS_PER_TIER    ; (o cmp deixou C = 1)
        iny
        jmp Loop
    Done:
        tya
        cmp #TIER_COUNT
        bcc :+
            lda #TIER_COUNT - 1
        :
        rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enfileira as cores da dificuldade A (moldura, área de jogo, blocos e inimigos).
;; As paletas também são escritas pelo PPU_ADDR/PPU_DATA, então passam pela
;; mesma fila do VramBuffer.
;;
;; Entrada: A = dificuldade
;; Destrói: A, X, Y
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc QueueTierPalette
    asl                         ; X = dificuldade * 12 (posição em TierPalettes)
    asl
    sta Temp                    ; Temp = dificuldade * 4
    asl                         ; A = dificuldade * 8
    clc
    adc Temp
    tax

    lda #0
    sta PalIndex                ; Qual das 4 paletas (moldura, área de jogo, blocos, inimigos)
    ldy VramIndex
    Palette:
        stx Temp                ; Endereço da paleta: PalAddrHi/Lo[PalIndex]
        ldx PalIndex
        lda PalAddrHi,x
        sta VramBuffer,y
        iny
        lda PalAddrLo,x
        sta VramBuffer,y
        iny
        lda #3                  ; 3 cores
        sta VramBuffer,y
        iny
        ldx Temp

        lda TierPalettes+0,x
        sta VramBuffer,y
        iny
        lda TierPalettes+1,x
        sta VramBuffer,y
        iny
        lda TierPalettes+2,x
        sta VramBuffer,y
        iny
        inx
        inx
        inx

        inc PalIndex
        lda PalIndex
        cmp #4
        bne Palette

    lda #0
    sta VramBuffer,y            ; Fim da fila
    sty VramIndex
    rts
.endproc

;; Endereço na PPU de cada paleta, na ordem de TierPalettes
PalAddrHi: .byte >PAL_FRAME, >PAL_PLAYFIELD, >PAL_BLOCKS, >PAL_ENEMIES
PalAddrLo: .byte <PAL_FRAME, <PAL_PLAYFIELD, <PAL_BLOCKS, <PAL_ENEMIES

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copia a fila do VramBuffer para a PPU. Chamada pela NMI, durante o VBlank.
;; Custa uns 50 ciclos por entrada + 14 por byte.
;;
;; Destrói: A, X, Y
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc FlushVramBuffer
    bit PPU_STATUS              ; Reseta o latch do PPU_ADDR
    ldx #0
    Entry:
        lda VramBuffer,x        ; Endereço alto (0 = fim da fila)
        beq Done
        sta PPU_ADDR
        lda VramBuffer+1,x      ; Endereço baixo
        sta PPU_ADDR
        ldy VramBuffer+2,x      ; Quantos bytes seguem
        inx
        inx
        inx
    Copy:
        lda VramBuffer,x
        sta PPU_DATA            ; O endereço da PPU avança 1 sozinho
        inx
        dey
        bne Copy
        jmp Entry
    Done:
        rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enfileira o desenho de um metatile 16x16 (2 linhas de 2 tiles).
;;
;; Endereço no nametable da célula (linha L, coluna C do mapa 16x16):
;;   $2000 + L*64 + C*2      (cada linha de metatiles = 2 linhas de 32 tiles)
;; Como a célula é L*16 + C, dá para montar o endereço só com bits:
;;   alto  = $20 | (célula >> 6)
;;   baixo = ((célula & $30) << 2) | ((célula & $0F) << 1)
;; A linha de baixo do metatile fica 32 bytes depois (bit 5 do byte baixo,
;; que sempre é 0 na linha de cima, então basta um "ora #$20").
;;
;; Entrada: A = tipo (MT_*), X = célula do mapa
;; Destrói: A, Y (preserva X)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc QueueMetatile
    cmp #MT_WALL
    bne :+
        lda LevelTier           ; Pilar: usa o modelo da dificuldade da fase
        clc
        adc #MT_PILLAR0
    :
    sta MtKind
    stx MtCell

    txa
    and #$30
    asl
    asl
    sta MtLo                    ; (linha & 3) << 6
    txa
    and #$0F
    asl                         ; coluna * 2
    ora MtLo
    sta MtLo

    txa
    lsr
    lsr
    lsr
    lsr
    lsr
    lsr                         ; linha >> 2
    ora #$20
    sta MtHi

    ldx MtKind                  ; X = tipo, para ler as tabelas Metatile*
    ldy VramIndex

    lda MtHi                    ; Linha de cima: 2 tiles
    sta VramBuffer,y
    iny
    lda MtLo
    sta VramBuffer,y
    iny
    lda #2
    sta VramBuffer,y
    iny
    lda MetatileTL,x
    sta VramBuffer,y
    iny
    lda MetatileTR,x
    sta VramBuffer,y
    iny

    lda MtHi                    ; Linha de baixo: 32 bytes depois
    sta VramBuffer,y
    iny
    lda MtLo
    ora #$20
    sta VramBuffer,y
    iny
    lda #2
    sta VramBuffer,y
    iny
    lda MetatileBL,x
    sta VramBuffer,y
    iny
    lda MetatileBR,x
    sta VramBuffer,y
    iny

    lda #0
    sta VramBuffer,y            ; Marca o fim da fila (sem avançar o índice)
    sty VramIndex

    ldx MtCell                  ; Devolve o X de quem chamou
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enfileira um texto em uma linha do nametable.
;; Os tiles da fonte ficam nas posições ASCII, então cada caractere já é o
;; número do tile (ver tools/add_font.py).
;;
;; Endereço: $2000 + linha*32 + coluna
;;   alto  = $20 | (linha >> 3)
;;   baixo = ((linha & 7) << 5) | coluna
;;
;; Entrada: TextPtr, A = tamanho, TextRow, TextCol
;; Destrói: A, X, Y
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc QueueText
    sta TextLen
    ldx VramIndex

    lda TextRow
    lsr
    lsr
    lsr
    ora #$20
    sta VramBuffer,x            ; Endereço alto
    inx
    lda TextRow
    and #%00000111
    asl
    asl
    asl
    asl
    asl
    ora TextCol
    sta VramBuffer,x            ; Endereço baixo
    inx
    lda TextLen
    sta VramBuffer,x            ; Tamanho
    inx

    ldy #0
    Loop:
        lda (TextPtr),y
        sta VramBuffer,x
        inx
        iny
        cpy TextLen
        bne Loop

    lda #0
    sta VramBuffer,x            ; Fim da fila
    stx VramIndex
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macro para enfileirar um texto fixo da ROM: QUEUE_TEXT label, linha, coluna
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.macro QUEUE_TEXT label, row, col
    lda #<label
    sta TextPtr
    lda #>label
    sta TextPtr+1
    lda #row
    sta TextRow
    lda #col
    sta TextCol
    lda #.sizeof(label)
    jsr QueueText
.endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Textos fixos. Cada um é uma .proc só com dados para que .sizeof(label)
;; devolva o tamanho da string.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc TextCongrats
    .byte "PARABENS!"
.endproc
.proc TextPressStart
    .byte "APERTE START"
.endproc
.proc TextBlank                 ; Apaga "APERTE START" (mesmo tamanho)
    .byte "            "
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enfileira "FASE NN" (NN = fase A + 1, em decimal) centralizado na linha X:
;; TEXT_ROW no meio da área de jogo ou HUD_ROW na faixa de baixo da moldura.
;;
;; Entrada: A = fase (0 = fase 1), X = linha de tiles
;; Destrói: A, X, Y
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc QueueLevelText
    stx TextRow
    pha
    ldx #0
    Copy:                       ; TextBuffer = "FASE "
        lda TextFase,x
        sta TextBuffer,x
        inx
        cpx #5
        bne Copy

    pla
    clc
    adc #1                      ; Fase para humanos começa em 1
    ldx #'0'                    ; X = dígito das dezenas
    Tens:                       ; Divide por 10 subtraindo
        cmp #10
        bcc Ones
        sbc #10                 ; (o cmp deixou C = 1, então é uma subtração normal)
        inx
        jmp Tens
    Ones:
        stx TextBuffer+5
        clc
        adc #'0'
        sta TextBuffer+6

    lda #<TextBuffer
    sta TextPtr
    lda #>TextBuffer
    sta TextPtr+1
    lda #(32 - 7) / 2           ; Centraliza os 7 caracteres
    sta TextCol
    lda #7
    jmp QueueText
.endproc

TextFase:
    .byte "FASE "

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enfileira a tela final: "PARABENS!" e "APERTE START".
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc QueueEndingText
    QUEUE_TEXT TextCongrats,   TEXT_ROW - 1, (32 - .sizeof(TextCongrats)) / 2
    QUEUE_TEXT TextPressStart, TEXT_ROW + 1, (32 - .sizeof(TextPressStart)) / 2
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A célula A do mapa faz parte da área de jogo?
;;
;; Entrada: A = célula (linha*16 + coluna)
;; Saída:   C = 1 se está dentro da área de jogo, A preservado
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc IsPlayCell
    pha
    and #$0F                    ; Coluna
    cmp #LEVEL_COL0
    bcc Outside
    cmp #LEVEL_COL0 + LEVEL_W
    bcs Outside
    pla
    pha
    lsr
    lsr
    lsr
    lsr                         ; Linha
    cmp #LEVEL_ROW0
    bcc Outside
    cmp #LEVEL_ROW0 + LEVEL_H
    bcs Outside
    pla
    sec
    rts
Outside:
    pla
    clc
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prepara um dissolve novo (tanto para sair quanto para entrar).
;; A semente do LFSR vem do contador de frames, então a ordem muda a cada
;; vez; qualquer valor diferente de 0 percorre as 255 células.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc DissolveBegin
    lda Frame
    bne :+
        lda #1                  ; 0 travaria o LFSR em 0
    :
    sta DissolveLfsr
    lda #255
    sta DissolveSteps
    lda #0
    sta PendingCount
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Um frame do dissolve:
;;   1. As células que estavam com brilho ganham o desenho final.
;;   2. Sorteia até DISSOLVE_PER_FRAME células novas, desenha o brilho nelas
;;      e faz o bloco daquela célula sumir (saindo) ou aparecer (entrando).
;;
;; Saída: C = 1 quando todas as células já foram processadas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc DissolveStep
    jsr FinishPendingCells

    lda DissolveSteps
    bne NextCell
    sec                         ; Acabou: nenhuma célula nova e nenhuma pendente
    rts

    NextCell:
        lda DissolveSteps
        beq Done                ; O LFSR já deu a volta completa
        dec DissolveSteps

        lda DissolveLfsr        ; LFSR de Galois de 8 bits (polinômio $B8):
        lsr                     ; desloca e, se saiu um 1, aplica o XOR.
        bcc :+                  ; Com esse polinômio a sequência passa por
            eor #$B8            ; todos os valores de 1 a 255.
        :
        sta DissolveLfsr

        jsr IsPlayCell          ; Fora da área de jogo? Pula
        bcc NextCell

        tax                     ; X = célula sorteada
        lda #MT_SPARKLE
        jsr QueueMetatile       ; Brilho agora, desenho final no próximo frame
        jsr UpdateCellObjects

        ldy PendingCount
        txa
        sta PendingCells,y
        inc PendingCount
        lda PendingCount
        cmp #DISSOLVE_PER_FRAME
        bne NextCell
    Done:
        clc
        rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Troca o brilho das células pendentes pelo desenho final:
;; chão se estamos saindo da fase, ou o metatile do LevelMap se entrando.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc FinishPendingCells
    lda PendingCount
    beq Done
    Loop:
        dec PendingCount
        ldy PendingCount
        ldx PendingCells,y      ; X = célula

        lda TransitionPhase
        cmp #TransPhase::DISSOLVE_IN
        beq Entering
        lda #MT_FLOOR           ; Saindo: tudo vira chão
        jmp Draw
    Entering:
        lda LevelMap,x          ; Entrando: o valor do mapa já é o tipo do metatile
    Draw:
        jsr QueueMetatile

        lda PendingCount
        bne Loop
    Done:
        rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Faz o bloco (e o inimigo) da célula X sumir (saindo) ou aparecer (entrando).
;;
;; Saindo: some o bloco cujo CENTRO está na célula (vale mesmo para um
;; bloco que parou desalinhado encostado no player).
;; Entrando: cria o bloco se a célula está na lista de posições iniciais.
;;
;; Entrada: X = célula
;; Destrói: A, Y (preserva X)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc UpdateCellObjects
    lda TransitionPhase
    cmp #TransPhase::DISSOLVE_IN
    beq Entering

    ldy #0                      ; Saindo: procura o bloco com centro nesta célula
    LoopOut:
        lda BlockType,y
        beq NextOut
        lda BlockY,y
        clc
        adc #8
        and #%11110000          ; Linha do centro * 16
        sta Temp
        lda BlockX,y
        clc
        adc #8
        lsr
        lsr
        lsr
        lsr                     ; Coluna do centro
        ora Temp
        stx Temp
        cmp Temp
        bne NextOut
        lda #GameObjectType::NULL
        sta BlockType,y         ; Some junto com o quadrado
    NextOut:
        iny
        cpy #MAX_BLOCKS
        bne LoopOut

        ldy #0                  ; Inimigos com o centro nesta célula também somem
    EnemyOut:
        lda EnemyType,y
        beq NextEnemyOut
        lda EnemyY,y
        clc
        adc #8
        and #%11110000
        sta Temp
        lda EnemyX,y
        clc
        adc #8
        lsr
        lsr
        lsr
        lsr
        ora Temp
        stx Temp
        cmp Temp
        bne NextEnemyOut
        lda #EnemyType::NONE
        sta EnemyType,y
    NextEnemyOut:
        iny
        cpy #MAX_ENEMIES
        bne EnemyOut
        rts

    Entering:
        ldy #0                  ; Entrando: a célula é a posição inicial de um bloco?
    LoopIn:
        cpy LevelStartCount
        beq Done
        txa
        cmp LevelStartCell,y
        bne NextIn

        and #$0F                ; Célula -> pixels: X = coluna * 16, Y = linha * 16
        asl
        asl
        asl
        asl
        sta ParamX
        txa
        and #$F0
        sta ParamY
        stx MtCell              ; AddNewBlock usa X
        jsr AddNewBlock         ; Preserva Y
        ldx MtCell
    NextIn:
        iny
        jmp LoopIn
    Done:
        ldy #0                  ; A célula é a posição inicial de um inimigo?
    EnemyIn:
        cpy LevelEnemyCount
        beq EnemiesDone
        txa
        cmp LevelEnemyCell,y
        bne NextEnemyIn
        and #$0F                ; Célula -> pixels
        asl
        asl
        asl
        asl
        sta ParamX
        txa
        and #$F0
        sta ParamY
        stx MtCell              ; AddEnemy usa X
        lda LevelEnemyType,y
        jsr AddEnemy            ; Preserva Y
        ldx MtCell
    NextEnemyIn:
        iny
        jmp EnemyIn
    EnemiesDone:
        rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Começa a transição para a fase A: desmancha a atual, mostra o texto e
;; monta a nova. A = LEVEL_COUNT mostra a tela final (depois volta à fase 1).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc StartTransition
    sta NextLevel
    lda #State::TRANSITION
    sta GameState
    lda #TransPhase::DISSOLVE_OUT
    sta TransitionPhase
    lda #0
    sta PlayerVisible           ; O player some logo no começo
    jsr ClearBall               ; E a bola de pedra também
    jmp DissolveBegin
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Começa a transição já no texto, sem desmanchar nada (usado ao ligar o
;; jogo, quando a área de jogo ainda está vazia).
;;
;; Entrada: A = fase a montar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc StartTransitionAtText
    sta NextLevel
    lda #State::TRANSITION
    sta GameState
    lda #0
    sta PlayerVisible
    jmp EnterTextPhase
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Um frame da transição (chamado pelo GameLoop enquanto GameState é
;; State::TRANSITION).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc UpdateTransition
    lda TransitionPhase
    cmp #TransPhase::DISSOLVE_OUT
    beq DissolveOut
    cmp #TransPhase::SHOW_TEXT
    beq ShowText

    DissolveIn:
        jsr DissolveStep
        bcc Done
        lda #1                  ; Fase montada: o player aparece e o jogo começa
        sta PlayerVisible
        lda #State::PLAYING
        sta GameState
        rts

    DissolveOut:
        jsr DissolveStep
        bcc Done
        jmp EnterTextPhase

    ShowText:
        lda NextLevel
        cmp #LEVEL_COUNT
        beq WaitForStart

        dec StateTimer          ; Texto "FASE 00": espera um tempo fixo
        bne Done
        jmp EnterDissolveIn

    WaitForStart:               ; Tela final: espera o Start (novo aperto)
        lda Buttons
        and #BUTTON_START
        beq Done
        lda PrevButtons
        and #BUTTON_START
        bne Done
        QUEUE_TEXT TextBlank, TEXT_ROW + 1, (32 - .sizeof(TextBlank)) / 2
        lda #0
        sta NextLevel           ; Volta para a fase 1
        jsr GetLevelTier
        jsr QueueTierPalette    ; Com as cores da primeira dificuldade
        jmp EnterDissolveIn

    Done:
        rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entra na etapa do texto: "FASE 00" ou a tela final.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc EnterTextPhase
    lda #TransPhase::SHOW_TEXT
    sta TransitionPhase
    lda #LEVEL_TEXT_DELAY
    sta StateTimer

    lda NextLevel               ; Troca as cores já no texto, com a tela vazia
    jsr GetLevelTier
    jsr QueueTierPalette

    lda NextLevel
    cmp #LEVEL_COUNT
    beq Ending
    ldx #TEXT_ROW
    jmp QueueLevelText
    Ending:
        jmp QueueEndingText
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Carrega a fase NextLevel e começa a montá-la com o dissolve.
;; O texto no meio da tela é coberto naturalmente pelas células novas.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc EnterDissolveIn
    lda NextLevel
    jsr LoadLevel
    lda CurrentLevel            ; Atualiza "FASE 00" na faixa de baixo da moldura
    ldx #HUD_ROW
    jsr QueueLevelText
    lda #TransPhase::DISSOLVE_IN
    sta TransitionPhase
    jmp DissolveBegin
.endproc
