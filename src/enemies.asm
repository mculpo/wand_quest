;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inimigos
;;
;; Cada inimigo anda de célula em célula (16x16) pelo mapa. Quando chega
;; exatamente numa célula, ele "pensa" (EnemyThink): na maioria das vezes
;; segue reto se dá; senão sorteia uma das direções livres, evitando voltar
;; para trás. A gosma e a aranha às vezes param um pouco.
;;
;; A velocidade é em 1/256 de pixel por frame (EnemySpeed): a cada frame a
;; velocidade é somada em EnemySub e, quando passa de 255 (carry), o inimigo
;; anda 1 pixel. Assim cada tipo tem um ritmo diferente e suave.
;;
;; Regras:
;;   - paredes param todos; blocos param todos menos o fantasma;
;;   - encostar no mago mata o mago (a fase reinicia);
;;   - a bola de pedra mata o inimigo que acertar (src/ball.asm);
;;   - um bloco deslizando por cima esmaga o inimigo (src/blocks.asm).
;; Os inimigos nunca param blocos, então o puzzle continua igual ao do solver.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "CODE"

;; Por tipo (índice = EnemyType; o 0 não é usado)
;;                  NONE  SLIME  BAT  SPIDER  GHOST
EnemySpeed:     .byte  0,    90,  210,   150,    70   ; 1/256 px por frame
EnemyAnimDelay: .byte  1,     8,    4,     5,     8   ; Frames por quadro de animação
EnemyPauses:    .byte  0,     1,    0,     1,     0   ; 1 = às vezes para numa célula

;; Bit de cada direção em FreeDirs (índice = Side)
SideBit:        .byte  0, %0010, %0100, %1000, %10000
;; Direção contrária (índice = Side)
SideOpposite:   .byte  Side::NONE, Side::DOWN, Side::UP, Side::LEFT, Side::RIGHT
;; Deslocamento na célula do mapa 16x16 ao andar para cada lado
SideCellDelta:  .byte  0, <-16, 16, 1, <-1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remove todos os inimigos.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc ClearEnemies
    ldx #MAX_ENEMIES - 1
    lda #EnemyType::NONE
    :
        sta EnemyType,x
        dex
        bpl :-
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cria um inimigo no primeiro slot livre.
;;
;; Entrada: A = tipo (EnemyType), ParamX/ParamY = posição em pixels
;; Destrói: A, X (preserva Y)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc AddEnemy
    pha
    ldx #0
    FindFree:
        lda EnemyType,x
        beq Found
        inx
        cpx #MAX_ENEMIES
        bne FindFree
        pla                         ; Cheio: não cria
        rts
    Found:
        pla
        sta EnemyType,x
        lda ParamX
        sta EnemyX,x
        lda ParamY
        sta EnemyY,x
        lda #0
        sta EnemyDying,x
        sta EnemySub,x
        sta EnemyFrame,x
        lda #Side::NONE             ; Escolhe a direção no primeiro passo
        sta EnemyDir,x
        lda #30                     ; Espera meio segundo antes de começar a andar
        sta EnemyPause,x
        lda #1
        sta EnemyAnimTimer,x
        rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Um frame de todos os inimigos: animação, poof dos que estão morrendo e
;; movimento dos vivos.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc UpdateEnemies
    ldx #0
    Loop:
        lda EnemyType,x
        beq Next

        lda EnemyDying,x            ; Morrendo: só conta o tempo do poof
        beq Alive
            dec EnemyDying,x
            bne Next
            lda #EnemyType::NONE    ; Acabou o poof: some
            sta EnemyType,x
            jmp Next

    Alive:
        jsr AnimateEnemy
        lda EnemyPause,x            ; Parado numa célula?
        beq Move
            dec EnemyPause,x
            jmp Next
    Move:
        ldy EnemyType,x             ; Soma a velocidade na fração de pixel
        lda EnemySub,x
        clc
        adc EnemySpeed,y
        sta EnemySub,x
        bcc Next                    ; Ainda não completou 1 pixel
        jsr StepEnemy
    Next:
        inx
        cpx #MAX_ENEMIES
        bne Loop
        rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Avança o quadro de animação do inimigo X (4 quadros em loop).
;; Destrói: A, Y (preserva X)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc AnimateEnemy
    dec EnemyAnimTimer,x
    bne Done
    ldy EnemyType,x
    lda EnemyAnimDelay,y
    sta EnemyAnimTimer,x
    lda EnemyFrame,x
    clc
    adc #1
    cmp #ENEMY_FRAMES
    bcc :+
        lda #0
    :
    sta EnemyFrame,x
Done:
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Anda 1 pixel com o inimigo X. Se ele está exatamente numa célula, antes
;; escolhe para onde ir (EnemyThink).
;; Destrói: A, Y (preserva X)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc StepEnemy
    lda EnemyX,x                    ; Alinhado no grid (X e Y múltiplos de 16)?
    ora EnemyY,x
    and #%00001111
    bne Walk
    jsr EnemyThink
    lda EnemyPause,x
    bne Done                        ; Resolveu parar um pouco
    Walk:
    ldy EnemyDir,x
    beq Done                        ; Sem direção livre: fica onde está
    lda EnemyX,x
    clc
    adc SideDeltaX,y
    sta EnemyX,x
    lda EnemyY,x
    clc
    adc SideDeltaY,y
    sta EnemyY,x
Done:
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; O inimigo X está numa célula: decide a próxima direção.
;;   1. Descobre quais das 4 células vizinhas estão livres (FreeDirs).
;;   2. Gosma/aranha: às vezes param um pouco (e depois pensam de novo).
;;   3. Se a direção atual está livre, segue nela com ENEMY_KEEP_CHANCE.
;;   4. Senão sorteia uma direção livre que não seja voltar para trás;
;;      se só dá para voltar, volta; se nada está livre, fica parado.
;; Destrói: A, Y (preserva X)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc EnemyThink
    lda EnemyY,x                    ; Célula atual = linha*16 + coluna
    and #%11110000
    sta EnemyCell
    lda EnemyX,x
    lsr
    lsr
    lsr
    lsr
    ora EnemyCell
    sta EnemyCell

    lda #0                          ; 1. Direções livres
    sta FreeDirs
    ldy #Side::UP
    CheckDir:
        tya
        pha
        jsr IsEnemyDirFree          ; C = 1 se livre
        pla
        tay
        bcc :+
            lda FreeDirs
            ora SideBit,y
            sta FreeDirs
        :
        iny
        cpy #Side::LEFT + 1
        bne CheckDir

    ldy EnemyType,x                 ; 2. Pausa de vez em quando
    lda EnemyPauses,y
    beq NoPause
    jsr GetRandomNumber
    cmp #ENEMY_PAUSE_CHANCE
    bcs NoPause
        jsr GetRandomNumber
        and #%00011111
        adc #12                     ; Para de 12 a 43 frames
        sta EnemyPause,x
        rts
    NoPause:

    ldy EnemyDir,x                  ; 3. Segue reto?
    beq Choose
    lda FreeDirs
    and SideBit,y
    beq Choose                      ; Bloqueado na frente
    jsr GetRandomNumber
    cmp #ENEMY_KEEP_CHANCE
    bcs Choose
    rts                             ; Segue reto

    Choose:                         ; 4. Sorteia a partir de uma direção qualquer
    jsr GetRandomNumber
    and #%00000011
    clc
    adc #Side::UP                   ; TryDir = 1..4
    sta TryDir
    lda #4
    sta TryCount
    TryLoop:
        ldy TryDir
        lda FreeDirs
        and SideBit,y
        beq TryNext                 ; Não está livre
        lda EnemyDir,x
        tay
        lda SideOpposite,y
        cmp TryDir
        beq TryNext                 ; Seria voltar para trás: só se não houver outra
        lda TryDir
        sta EnemyDir,x
        rts
    TryNext:
        inc TryDir
        lda TryDir
        cmp #Side::LEFT + 1
        bcc :+
            lda #Side::UP
            sta TryDir
        :
        dec TryCount
        bne TryLoop

    ldy EnemyDir,x                  ; Só dá para voltar?
    lda SideOpposite,y
    tay
    lda FreeDirs
    and SideBit,y
    beq Stuck
    tya
    sta EnemyDir,x
    rts
    Stuck:
    lda #Side::NONE
    sta EnemyDir,x
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A célula vizinha de EnemyCell na direção Y está livre para o inimigo X?
;; Parede nunca; bloco só para quem não é fantasma.
;;
;; Entrada: Y = Side, EnemyCell, X = inimigo
;; Saída:   C = 1 se livre
;; Destrói: A, Y (preserva X)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc IsEnemyDirFree
    lda EnemyCell
    clc
    adc SideCellDelta,y
    sta Temp                        ; Temp = célula vizinha
    tay
    lda LevelMap,y
    cmp #TILE_SOLID
    beq Blocked

    lda EnemyType,x
    cmp #EnemyType::GHOST
    beq Free                        ; Fantasma atravessa blocos

    ldy #0                          ; Algum bloco com o centro nessa célula?
    Loop:
        lda BlockType,y
        beq Next
        lda BlockY,y
        clc
        adc #8
        and #%11110000
        sta PointY
        lda BlockX,y
        clc
        adc #8
        lsr
        lsr
        lsr
        lsr
        ora PointY
        cmp Temp
        beq Blocked
    Next:
        iny
        cpy #MAX_BLOCKS
        bne Loop
    Free:
        sec
        rts
    Blocked:
        clc
        rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rect = hitbox do inimigo Y (16x16 com ENEMY_HIT_INSET a menos de cada lado).
;; Destrói: A (preserva X e Y)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc SetEnemyRect
    lda EnemyX,y
    clc
    adc #ENEMY_HIT_INSET
    sta RectX1
    clc
    adc #HITBOX_SIZE - 2 * ENEMY_HIT_INSET
    sta RectX2
    lda EnemyY,y
    clc
    adc #ENEMY_HIT_INSET
    sta RectY1
    clc
    adc #HITBOX_SIZE - 2 * ENEMY_HIT_INSET
    sta RectY2
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Algum inimigo vivo encosta na caixa Box?
;; Entrada: BoxX1..BoxY2
;; Saída:   C = 1 e Y = índice do primeiro inimigo que encosta
;; Destrói: A, Y, RectX1..RectY2 (preserva X)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc FindEnemyInBox
    ldy #0
    Loop:
        lda EnemyType,y
        beq Next
        lda EnemyDying,y
        bne Next                    ; Já está morrendo
        jsr SetEnemyRect
        jsr IsBoxColliding
        bcs Done
    Next:
        iny
        cpy #MAX_ENEMIES
        bne Loop
        clc
    Done:
        rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mata o inimigo Y (começa o poof).
;; Destrói: A (preserva X e Y)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc KillEnemy
    lda #POOF_FRAMES * POOF_ANIM_DELAY
    sta EnemyDying,y
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mata todos os inimigos que encostam na caixa Box (bloco deslizando).
;; Entrada: BoxX1..BoxY2
;; Destrói: A, Y, RectX1..RectY2 (preserva X)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc CrushEnemiesInBox
    ldy #0
    Loop:
        lda EnemyType,y
        beq Next
        lda EnemyDying,y
        bne Next
        jsr SetEnemyRect
        jsr IsBoxColliding
        bcc Next
        jsr KillEnemy
    Next:
        iny
        cpy #MAX_ENEMIES
        bne Loop
        rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Algum inimigo encostou no mago? Então ele morre (StartPlayerDeath).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc CheckPlayerVsEnemies
    lda PlayerVisible
    beq Done
    lda PlayerX
    sta BoxX1
    lda PlayerY
    sta BoxY1
    jsr SetBoxSize
    jsr FindEnemyInBox
    bcc Done
    jmp StartPlayerDeath
Done:
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; O mago morreu: toca a animação de morte e depois reinicia a fase.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc StartPlayerDeath
    lda #State::PLAYER_DYING
    sta GameState
    lda #MAGE_DIE_FRAMES * DIE_ANIM_DELAY + DIE_EXTRA_TIME
    sta StateTimer
    lda #0
    sta CastActive
    sta AnimPtr+1                   ; Força a animação de morte a começar do quadro 0
    jmp ClearBall
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Estado PLAYER_DYING: espera a animação de morte e reinicia a fase.
;; Os inimigos continuam animando, mas não andam.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc UpdatePlayerDeath
    ldx #0
    :
        lda EnemyType,x
        beq :+
        lda EnemyDying,x
        bne :+
        jsr AnimateEnemy
    :
        inx
        cpx #MAX_ENEMIES
        bne :--

    dec StateTimer
    bne Done
    lda CurrentLevel
    jmp StartTransition             ; Reinicia a fase
Done:
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Desenha todos os inimigos: vivo = quadro da animação do tipo; morrendo =
;; quadro do poof.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc RenderEnemies
    lda #0
    sta MetaAttr                    ; A paleta dos inimigos já vem na tabela
    ldx #0
    Loop:
        lda EnemyType,x
        beq Next

        lda EnemyX,x
        sta MetaX
        lda EnemyY,x
        sta MetaY

        lda EnemyDying,x
        beq Alive
            lda #POOF_FRAMES * POOF_ANIM_DELAY   ; Quadro = tempo passado / POOF_ANIM_DELAY
            sec
            sbc EnemyDying,x
            lsr
            lsr
            asl                     ; x2 (endereços de 2 bytes)
            tay
            lda PoofFrames,y
            sta MetaPtr
            lda PoofFrames+1,y
            sta MetaPtr+1
            jmp Draw

        Alive:
            lda EnemyType,x         ; Tabela de quadros do tipo
            sec
            sbc #1
            asl
            tay
            lda EnemyAnimByType,y
            sta MetaPtr
            lda EnemyAnimByType+1,y
            sta MetaPtr+1
            lda EnemyFrame,x        ; Endereço do quadro atual
            asl
            tay
            lda (MetaPtr),y
            pha
            iny
            lda (MetaPtr),y
            sta MetaPtr+1
            pla
            sta MetaPtr
        Draw:
            jsr DrawMetasprite      ; Preserva X
    Next:
        inx
        cpx #MAX_ENEMIES
        bne Loop
        rts
.endproc
