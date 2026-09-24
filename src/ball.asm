;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bola de pedra
;;
;; O B lança uma bola de pedra (8x8) na direção em que o mago está virado.
;; Ela anda BALL_SPEED pixels por frame, 1 pixel por vez (assim nunca
;; atravessa nada), até bater:
;;   - num inimigo: ele morre (poof);
;;   - num bloco parado: o bloco começa a deslizar na direção da bola;
;;   - num bloco que já está deslizando ou numa parede: só se desfaz.
;; No lugar do impacto aparece uma poeirinha (PuffFrames).
;; Só existe uma bola por vez: enquanto ela voa, o B não faz nada.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "CODE"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lança a bola, a partir do meio do mago, na direção PlayerSide.
;; Se ele ainda não virou para lado nenhum (começo da fase), lança para baixo,
;; que é para onde o idle olha. Também dispara a animação de lançar.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc FireBall
    lda BallActive
    bne Done                        ; Já tem uma bola voando

    lda PlayerSide
    bne :+
        lda #Side::DOWN             ; Ainda sem direção: de frente, como o idle
        sta PlayerSide
    :
    sta BallSide

    lda PlayerX                     ; Bola 8x8 centralizada no mago 16x16
    clc
    adc #4
    sta BallX
    lda PlayerY
    clc
    adc #4
    sta BallY
    lda #1
    sta BallActive

    lda #1                          ; Animação de lançar a varinha
    sta CastActive
    lda #0
    sta AnimPtr+1                   ; Força a animação a recomeçar do quadro 0
    lda #FACE_HOLD                  ; Depois dela, fica virado para onde atirou
    sta FaceTimer
Done:
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Move a bola e trata as colisões (chamada uma vez por frame no PLAYING).
;; Destrói: A, Y, e tudo que CheckBoxOnTile/CheckBoxVsBlocks destroem (preserva X)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc UpdateBall
    lda BallActive
    beq Done

    lda #BALL_SPEED
    sta BallSteps
    Step:
        ldy BallSide                ; Anda 1 pixel na direção da bola
        lda BallX
        clc
        adc SideDeltaX,y
        sta BallX
        lda BallY
        clc
        adc SideDeltaY,y
        sta BallY

        lda BallX                   ; Box = hitbox da bola (8x8)
        sta BoxX1
        clc
        adc #BALL_HITBOX
        sta BoxX2
        lda BallY
        sta BoxY1
        clc
        adc #BALL_HITBOX
        sta BoxY2

        lda #TILE_SOLID             ; Bateu numa parede?
        sta ParamTile
        jsr CheckBoxOnTile
        bcs Impact

        jsr FindEnemyInBox          ; Acertou um inimigo? (Y = índice dele)
        bcs HitEnemy

        lda #NO_BLOCK               ; Bateu num bloco? (Y = índice dele)
        sta IgnoreBlock
        jsr CheckBoxVsBlocks
        bcs HitBlock

        dec BallSteps
        bne Step
    Done:
        rts

    HitEnemy:
        jsr KillEnemy
        jmp Impact

    HitBlock:
        lda BlockSide,y
        bne Impact                  ; Bloco já deslizando: não muda de direção
        lda BallSide
        sta BlockSide,y             ; O bloco começa a deslizar para o mesmo lado

    Impact:                         ; A bola se desfaz em poeira ali
        lda #0
        sta BallActive
        lda BallX
        sta ImpactX
        lda BallY
        sta ImpactY
        lda #PUFF_FRAMES * IMPACT_ANIM_DELAY
        sta ImpactTimer
        rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some com a bola e a poeira (usada no começo das transições).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc ClearBall
    lda #0
    sta BallActive
    sta ImpactTimer
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Desenha a bola (girando: troca de quadro a cada 2 frames) e a poeira do
;; impacto (PUFF_FRAMES quadros de IMPACT_ANIM_DELAY frames cada).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc RenderBall
    lda #0
    sta MetaAttr                    ; A paleta da pedra já vem na tabela

    lda BallActive
    beq Puff
        lda BallX
        sta MetaX
        lda BallY
        sta MetaY
        lda Frame
        lsr                         ; Gira a cada 2 frames
        and #BALL_FRAMES - 1
        asl
        tay
        lda BallFrames,y
        sta MetaPtr
        lda BallFrames+1,y
        sta MetaPtr+1
        jsr DrawMetasprite

    Puff:
    lda ImpactTimer
    beq Done
        dec ImpactTimer
        lda ImpactX
        sta MetaX
        lda ImpactY
        sta MetaY
        lda ImpactTimer             ; Quadro = (PUFF_FRAMES - 1) - timer / delay
        ldy #PUFF_FRAMES - 1
        :
            cmp #IMPACT_ANIM_DELAY
            bcc :+
            sbc #IMPACT_ANIM_DELAY
            dey
            jmp :-
        :
        tya
        asl
        tay
        lda PuffFrames,y
        sta MetaPtr
        lda PuffFrames+1,y
        sta MetaPtr+1
        jmp DrawMetasprite
    Done:
        rts
.endproc
