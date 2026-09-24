.segment "CODE"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inicializa o player ao ligar o jogo: parado, sem direção e escondido.
;; A posição vem de cada fase (LoadLevel).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc LoadPlayer
    lda #0
    sta AnimTimer
    sta AnimFrame
    sta CastActive
    sta FaceTimer
    sta PlayerMoved
    sta PlayerVisible
    lda #Side::NONE
    sta PlayerSide
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Trata o controle do player: bola de pedra (B) e movimento (direcional).
;;
;; - A bola só sai no frame em que o B é APERTADO (B agora e não no frame
;;   anterior); segurar B não relança. Ver src/ball.asm.
;; - As 4 direções são testadas em sequência (direita, esquerda, baixo,
;;   cima) usando as tabelas DirButtonMask/DirSide. Cada eixo é testado a
;;   partir da posição real do player, então segurar duas direções contra
;;   uma parede faz o player deslizar por ela.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc InputPlayer
    CheckBButton:
        lda Buttons
        and #BUTTON_B
        beq CheckDirections         ; B não está apertado

        lda PrevButtons
        and #BUTTON_B
        bne CheckDirections         ; B já estava apertado no frame anterior

        jsr FireBall

    CheckDirections:
        lda #0                      ; A assistência de quina só vale com UMA direção
        sta AssistAllowed           ; apertada (senão brigaria com o movimento diagonal)
        lda Buttons
        and #BUTTON_UP | BUTTON_DOWN | BUTTON_LEFT | BUTTON_RIGHT
        beq :+
        sta Temp
        sec
        sbc #1                      ; v & (v - 1) = 0  <=>  v tem um bit só
        and Temp
        bne :+
            lda #1
            sta AssistAllowed
        :

        ldx #0                      ; X = qual direção está sendo testada (0..3)
    LoopDirections:
        lda Buttons
        and DirButtonMask,x
        beq NextDirection           ; Esse botão não está apertado

        lda DirSide,x
        jsr TryMovePlayer           ; Tenta andar 1 pixel nessa direção (preserva X)

    NextDirection:
        inx
        cpx #4
        bne LoopDirections
        rts
.endproc

;; Botão de cada direção e o Side correspondente, na ordem em que são testados
DirButtonMask:  .byte BUTTON_RIGHT, BUTTON_LEFT, BUTTON_DOWN, BUTTON_UP
DirSide:        .byte Side::RIGHT,  Side::LEFT,  Side::DOWN,  Side::UP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tenta mover o player 1 pixel na direção A.
;;
;; - O player sempre passa a OLHAR para a direção apertada, mesmo se não
;;   conseguir andar (assim dá para virar para um bloco encostado nele).
;; - Se a nova posição está livre, ele anda.
;; - Se está bloqueada e só uma direção está apertada, entra a assistência
;;   de quina: se a até CORNER_ASSIST pixels para o lado o caminho estiver
;;   livre (a entrada de um corredor, por exemplo), o player é empurrado
;;   1 pixel para esse lado. Sem isso seria preciso alinhar pixel a pixel
;;   para entrar num corredor de 16 px.
;;
;; Entrada: A = Side (UP, DOWN, RIGHT ou LEFT)
;; Destrói: A, Y (preserva X)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc TryMovePlayer
    sta MoveSide
    sta PlayerSide                  ; Vira para a direção apertada
    lda #FACE_HOLD                  ; E fica virado para ela um pouco depois de soltar
    sta FaceTimer

    lda #0                          ; Passo normal: sem deslocamento para o lado
    jsr SetMoveOffset
    jsr IsOffsetBlocked
    bcc CommitOffset                ; Livre: anda

    lda AssistAllowed
    beq Done

    lda #1                          ; Procura um caminho livre a 1, 2, ... pixels
    sta AssistK                     ; para cada lado
    AssistLoop:
        lda AssistK
        jsr SetMoveOffset           ; Lado "positivo" (baixo/direita)
        jsr IsOffsetBlocked
        bcc NudgePositive

        lda #0
        sec
        sbc AssistK
        jsr SetMoveOffset           ; Lado "negativo" (cima/esquerda)
        jsr IsOffsetBlocked
        bcc NudgeNegative

        inc AssistK
        lda AssistK
        cmp #CORNER_ASSIST + 1
        bne AssistLoop
        rts                         ; Nenhum caminho perto: fica parado

    NudgePositive:
        lda #1
        jmp Nudge
    NudgeNegative:
        lda #$FF                    ; -1
    Nudge:
        jsr SetNudgeOffset          ; Só 1 pixel para o lado, sem andar para frente
        jsr IsOffsetBlocked
        bcs Done

    CommitOffset:                   ; Posição = posição + (OffX, OffY)
        lda #1
        sta PlayerMoved             ; Andou: usa a animação de andar neste frame
        lda PlayerX
        clc
        adc OffX
        sta PlayerX
        lda PlayerY
        clc
        adc OffY
        sta PlayerY
Done:
    rts
.endproc

;; Eixo perpendicular de cada direção (NONE, UP, DOWN, RIGHT, LEFT):
;; andando na vertical o lado é o X, andando na horizontal o lado é o Y.
SidePerpX: .byte 0, 1, 1, 0, 0
SidePerpY: .byte 0, 0, 0, 1, 1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OffX/OffY = 1 pixel na direção MoveSide + A pixels para o lado.
;;
;; Entrada: A = deslocamento perpendicular (com sinal), MoveSide
;; Destrói: A, Y
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc SetMoveOffset
    sta PerpShift
    ldy MoveSide

    lda SidePerpX,y
    beq :+
        lda PerpShift           ; O lado é o X: soma o deslocamento
    :
    clc
    adc SideDeltaX,y
    sta OffX

    lda SidePerpY,y
    beq :+
        lda PerpShift           ; O lado é o Y: soma o deslocamento
    :
    clc
    adc SideDeltaY,y
    sta OffY
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OffX/OffY = A pixels só para o lado (sem andar na direção MoveSide).
;;
;; Entrada: A = deslocamento perpendicular (com sinal), MoveSide
;; Destrói: A, Y
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc SetNudgeOffset
    sta PerpShift
    ldy MoveSide

    lda SidePerpX,y
    beq :+
        lda PerpShift
    :
    sta OffX

    lda SidePerpY,y
    beq :+
        lda PerpShift
    :
    sta OffY
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; O player bateria em algo se estivesse em (PlayerX + OffX, PlayerY + OffY)?
;;
;; Saída:   C = 1 se bloqueado
;; Destrói: A, Y, e tudo que o CheckBoxVsWorld destrói (preserva X)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc IsOffsetBlocked
    lda PlayerX
    clc
    adc OffX
    sta BoxX1
    lda PlayerY
    clc
    adc OffY
    sta BoxY1
    jsr SetBoxSize

    lda #NO_BLOCK                   ; O player colide com todos os blocos
    sta IgnoreBlock
    jmp CheckBoxVsWorld             ; Tail call: devolve o carry direto
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Escolhe a animação do mago neste frame e avança o quadro:
;;   0. morrendo (PLAYER_DYING)       -> Die (8 quadros), para no último;
;;   1. lançando a bola (CastActive)  -> Cast na direção PlayerSide, toca uma
;;                                       vez só (4 quadros) e termina;
;;   2. andou neste frame             -> Walk na direção PlayerSide (12 quadros);
;;   3. parou há pouco (FaceTimer > 0) -> parado virado para PlayerSide
;;                                       (1º quadro do Walk, que é a pose em pé);
;;   4. senão                          -> Idle de frente (16 quadros).
;; Se a animação mudou, ela começa do quadro 0; senão o quadro avança a cada
;; NewAnimDelay frames. O resultado fica em AnimPtr e AnimFrame.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc UpdatePlayerAnimation
    lda GameState               ; Morrendo: animação de morte, uma vez só
    cmp #State::PLAYER_DYING
    bne NotDying
        lda #<MageDie
        sta NewAnimPtr
        lda #>MageDie
        sta NewAnimPtr+1
        lda #MAGE_DIE_FRAMES
        ldy #DIE_ANIM_DELAY
        jsr PlayAnimation
        bcc :+
            lda #MAGE_DIE_FRAMES - 1
            sta AnimFrame           ; Segura o último quadro
        :
        rts

    NotDying:
    lda CastActive
    beq NotCasting
        lda #<MageCastBySide
        ldy #>MageCastBySide
        jsr PickBySide
        lda #MAGE_CAST_FRAMES
        ldy #CAST_ANIM_DELAY
        jsr PlayAnimation           ; C = 1 quando chegou ao fim
        bcc :+
            lda #0
            sta CastActive          ; Acabou de lançar
            lda #MAGE_CAST_FRAMES - 1
            sta AnimFrame           ; Segura o último quadro neste frame
        :
        lda #0
        sta PlayerMoved
        rts

    NotCasting:
    lda PlayerMoved
    beq NotWalking
        lda #0
        sta PlayerMoved             ; Consome o aviso deste frame
        lda #<MageWalkBySide
        ldy #>MageWalkBySide
        jsr PickBySide
        lda #MAGE_WALK_FRAMES
        ldy #WALK_ANIM_DELAY
        jsr PlayAnimation
        bcc :+
            lda #0
            sta AnimFrame           ; Andar repete sem parar
        :
        rts

    NotWalking:
    lda FaceTimer
    beq Idle
        dec FaceTimer
        lda #<MageWalkBySide        ; Em pé, virado para a direção
        ldy #>MageWalkBySide
        jsr PickBySide
        lda NewAnimPtr
        sta AnimPtr
        lda NewAnimPtr+1
        sta AnimPtr+1
        lda #0
        sta AnimFrame               ; O próximo passo começa do quadro 0
        lda #WALK_ANIM_DELAY
        sta AnimTimer
        rts

    Idle:
    lda #<MageIdle
    sta NewAnimPtr
    lda #>MageIdle
    sta NewAnimPtr+1
    lda #MAGE_IDLE_FRAMES
    ldy #IDLE_ANIM_DELAY
    jsr PlayAnimation
    bcc :+
        lda #0
        sta AnimFrame               ; Idle repete sem parar
    :
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NewAnimPtr = entrada PlayerSide da tabela *BySide no endereço (A = baixo,
;; Y = alto). Destrói: A, Y
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc PickBySide
    sta MetaPtr                     ; MetaPtr como ponteiro temporário
    sty MetaPtr+1
    lda PlayerSide
    asl                             ; Cada entrada tem 2 bytes
    tay
    lda (MetaPtr),y
    sta NewAnimPtr
    iny
    lda (MetaPtr),y
    sta NewAnimPtr+1
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Toca a animação NewAnimPtr (A = quantidade de quadros, Y = frames por
;; quadro). Se é uma animação diferente da atual, começa do quadro 0.
;; Saída: C = 1 quando o quadro passou do último (quem chama decide se
;;        repete ou termina); AnimFrame fica igual à quantidade nesse caso.
;; Destrói: A
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc PlayAnimation
    sta NewAnimCount
    sty NewAnimDelay

    lda NewAnimPtr                  ; Mesma animação de antes?
    cmp AnimPtr
    bne Restart
    lda NewAnimPtr+1
    cmp AnimPtr+1
    bne Restart

    dec AnimTimer                   ; Mesma: espera o tempo do quadro
    bne Same
    lda NewAnimDelay
    sta AnimTimer
    inc AnimFrame
    lda AnimFrame
    cmp NewAnimCount                ; C = 1 se passou do último quadro
    rts

    Restart:
        lda NewAnimPtr
        sta AnimPtr
        lda NewAnimPtr+1
        sta AnimPtr+1
        lda #0
        sta AnimFrame
        lda NewAnimDelay
        sta AnimTimer
    Same:
        clc
        rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Desenha o player no OAM com o quadro escolhido pelo UpdatePlayerAnimation.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc RenderPlayer
    lda PlayerVisible
    bne :+
        rts                         ; Escondido (transição)
    :
    lda PlayerX
    sta MetaX
    lda PlayerY
    sta MetaY
    lda #0
    sta MetaAttr                    ; A paleta e o flip do mago já vêm na tabela

    lda AnimFrame
    asl                             ; Cada quadro da tabela é um endereço de 2 bytes
    tay
    lda (AnimPtr),y
    sta MetaPtr
    iny
    lda (AnimPtr),y
    sta MetaPtr+1

    jmp DrawMetasprite              ; Tail call: o rts do DrawMetasprite volta direto
.endproc
