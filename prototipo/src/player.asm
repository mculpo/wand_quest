;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; O agente de armadura vermelha
;;
;; Posição (PX, PY) = canto superior esquerdo da caixa 16x24, em pixels do
;; mundo, com fração (PXSub/PYSub). Velocidades (PVX/PVY) em 8.8 com sinal.
;;
;; Controles:
;;   Esquerda/Direita  anda          Baixo          agacha
;;   A                 pula (segurar = mais alto)   Baixo + A      rola
;;   B                 atira         Cima + B       atira para cima
;;
;; Colisão com o cenário, eixo por eixo:
;;   - horizontal: a borda da frente da caixa não pode entrar numa parede;
;;   - vertical: o pixel logo abaixo dos pés decide se está no chão (parede
;;     ou passarela, se veio de cima); subindo, a cabeça bate no teto.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "CODE"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Agente no começo da fase, com a vida cheia.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc InitPlayer
    MOV16I PX, PLAYER_START_X
    MOV16I PY, PLAYER_START_Y
    lda #0
    sta PXSub
    sta PYSub
    sta PVX
    sta PVX+1
    sta PVY
    sta PVY+1
    sta PFacing
    sta POnGround
    sta PTimer
    sta PInvuln
    sta PShootCD
    sta PShootPose
    sta PShootUp
    sta AnimPtr+1
    lda #PState::NORMAL
    sta PState
    lda #PBOX_Y1
    sta PBoxTop
    lda #PLAYER_MAX_HP
    sta PHealth
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Um frame do agente: controle conforme o estado, física e perigos.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc UpdatePlayer
    lda PState
    cmp #PState::DEAD
    bne :+
        rts
    :
    lda PShootCD                ; Contadores
    beq :+
        dec PShootCD
    :
    lda PShootPose
    beq :+
        dec PShootPose
    :
    lda PInvuln
    beq :+
        dec PInvuln
    :

    lda PState
    cmp #PState::NORMAL
    bne :+
        jsr HandleNormal
        jmp Physics
    :
    cmp #PState::CROUCH
    bne :+
        jsr HandleCrouch
        jmp Physics
    :
    cmp #PState::ROLL
    bne :+
        jsr HandleRoll
        jmp Physics
    :
        jsr HandleHurt          ; PState::HURT

    Physics:
    jsr ApplyGravity
    jsr MoveX
    jsr MoveY
    jmp CheckExit
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Estado NORMAL: anda, pula, agacha, rola e atira.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc HandleNormal
    jsr RunInput

    lda POnGround               ; Agachar / rolar só no chão
    beq NotCrouch
    lda Buttons
    and #BUTTON_DOWN
    beq NotCrouch
        lda Pressed
        and #BUTTON_A
        beq :+
            jmp StartRoll
        :
        lda #PState::CROUCH
        sta PState
        lda #PBOX_Y1_LOW
        sta PBoxTop
        rts
    NotCrouch:

    lda POnGround               ; Pulo
    beq :+
    lda Pressed
    and #BUTTON_A
    beq :+
        MOV16I PVY, -JUMP_SPEED
        lda #0
        sta POnGround
    :
    lda Pressed
    and #BUTTON_B
    beq :+
        jmp TryShoot
    :
    rts
.endproc

;; Esquerda/direita: acelera até RUN_SPEED; sem direção, freia.
.proc RunInput
    lda Buttons
    and #BUTTON_RIGHT
    beq :+
        lda #0
        sta PFacing
        MOV16I Temp, RUN_SPEED  ; Temp/Temp2 = velocidade alvo
        lda #RUN_ACCEL
        jmp ApproachVX
    :
    lda Buttons
    and #BUTTON_LEFT
    beq :+
        lda #1
        sta PFacing
        MOV16I Temp, -RUN_SPEED
        lda #RUN_ACCEL
        jmp ApproachVX
    :
    jmp BrakeX
.endproc

;; Freia até parar
.proc BrakeX
    lda #0
    sta Temp
    sta Temp2
    lda #RUN_FRICTION
    jmp ApproachVX
.endproc

;; Aproxima PVX do alvo (Temp = baixo, Temp2 = alto) em passos de A.
.proc ApproachVX
    sta Temp3
    sec                         ; Diferença = alvo - PVX
    lda Temp
    sbc PVX
    sta Ptr
    lda Temp2
    sbc PVX+1
    sta Ptr+1
    ora Ptr
    beq Done                    ; Já está no alvo
    lda Ptr+1
    bmi Down
        lda Ptr+1               ; Alvo acima: soma o passo (sem passar)
        bne AddStep
        lda Ptr
        cmp Temp3
        bcc SetTarget
    AddStep:
        clc
        lda PVX
        adc Temp3
        sta PVX
        lda PVX+1
        adc #0
        sta PVX+1
        rts
    Down:
        lda Ptr+1               ; Alvo abaixo: diferença negativa
        cmp #$FF
        bne SubStep
        lda Ptr
        eor #$FF
        clc
        adc #1                  ; |diferença|
        cmp Temp3
        bcc SetTarget
    SubStep:
        sec
        lda PVX
        sbc Temp3
        sta PVX
        lda PVX+1
        sbc #0
        sta PVX+1
        rts
    SetTarget:
        lda Temp
        sta PVX
        lda Temp2
        sta PVX+1
    Done:
        rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Estado CROUCH: parado agachado (vira para os lados), rola com A, atira baixo.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc HandleCrouch
    jsr BrakeX
    lda Buttons
    and #BUTTON_RIGHT
    beq :+
        lda #0
        sta PFacing
    :
    lda Buttons
    and #BUTTON_LEFT
    beq :+
        lda #1
        sta PFacing
    :
    lda Pressed
    and #BUTTON_A
    beq :+
        jmp StartRoll
    :
    lda Pressed
    and #BUTTON_B
    beq :+
        jsr TryShoot
    :
    lda POnGround               ; Caiu de uma beirada ou soltou o Baixo: tenta levantar
    beq TryStand
    lda Buttons
    and #BUTTON_DOWN
    bne Done
    TryStand:
        jsr CanStand
        bcc Done
        lda #PState::NORMAL
        sta PState
        lda #PBOX_Y1
        sta PBoxTop
    Done:
        rts
.endproc

;; Tem espaço para ficar de pé? C = 1 se sim.
.proc CanStand
    clc
    lda PY
    adc #PBOX_Y1
    sta PointY
    lda PY+1
    adc #0
    sta PointY+1
    ADD16_8 PointX, PX, PBOX_X1
    jsr IsSolidAt
    bcs No
    ADD16_8 PointX, PX, PBOX_X2
    jsr IsSolidAt
    bcs No
    sec
    rts
No:
    clc
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rolamento: velocidade fixa para frente, caixa baixa, invencível no meio.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc StartRoll
    lda #PState::ROLL
    sta PState
    lda #ROLL_TIME
    sta PTimer
    lda #PBOX_Y1_LOW
    sta PBoxTop
    lda PFacing
    bne :+
        MOV16I PVX, ROLL_SPEED
        rts
    :
    MOV16I PVX, -ROLL_SPEED
    rts
.endproc

.proc HandleRoll
    dec PTimer
    bne Done
    jsr CanStand                ; Acabou: levanta se couber, senão fica agachado
    bcc Low
        lda #PState::NORMAL
        sta PState
        lda #PBOX_Y1
        sta PBoxTop
        rts
    Low:
        lda #PState::CROUCH
        sta PState
    Done:
        rts
.endproc

;; O agente está invencível agora? (piscando ou no meio do rolamento) C = 1 se sim.
.proc IsPlayerInvulnerable
    lda PInvuln
    bne Yes
    lda PState
    cmp #PState::DEAD
    beq Yes
    cmp #PState::ROLL
    bne No
    lda PTimer
    cmp #ROLL_INVULN_FROM + 1
    bcs No
    cmp #ROLL_INVULN_TO
    bcc No
Yes:
    sec
    rts
No:
    clc
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dano: A = quanto. Recuo para trás, pisca invencível; sem vida, explode.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc DamagePlayer
    sta Temp3
    jsr IsPlayerInvulnerable
    bcc :+
        rts
    :
    lda PHealth
    sec
    sbc Temp3
    bcs :+
        lda #0
    :
    sta PHealth
    bne Hurt
    jmp KillPlayer
    Hurt:
    lda #PState::HURT
    sta PState
    lda #HURT_TIME
    sta PTimer
    lda #INVULN_TIME
    sta PInvuln
    lda #PBOX_Y1
    sta PBoxTop
    MOV16I PVY, -KNOCK_Y
    lda #0
    sta POnGround
    lda PFacing                 ; Empurra para trás de onde está olhando
    bne :+
        MOV16I PVX, -KNOCK_X
        rts
    :
    MOV16I PVX, KNOCK_X
    rts
.endproc

.proc HandleHurt
    dec PTimer
    bne :+
        lda #PState::NORMAL
        sta PState
    :
    rts
.endproc

;; Sem vida: explode e o jogo espera para recomeçar a fase.
.proc KillPlayer
    lda #PState::DEAD
    sta PState
    lda #GState::DEAD
    sta GameState
    lda #DEAD_TIME
    sta StateTimer
    ADD16_8 SpawnX, PX, 0       ; Duas explosões no corpo
    ADD16_8 SpawnY, PY, 0
    lda #FxType::EXPLOSION
    jsr SpawnFx
    ADD16_8 SpawnY, PY, 8
    lda #FxType::EXPLOSION
    jmp SpawnFx
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Atira (se a arma já esfriou e há um slot de tiro livre).
;; Cima + B atira para cima (em pé); agachado sai rente ao chão.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc TryShoot
    lda PShootCD
    beq :+
        rts
    :
    ldx #0
    FindSlot:
        lda PBActive,x
        beq Found
        inx
        cpx #MAX_PBULLETS
        bne FindSlot
        rts
    Found:
    lda #SHOOT_COOLDOWN
    sta PShootCD
    lda #SHOOT_POSE
    sta PShootPose
    lda #1
    sta PBActive,x
    lda #0
    sta PShootUp

    lda PState                  ; Para cima: só de pé (no chão ou no ar)
    cmp #PState::NORMAL
    bne Horizontal
    lda Buttons
    and #BUTTON_UP
    beq Horizontal
        lda #1
        sta PShootUp
        lda #0
        sta PBVX,x
        lda #<-PBULLET_SPEED
        sta PBVY,x
        lda PFacing             ; X = na frente do cano (coluna 9 do sprite)
        bne :+
            ADD16_8 SpawnX, PX, 5
            jmp :++
        :
            ADD16_8 SpawnX, PX, 2
        :
        sec                     ; Y = acima da cabeça
        lda PY
        sbc #8
        sta SpawnY
        lda PY+1
        sbc #0
        sta SpawnY+1
        jmp Place

    Horizontal:
        lda #0
        sta PBVY,x
        lda PState              ; Altura do cano: de pé ou agachado
        cmp #PState::CROUCH
        bne :+
            ADD16_8 SpawnY, PY, 15
            jmp :++
        :
            ADD16_8 SpawnY, PY, 8
        :
        lda PFacing
        bne Left
            lda #PBULLET_SPEED
            sta PBVX,x
            ADD16_8 SpawnX, PX, 14
            jmp Place
        Left:
            lda #<-PBULLET_SPEED
            sta PBVX,x
            sec
            lda PX
            sbc #6
            sta SpawnX
            lda PX+1
            sbc #0
            sta SpawnX+1

    Place:
    lda SpawnX
    sta PBXL,x
    lda SpawnX+1
    sta PBXH,x
    lda SpawnY
    sta PBYL,x
    lda SpawnY+1
    sta PBYH,x
    lda #FxType::MUZZLE         ; Clarão no cano
    jmp SpawnFx
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gravidade: mais fraca subindo com o A apertado (pulo variável).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc ApplyGravity
    lda PVY+1
    bpl Normal                  ; Caindo: gravidade normal
    lda PState
    cmp #PState::NORMAL
    bne Normal
    lda Buttons
    and #BUTTON_A
    beq Normal
        lda #GRAVITY_HOLD       ; Subindo com o A apertado: gravidade fraca
        bne Add
    Normal:
        lda #GRAVITY
    Add:
    clc
    adc PVY
    sta PVY
    lda PVY+1
    adc #0
    sta PVY+1
    bmi Done                    ; Subindo: sem limite
    cmp #>MAX_FALL
    bcc Done
        MOV16I PVY, MAX_FALL
    Done:
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Movimento horizontal com colisão na borda da frente (3 pontos na altura).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc MoveX
    ADD_VEL PXSub, PX, PVX
    lda PVX
    ora PVX+1
    bne :+
        rts
    :
    lda PVX+1
    bmi Left
        ADD16_8 PointX, PX, PBOX_X2
        jsr HitsWallX
        bcc Done
        lda PointX              ; PX = início do tile - 1 - PBOX_X2
        and #$F0
        sec
        sbc #PBOX_X2 + 1
        sta PX
        lda PointX+1
        sbc #0
        sta PX+1
        jmp Stop
    Left:
        ADD16_8 PointX, PX, PBOX_X1
        jsr HitsWallX
        bcc Done
        lda PointX              ; PX = fim do tile + 1 - PBOX_X1
        and #$F0
        clc
        adc #16 - PBOX_X1
        sta PX
        lda PointX+1
        adc #0
        sta PX+1
    Stop:
        lda #0
        sta PXSub
        sta PVX
        sta PVX+1
    Done:
        rts
.endproc

;; A coluna PointX bate em parede na altura do agente (topo, meio, pés)? C = 1 se sim.
.proc HitsWallX
    clc
    lda PY
    adc PBoxTop
    sta PointY
    lda PY+1
    adc #0
    sta PointY+1
    jsr IsSolidAt
    bcs Yes
    ADD16_8 PointY, PY, 13
    lda PBoxTop
    cmp #13
    bcs :+                      ; Agachado: o meio já é o topo
        jsr IsSolidAt
        bcs Yes
    :
    ADD16_8 PointY, PY, PBOX_Y2
    jsr IsSolidAt
Yes:
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Movimento vertical: chão (parede ou passarela vinda de cima), teto e espinhos.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc MoveY
    ADD16_8 OldFeet, PY, PBOX_Y2
    ADD_VEL PYSub, PY, PVY
    lda #0
    sta POnGround

    lda PVY+1
    bpl Falling
        ; Subindo: a cabeça bate no teto?
        clc
        lda PY
        adc PBoxTop
        sta PointY
        lda PY+1
        adc #0
        sta PointY+1
        ADD16_8 PointX, PX, PBOX_X1
        jsr IsSolidAt
        bcs Ceiling
        ADD16_8 PointX, PX, PBOX_X2
        jsr IsSolidAt
        bcc Done
    Ceiling:
        lda PointY              ; PY = fim do tile + 1 - topo da caixa
        and #$F0
        clc
        adc #16
        sta Temp
        lda PointY+1
        adc #0
        sta Temp2
        sec
        lda Temp
        sbc PBoxTop
        sta PY
        lda Temp2
        sbc #0
        sta PY+1
        lda #0
        sta PYSub
        sta PVY
        sta PVY+1
    Done:
        rts

    Falling:
        ; O pixel logo abaixo dos pés
        ADD16_8 PointY, PY, PBOX_Y2 + 1
        lda #0
        sta Temp3               ; Propriedades do chão encontrado
        ADD16_8 PointX, PX, PBOX_X1
        jsr GroundAt
        ADD16_8 PointX, PX, PBOX_X2
        jsr GroundAt
        lda Temp3
        beq Done
        ; Pousou: pés logo acima do tile
        lda PointY
        and #$F0
        sec
        sbc #PBOX_Y2 + 1
        sta PY
        lda PointY+1
        sbc #0
        sta PY+1
        lda #0
        sta PYSub
        sta PVY
        sta PVY+1
        lda #1
        sta POnGround
        lda Temp3               ; Espinho?
        and #MTF_HURT
        beq :+
            lda #SPIKE_DAMAGE
            jmp DamagePlayer
        :
        rts
.endproc

;; Soma em Temp3 as propriedades do chão em (PointX, PointY), se ele segura:
;; parede sempre; passarela só se os pés estavam acima dela (OldFeet < topo).
.proc GroundAt
    jsr GetFlagsAt
    sta Temp
    and #MTF_SOLID
    bne Yes
    lda Temp
    and #MTF_ONEWAY
    beq No
    lda PointY                  ; Topo do tile
    and #$F0
    sta Temp2
    lda OldFeet                 ; OldFeet < topo?
    cmp Temp2
    lda OldFeet+1
    sbc PointY+1
    bcs No
Yes:
    lda Temp
    ora #MTF_SOLID              ; Marca que achou chão
    ora Temp3
    sta Temp3
No:
    rts
.endproc

;; Chegou na saída (centro do agente numa porta)?
.proc CheckExit
    ADD16_8 PointX, PX, 8
    ADD16_8 PointY, PY, 12
    jsr GetFlagsAt
    and #MTF_EXIT
    beq :+
        lda #GState::CLEAR
        sta GameState
        lda #CLEAR_TIME
        sta StateTimer
    :
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Animação: escolhe a tabela conforme o estado e avança o quadro.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Macro: toca a animação (tabela direita/esquerda conforme PFacing)
.macro PLAY_ANIM right, left, frames, delay
    .local LeftSide, Go
    lda PFacing
    bne LeftSide
        lda #<right
        ldx #>right
        jmp Go
    LeftSide:
        lda #<left
        ldx #>left
    Go:
    ldy #frames
    sty AnimCount
    ldy #delay
    jmp PlayAnim
.endmacro

.proc UpdatePlayerAnim
    lda PState
    cmp #PState::ROLL
    bne :+
        PLAY_ANIM AgentRollR, AgentRollL, AGENT_ROLL_R_FRAMES, 3
    :
    cmp #PState::HURT
    bne :+
        PLAY_ANIM AgentHurtR, AgentHurtL, 1, 8
    :
    cmp #PState::CROUCH
    bne NotCrouch
        lda PShootPose          ; Agachado: quadro 1 é o recuo do tiro
        beq :+
            lda #1
        :
        sta Temp
        PLAY_ANIM AgentCrouchR, AgentCrouchL, 1, 8
    NotCrouch:

    lda PShootPose
    beq NotShooting
    lda PShootUp
    beq :+
        PLAY_ANIM AgentShootUpR, AgentShootUpL, 1, 8
    :
    lda POnGround
    beq NotShooting             ; No ar: a pose do pulo já tem a arma para frente
    lda PVX
    ora PVX+1
    bne NotShooting             ; Correndo e atirando: continua a corrida
        PLAY_ANIM AgentShootStandR, AgentShootStandL, 1, 8
    NotShooting:

    lda POnGround
    bne OnGround
        lda PVY+1
        bpl :+
            PLAY_ANIM AgentJumpR, AgentJumpL, AGENT_JUMP_R_FRAMES, 6
        :
        PLAY_ANIM AgentFallR, AgentFallL, 1, 8
    OnGround:
    lda PVX+1                   ; Andando (|velocidade| >= 0,375 px)?
    bpl Positive
        lda #0                  ; Negativa: |v| = -v
        sec
        sbc PVX
        sta Temp
        lda #0
        sbc PVX+1
        jmp CheckSpeed
    Positive:
        lda PVX
        sta Temp
        lda PVX+1
    CheckSpeed:
    bne Running
    lda Temp
    cmp #$60
    bcc Idle
    Running:
        PLAY_ANIM AgentRunR, AgentRunL, AGENT_RUN_R_FRAMES, 4
    Idle:
        PLAY_ANIM AgentIdleR, AgentIdleL, AGENT_IDLE_R_FRAMES, 8
.endproc

;; Toca a animação A/X (tabela), AnimCount quadros, Y frames por quadro.
;; Se é outra tabela, começa do quadro 0 (ou do quadro Temp, no agachado).
.proc PlayAnim
    cmp AnimPtr
    bne Restart
    cpx AnimPtr+1
    bne Restart
    lda PState                  ; Agachado: o quadro é escolhido direto
    cmp #PState::CROUCH
    bne :+
        lda Temp
        sta AnimFrame
        rts
    :
    dec AnimTimer
    bne Done
    sty AnimTimer
    inc AnimFrame
    lda AnimFrame
    cmp AnimCount
    bcc Done
        lda #0
        sta AnimFrame
    Done:
        rts
    Restart:
        sta AnimPtr
        stx AnimPtr+1
        sty AnimTimer
        lda #0
        sta AnimFrame
        lda PState
        cmp #PState::CROUCH
        bne :+
            lda Temp
            sta AnimFrame
        :
        rts
.endproc

;; Desenha o agente (pisca quando está invencível depois de levar dano).
.proc RenderPlayer
    lda PState
    cmp #PState::DEAD
    beq Done
    lda PInvuln
    and #%00000100
    bne Done
    lda AnimFrame
    asl
    tay
    lda (AnimPtr),y
    sta MetaPtr
    iny
    lda (AnimPtr),y
    sta MetaPtr+1
    lda PX
    sta MetaWX
    lda PX+1
    sta MetaWX+1
    lda PY
    sta MetaWY
    lda PY+1
    sta MetaWY+1
    jmp DrawMetaWorld
Done:
    rts
.endproc
