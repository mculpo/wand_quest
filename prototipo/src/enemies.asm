;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inimigos
;;
;; A fase tem uma lista (EnemyListCol/Row/Type em src/data/level1.asm). Um
;; inimigo da lista nasce num slot livre quando a posição dele chega perto da
;; câmera, e volta para a lista (esperando) se a câmera se afasta muito.
;; Morto, fica marcado e não volta mais (até a fase recomeçar).
;;
;;   Soldado-robô (16x24): patrulha a plataforma, vira na parede e na beirada;
;;       quando vê o agente na frente, na mesma altura, para e atira.
;;   Torreta (16x16): fixa no chão; mira para frente, na diagonal ou para
;;       cima, conforme onde o agente está, e atira de tempos em tempos.
;;   Drone (16x16): voa atrás do agente (atravessa paredes) e machuca no contato.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "CODE"

;; Por tipo (índice = EnemyType)
EnemyHPTable:   .byte 0, ROBOT_HP, TURRET_HP, DRONE_HP
EnemyYOffset:   .byte 0, 8, 0, 0     ; O robô tem 24 px: sobe 8 para os pés ficarem no chão
EnemyFirstShot: .byte 0, 45, 60, 0   ; Espera antes do primeiro tiro

;; Caixa de colisão por tipo: x1, x2, y1, y2 (em relação ao canto do sprite)
EnemyBoxX1: .byte 0, 2, 1, 2
EnemyBoxX2: .byte 0, 13, 14, 13
EnemyBoxY1: .byte 0, 2, 6, 2
EnemyBoxY2: .byte 0, 23, 15, 13

.proc ClearEnemies
    ldx #MAX_ENEMIES - 1
    lda #EnemyType::NONE
    :
        sta EType,x
        dex
        bpl :-
    ldx #MAX_LIST - 1
    lda #0
    :
        sta EListState,x
        dex
        bpl :-
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Posição de mundo do inimigo i da lista em (PointX, PointY). Entrada: Y = i.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc ListPosition
    lda EnemyListCol,y          ; X = coluna * 16
    sta PointX
    lda #0
    sta PointX+1
    .repeat 4
        asl PointX
        rol PointX+1
    .endrepeat
    lda EnemyListRow,y          ; Y = linha * 16 - deslocamento do tipo
    sta PointY
    lda #0
    sta PointY+1
    .repeat 4
        asl PointY
        rol PointY+1
    .endrepeat
    ldx EnemyListType,y
    sec
    lda PointY
    sbc EnemyYOffset,x
    sta PointY
    lda PointY+1
    sbc #0
    sta PointY+1
    rts
.endproc

;; (PointX, PointY) está dentro da janela da câmera ampliada em Temp3 px?
;; Janela = [CamX - m, CamX + 256 + m) x [CamY - m, CamY + 240 + m). C = 1 se dentro.
.proc InCameraWindow
    sec                         ; dx = PointX - CamX + m (tem que ser >= 0 e < 256 + 2m)
    lda PointX
    sbc CamX
    sta Temp
    lda PointX+1
    sbc CamX+1
    sta Temp2
    clc
    lda Temp
    adc Temp3
    sta Temp
    lda Temp2
    adc #0
    bmi Out
    sta Temp2
    lda Temp3                   ; Limite = 256 + 2m
    asl
    sta Ptr
    lda #1
    adc #0
    sta Ptr+1
    lda Temp
    cmp Ptr
    lda Temp2
    sbc Ptr+1
    bcs Out
    sec                         ; dy = PointY - CamY + m (tem que ser < 240 + 2m)
    lda PointY
    sbc CamY
    sta Temp
    lda PointY+1
    sbc CamY+1
    sta Temp2
    clc
    lda Temp
    adc Temp3
    sta Temp
    lda Temp2
    adc #0
    bmi Out
    sta Temp2
    lda Temp3
    asl
    clc
    adc #240
    sta Ptr
    lda #0
    adc #0
    sta Ptr+1
    lda Temp
    cmp Ptr
    lda Temp2
    sbc Ptr+1
    bcs Out
    sec
    rts
Out:
    clc
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Faz nascer os inimigos da lista que entraram perto da câmera.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc SpawnEnemies
    ldy #0
    Loop:
        cpy #ENEMY_LIST_COUNT
        bcs Done
        lda EListState,y
        bne Next                ; Ativo ou morto
        tya                     ; ListPosition e InCameraWindow mexem em X e nos temporários
        pha
        jsr ListPosition
        lda #24
        sta Temp3
        jsr InCameraWindow
        pla
        tay
        bcc Next
        jsr SpawnFromList
    Next:
        iny
        jmp Loop
    Done:
        rts
.endproc

;; Cria no primeiro slot livre o inimigo Y da lista, em (PointX, PointY). Preserva Y.
.proc SpawnFromList
    ldx #0
    :
        lda EType,x
        beq Found
        inx
        cpx #MAX_ENEMIES
        bne :-
    rts                         ; Sem slot: tenta de novo depois
    Found:
    lda #1
    sta EListState,y
    tya
    sta EList,x
    lda EnemyListType,y
    sta EType,x
    stx Temp
    tax
    lda EnemyHPTable,x
    pha
    lda EnemyFirstShot,x
    ldx Temp
    sta ETimer,x
    pla
    sta EHP,x
    lda PointX
    sta EXL,x
    lda PointX+1
    sta EXH,x
    lda PointY
    sta EYL,x
    lda PointY+1
    sta EYH,x
    lda #0
    sta EXS,x
    sta EYS,x
    sta EVXL,x
    sta EVXH,x
    sta EVYL,x
    sta EVYH,x
    sta EPose,x
    sta EFlash,x
    sta EFrame,x
    sta EAim,x
    lda #1
    sta EAnimT,x
    sta EDir,x                  ; Começa olhando para a esquerda
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Diferença entre o centro do agente e o centro do inimigo X:
;;   DeltaX = (PX + 8) - (EX + 8) = PX - EX
;;   DeltaY = (PY + 12) - (EY + meia altura)
;; Saída em PointX/PointY (16 bits com sinal). Preserva X.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc EnemyDelta
    sec
    lda PX
    sbc EXL,x
    sta PointX
    lda PX+1
    sbc EXH,x
    sta PointX+1
    ldy EType,x                 ; Meia altura: 12 para o robô, 8 para os outros
    lda #8
    cpy #EnemyType::ROBOT
    bne :+
        lda #12
    :
    sta Temp
    lda #12
    sec
    sbc Temp
    clc                         ; DeltaY = PY - EY + (12 - meia altura)
    adc PY
    sta Temp
    lda PY+1
    adc #0
    sta Temp2
    sec
    lda Temp
    sbc EYL,x
    sta PointY
    lda Temp2
    sbc EYH,x
    sta PointY+1
    rts
.endproc

;; |valor de 16 bits com sinal| limitado a 255, em A. Entrada: Ptr = valor.
.proc Abs16Clamp
    lda Ptr+1
    bpl Positive
        lda #0
        sec
        sbc Ptr
        sta Temp
        lda #0
        sbc Ptr+1
        bne Big
        lda Temp
        rts
    Positive:
        bne Big
        lda Ptr
        rts
    Big:
        lda #255
        rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Um frame de todos os inimigos.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc UpdateEnemies
    ldx #0
    Loop:
        lda EType,x
        bne :+
            jmp Next
        :
        lda EFlash,x
        beq :+
            dec EFlash,x
        :
        ; Longe demais da câmera: volta para a lista
        lda EXL,x
        sta PointX
        lda EXH,x
        sta PointX+1
        lda EYL,x
        sta PointY
        lda EYH,x
        sta PointY+1
        lda #64
        sta Temp3
        jsr InCameraWindow
        bcs :+
            ldy EList,x
            lda #0
            sta EListState,y
            lda #EnemyType::NONE
            sta EType,x
            jmp Next
        :
        lda EType,x
        cmp #EnemyType::ROBOT
        bne :+
            jsr UpdateRobot
            jmp Touch
        :
        cmp #EnemyType::TURRET
        bne :+
            jsr UpdateTurret
            jmp Touch
        :
            jsr UpdateDrone
        Touch:
        jsr CheckEnemyContact
    Next:
        inx
        cpx #MAX_ENEMIES
        beq :+
        jmp Loop
    :
    rts
.endproc

;; Caixa B = caixa do inimigo X. Preserva X.
.proc SetEnemyBoxB
    ldy EType,x
    clc
    lda EXL,x
    adc EnemyBoxX1,y
    sta BX1
    lda EXH,x
    adc #0
    sta BX1+1
    clc
    lda EXL,x
    adc EnemyBoxX2,y
    sta BX2
    lda EXH,x
    adc #0
    sta BX2+1
    clc
    lda EYL,x
    adc EnemyBoxY1,y
    sta BY1
    lda EYH,x
    adc #0
    sta BY1+1
    clc
    lda EYL,x
    adc EnemyBoxY2,y
    sta BY2
    lda EYH,x
    adc #0
    sta BY2+1
    rts
.endproc

;; Encostou no agente? Então machuca.
.proc CheckEnemyContact
    lda PState
    cmp #PState::DEAD
    beq Done
    jsr SetPlayerBoxA
    jsr SetEnemyBoxB
    jsr BoxesOverlap
    bcc Done
    lda #CONTACT_DAMAGE
    jmp DamagePlayerX
Done:
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Soldado-robô
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc UpdateRobot
    lda EPose,x
    beq :+
        dec EPose,x
    :
    ; Vê o agente? Mesma altura (|dy| < 20), perto (|dx| < ROBOT_SIGHT) e na frente
    jsr EnemyDelta
    lda PointY
    sta Ptr
    lda PointY+1
    sta Ptr+1
    jsr Abs16Clamp
    cmp #20
    bcs Walk
    lda PointX
    sta Ptr
    lda PointX+1
    sta Ptr+1
    jsr Abs16Clamp
    cmp #ROBOT_SIGHT
    bcs Walk
    lda PointX+1                ; Agente à direita (dx >= 0) = direção 0
    rol                         ; Carry = sinal
    lda #0
    rol                         ; A = 1 se o agente está à esquerda
    cmp EDir,x
    bne Walk                    ; Está atrás do robô: continua andando

    ; Na mira: para e atira de tempos em tempos
    dec ETimer,x
    bne Anim
    lda #70
    sta ETimer,x
    lda #12
    sta EPose,x
    jsr RobotShoot
    jmp Anim

    Walk:
    lda EPose,x
    bne Anim                    ; Terminando a pose de tiro
    lda EXS,x                   ; Anda 0,5 px por frame
    clc
    adc #$80
    sta EXS,x
    bcc Anim
    jsr RobotBlocked            ; Parede ou beirada na frente? Vira
    bcc :+
        lda EDir,x
        eor #1
        sta EDir,x
        jmp Anim
    :
    lda EDir,x
    bne Left
        inc EXL,x
        bne Anim
        inc EXH,x
        jmp Anim
    Left:
        lda EXL,x
        bne :+
            dec EXH,x
        :
        dec EXL,x

    Anim:
    dec EAnimT,x                ; Passos: 4 quadros, 8 frames cada
    bne Done
    lda #8
    sta EAnimT,x
    lda EFrame,x
    clc
    adc #1
    and #%00000011
    sta EFrame,x
    Done:
    rts
.endproc

;; Tem parede na frente ou falta chão na frente do robô X? C = 1 se sim.
.proc RobotBlocked
    lda EDir,x                  ; X da frente: EX + 16 (direita) ou EX - 1 (esquerda)
    bne :+
        clc
        lda EXL,x
        adc #16
        sta PointX
        lda EXH,x
        adc #0
        sta PointX+1
        jmp :++
    :
        sec
        lda EXL,x
        sbc #1
        sta PointX
        lda EXH,x
        sbc #0
        sta PointX+1
    :
    clc                         ; Parede na altura do corpo
    lda EYL,x
    adc #12
    sta PointY
    lda EYH,x
    adc #0
    sta PointY+1
    jsr GetFlagsAt
    and #MTF_SOLID
    bne Yes
    clc                         ; Chão logo abaixo dos pés
    lda EYL,x
    adc #24
    sta PointY
    lda EYH,x
    adc #0
    sta PointY+1
    jsr GetFlagsAt
    and #MTF_SOLID | MTF_ONEWAY
    beq Yes
    clc
    rts
Yes:
    sec
    rts
.endproc

;; O robô X atira para o lado em que está olhando.
.proc RobotShoot
    clc                         ; Y do cano
    lda EYL,x
    adc #8
    sta SpawnY
    lda EYH,x
    adc #0
    sta SpawnY+1
    lda #0
    sta SpawnVY
    sta SpawnVY+1
    lda EDir,x
    bne Left
        ADD16_8_X SpawnX, EXL, EXH, 14
        MOV16I SpawnVX, EBULLET_SPEED
        jmp SpawnEBullet
    Left:
        sec
        lda EXL,x
        sbc #6
        sta SpawnX
        lda EXH,x
        sbc #0
        sta SpawnX+1
        MOV16I SpawnVX, -EBULLET_SPEED
        jmp SpawnEBullet
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Torreta
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc UpdateTurret
    jsr EnemyDelta
    lda PointX+1                ; Vira para o lado do agente
    rol
    lda #0
    rol
    sta EDir,x

    ; Mira: agente abaixo ou na mesma altura = frente; muito acima = cima;
    ; senão diagonal. Compara |dx| com |dy|.
    lda PointX
    sta Ptr
    lda PointX+1
    sta Ptr+1
    jsr Abs16Clamp
    sta Temp3                   ; Temp3 = |dx|
    lda PointY+1
    bpl Forward                 ; Agente abaixo: frente
    lda PointY
    sta Ptr
    lda PointY+1
    sta Ptr+1
    jsr Abs16Clamp              ; A = |dy|
    cmp #12
    bcc Forward                 ; Quase na mesma altura
    sta Temp2
    lsr                         ; |dy| / 2 > |dx|: cima
    cmp Temp3
    bcs Up
    lda Temp3                   ; |dx| / 2 > |dy|: frente
    lsr
    cmp Temp2
    bcs Forward
    lda #1
    jmp SetAim
    Up:
        lda #2
        jmp SetAim
    Forward:
        lda #0
    SetAim:
    sta EAim,x

    lda Temp3                   ; Só atira com o agente perto
    cmp #160
    bcs Done
    dec ETimer,x
    bne Done
    lda #90
    sta ETimer,x
    jmp TurretShoot
Done:
    rts
.endproc

;; Velocidades do tiro da torreta por mira (0 frente, 1 diagonal, 2 cima), olhando para a direita
TurretVX:   .word EBULLET_SPEED, EBULLET_DIAG, 0
TurretVY:   .word 0, (-EBULLET_DIAG) & $FFFF, (-EBULLET_SPEED) & $FFFF
TurretOffX: .byte 14, 12, 4          ; Ponta do cano (olhando para a direita)
TurretOffY: .byte 5, 0, <-6

.proc TurretShoot
    lda EAim,x
    asl
    tay
    lda TurretVY,y
    sta SpawnVY
    lda TurretVY+1,y
    sta SpawnVY+1
    lda TurretVX,y
    sta SpawnVX
    lda TurretVX+1,y
    sta SpawnVX+1
    ldy EAim,x
    lda TurretOffY,y            ; Y = EY + deslocamento (com sinal)
    sta Temp
    clc
    adc EYL,x
    sta SpawnY
    lda Temp
    and #$80
    beq :+
        lda #$FF
    :
    adc EYH,x
    sta SpawnY+1
    lda EDir,x
    bne Left
        lda TurretOffX,y
        clc
        adc EXL,x
        sta SpawnX
        lda EXH,x
        adc #0
        sta SpawnX+1
        jmp SpawnEBullet
    Left:                       ; Olhando para a esquerda: espelha o X e a velocidade
        lda #8
        sec
        sbc TurretOffX,y        ; EX + 8 - deslocamento (o tiro tem 8 px)
        sta Temp
        lda EXL,x
        clc
        adc Temp
        sta SpawnX
        lda EXH,x
        adc #0
        sta SpawnX+1
        lda Temp
        bpl :+
            dec SpawnX+1        ; Deslocamento negativo
        :
        lda #0
        sec
        sbc SpawnVX
        sta SpawnVX
        lda #0
        sbc SpawnVX+1
        sta SpawnVX+1
        jmp SpawnEBullet
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Drone: acelera na direção do agente (limite de velocidade), atravessa paredes.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DRONE_ACCEL_X = $0010
DRONE_ACCEL_Y = $000C
DRONE_MAX_X   = $00C0
DRONE_MAX_Y   = $0090

.proc UpdateDrone
    jsr EnemyDelta
    ; X
    lda PointX+1
    bmi AccLeft
        clc
        lda EVXL,x
        adc #<DRONE_ACCEL_X
        sta EVXL,x
        lda EVXH,x
        adc #0
        sta EVXH,x
        bmi DoneX
        lda EVXL,x              ; Limite positivo
        cmp #<DRONE_MAX_X
        lda EVXH,x
        sbc #>DRONE_MAX_X
        bcc DoneX
            lda #<DRONE_MAX_X
            sta EVXL,x
            lda #>DRONE_MAX_X
            sta EVXH,x
        jmp DoneX
    AccLeft:
        sec
        lda EVXL,x
        sbc #<DRONE_ACCEL_X
        sta EVXL,x
        lda EVXH,x
        sbc #0
        sta EVXH,x
        bpl DoneX
        lda EVXL,x              ; Limite negativo
        cmp #<-DRONE_MAX_X
        lda EVXH,x
        sbc #>-DRONE_MAX_X
        bcs DoneX
            lda #<-DRONE_MAX_X
            sta EVXL,x
            lda #>-DRONE_MAX_X
            sta EVXH,x
    DoneX:
    ; Y
    lda PointY+1
    bmi AccUp
        clc
        lda EVYL,x
        adc #<DRONE_ACCEL_Y
        sta EVYL,x
        lda EVYH,x
        adc #0
        sta EVYH,x
        bmi DoneY
        lda EVYL,x
        cmp #<DRONE_MAX_Y
        lda EVYH,x
        sbc #>DRONE_MAX_Y
        bcc DoneY
            lda #<DRONE_MAX_Y
            sta EVYL,x
            lda #>DRONE_MAX_Y
            sta EVYH,x
        jmp DoneY
    AccUp:
        sec
        lda EVYL,x
        sbc #<DRONE_ACCEL_Y
        sta EVYL,x
        lda EVYH,x
        sbc #0
        sta EVYH,x
        bpl DoneY
        lda EVYL,x
        cmp #<-DRONE_MAX_Y
        lda EVYH,x
        sbc #>-DRONE_MAX_Y
        bcs DoneY
            lda #<-DRONE_MAX_Y
            sta EVYL,x
            lda #>-DRONE_MAX_Y
            sta EVYH,x
    DoneY:
    ; Posição += velocidade
    clc
    lda EXS,x
    adc EVXL,x
    sta EXS,x
    lda EXL,x
    adc EVXH,x
    sta EXL,x
    lda EVXH,x
    and #$80
    beq :+
        lda #$FF
    :
    adc EXH,x
    sta EXH,x
    clc
    lda EYS,x
    adc EVYL,x
    sta EYS,x
    lda EYL,x
    adc EVYH,x
    sta EYL,x
    lda EVYH,x
    and #$80
    beq :+
        lda #$FF
    :
    adc EYH,x
    sta EYH,x
    ; Hélice: 4 quadros, 3 frames cada
    dec EAnimT,x
    bne :+
        lda #3
        sta EAnimT,x
        lda EFrame,x
        clc
        adc #1
        and #%00000011
        sta EFrame,x
    :
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tiros do agente contra os inimigos: cada acerto tira 1 de vida e o
;; inimigo pisca; sem vida, explode e fica marcado como morto na lista.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc CheckBulletHits
    ldy #0
    BulletLoop:
        lda PBActive,y
        beq NextBullet
        lda PBXL,y              ; Caixa A = tiro (8x8, um pouco menor)
        clc
        adc #1
        sta AX1
        lda PBXH,y
        adc #0
        sta AX1+1
        lda PBXL,y
        clc
        adc #6
        sta AX2
        lda PBXH,y
        adc #0
        sta AX2+1
        lda PBYL,y
        clc
        adc #1
        sta AY1
        lda PBYH,y
        adc #0
        sta AY1+1
        lda PBYL,y
        clc
        adc #6
        sta AY2
        lda PBYH,y
        adc #0
        sta AY2+1
        sty Temp2               ; Guarda o índice do tiro
        ldx #0
        EnemyLoop:
            lda EType,x
            beq NextEnemy
            jsr SetEnemyBoxB
            jsr BoxesOverlap
            bcc NextEnemy
            ldy Temp2           ; Acertou: o tiro some e o inimigo leva dano
            lda #0
            sta PBActive,y
            jsr HitEnemy
            jmp NextBulletY
        NextEnemy:
            inx
            cpx #MAX_ENEMIES
            bne EnemyLoop
        NextBulletY:
        ldy Temp2
    NextBullet:
        iny
        cpy #MAX_PBULLETS
        bne BulletLoop
    rts
.endproc

;; O inimigo X leva um tiro.
.proc HitEnemy
    lda #8
    sta EFlash,x
    dec EHP,x
    bne Done
    ; Morreu: explosão no meio e marca na lista
    lda EXL,x
    sta SpawnX
    lda EXH,x
    sta SpawnX+1
    lda EYL,x
    sta SpawnY
    lda EYH,x
    sta SpawnY+1
    lda EType,x
    cmp #EnemyType::ROBOT
    bne :+
        ADD16_8 SpawnY, SpawnY, 4
    :
    lda #FxType::EXPLOSION
    jsr SpawnFx
    ldy EList,x
    lda #2
    sta EListState,y
    lda #EnemyType::NONE
    sta EType,x
Done:
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Desenha os inimigos (piscando depois de levar tiro).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc RenderEnemies
    ldx #0
    Loop:
        lda EType,x
        beq SkipNear            ; (salto longo: Next fica longe)
        lda EFlash,x
        and #%00000010
        beq :+
        SkipNear:
            jmp Next
        :
        lda EType,x
        cmp #EnemyType::ROBOT
        bne NotRobot
            lda EPose,x         ; Quadro: 4 = atirando, senão o passo
            beq :+
                lda #4
                jmp :++
            :
                lda EFrame,x
            :
            asl
            tay
            lda EDir,x
            bne RobotLeft
                lda RobotR,y
                sta MetaPtr
                lda RobotR+1,y
                sta MetaPtr+1
                jmp Draw
            RobotLeft:
                lda RobotL,y
                sta MetaPtr
                lda RobotL+1,y
                sta MetaPtr+1
                jmp Draw
        NotRobot:
        cmp #EnemyType::TURRET
        bne NotTurret
            lda EAim,x
            asl
            tay
            lda EDir,x
            bne TurretLeft
                lda TurretR,y
                sta MetaPtr
                lda TurretR+1,y
                sta MetaPtr+1
                jmp Draw
            TurretLeft:
                lda TurretL,y
                sta MetaPtr
                lda TurretL+1,y
                sta MetaPtr+1
                jmp Draw
        NotTurret:
            lda EFrame,x
            asl
            tay
            lda Drone,y
            sta MetaPtr
            lda Drone+1,y
            sta MetaPtr+1
        Draw:
        lda EXL,x
        sta MetaWX
        lda EXH,x
        sta MetaWX+1
        lda EYL,x
        sta MetaWY
        lda EYH,x
        sta MetaWY+1
        jsr DrawMetaWorld
    Next:
        inx
        cpx #MAX_ENEMIES
        beq :+
        jmp Loop
    :
    rts
.endproc
