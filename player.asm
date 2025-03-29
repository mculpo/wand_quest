.segment "CODE"

.proc InputPlayer
    lda Players+Player::x_pos       ; Carrega x_pos do player
    sta ParamXPos                  ; Armazena em ParamXPos
    lda Players+Player::y_pos       ; Carrega y_pos do player
    sta ParamYPos                  ; Armazena em ParamYPos
    CheckRightButton:
            lda Buttons
            and #BUTTON_RIGHT                                   ; Perform a bitwise AND with the accumulator
            beq CheckLeftButton                                 ; If the right button is not pressed, we skip to test the left button

                inc ParamXPos                                   ; Incrementa ParamXPos

                jsr CheckBlocksCollision
                lda Collision                                        ; Compara o valor do acumulador A com 1
                cmp #1
                beq :+ 
                inc Players+Player::x_pos                       ; X++, which is only performed if right button is being pressed
                lda #SIDE_RIGHT
                sta Players+Player::side
    CheckLeftButton:
            lda Buttons
            and #BUTTON_LEFT                                  ; Perform a bitwise AND with the accumulator
            beq CheckDownButton                               ; If the left button is not pressed, we skip to test the down button

                dec ParamXPos                  ; Incrementa ParamXPos

                jsr CheckBlocksCollision
                lda Collision
                cmp #1
                beq :+ 
                dec Players+Player::x_pos                     ; X--, which is only performed if left button is being pressed
                lda #SIDE_LEFT
                sta Players+Player::side
    CheckDownButton:
            lda Buttons
            and #BUTTON_DOWN                                  ; Perform a bitwise AND with the accumulator
            beq CheckUpButton                                 ; If the down button is not pressed, we skip to test the up button

                inc ParamYPos                  ; Incrementa ParamXPos

                jsr CheckBlocksCollision
                lda Collision
                cmp #1
                beq :+ 
                inc Players+Player::y_pos                     ; Y++, which is only performed if down button is being pressed
                lda #SIDE_DOWN
                sta Players+Player::side
    CheckUpButton:
            lda Buttons
            and #BUTTON_UP                                    ; Perform a bitwise AND with the accumulator
            beq :+                                            ; If the up button is not pressed, we skip to the end of our button check
                dec ParamYPos                                 ; Incrementa ParamXPos
                jsr CheckBlocksCollision
                lda Collision
                cmp #1
                beq :+ 
                dec Players+Player::y_pos                     ; Y--, which is only performed if up button is being pressed
                lda #SIDE_UP
                sta Players+Player::side
    :
    rts
.endproc

.proc LoadPlayer
    lda #0
    sta Players+Player::t_anim
    sta Players+Player::c_anim
    sta PrevOAMCount

    ; ldx #0
    ; lda SpriteData,x
    ; sta Players+Player::y_pos
    ; inx
    ; inx
    ; inx
    ; lda SpriteData,x
    ; sta Players+Player::x_pos

    lda #70
    sta Players+Player::y_pos
    sta Players+Player::x_pos
    rts
.endproc

.proc UpdateAnimationSprites
    lda #$02
    sta SprPtr+1
    lda #$00
    sta SprPtr                         ; Point SprPtr to $0200

    ;; Apenas para teste.
    lda Players+Player::c_anim
    beq init_sprite      ; Se currentAnim == 0, começa do primeiro MetaSprite

    lda #16              ; Se currentAnim == 1, começa do segundo MetaSprite (pula 16 bytes)
    jmp StartLoop

    init_sprite:
        lda #0               ; Define o índice inicial como 0

    StartLoop:
        tax
        ldy PrevOAMCount
    LoopSprite:
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;
        ;; Atualiza o Y  attributes
        ;; Apenas incremento para pular a inserção do valor Y
        ;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        inx
        iny
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;
        ;; Atualiza o Tiletile#  attributes
        ;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        lda SpriteData, x
        sta (SprPtr), y      ; We store the bytes starting at OAM address $0200
        inx
        iny

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;
        ;; Atualiza o attributes
        ;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        lda SpriteData, x
        sta (SprPtr), y      ; We store the bytes starting at OAM address $0200
        inx
        iny

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;
        ;; Atualiza o X  attributes
        ;; Apenas incremento para pular a inserção do valor X
        ;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        inx
        iny

        cpy #16              ; 16 bytes carregados? (4 sprites de 4 bytes)
        bne LoopSprite       ; Se não, continua carregando
        tya
        sta PrevOAMCount
        rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to loop all enemy actors checking for collision with missile
;; Params = ParamXPos, ParamYPos (are the X and Y position of the missile)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc CheckBlocksCollision
    txa
    pha                                ; Push and save X register in the stack

    ldx #0
    stx Collision                      ; Collision = 0

    jsr FindBackgroundTile
    lda Collision
    cmp #1
    bne :+
        jmp FinishCollisionCheck
    :

  CollisionLoop:
    cpx #MAX_BLOCKS * .sizeof(Block)   ; We loop all entities, looking for blocks
    beq FinishCollisionCheck
      lda blocks+Block::Type,x          ; Load the type of the actor we are looping
      cmp #GameObjectType::NULL
      beq NextEnemy                     ; If it's NOT NULL, bypass this enemy and move check the next one

      ;; LOAD BOUNDING BOX X1, Y1, X2, and Y2
      lda blocks+Block::XPos,x          ; Bounding Box X1
      sta ParamRectX1
      lda blocks+Block::YPos,x          ; Bouding Box Y1
      sta ParamRectY1

      lda blocks+Block::XPos,x
      clc
      adc #16
      sta ParamRectX2

      lda blocks+Block::YPos,x
      clc
      adc #16
      sta ParamRectY2

      jsr IsBoundingBoxColliding

      lda Collision
      beq NextEnemy                    ; If no collision, don't do anything
        jmp FinishCollisionCheck       ; Also, if collision happened we stop looping other enemies and leave the subroutine

  NextEnemy:
      txa
      clc
      adc #.sizeof(Block)              ; X += sizeof(Actor)
      tax
      jmp CollisionLoop         ; Loop to check the next actor to see if it's an enemy (airplane)

  FinishCollisionCheck:
      pla
      tax                                ; Pull and restore the old value of X

      rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to check if a point is inside a bounding box.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc IsBoundingBoxColliding
    ;; Verifica colisão entre duas bounding boxes: Jogador e Bloco

    ;; 1. Verifica se o lado direito do jogador não está à esquerda do bloco
    lda ParamXPos
    clc
    adc #12           ;; valor se tivesse uma variavel para o PlayerRectX2
    cmp ParamRectX1    
    bcc NoCollision    

    ;; 2. Verifica se o lado esquerdo do jogador não está à direita do bloco
    lda ParamXPos
    clc
    adc #2
    cmp ParamRectX2    
    bcs NoCollision    

    ;; 3. Verifica se a parte inferior do jogador não está acima do bloco
    lda ParamYPos
    clc
    adc #16        ;; valor se tivesse uma variavel para o PlayerRectY2
    cmp ParamRectY1    
    bcc NoCollision    

    ;; 4. Verifica se o topo do jogador não está abaixo da parte inferior do bloco
    lda ParamYPos
    cmp ParamRectY2    
    bcs NoCollision    

    ;; Se passou por todas as verificações, há colisão!
    lda #1
    sta Collision
    jmp EndCollisionCheck

  NoCollision:
      lda #0
      sta Collision

  EndCollisionCheck:
      rts
.endproc

.proc FindBackgroundTile
    PUSH_REGS

CheckA:
    lda #<BackgroundData     ; Fetch the lo-byte of BackgroundData address
    sta BgPtr
    lda #>BackgroundData     ; Fetch the hi-byte of BackgroundData address
    sta BgPtr+1

    lda ParamXPos
    lsr 
    lsr 
    lsr 
    sta ParamRectX1

    lda ParamYPos
    lsr 
    lsr 
    lsr 
    sta ParamRectX2
    
    lda ParamRectX2
    jsr MultiplyBy32YAndAddX

    ldy ParamRectX2
    lda (BgPtr), y
    sta ParamData

    lda ParamData
    cmp #$99      ; Compara A com $99
    bcc :+
        lda #1
        sta Collision
        PULL_REGS
        rts 
    :
    
CheckB:
    lda #<BackgroundData     ; Fetch the lo-byte of BackgroundData address
    sta BgPtr
    lda #>BackgroundData     ; Fetch the hi-byte of BackgroundData address
    sta BgPtr+1

    lda ParamXPos
    clc 
    adc #16
    lsr 
    lsr 
    lsr 
    sta ParamRectX1

    lda ParamYPos
    lsr 
    lsr 
    lsr 
    sta ParamRectX2
    
    lda ParamRectX2
    jsr MultiplyBy32YAndAddX

    ldy ParamRectX2
    lda (BgPtr), y
    sta ParamData

    lda ParamData
    cmp #$99      ; Compara A com $99
    bcc :+
        lda #1
        sta Collision
        PULL_REGS
        rts 
    :
CheckC:
    lda #<BackgroundData     ; Fetch the lo-byte of BackgroundData address
    sta BgPtr
    lda #>BackgroundData     ; Fetch the hi-byte of BackgroundData address
    sta BgPtr+1

    lda ParamXPos
    lsr 
    lsr 
    lsr 
    sta ParamRectX1

    lda ParamYPos
    clc 
    adc #16
    lsr 
    lsr 
    lsr 
    sta ParamRectX2
    
    lda ParamRectX2
    jsr MultiplyBy32YAndAddX

    ldy ParamRectX2
    lda (BgPtr), y
    sta ParamData

    lda ParamData
    cmp #$99      ; Compara A com $99
    bcc :+
        lda #1
        sta Collision
        PULL_REGS
        rts 
    :
CheckD:
    lda #<BackgroundData     ; Fetch the lo-byte of BackgroundData address
    sta BgPtr
    lda #>BackgroundData     ; Fetch the hi-byte of BackgroundData address
    sta BgPtr+1

    lda ParamXPos
    clc 
    adc #16
    lsr 
    lsr 
    lsr 
    sta ParamRectX1

    lda ParamYPos
    clc 
    adc #16
    lsr 
    lsr 
    lsr 
    sta ParamRectX2
    
    lda ParamRectX2
    jsr MultiplyBy32YAndAddX

    ldy ParamRectX2
    lda (BgPtr), y
    sta ParamData

    lda ParamData
    cmp #$99      ; Compara A com $99
    bcc :+
        lda #1
        sta Collision
        PULL_REGS
        rts 
    :

    PULL_REGS
    rts             ; Retorna

.endproc

; Calcula a posição do tile no Name Table (32 tiles por linha)
; Endereço = tileY * 32 + tileX
.proc MultiplyBy32YAndAddX
    ldx #0
    LoopMultGeneric:
        asl
        bcc NoOverflow
        inc BgPtr+1
    NoOverflow:
        inx
        cpx #5
        bne LoopMultGeneric

    clc 
    adc ParamRectX1
    sta ParamRectX2
    bcc :+
       inc BgPtr+1                  ; Se houve overflow (carry setada), incrementa o byte alto de BgPtr 
    :
    rts
.endproc