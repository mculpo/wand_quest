.segment "CODE"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to process player input and update position accordingly.
;;
;; This function checks for directional button presses (Right, Left, Down, Up)
;; and updates the player's position accordingly. Before finalizing a move,
;; it calls `CheckCollisions` to ensure movement is valid.
;;
;; If a collision is detected (`Collision == 1`), movement is canceled.
;; The player's facing direction (`side`) is also updated based on movement.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc InputPlayer
    lda Players+Player::x_pos       ; Carrega x_pos do player
    sta ParamXPos                  ; Armazena em ParamXPos
    lda Players+Player::y_pos       ; Carrega y_pos do player
    sta ParamYPos                  ; Armazena em ParamYPos
    CheckBButton:
        lda Buttons
        and #BUTTON_B
        beq CheckRightButton

            lda PrevButtons       
            cmp Buttons
            beq CheckRightButton  

                lda Players+Player::side
                cmp #Side::RIGHT
                beq MoveRight
                cmp #Side::LEFT
                beq MoveLeft
                cmp #Side::DOWN
                beq MoveDown
                cmp #Side::UP
                beq MoveUp

        MoveRight:
            inc ParamXPos
            jsr CastSpell

        MoveLeft:
            dec ParamXPos
            jsr CastSpell

        MoveDown:
            inc ParamYPos
            jsr CastSpell

        MoveUp:
            dec ParamYPos
            jsr CastSpell

    CheckRightButton:
            lda Buttons
            and #BUTTON_RIGHT                                   ; Perform a bitwise AND with the accumulator
            beq CheckLeftButton                                 ; If the right button is not pressed, we skip to test the left button

                inc ParamXPos                                   ; Incrementa ParamXPos

                jsr CheckCollisions
                lda Collision                                        ; Compara o valor do acumulador A com 1
                cmp #1
                beq :+ 
                inc Players+Player::x_pos                       ; X++, which is only performed if right button is being pressed
                lda #Side::RIGHT
                sta Players+Player::side
    CheckLeftButton:
            lda Buttons
            and #BUTTON_LEFT                                  ; Perform a bitwise AND with the accumulator
            beq CheckDownButton                               ; If the left button is not pressed, we skip to test the down button

                dec ParamXPos                  ; Incrementa ParamXPos

                jsr CheckCollisions
                lda Collision
                cmp #1
                beq :+ 
                dec Players+Player::x_pos                     ; X--, which is only performed if left button is being pressed
                lda #Side::LEFT
                sta Players+Player::side
    CheckDownButton:
            lda Buttons
            and #BUTTON_DOWN                                  ; Perform a bitwise AND with the accumulator
            beq CheckUpButton                                 ; If the down button is not pressed, we skip to test the up button

                inc ParamYPos                  ; Incrementa ParamXPos

                jsr CheckCollisions
                lda Collision
                cmp #1
                beq :+ 
                inc Players+Player::y_pos                     ; Y++, which is only performed if down button is being pressed
                lda #Side::DOWN
                sta Players+Player::side
    CheckUpButton:
            lda Buttons
            and #BUTTON_UP                                    ; Perform a bitwise AND with the accumulator
            beq :+                                            ; If the up button is not pressed, we skip to the end of our button check
                dec ParamYPos                                 ; Incrementa ParamXPos
                jsr CheckCollisions
                lda Collision
                cmp #1
                beq :+ 
                dec Players+Player::y_pos                     ; Y--, which is only performed if up button is being pressed
                lda #Side::UP
                sta Players+Player::side
    :
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to initialize the player.
;; 
;; This function sets the player's animation counters (`t_anim`, `c_anim`)
;; and resets the previous OAM sprite count. It also assigns an initial
;; position for the player on the screen.
;;
;; The player's X and Y coordinates are hardcoded for now.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to update the player's animation sprites.
;;
;; This function selects the correct sprite data based on the player's
;; current animation state (`c_anim`). It then loads the appropriate 
;; MetaSprite data from `SpriteData` and writes it to OAM ($0200).
;;
;; The animation system works by loading 16 bytes (4 sprites of 4 bytes each).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc RenderPlayer
    lda #$02
    sta SprPtr+1
    lda #$00
    sta SprPtr                         ; Point SprPtr to $0200

    ;; Apenas para teste.
    lda Players+Player::c_anim
    beq SetSprite      ; Se currentAnim == 0, começa do primeiro MetaSprite

    lda #16              ; Se currentAnim == 1, começa do segundo MetaSprite (pula 16 bytes)
    jmp StartLoop

    SetSprite:
        lda #0               ; Define o índice inicial como 0

    StartLoop:
        tax
        ldy PrevOAMCount
    Side1:
        lda Players+Player::y_pos
        sta (SprPtr), y              ; We store the bytes starting at OAM address $0200
        inx
        iny

        lda SpriteData, x
        sta (SprPtr), y      ; We store the bytes starting at OAM address $0200
        inx
        iny

        lda SpriteData, x
        sta (SprPtr), y      ; We store the bytes starting at OAM address $0200
        inx
        iny

        lda Players+Player::x_pos
        sta (SprPtr), y              ; We store the bytes starting at OAM address $0200
        inx
        iny

    Side2:
        lda Players+Player::y_pos
        sta (SprPtr), y              ; We store the bytes starting at OAM address $0200
        inx
        iny
        
        lda SpriteData, x
        sta (SprPtr), y      ; We store the bytes starting at OAM address $0200
        inx
        iny

        lda SpriteData, x
        sta (SprPtr), y      ; We store the bytes starting at OAM address $0200
        inx
        iny

        lda Players+Player::x_pos
        clc
        adc #8
        sta (SprPtr), y              ; We store the bytes starting at OAM address $0200
        inx
        iny

    Side3:
        lda Players+Player::y_pos
        clc
        adc #8
        sta (SprPtr), y              ; We store the bytes starting at OAM address $0200
        inx
        iny
        
        lda SpriteData, x
        sta (SprPtr), y      ; We store the bytes starting at OAM address $0200
        inx
        iny

        lda SpriteData, x
        sta (SprPtr), y      ; We store the bytes starting at OAM address $0200
        inx
        iny

        lda Players+Player::x_pos
        sta (SprPtr), y      ; We store the bytes starting at OAM address $0200
        inx
        iny

    Side4:
        lda Players+Player::y_pos
        clc
        adc #8
        sta (SprPtr), y              ; We store the bytes starting at OAM address $0200
        inx
        iny
        
        lda SpriteData, x
        sta (SprPtr), y      ; We store the bytes starting at OAM address $0200
        inx
        iny

        lda SpriteData, x
        sta (SprPtr), y      ; We store the bytes starting at OAM address $0200
        inx
        iny

        lda Players+Player::x_pos
        clc
        adc #8
        sta (SprPtr), y      ; We store the bytes starting at OAM address $0200
        inx
        iny

        tya
        sta PrevOAMCount
        rts
.endproc

.proc CastSpell
    ldx #0
    stx Collision                      ; Collision = 0

    CollisionLoop:
        cpx #MAX_BLOCKS * .sizeof(Block)   ; Verifica se chegou ao fim da lista
        beq FinishCollisionCheck

        lda blocks+Block::Type,x           
        cmp #GameObjectType::NULL          
        beq NextEnemy

        lda blocks+Block::XPos,x
        sta ParamRectX1
        adc #16                             ; X2 = X1 + 16
        sta ParamRectX2

        lda blocks+Block::YPos,x
        sta ParamRectY1
        adc #16                             ; Y2 = Y1 + 16
        sta ParamRectY2
        
        jsr IsBoundingBoxColliding

        lda Collision
        beq NextEnemy

        lda Players+Player::side
        sta blocks+Block::Side, x
        jmp FinishCollisionCheck            ; Sai do loop, pois encontrou colisão
    
  NextEnemy:
      txa
      clc
      adc #.sizeof(Block)              ; X += sizeof(Actor)
      tax
      jmp CollisionLoop         ; Loop to check the next actor to see if it's an enemy (airplane)

  FinishCollisionCheck:
    rts
.endproc

.proc CheckCollisions
    txa
    pha                                ; Push and save X register in the stack
    ldx #0
    stx Collision                      ; Collision = 0

    ; jsr CheckBackgroudCollision
    ; lda Collision
    ; cmp #1
    ; bne :+
    ;     jmp FinishCollisionCheck
    ; :

    jsr CheckBlocksCollision
    ; lda Collision
    ; cmp #1
    ; bne :+
    ;     jmp FinishCollisionCheck
    ; :

    FinishCollisionCheck:

    pla
    tax                                ; Pull and restore the old value of X

    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to loop all enemy actors checking for collision with missile
;; Params = ParamXPos, ParamYPos (are the X and Y position of the missile)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc CheckBlocksCollision
    CollisionLoop:
        cpx #MAX_BLOCKS * .sizeof(Block)   ; We loop all entities, looking for blocks
        beq FinishCollisionCheck

        lda blocks+Block::Type,x          ; Load the type of the actor we are looping
        cmp #GameObjectType::NULL
        beq NextEnemy                     ; If it's NOT NULL, bypass this enemy and move check the next one

        ;; SETUP BOUNDING BOX
            lda blocks+Block::XPos,x
            sta ParamRectX1
            adc #16                             ; X2 = X1 + 16
            sta ParamRectX2

            lda blocks+Block::YPos,x
            sta ParamRectY1
            adc #16                             ; Y2 = Y1 + 16
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
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to check if a point is inside a bounding box.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc IsBoundingBoxColliding
    ;; Checks for collision between two bounding boxes: Player and Block

    ;; 1. The player's right side must not be to the left of the block
    lda ParamXPos
    adc #12           ;; PlayerRectX2
    sbc ParamRectX1   ;; Subtracts block's X1
    bcc NoCollision   ;; If result < 0, no collision

    ;; 2. The player's left side must not be to the right of the block
    lda ParamXPos
    adc #2            ;; PlayerRectX1
    sbc ParamRectX2   ;; Subtracts block's X2
    bcs NoCollision   ;; If result >= 0, no collision

    ;; 3. The player's bottom must not be above the block
    lda ParamYPos
    adc #16           ;; PlayerRectY2
    sbc ParamRectY1   ;; Subtracts block's Y1
    bcc NoCollision   ;; If result < 0, no collision

    ;; 4. The player's top must not be below the block's bottom
    lda ParamYPos
    sbc ParamRectY2   ;; Subtracts block's Y2
    bcs NoCollision   ;; If result >= 0, no collision

    ;; If all checks passed, there is a collision!
    lda #1
    sta Collision
    rts               ;; Exit function

  NoCollision:
    lda #0
    sta Collision
    rts
.endproc


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to check for collisions between the player and the background.
;; This function reads the Name Table and verifies if any of the four corners 
;; of the player's hitbox overlap with a predefined collision tile ($99).
;;
;; The collision check is performed at four points:
;;   - CheckA: Top-left corner of the player’s hitbox
;;   - CheckB: Top-right corner
;;   - CheckC: Bottom-left corner
;;   - CheckD: Bottom-right corner
;;
;; The player's pixel coordinates (X, Y) are converted into tile coordinates
;; by dividing by 8 (since each tile is 8x8 pixels).
;;
;; If any of the four points detect a collision tile, the Collision flag is set.
;; Registers are pushed at the beginning and restored before returning.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.proc CheckBackgroudCollision
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculates the tile position in the Name Table (32 tiles per row)
;; Address = tileY * 32 + tileX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


SpriteOffsetsX:
    .byte 0,  8,  0,  8    ; X offsets for 16x16 MetaSprite
SpriteOffsetsY:
    .byte 0,  0,  8,  8    ; Y offsets for 16x16 MetaSprite