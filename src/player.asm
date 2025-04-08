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
    clc 
    lda Players+Player::x_pos
    sta ParamXPos
    adc #16
    sta ParamX2Pos

    clc 
    lda Players+Player::y_pos
    sta ParamYPos
    adc #16
    sta ParamY2Pos

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
    inc ParamX2Pos
    jsr CastSpell

MoveLeft:
    dec ParamXPos
    dec ParamX2Pos
    jsr CastSpell

MoveDown:
    inc ParamYPos
    inc ParamY2Pos
    jsr CastSpell

MoveUp:
    dec ParamYPos
    dec ParamY2Pos
    jsr CastSpell

CheckRightButton:
    lda Buttons
    and #BUTTON_RIGHT
    beq CheckLeftButton

    inc ParamXPos
    inc ParamX2Pos 
    jsr CheckCollisions
    lda Collision
    cmp #1
    beq CheckLeftButton
    inc Players+Player::x_pos
    lda #Side::RIGHT
    sta Players+Player::side

CheckLeftButton:
    lda Buttons
    and #BUTTON_LEFT
    beq CheckDownButton

    dec ParamXPos
    dec ParamX2Pos 
    jsr CheckCollisions
    lda Collision
    cmp #1
    beq CheckDownButton
    dec Players+Player::x_pos
    lda #Side::LEFT
    sta Players+Player::side

CheckDownButton:
    lda Buttons
    and #BUTTON_DOWN
    beq CheckUpButton

    inc ParamYPos
    inc ParamY2Pos
    jsr CheckCollisions
    lda Collision
    cmp #1
    beq CheckUpButton
    inc Players+Player::y_pos
    lda #Side::DOWN
    sta Players+Player::side

CheckUpButton:
    lda Buttons
    and #BUTTON_UP
    beq :+

    dec ParamYPos
    dec ParamY2Pos
    jsr CheckCollisions
    lda Collision
    cmp #1
    beq :+
    dec Players+Player::y_pos
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
    sta SprPtr

    lda Players+Player::c_anim
    beq SetSprite

    lda #16
    jmp StartLoop

    SetSprite:
        lda #0

    StartLoop:
        tax
        ldy PrevOAMCount

    Side1:
        lda Players+Player::y_pos
        sta (SprPtr), y
        inx
        iny

        lda SpriteData, x
        sta (SprPtr), y
        inx
        iny

        lda SpriteData, x
        sta (SprPtr), y
        inx
        iny

        lda Players+Player::x_pos
        sta (SprPtr), y
        inx
        iny

    Side2:
        lda Players+Player::y_pos
        sta (SprPtr), y
        inx
        iny

        lda SpriteData, x
        sta (SprPtr), y
        inx
        iny

        lda SpriteData, x
        sta (SprPtr), y
        inx
        iny

        lda Players+Player::x_pos
        clc
        adc #8
        sta (SprPtr), y
        inx
        iny

    Side3:
        lda Players+Player::y_pos
        clc
        adc #8
        sta (SprPtr), y
        inx
        iny

        lda SpriteData, x
        sta (SprPtr), y
        inx
        iny

        lda SpriteData, x
        sta (SprPtr), y
        inx
        iny

        lda Players+Player::x_pos
        sta (SprPtr), y
        inx
        iny

    Side4:
        lda Players+Player::y_pos
        clc
        adc #8
        sta (SprPtr), y
        inx
        iny

        lda SpriteData, x
        sta (SprPtr), y
        inx
        iny

        lda SpriteData, x
        sta (SprPtr), y
        inx
        iny

        lda Players+Player::x_pos
        clc
        adc #8
        sta (SprPtr), y
        inx
        iny

        tya
        sta PrevOAMCount
        rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to cast a spell and detect collision with any Block.
;;
;; This routine iterates through all blocks and checks if any is colliding
;; with the player's spell hitbox using the bounding box method. If a collision
;; is found, it updates the Side of the block to match the player and ends.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc CastSpell
    ldx #0
    stx Collision

    CollisionLoop:
        cpx #MAX_BLOCKS * .sizeof(Block)
        beq FinishCollisionCheck

        lda blocks+Block::Type,x
        cmp #GameObjectType::NULL
        beq NextEnemy

        lda blocks+Block::XPos,x
        sta ParamRectX1
        adc #16
        sta ParamRectX2

        lda blocks+Block::YPos,x
        sta ParamRectY1
        adc #16
        sta ParamRectY2

        jsr IsBoundingBoxColliding

        lda Collision
        beq NextEnemy

        lda Players+Player::side
        sta blocks+Block::Side, x
        jmp FinishCollisionCheck

    NextEnemy:
        txa
        clc
        adc #.sizeof(Block)
        tax
        jmp CollisionLoop

    FinishCollisionCheck:
        rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to check collisions with background and blocks.
;;
;; This routine first saves the X register, resets collision state,
;; checks for background and then block collision. If any of them
;; sets the Collision flag to 1, it skips further checks.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc CheckCollisions
        txa
        pha
        ldx #0
        stx Collision

        jsr CheckBackgroudCollision
        lda Collision
        cmp #1
        bne :+
            jmp FinishCollisionCheck
    :

        jsr CheckBlocksCollision

    FinishCollisionCheck:
        pla
        tax
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