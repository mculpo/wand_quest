.segment "CODE"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to load all block sprites.
;; 
;; This routine iterates through the SpriteBlockData table and loads blocks 
;; based on a pair of (Y, X) positions. For each valid pair found, it calls 
;; AddNewBlock to register a new block in the system.
;;
;; It uses the X register to traverse the table, and stops when a zero is found.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc LoadAllBlock
    ldx #0
    LoopSprite:
        lda SpriteBlockData, x       ; We fetch bytes from the  lookup table
        beq :+
        sta ParamYPos
        inx                          ; X+

        lda SpriteBlockData, x       ; We fetch bytes from the  lookup table
        beq :+
        sta ParamXPos
        inx
        jsr AddNewBlock
        jmp LoopSprite               
    :
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to add a new block.
;;
;; This routine finds the first available slot in the block array (i.e., where 
;; the type is NULL) and fills in the block data using global parameters for 
;; position (X, Y). If the array is full, it exits without making changes.
;;
;; Initializes each block with default screen, side, and velocity values.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc AddNewBlock
    PUSH_REGS
    ldx #0
    LoopBlocks:
        cpx #MAX_BLOCKS * .sizeof(Block)        ; Reached maximum number of blocks allowed in the array?
        beq EndRoutine                          ; Then we skip and don't add a new actor
        lda blocks+Block::Type,x
        cmp #GameObjectType::NULL               ; If the actor type of this array position is NULL
        beq AddNewActorToArray                  ; Then: we found an empty slot, proceed to add actor to position [x]
    NextBlock:
        txa
        clc
        adc #.sizeof(Block)               ; Otherwise, we offset to the check the next actor in the array
        tax                               ; X += sizeof(Block)
        jmp LoopBlocks

    AddNewActorToArray:                 ; Here we add a new actor at index [x] of the array
        lda #GameObjectType::BLOCKS       ; Fetch parameter "actor type" from RAM
        sta blocks+Block::Type,x
        lda ParamXPos                     ; Fetch parameter "actor position X" from RAM
        sta blocks+Block::XPos,x
        lda ParamYPos                     ; Fetch parameter "actor position Y" from RAM
        sta blocks+Block::YPos,x

        lda #0
        sta blocks+Block::Screen,x      ; Every actor starts at Screen 0 
        sta blocks+Block::Side, x

        lda #1
        sta blocks+Block::XVel,x        ; Every actor starts at Screen 0
        sta blocks+Block::YVel,x        ; Every actor starts at Screen 0
    EndRoutine:
        PULL_REGS
        rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to update all active blocks.
;; This function iterates through the block array and updates only blocks 
;; that are not marked as NULL.
;;
;; Each block's position (X, Y) is updated by adding its velocity (XVel, YVel).
;; Blocks that are marked as NULL are skipped.
;;
;; The loop iterates over all possible block slots, checking their type 
;; before applying updates.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc UpdateBlock
    ldy #0
    ldx #0 
    LoopBlocks:
        cpx #MAX_BLOCKS * .sizeof(Block)
        bne :+ 
            jmp EndRoutine
        :

        lda blocks+Block::Type,x
        cmp #GameObjectType::NULL         
        bne :+ 
            jmp NextBlock
        :

        clc 
        lda blocks+Block::XPos,x
        sta ParamXPos
        adc #16
        sta ParamX2Pos

        clc 
        lda blocks+Block::YPos,x
        sta ParamYPos
        adc #16
        sta ParamY2Pos

        tya 
        sta ParamCurrentNumActor
        sta Collision

        lda blocks+Block::Side,x
        cmp #Side::NONE                   
        bne :+ 
            jmp NextBlock
        :

        cmp #Side::UP
        beq MoveUp

        cmp #Side::DOWN
        beq MoveDown

        cmp #Side::RIGHT
        beq MoveRight

        cmp #Side::LEFT
        beq MoveLeft

        jmp NextBlock

    MoveUp:
        sec  
        lda ParamYPos
        sbc blocks+Block::YVel,x
        sta ParamYPos
        clc 
        adc #16
        sta ParamY2Pos

        jsr BlockCheckCollisions
        lda Collision
        cmp #1
        beq HasCollision
            lda ParamYPos
            sta blocks+Block::YPos,x
            jmp NextBlock
    MoveDown:
        clc 
        lda ParamYPos
        adc blocks+Block::YVel,x
        sta ParamYPos
        adc #16
        sta ParamY2Pos

        jsr BlockCheckCollisions
        lda Collision
        cmp #1
        beq HasCollision
            lda ParamYPos
            sta blocks+Block::YPos,x
            jmp NextBlock

    MoveRight:
        clc 
        lda ParamXPos
        adc blocks+Block::XVel,x
        sta ParamXPos
        adc #16
        sta ParamX2Pos

        jsr BlockCheckCollisions
        lda Collision
        cmp #1
        beq HasCollision
            lda ParamXPos
            sta blocks+Block::XPos,x
            jmp NextBlock

    MoveLeft:
        sec 
        lda ParamXPos
        sbc blocks+Block::XVel,x
        sta ParamXPos
        clc 
        adc #16
        sta ParamX2Pos

        jsr BlockCheckCollisions
        lda Collision
        cmp #1
        beq HasCollision
            lda ParamXPos
            sta blocks+Block::XPos,x
            jmp NextBlock
    
    HasCollision:
        lda #Side::NONE
        sta blocks+Block::Side,x

    NextBlock:
        iny 

        txa 
        clc 
        adc #.sizeof(Block)                ; Move to the next block
        tax 
        jmp LoopBlocks

    EndRoutine:
        rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to render all active blocks.
;; This function iterates through the block array and renders only blocks 
;; that are not marked as NULL.
;;
;; The rendering process updates OAM memory to display the blocks as sprites.
;; Each block's position (X, Y) is stored in temporary parameters before 
;; calling the RenderOAMBlock subroutine.
;;
;; The loop iterates over all possible block slots, checking their type 
;; before rendering.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc RenderBlocks
    lda #$02
    sta SprPtr+1
    lda #$00
    sta SprPtr
    ldx #0

    LoopBlocks:
        cpx #MAX_BLOCKS * .sizeof(Block)
        beq EndRoutine

        lda blocks+Block::Type,x
        cmp #GameObjectType::NULL
        beq NextBlock

        lda blocks+Block::XPos, x 
        sta ParamXPos
        lda blocks+Block::YPos, x 
        sta ParamYPos

        jsr RenderOAMBlock

    NextBlock:
        txa
        clc
        adc #.sizeof(Block)
        tax
        jmp LoopBlocks

    EndRoutine:
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to render a block into the OAM (Object Attribute Memory).
;;
;; This routine renders a 16x16 block using four 8x8 sprites (Side1 to Side4).
;; It places each sprite in the appropriate position using ParamXPos and 
;; ParamYPos, and updates the OAM pointer accordingly.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc RenderOAMBlock
    ldy PrevOAMCount

    Side1:
        lda ParamYPos
        sta (SprPtr), y
        iny

        lda #$06
        sta (SprPtr), y
        iny

        lda #%00000000
        sta (SprPtr), y
        iny

        lda ParamXPos
        sta (SprPtr), y
        iny

    Side2:
        lda ParamYPos
        sta (SprPtr), y
        iny

        lda #$07
        sta (SprPtr), y
        iny

        lda #%00000000
        sta (SprPtr), y
        iny

        lda ParamXPos
        clc
        adc #8
        sta (SprPtr), y
        iny

    Side3:
        lda ParamYPos
        clc
        adc #8
        sta (SprPtr), y
        iny

        lda #$08
        sta (SprPtr), y
        iny

        lda #%01000000
        sta (SprPtr), y
        iny

        lda ParamXPos
        sta (SprPtr), y
        iny

    Side4:
    lda ParamYPos
    clc
    adc #8
    sta (SprPtr), y
    iny

    lda #$08
    sta (SprPtr), y
    iny

    lda #%00000000
    sta (SprPtr), y
    iny

    lda ParamXPos
    clc
    adc #8
    sta (SprPtr), y
    iny

    tya
    sta PrevOAMCount

    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to check block collisions.
;;
;; This routine checks for collisions between the player and blocks, as well 
;; as with the background. It sets the Collision flag to 1 if a collision is 
;; detected. The check is performed in the following order:
;;   1. Per-block collisions
;;   2. Background collisions
;;   3. Collision with the player
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc BlockCheckCollisions
    PUSH_REGS

    ldx #0
    stx Collision

    jsr CheckCollisionPerBlock
    lda Collision
    cmp #1
    bne :+
        jmp FinishCollisionCheck
    :

    jsr CheckBackgroudCollision
    lda Collision
    cmp #1
    bne :+
        jmp FinishCollisionCheck
    :

    jsr CheckCollisionPlayer

    FinishCollisionCheck:

    PULL_REGS

    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to check collision between the player and the blocks.
;;
;; This routine sets up a bounding box around the player and calls the 
;; IsBoundingBoxColliding function to test collision. If a collision occurs, 
;; the Collision flag will be set.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc CheckCollisionPlayer
    tya 
    pha 

    sec 
    lda Players+Player::x_pos
    sta ParamRectX1
    adc #16
    sta ParamRectX2
    lda Players+Player::y_pos
    sta ParamRectY1
    adc #16
    sta ParamRectY2

    jsr IsBoundingBoxColliding

    pla 
    tay 
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to loop all enemy actors checking for collision with missile
;; Params = ParamXPos, ParamYPos (are the X and Y position of the missile)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc CheckCollisionPerBlock
    tya 
    pha
    ldy #0
    CollisionLoop:
        cpx #MAX_BLOCKS * .sizeof(Block)
        beq FinishCollisionCheck

        tya 
        cmp ParamCurrentNumActor
        beq NextEnemy

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
        jmp FinishCollisionCheck

    NextEnemy:
        clc 
        iny 
        txa 
        clc 
        adc #.sizeof(Block)
        tax 
        jmp CollisionLoop

    FinishCollisionCheck:
    pla 
    tya 
    rts
.endproc
