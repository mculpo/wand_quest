.segment "CODE"

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

        lda #3
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

        lda blocks+Block::XPos,x
        sta ParamRectX1
        lda blocks+Block::YPos,x
        sta ParamRectY1

        tya 
        sta ParamCurrentNumActor
        sta Collision

        lda blocks+Block::Side,x
        cmp #Side::NONE                   
        beq NextBlock

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
        lda ParamRectY1
        sbc blocks+Block::YVel,x
        sta ParamRectY1

        jsr BlockCheckCollisions
        lda Collision
        cmp #1
        beq HasCollision
            lda ParamRectY1
            sta blocks+Block::YPos,x
            jmp NextBlock
    MoveDown:
        clc 
        lda ParamRectY1
        adc blocks+Block::YVel,x
        sta ParamRectY1

        jsr BlockCheckCollisions
        lda Collision
        cmp #1
        beq HasCollision
            lda ParamRectY1
            sta blocks+Block::YPos,x
            jmp NextBlock

    MoveRight:
        clc 
        lda ParamRectX1
        adc blocks+Block::XVel,x
        sta ParamRectX1

        jsr BlockCheckCollisions
        lda Collision
        cmp #1
        beq HasCollision
            lda ParamRectX1
            sta blocks+Block::XPos,x
            jmp NextBlock

    MoveLeft:
        sec 
        lda ParamRectX1
        sbc blocks+Block::XVel,x
        sta ParamRectX1

        jsr BlockCheckCollisions
        lda Collision
        cmp #1
        beq HasCollision
            lda ParamRectX1
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


.proc RenderOAMBlock
    ldy PrevOAMCount

    Side1:
        lda ParamYPos
        sta (SprPtr), y
        iny

        lda #$04
        sta (SprPtr), y
        iny

        lda #%00000010
        sta (SprPtr), y
        iny

        lda ParamXPos
        sta (SprPtr), y
        iny

    Side2:
        lda ParamYPos
        sta (SprPtr), y
        iny

        lda #$04
        sta (SprPtr), y
        iny

        lda #%01000010
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

        lda #$04
        sta (SprPtr), y
        iny

        lda #%10000010
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

    lda #$04
    sta (SprPtr), y
    iny

    lda #%11000010
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


.proc BlockCheckCollisions
    PUSH_REGS

    ldx #0
    stx Collision                      ; Collision = 0

    ; jsr CheckBackgroudCollision
    ; lda Collision
    ; cmp #1
    ; bne :+
    ;     jmp FinishCollisionCheck
    ; :

    jsr CheckCollisionPerBlock
    ; lda Collision
    ; cmp #1
    ; bne :+
    ;     jmp FinishCollisionCheck
    ; :

    FinishCollisionCheck:

    PULL_REGS

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
        sta ParamRectX2

        lda blocks+Block::YPos,x
        sta ParamRectY2

        jsr BlockIsBoundingBoxColliding

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to check if a point is inside a bounding box.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc BlockIsBoundingBoxColliding

    lda ParamRectX1
    clc
    adc #16         
    cmp ParamRectX2 
    bcc NoCollision

    lda ParamRectX2
    clc
    adc #16         
    cmp ParamRectX1 
    bcc NoCollision

    lda ParamRectY1
    clc
    adc #16         
    cmp ParamRectY2
    bcc NoCollision

    lda ParamRectY2
    clc
    adc #16         
    cmp ParamRectY1
    bcc NoCollision

    lda #1
    sta Collision
    rts

    NoCollision:
        lda #0
        sta Collision
        rts
.endproc
