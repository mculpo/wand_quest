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
        sta blocks+Block::XVel,x        ; Every actor starts at Screen 0
        sta blocks+Block::YVel,x        ; Every actor starts at Screen 0
        sta blocks+Block::Screen,x      ; Every actor starts at Screen 0 
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
    
    ldx #0
    LoopBlocks:
        cpx #MAX_BLOCKS * .sizeof(Block)        ; Reached maximum number of blocks allowed in the array?
        beq EndRoutine                          ; Then we skip and don't add a new actor
        lda blocks+Block::Type,x
        cmp #GameObjectType::NULL               ; If the actor type of this array position is NULL
        beq NextBlock                              ; Then: we found an empty slot, proceed to add actor to position [x]
    ; Atualiza posição apenas se o bloco for válido
        lda blocks+Block::XPos,x
        adc blocks+Block::XVel,x
        sta blocks+Block::XPos,x

        lda blocks+Block::YPos,x
        adc blocks+Block::YVel,x
        sta blocks+Block::YPos,x

    NextBlock:
        txa
        clc
        adc #.sizeof(Block)                     ; Move para o próximo bloco
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
    sta SprPtr                         ; Point SprPtr to $0200
    ldx #0
    LoopBlocks:
        cpx #MAX_BLOCKS * .sizeof(Block)        ; Reached maximum number of blocks allowed in the array?
        beq EndRoutine                          ; Then we skip and don't add a new actor
        lda blocks+Block::Type,x
        cmp #GameObjectType::NULL               ; If the actor type of this array position is NULL
        beq NextBlock                           ; Then: we found an empty slot, proceed to add actor to position [x]

        lda blocks+Block::XPos, x 
        sta ParamXPos
        lda blocks+Block::YPos, x 
        sta ParamYPos

        jsr RenderOAMBlock

    NextBlock:
        txa
        clc
        adc #.sizeof(Block)               ; Otherwise, we offset to the check the next actor in the array
        tax                               ; X += sizeof(Actor)
        jmp LoopBlocks
    EndRoutine:
        rts       
.endproc

.proc RenderOAMBlock
    
    ldy PrevOAMCount
    ;;Side 1 - 8x8
    Side1:
        lda ParamYPos
        sta (SprPtr), y              ; We store the bytes starting at OAM address $0200
        iny

        lda #$04
        sta (SprPtr), y              ; We store the bytes starting at OAM address $0200
        iny

        lda #%00000010
        sta (SprPtr), y              ; We store the bytes starting at OAM address $0200
        iny

        lda ParamXPos
        sta (SprPtr), y              ; We store the bytes starting at OAM address $0200
        iny
    ;;Side 2 - 8x8
    Side2:
        lda ParamYPos
        sta (SprPtr), y              ; We store the bytes starting at OAM address $0200
        iny

        lda #$04
        sta (SprPtr), y              ; We store the bytes starting at OAM address $0200
        iny

        lda #%01000010
        sta (SprPtr), y              ; We store the bytes starting at OAM address $0200
        iny

        lda ParamXPos
        clc
        adc #8  ; Move 8 pixels para a direita
        sta (SprPtr), y              ; We store the bytes starting at OAM address $0200
        iny
    ;;Side 3 - 8x8
    Side3:
        lda ParamYPos
        clc
        adc #8  ; Move 8 pixels para a direita
        sta (SprPtr), y              ; We store the bytes starting at OAM address $0200
        iny

        lda #$04
        sta (SprPtr), y              ; We store the bytes starting at OAM address $0200
        iny

        lda #%10000010
        sta (SprPtr), y              ; We store the bytes starting at OAM address $0200
        iny

        lda ParamXPos
        sta (SprPtr), y              ; We store the bytes starting at OAM address $0200
        iny
    ;;Side 4 - 8x8
    Side4:

        lda ParamYPos
        clc
        adc #8  ; Move 8 pixels para a direita
        sta (SprPtr), y              ; We store the bytes starting at OAM address $0200
        iny

        lda #$04
        sta (SprPtr), y              ; We store the bytes starting at OAM address $0200
        iny

        lda #%11000010
        sta (SprPtr), y              ; We store the bytes starting at OAM address $0200
        iny

        lda ParamXPos
        clc
        adc #8  ; Move 8 pixels para a direita
        sta (SprPtr), y              ; We store the bytes starting at OAM address $0200
        iny
    tya
    sta PrevOAMCount

    rts 
.endproc