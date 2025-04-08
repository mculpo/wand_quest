;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Collision System for Background Map
;; 
;; This module provides routines to detect collision between an object
;; and the background collision map (bgcollision), based on a tilemap layout.
;;
;; === Requirements ===
;; The actor (object) must provide the positions of its bounding box corners:
;; 
;;   - ParamXPos / ParamX2Pos : horizontal bounds (left/right)
;;   - ParamYPos / ParamY2Pos : vertical bounds (top/bottom)
;;
;; These positions must be in **pixel units**, and the collision system
;; will internally convert them to tile coordinates (divided by 16).
;;
;; The collision detection will mark:
;;   Collision = 1   → if any corner overlaps a solid tile (#$01)
;;   Collision = 0   → otherwise
;;
;; Use `CheckBackgroudCollision` to test the four corners of the object.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "CODE"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Checks collision between the object and the background collision map.
;; It tests four corners of the object's bounding box:
;; (X, Y), (X2, Y), (X, Y2), (X2, Y2)
;; Each corner is passed through VerifySideCollision to check if it hits
;; a tile marked as solid (value #$01) in the bgcollision map.
;;
;; Coordinates are passed via:
;;   - ParamXPos / ParamX2Pos (horizontal bounds)
;;   - ParamYPos / ParamY2Pos (vertical bounds)
;;
;; If any corner has collision, it sets Collision = 1 and exits early.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc CheckBackgroudCollision
    PUSH_REGS

    lda ParamXPos
    sta ParamRectX1
    lda ParamYPos
    sta ParamRectX2

    jsr VerifySideCollision
    lda Collision
    cmp #$01      
    bne :+ 
        PULL_REGS
        rts 
    :

    lda ParamX2Pos
    sta ParamRectX1
    lda ParamYPos
    sta ParamRectX2

    jsr VerifySideCollision
    lda Collision
    cmp #$01
    bne :+ 
        PULL_REGS
        rts 
    :

    lda ParamXPos
    sta ParamRectX1
    lda ParamY2Pos
    sta ParamRectX2

    jsr VerifySideCollision
    lda Collision
    cmp #$01
    bne :+ 
        PULL_REGS
        rts 
    :

    lda ParamX2Pos
    sta ParamRectX1
    lda ParamY2Pos
    sta ParamRectX2

    jsr VerifySideCollision
    lda Collision
    cmp #$01
    bne :+ 
        PULL_REGS
        rts 
    :
    PULL_REGS
    rts 
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Converts object pixel coordinates into tile coordinates (X and Y),
;; then calculates the tile index in the collision map.
;; If the value at that index in bgcollision is #$01,
;; it sets Collision = 1 (meaning a solid tile was hit).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc VerifySideCollision
        lda ParamRectX1
        lsr 
        lsr 
        lsr 
        lsr 
        sta ParamRectX1

        lda ParamRectX2
        lsr 
        lsr 
        lsr 
        lsr 
        sta ParamRectX2
        jsr MultiplyBy16YAndAddX

        lda bgcollision, y
        cmp #$01 
        bne :+ 
            lda #1
            sta Collision
    :
    rts 
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculates the tile index: (Y * 16 + X), since the map is 16 tiles wide.
;; The index is stored in Y for accessing the bgcollision array.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc MultiplyBy16YAndAddX
    asl
    asl
    asl
    asl
    clc
    adc ParamRectX1
    sta ParamRectX2
    lda ParamRectX2
    tay                     
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to check if a point is inside a bounding box.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Using 1º Actor ParamXPos, ParamX2Pos, ParamYPos, ParamY2Pos ()
;; Using 2º Actor ParamRectX1, ParamRectX2, ParamRectY1, ParamRectY2 ()
.proc IsBoundingBoxColliding
    ;; Checks for collision between two bounding boxes: Player and Block

    ;; 1. A.x2 < B.x1 → no collision
    lda ParamX2Pos
    cmp ParamRectX1
    bcc NoCollision

    ;; 2. B.x2 < A.x1 → no collision
    lda ParamRectX2
    cmp ParamXPos
    bcc NoCollision

    ;; 3. A.y2 < B.y1 → no collision
    lda ParamY2Pos
    cmp ParamRectY1
    bcc NoCollision

    ;; 4. B.y2 < A.y1 → no collision
    lda ParamRectY2
    cmp ParamYPos
    bcc NoCollision

    ;; Collision detected
    lda #1
    sta Collision
    rts

  NoCollision:
    lda #0
    sta Collision
    rts
.endproc