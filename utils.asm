.segment "CODE"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routine to read controller state and store it inside "Buttons" in RAM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc ReadControllers
    lda #1                   ; A = 1
    sta Buttons              ; Buttons = 1
    sta JOYPAD1              ; Set Latch=1 to begin 'Input'/collection mode
    lsr                      ; A = 0
    sta JOYPAD1              ; Set Latch=0 to begin 'Output' mode
  LoopButtons:
      lda JOYPAD1              ; This reads a bit from the controller data line and inverts its value,
                              ; And also sends a signal to the Clock line to shift the bits
      lsr                      ; We shift-right to place that 1-bit we just read into the Carry flag
      rol Buttons              ; Rotate bits left, placing the Carry value into the 1st bit of 'Buttons' in RAM
      bcc LoopButtons          ; Loop until Carry is set (from that initial 1 we loaded inside Buttons)
      rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Returns a random 8-bit number inside A (0-255), clobbers Y (0).
;; Requires a 2-byte value on the zero page called "Seed".
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This is a 16-bit Galois linear feedback shift register with polynomial $0039.
; The sequence of numbers it generates will repeat after 65535 calls.
; Execution time is an average of 125 cycles (excluding jsr and rts)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc GetRandomNumber
    ldy #8                   ; Loop counter (generates 8 bits)
    lda Seed+0
  :   asl                      ; Shift the register
    rol Seed+1
    bcc :+
      eor #$39               ; Apply XOR feedback when a 1 bit is shifted out
    :
    dey
    bne :--
    sta Seed+0               ; Saves the value in A into the Seed
    cmp #0                   ; Set flags
    rts
.endproc