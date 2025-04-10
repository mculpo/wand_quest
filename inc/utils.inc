;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macro to disable PPU rendering and NMI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.macro PPU_DISABLE_NMI
    lda #0
    sta PPU_CTRL
    sta PPU_MASK
.endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macro to set a 16-bit address to the PPU_ADDR register (at $2006)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.macro PPU_SETADDR addr
    bit PPU_STATUS           ; Read from PPU_STATUS to reset the address latch
    lda #>addr               ; Fetch the hi-byte from 16-bit address
    sta PPU_ADDR             ; Store the hi-byte into PPU_ADDR $2006
    lda #<addr               ; Fetch the lo-byte from 16-bit address
    sta PPU_ADDR             ; Store the lo-byte into PPU_ADDR $2006
.endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macro to send a value to the PPU_DATA (at $2007)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.macro PPU_SETDATA val
    lda val
    sta PPU_DATA             ; Send value to PPU register at $2007
.endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macro to push and preserv registers A, X, Y, and status flags on the Stack.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.macro PUSH_REGS
    pha                      ; Push A to the stack
    txa
    pha                      ; Push X to the stack
    tya
    pha                      ; Push Y to the stack
    php                      ; Push Processor Status flags to the stack
.endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macro to pull and restore registers A,X,Y, and status flags from the Stack.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.macro PULL_REGS
    plp                      ; Restore the the status flags from the stack
    pla                      ; Restore the old value of X from the stack
    tay
    pla                      ; Restore the old value of X from the stack
    tax
    pla                      ; Pull A from the stack
.endmacro


.macro PUSH_REG_X
    txa     ; Transfere X para A
    pha     ; Salva X (agora em A) na pilha
.endmacro

.macro PULL_REG_X
    pla     ; Recupera X (salvo como A) da pilha
    tax     ; Transfere de volta para X
.endmacro
