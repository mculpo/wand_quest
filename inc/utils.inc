;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macro para desligar a renderização da PPU e a NMI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.macro PPU_DISABLE_NMI
    lda #0
    sta PPU_CTRL
    sta PPU_MASK
.endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macro para escrever um endereço de 16 bits em PPU_ADDR ($2006)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.macro PPU_SETADDR addr
    bit PPU_STATUS           ; Lê PPU_STATUS para resetar o latch de endereço
    lda #>addr               ; Byte alto do endereço
    sta PPU_ADDR             ; Primeira escrita em $2006 = byte alto
    lda #<addr               ; Byte baixo do endereço
    sta PPU_ADDR             ; Segunda escrita em $2006 = byte baixo
.endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macro para enviar um valor para PPU_DATA ($2007)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.macro PPU_SETDATA val
    lda val
    sta PPU_DATA             ; Envia o valor para a PPU em $2007
.endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macro para salvar A, X, Y e as flags na pilha
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.macro PUSH_REGS
    pha                      ; Empilha A
    txa
    pha                      ; Empilha X
    tya
    pha                      ; Empilha Y
    php                      ; Empilha as flags do processador
.endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macro para restaurar A, X, Y e as flags da pilha (ordem inversa do PUSH_REGS)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.macro PULL_REGS
    plp                      ; Restaura as flags
    pla                      ; Restaura Y
    tay
    pla                      ; Restaura X
    tax
    pla                      ; Restaura A
.endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macros para salvar/restaurar só o X (destroem A)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.macro PUSH_REG_X
    txa     ; Transfere X para A
    pha     ; Salva X (agora em A) na pilha
.endmacro

.macro PULL_REG_X
    pla     ; Recupera X (salvo como A) da pilha
    tax     ; Transfere de volta para X
.endmacro
