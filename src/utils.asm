.segment "CODE"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lê o controle 1 e guarda o estado dos 8 botões em Buttons.
;;
;; A leitura é serial: cada "lda JOYPAD1" devolve 1 botão no bit 0.
;; Buttons começa com 1: esse bit vai sendo empurrado para a esquerda e,
;; quando sai no carry (depois de 8 leituras), o loop termina.
;; Ordem final dos bits: A, B, Select, Start, Cima, Baixo, Esquerda, Direita.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc ReadControllers
    lda #1                   ; A = 1
    sta Buttons              ; Buttons = 1 (bit sentinela do fim do loop)
    sta JOYPAD1              ; Latch = 1: o controle captura o estado dos botões
    lsr                      ; A = 0
    sta JOYPAD1              ; Latch = 0: começa a enviar os bits um a um
  LoopButtons:
      lda JOYPAD1              ; Lê o próximo botão no bit 0
      lsr                      ; Bit 0 vai para o carry
      rol Buttons              ; Carry entra no bit 0 de Buttons; o bit 7 sai no carry
      bcc LoopButtons          ; Repete até o bit sentinela sair (8 leituras)
      rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Devolve um número aleatório de 8 bits em A (0-255). Destrói Y (fica 0).
;; Precisa da variável Seed (2 bytes) com um valor diferente de zero.
;;
;; É um LFSR de Galois de 16 bits com polinômio $0039: a sequência só se
;; repete depois de 65535 chamadas. Custa em média 125 ciclos.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc GetRandomNumber
    ldy #8                   ; Gera 8 bits
    lda Seed+0
  :   asl                      ; Desloca o registrador de 16 bits
    rol Seed+1
    bcc :+
      eor #$39               ; Se saiu um bit 1, aplica o XOR de realimentação
    :
    dey
    bne :--
    sta Seed+0               ; Guarda o novo valor na semente
    cmp #0                   ; Atualiza as flags Z/N de acordo com A
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copia as 32 cores da paleta (4 de fundo + 4 de sprite) da ROM para a PPU.
;; Só pode ser chamada com a renderização desligada ou durante o VBlank.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc LoadPalette
    PPU_SETADDR $3F00        ; As paletas começam em $3F00 na memória da PPU
    ldy #0
  :   lda PaletteData,y        ; Lê uma cor da ROM
      sta PPU_DATA             ; Envia para a PPU (o endereço avança sozinho)
      iny
      cpy #32
      bne :-
      rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copia a nametable 0 inteira (960 tiles + 64 de atributos = 1024 bytes)
;; de BackgroundData para a PPU em $2000.
;; Só pode ser chamada com a renderização desligada.
;;
;; Como "(ponteiro),y" só alcança 256 bytes, copiamos 4 blocos de 256:
;; Y percorre cada bloco e o byte alto do ponteiro avança entre eles.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc LoadBackground
    lda #<BackgroundData     ; Byte baixo do endereço de BackgroundData
    sta BgPtr
    lda #>BackgroundData     ; Byte alto do endereço de BackgroundData
    sta BgPtr+1

    PPU_SETADDR $2000

    ldx #4                   ; X = quantos blocos de 256 bytes faltam
    ldy #0                   ; Y = posição dentro do bloco atual
    Loop:
        lda (BgPtr),y            ; Lê o byte apontado por BgPtr + Y
        sta PPU_DATA             ; Envia para a PPU
        iny
        bne Loop                 ; Enquanto Y não der a volta (256 bytes), continua
        inc BgPtr+1              ; Próximo bloco de 256 bytes
        dex
        bne Loop                 ; Repete até os 4 blocos
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Desenha um metasprite (vários sprites 8x8 formando uma figura maior)
;; no buffer de OAM, a partir da posição OamIndex.
;;
;; A tabela apontada por MetaPtr tem 4 bytes por sprite e termina com
;; METASPRITE_END no lugar do dy:
;;   .byte dy, tile, atributo, dx     ; dy/dx relativos a (MetaX, MetaY)
;;   ...
;;   .byte METASPRITE_END
;;
;; O atributo final de cada sprite é "atributo da tabela OR MetaAttr":
;; a tabela define os flips e MetaAttr pode trocar a paleta do objeto todo.
;;
;; Entrada: MetaPtr, MetaX, MetaY, MetaAttr, OamIndex
;; Saída:   OamIndex avançado para depois do último sprite escrito
;; Destrói: A, Y (preserva X)
;;
;; Obs.: não há checagem de estouro; o total de sprites do frame precisa
;; caber nos 64 do OAM.
;;
;; A PPU desenha cada sprite 1 linha abaixo do Y gravado no OAM, então a
;; rotina subtrai 1 de MetaY: assim o sprite fica alinhado com o fundo
;; (um bloco em Y = 32 cobre exatamente o metatile da linha 2).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc DrawMetasprite
    txa
    pha                         ; Salva o X de quem chamou (índice de loop)
    dec MetaY                   ; Compensa o atraso de 1 linha dos sprites

    ldx OamIndex                ; X = posição no OAM
    ldy #0                      ; Y = posição na tabela do metasprite
    Loop:
        lda (MetaPtr),y         ; dy
        cmp #METASPRITE_END
        beq Done
        clc
        adc MetaY
        sta OAM_BUFFER+0,x      ; Byte 0: Y na tela
        iny

        lda (MetaPtr),y         ; tile
        sta OAM_BUFFER+1,x      ; Byte 1: número do tile
        iny

        lda (MetaPtr),y         ; atributo da tabela (flips/paleta)
        ora MetaAttr            ; + atributo do objeto
        sta OAM_BUFFER+2,x      ; Byte 2: atributos
        iny

        lda (MetaPtr),y         ; dx
        clc
        adc MetaX
        sta OAM_BUFFER+3,x      ; Byte 3: X na tela
        iny

        inx                     ; Próximo sprite do OAM (4 bytes à frente)
        inx
        inx
        inx
        jmp Loop

    Done:
        stx OamIndex

        pla
        tax                     ; Restaura o X de quem chamou
        rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Esconde todos os sprites do OAM que não foram usados neste frame.
;; Coloca Y = $FF (fora da tela) de OamIndex até o fim do buffer, para que
;; sprites de objetos removidos não fiquem "fantasmas" na tela.
;; Obs.: OamIndex = 0 é tratado como "nenhum sprite usado" (esconde os 64).
;;
;; Destrói: A, X
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc HideUnusedSprites
    ldx OamIndex
    lda #$FF
  :
    sta OAM_BUFFER,x         ; O byte 0 de cada sprite é a posição Y
    inx
    inx
    inx
    inx
    bne :-                   ; Para quando X dá a volta depois do último sprite
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Byte de atributo de sprite (byte 2 no OAM):
;;   76543210
;;   |||   ||
;;   |||   ++- Paleta de sprite (0 a 3)
;;   ||+------ Prioridade (0: na frente do fundo; 1: atrás do fundo)
;;   |+------- Espelhamento horizontal
;;   +-------- Espelhamento vertical
;;
;; As rotinas abaixo alteram um desses campos em ParamAttrOut sem mexer
;; nos outros bits. Todas destroem apenas A.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define a paleta do sprite (bits 0-1)
;; Entrada: ParamAttrIn  = 0 a 3 (qual das 4 paletas de sprite usar)
;; Saída:   ParamAttrOut = byte de atributo atualizado
;;
;; Uso:
;;   lda #2                ; Escolhe a paleta 2
;;   sta ParamAttrIn
;;   jsr SetSpritePalette
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc SetSpritePalette
  lda ParamAttrIn
  and #%00000011
  sta ParamAttrIn

  lda ParamAttrOut
  and #%11111100             ; Limpa a paleta antiga
  ora ParamAttrIn            ; Coloca a nova
  sta ParamAttrOut
  rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define a prioridade do sprite (bit 5)
;; Entrada: ParamAttrIn  = 0 (na frente do fundo) ou 1 (atrás do fundo)
;; Saída:   ParamAttrOut = byte de atributo atualizado
;;
;; Uso:
;;   lda #1                ; Coloca o sprite atrás do background
;;   sta ParamAttrIn
;;   jsr SetSpritePriority
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc SetSpritePriority
    lda ParamAttrIn
    and #%00000001
    asl
    asl
    asl
    asl
    asl                      ; Move o bit 0 para o bit 5
    sta ParamAttrIn

    lda ParamAttrOut
    and #%11011111
    ora ParamAttrIn
    sta ParamAttrOut
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Espelha o sprite horizontalmente (bit 6)
;; Entrada: ParamAttrIn  = 0 (normal) ou 1 (espelhado horizontalmente)
;; Saída:   ParamAttrOut = byte de atributo atualizado
;;
;; Uso:
;;   lda #1                ; Espelha horizontalmente
;;   sta ParamAttrIn
;;   jsr SetSpriteFlipH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc SetSpriteFlipH
    lda ParamAttrIn
    and #%00000001
    asl
    asl
    asl
    asl
    asl
    asl                      ; Move o bit 0 para o bit 6
    sta ParamAttrIn

    lda ParamAttrOut
    and #%10111111
    ora ParamAttrIn
    sta ParamAttrOut
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Espelha o sprite verticalmente (bit 7)
;; Entrada: ParamAttrIn  = 0 (normal) ou 1 (espelhado verticalmente)
;; Saída:   ParamAttrOut = byte de atributo atualizado
;;
;; Uso:
;;   lda #1                ; Espelha verticalmente
;;   sta ParamAttrIn
;;   jsr SetSpriteFlipV
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc SetSpriteFlipV
    lda ParamAttrIn
    and #%00000001
    lsr                      ; Bit 0 vai para o carry...
    ror                      ; ...e do carry para o bit 7
    sta ParamAttrIn

    lda ParamAttrOut
    and #%01111111
    ora ParamAttrIn
    sta ParamAttrOut
    rts
.endproc
