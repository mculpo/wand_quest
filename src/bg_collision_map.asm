;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sistema de colisão
;;
;; Todas as rotinas "Is*" e "Check*" devolvem o resultado no CARRY:
;;   C = 1  -> houve colisão / condição verdadeira
;;   C = 0  -> não houve
;; Quem chama testa com "bcs Colidiu" ou "bcc Livre", sem variável extra.
;;
;; Isso funciona bem no 6502 porque "cmp" já deixa o carry pronto:
;; depois de "lda A / cmp B", C = 1 se A >= B e C = 0 se A < B.
;;
;; === Mapa de colisão da fase (LevelMap) ===
;; É uma grade de 16x16 metatiles (cada um com 16x16 pixels) na RAM, um byte
;; por metatile, lida linha por linha, montada pelo LoadLevel (src/level.asm).
;; A tela tem 15 linhas; a 16ª é sempre parede, então um Y entre 240 e 255
;; também é tratado como parede. Os valores estão em consts.inc:
;; TILE_EMPTY, TILE_SOLID e TILE_SLOT.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "CODE"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lê o valor do mapa de colisão no ponto (PointX, PointY), em pixels.
;;
;; O índice no mapa é linha * 16 + coluna, onde linha = Y / 16 e
;; coluna = X / 16. Como (Y / 16) * 16 é a mesma coisa que zerar os
;; 4 bits baixos de Y, o índice sai com um "and" e um "ora":
;;   índice = (Y & %11110000) | (X >> 4)
;;
;; Entrada: PointX, PointY
;; Saída:   A = valor do tile (TILE_*)
;; Destrói: A, Y, Temp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc GetMapTile
    lda PointY
    and #%11110000              ; A = linha * 16
    sta Temp
    lda PointX
    lsr
    lsr
    lsr
    lsr                         ; A = coluna (X / 16)
    ora Temp                    ; A = linha * 16 + coluna
    tay
    lda LevelMap,y              ; A = valor do tile nessa posição
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; O ponto (PointX, PointY) está sobre um tile do tipo ParamTile?
;;
;; Entrada: PointX, PointY, ParamTile
;; Saída:   C = 1 se o tile for igual a ParamTile, C = 0 se não for
;; Destrói: A, Y, Temp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc IsPointOnTile
    jsr GetMapTile
    cmp ParamTile               ; Se for igual, o cmp já deixa C = 1
    beq Done
    clc                         ; Diferente: garante C = 0 (o cmp pode ter deixado C = 1)
Done:
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Algum dos 4 cantos da hitbox (BoxX1..BoxX2, BoxY1..BoxY2) está sobre um
;; tile do tipo ParamTile?
;;
;; Testar só os cantos é suficiente porque os objetos têm o mesmo tamanho
;; dos metatiles do mapa (16x16): não existe tile que caiba "no meio" deles.
;; Os cantos são visitados em volta da caixa, assim cada passo só troca
;; uma coordenada do ponto.
;;
;; Entrada: BoxX1, BoxX2, BoxY1, BoxY2, ParamTile
;; Saída:   C = 1 se algum canto está sobre ParamTile
;; Destrói: A, Y, Temp, PointX, PointY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc CheckBoxOnTile
    lda BoxX1
    sta PointX
    lda BoxY1
    sta PointY
    jsr IsPointOnTile           ; Canto superior esquerdo
    bcs Done

    lda BoxX2
    sta PointX
    jsr IsPointOnTile           ; Canto superior direito
    bcs Done

    lda BoxY2
    sta PointY
    jsr IsPointOnTile           ; Canto inferior direito
    bcs Done

    lda BoxX1
    sta PointX
    jsr IsPointOnTile           ; Canto inferior esquerdo
Done:
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calcula BoxX2 e BoxY2 a partir de BoxX1 e BoxY1 (objeto de 16x16).
;;
;; Entrada: BoxX1, BoxY1
;; Saída:   BoxX2 = BoxX1 + HITBOX_SIZE, BoxY2 = BoxY1 + HITBOX_SIZE
;; Destrói: A
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc SetBoxSize
    lda BoxX1
    clc
    adc #HITBOX_SIZE
    sta BoxX2
    lda BoxY1
    clc
    adc #HITBOX_SIZE
    sta BoxY2
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Teste de colisão entre duas caixas (AABB), com limites inclusivos.
;;
;; As caixas NÃO se tocam se uma estiver totalmente de um lado da outra.
;; Cada "bcc" sai com C = 0 (sem colisão). Se passar pelos quatro testes,
;; o último cmp deixa C = 1 exatamente quando há sobreposição.
;;
;; Entrada: BoxX1..BoxY2 (caixa A) e RectX1..RectY2 (caixa B)
;; Saída:   C = 1 se as caixas se sobrepõem
;; Destrói: A (preserva X e Y)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc IsBoxColliding
    lda BoxX2                   ; A.x2 < B.x1 ?  (A está à esquerda de B)
    cmp RectX1
    bcc Done

    lda RectX2                  ; B.x2 < A.x1 ?  (A está à direita de B)
    cmp BoxX1
    bcc Done

    lda BoxY2                   ; A.y2 < B.y1 ?  (A está acima de B)
    cmp RectY1
    bcc Done

    lda RectY2                  ; B.y2 < A.y1 ?  (A está abaixo de B)
    cmp BoxY1                   ; C = 1 aqui significa que se sobrepõem
Done:
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; O ponto (PointX, PointY) está dentro da caixa BoxX1..BoxX2, BoxY1..BoxY2?
;; (Não usada no momento; fica disponível para projéteis, itens etc.)
;;
;; Entrada: PointX, PointY, BoxX1..BoxY2
;; Saída:   C = 1 se o ponto está dentro da caixa (limites inclusivos)
;; Destrói: A (preserva X e Y)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc IsPointInBox
    lda PointX                  ; ponto.x < x1 ?  -> fora
    cmp BoxX1
    bcc Done

    lda BoxX2                   ; x2 < ponto.x ?  -> fora
    cmp PointX
    bcc Done

    lda PointY                  ; ponto.y < y1 ?  -> fora
    cmp BoxY1
    bcc Done

    lda BoxY2                   ; y2 < ponto.y ?  -> fora (C = 1 = dentro)
    cmp PointY
Done:
    rts
.endproc
