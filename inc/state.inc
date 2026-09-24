;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Estados possíveis do jogo (guardado em GameState)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.enum State
    TITLESCREEN             ; Tela de título (ainda não usada)
    PLAYING                 ; Jogando: controle, blocos e checagem de vitória
    GAMEOVER                ; Fim de jogo (ainda não usado)
    PAUSED                  ; Pausado (ainda não usado)
    LEVEL_CLEAR             ; Fase completa: pausa curta antes da transição
    TRANSITION              ; Efeito de troca de fase (src/effects.asm)
    PLAYER_DYING            ; O mago encostou num inimigo: animação de morte e reinicia
.endenum

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Etapas da transição entre fases (guardado em TransitionPhase)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.enum TransPhase
    DISSOLVE_OUT            ; Desmancha a fase atual
    SHOW_TEXT               ; Mostra "FASE 00" (ou "PARABENS!" e espera o Start)
    DISSOLVE_IN             ; Monta a próxima fase
.endenum
