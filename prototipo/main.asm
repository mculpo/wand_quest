;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AGENTE VERMELHO — protótipo de metroidvania para NES
;; (inspirado no Shatterhand; câmera livre nos dois eixos)
;;
;; Build: makefile build    Rodar: makefile run    (ver LEIAME.md)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.include "inc/consts.inc"
.include "inc/macros.inc"

;; Header iNES: NROM (mapper 0), 32 KB de PRG, 8 KB de CHR, mirroring VERTICAL
;; (os dois nametables lado a lado, para o scroll horizontal ser perfeito)
.segment "HEADER"
    .byte "NES", $1A
    .byte 2                     ; 2 x 16 KB de PRG
    .byte 1                     ; 1 x 8 KB de CHR
    .byte %00000001             ; Mapper 0, mirroring vertical
    .byte %00000000
    .byte 0, 0, 0, 0, 0, 0, 0, 0

.include "src/variables.asm"

.segment "CODE"
.include "src/data/metatiles.asm"
.include "src/data/level1.asm"
.include "src/data/sprites.asm"
.include "src/utils.asm"
.include "src/sprites.asm"
.include "src/level.asm"
.include "src/scroll.asm"
.include "src/player.asm"
.include "src/bullets.asm"
.include "src/fx.asm"
.include "src/enemies.asm"
.include "src/hud.asm"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reset
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Reset:
    INIT_NES
    MOV16I Seed, $5A17
    jsr LoadPalettes
    lda #PPU_CTRL_BASE          ; Liga a NMI (com RenderOn = 0 ela não mexe na PPU)
    sta PPU_CTRL
    jsr StartLevel

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loop principal: um frame de lógica, desenha os sprites e espera a NMI.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
MainLoop:
    jsr ReadControllers

    lda GameState
    cmp #GState::PLAY
    bne :+
        jsr UpdatePlaying
        jmp Render
    :
    cmp #GState::PAUSE
    bne :+
        lda Pressed             ; Start volta ao jogo
        and #BUTTON_START
        beq Render
        lda #GState::PLAY
        sta GameState
        jmp Render
    :
    ; DEAD ou CLEAR: os efeitos terminam e, no fim do tempo, a fase recomeça
    jsr UpdateFx
    dec StateTimer
    bne Render
    jsr StartLevel
    jmp MainLoop

Render:
    lda #0
    sta OamIndex
    jsr RenderHud               ; A barra de vida vem primeiro (prioridade no OAM)
    jsr RenderTexts
    lda Frame                   ; Alterna a ordem de desenho: com mais de 8 sprites
    and #1                      ; numa linha, os sprites piscam em vez de sumir
    beq :+
        jsr RenderPlayer
        jsr RenderEnemies
        jsr RenderBullets
        jsr RenderFx
        jmp :++
    :
        jsr RenderFx
        jsr RenderBullets
        jsr RenderEnemies
        jsr RenderPlayer
    :
    jsr HideUnusedSprites

    lda #1
    sta IsFrameReady            ; Frame pronto: a NMI pode mandar para a PPU
    lda #0
    sta NmiDone
    :
        lda NmiDone
        beq :-
    jmp MainLoop

;; Um frame de jogo
.proc UpdatePlaying
    lda Pressed
    and #BUTTON_START
    beq :+
        lda #GState::PAUSE
        sta GameState
        rts
    :
    jsr UpdatePlayer
    jsr UpdatePBullets
    jsr UpdateEBullets
    jsr SpawnEnemies
    jsr UpdateEnemies
    jsr CheckBulletHits
    jsr UpdateFx
    jsr UpdatePlayerAnim
    jmp UpdateCamera
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (Re)começa a fase: desliga a tela, zera tudo, desenha a tela em volta do
;; agente e liga de novo.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc StartLevel
    lda #0
    sta RenderOn                ; A NMI para de mexer na PPU
    sta PPU_MASK                ; Tela desligada: dá para escrever na VRAM à vontade
    jsr ClearNametables
    jsr ClearEnemies
    jsr ClearBullets
    jsr ClearFx
    jsr InitPlayer
    jsr CenterCamera
    jsr LoadFullScreen
    lda #0
    sta OamIndex
    jsr HideUnusedSprites
    lda #GState::PLAY
    sta GameState
    lda #0
    sta IsFrameReady
    jsr WaitNmi                 ; Liga no começo de um VBlank
    lda #1
    sta RenderOn
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NMI: coluna/linha nova do cenário, OAM, scroll. Tudo dentro do VBlank.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
NMI:
    PUSH_REGS
    inc Frame
    lda RenderOn
    beq Done

    lda IsFrameReady
    beq ApplyScroll             ; Frame atrasado: mantém o scroll anterior
        lda ColReady
        beq :+
            jsr FlushColumn
            lda #0
            sta ColReady
        :
        lda RowReady
        beq :+
            jsr FlushRow
            lda #0
            sta RowReady
        :
        lda #0
        sta OAM_ADDR
        lda #>OAM_BUFFER
        sta PPU_OAM_DMA
        lda ScrollX             ; O scroll deste frame (combina com o que foi escrito)
        sta NmiScrollX
        lda ScrollY
        sta NmiScrollY
        lda ScrollNt
        sta NmiScrollNt
        lda #0
        sta IsFrameReady

    ApplyScroll:
        lda #PPU_CTRL_BASE
        ora NmiScrollNt
        sta PPU_CTRL
        bit PPU_STATUS
        lda NmiScrollX
        sta PPU_SCROLL
        lda NmiScrollY
        sta PPU_SCROLL
        lda #PPU_MASK_ON
        sta PPU_MASK
    Done:
    lda #1
    sta NmiDone
    PULL_REGS
IRQ:
    rti

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CHR: pattern table 0 = sprites, pattern table 1 = cenário
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "CHARS"
    .incbin "spr.chr"
    .incbin "bg.chr"

.segment "VECTORS"
    .word NMI, Reset, IRQ
