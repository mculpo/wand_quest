.include "inc/consts.inc"
.include "inc/header.inc"
.include "inc/actor.inc"
.include "inc/state.inc"
.include "inc/reset.inc"
.include "inc/utils.inc"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variáveis (zero page e RAM)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.include "src/variables.asm"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Código na PRG-ROM, a partir de $8000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "CODE"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuração do engine de áudio FamiStudio
;; (incluído, mas ainda não inicializado nem chamado na NMI)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.define FAMISTUDIO_CA65_ZP_SEGMENT   ZEROPAGE
.define FAMISTUDIO_CA65_RAM_SEGMENT  RAM
.define FAMISTUDIO_CA65_CODE_SEGMENT CODE

FAMISTUDIO_CFG_EXTERNAL       = 1
FAMISTUDIO_CFG_DPCM_SUPPORT   = 1
FAMISTUDIO_CFG_SFX_SUPPORT    = 1
FAMISTUDIO_CFG_SFX_STREAMS    = 2
FAMISTUDIO_CFG_EQUALIZER      = 1
FAMISTUDIO_USE_VOLUME_TRACK   = 1
FAMISTUDIO_USE_PITCH_TRACK    = 1
FAMISTUDIO_USE_SLIDE_NOTES    = 1
FAMISTUDIO_USE_VIBRATO        = 1
FAMISTUDIO_USE_ARPEGGIO       = 1
FAMISTUDIO_CFG_SMOOTH_VIBRATO = 1
FAMISTUDIO_USE_RELEASE_NOTES  = 1
FAMISTUDIO_DPCM_OFF           = $E000

.include "src/audioengine.asm"
.include "src/utils.asm"
.include "src/bg_collision_map.asm"
.include "src/blocks.asm"
.include "src/player.asm"
.include "src/ball.asm"
.include "src/enemies.asm"
.include "src/effects.asm"
.include "src/level.asm"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reset: executado quando a NES liga ou é resetada
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Reset:
    INIT_NES                 ; Coloca a NES em um estado conhecido (ver inc/reset.inc)
                             ; e continua direto no GamePlay abaixo

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;   G A M E   P L A Y   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc GamePlay

    ;; A renderização ainda está desligada: dá para escrever na PPU à vontade
    jsr LoadPalette
    jsr LoadBackground       ; Só a moldura: a área de jogo começa vazia
    jsr LoadPlayer

  InitVariables:
      lda #0
      sta Frame                ; Frame = 0
      sta Clock60              ; Clock60 = 0
      sta VramIndex
      sta VramBuffer           ; Fila de VRAM vazia

      lda #$10
      sta Seed+1
      sta Seed                 ; A semente do aleatório pode ser qualquer valor diferente de zero

      lda #0                   ; Começa mostrando "FASE 01" e montando a fase 1
      jsr StartTransitionAtText

  EnableRendering:
      lda #PPU_CTRL_GAME       ; Liga a NMI; fundo na pattern table $1000, sprites na $0000
      sta PPU_CTRL
      lda #0
      sta PPU_SCROLL           ; Scroll X = 0
      sta PPU_SCROLL           ; Scroll Y = 0
      lda #PPU_MASK_GAME       ; PPU_MASK (%00011110):
                 ; Bit 7: 0 - Intensifica o azul     (não usado)
                 ; Bit 6: 0 - Intensifica o verde    (não usado)
                 ; Bit 5: 0 - Intensifica o vermelho (não usado)
                 ; Bit 4: 1 - Mostra os sprites
                 ; Bit 3: 1 - Mostra o background
                 ; Bit 2: 1 - Mostra os sprites na borda esquerda de 8 pixels (sem recorte)
                 ; Bit 1: 1 - Mostra o background na borda esquerda de 8 pixels (sem recorte)
                 ; Bit 0: 0 - Desativa o modo em tons de cinza (usa a paleta colorida normal)
      sta PPU_MASK

  ;; Loop principal: roda uma vez por frame. Faz toda a lógica, monta o
  ;; buffer de OAM e depois espera a NMI mandar o frame para a PPU.
  GameLoop:
    lda Buttons
    sta PrevButtons          ; Guarda os botões do frame anterior (para detectar "apertou agora")

    jsr ReadControllers      ; Lê o controle em Buttons

    lda #0
    sta IsOamReady           ; Os buffers vão ser reescritos: a NMI não pode copiá-los agora
    sta IsVramReady

    ;; Lógica do frame conforme o estado do jogo
    lda GameState
    cmp #State::PLAYING
    bne :+
        jsr UpdatePlaying
        jmp Render
    :
    cmp #State::LEVEL_CLEAR
    bne :+
        jsr UpdateLevelClear
        jmp Render
    :
    cmp #State::PLAYER_DYING
    bne :+
        jsr UpdatePlayerDeath
        jmp Render
    :
    cmp #State::TRANSITION
    bne Render
        jsr UpdateTransition

  Render:
    jsr UpdatePlayerAnimation

    lda #0
    sta OamIndex             ; Começa a desenhar do primeiro sprite
    jsr RenderBall           ; A bola vai primeiro: fica sempre por cima
    lda Frame                ; Alterna quem vai primeiro no OAM (quem vem antes tem
    and #1                   ; prioridade no limite de 8 sprites por linha)
    beq :+
        jsr RenderPlayer
        jsr RenderEnemies
        jsr RenderBlocks
        jmp :++
    :
        jsr RenderBlocks
        jsr RenderEnemies
        jsr RenderPlayer
    :
    jsr HideUnusedSprites    ; Esconde o que sobrou do frame anterior

    lda #1
    sta IsOamReady           ; Buffers completos: podem ser enviados para a PPU
    sta IsVramReady

    WaitForVBlank:              ; Trava a lógica aqui até a NMI terminar o frame
        lda IsDrawComplete
        beq WaitForVBlank

        lda #0
        sta IsDrawComplete      ; Consome o aviso da NMI

        jmp GameLoop
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Um frame do estado PLAYING: Select (reiniciar/pular), player e blocos.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc UpdatePlaying
    jsr HandleSelect
    lda GameState
    cmp #State::PLAYING
    bne Done                 ; O Select começou uma transição
    jsr InputPlayer          ; Bola de pedra e movimento do player
    jsr UpdateBall           ; A bola voa: empurra o bloco ou mata o inimigo que acertar
    jsr UpdateEnemies        ; Inimigos andam pelo mapa
    jsr UpdateBlock          ; Blocos que estão deslizando (pode completar a fase)
    lda GameState
    cmp #State::PLAYING
    bne Done                 ; A fase acabou de ser completada
    jsr CheckPlayerVsEnemies ; Encostou num inimigo: morre
Done:
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NMI: chamada pela PPU no início de cada VBlank (60x por segundo em NTSC).
;; Só mexe com a PPU (OAM, PPU_CTRL/MASK, scroll) e com os contadores de
;; tempo; a lógica do jogo fica no GameLoop.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
NMI:
        PUSH_REGS                ; Salva A, X, Y e flags (a NMI pode interromper qualquer código)

        inc Frame                ; Frame++

    VramUpdate:                  ; Escritas no nametable enfileiradas pelo jogo (efeitos)
        lda IsVramReady          ; Só se o loop terminou de montar a fila
        beq :+
        jsr FlushVramBuffer
        lda #0
        sta VramIndex            ; Fila vazia de novo
        sta VramBuffer
        sta IsVramReady
    :

    OAMStartDMACopy:             ; Cópia via DMA do buffer de OAM (RAM) para a PPU
        lda IsOamReady           ; Se o loop não terminou o frame a tempo (lag frame),
        beq :+                   ; pula o DMA e mantém os sprites do frame anterior
        lda #0
        sta OAM_ADDR             ; O DMA começa no endereço atual do OAM: zera para começar do sprite 0
        lda #>OAM_BUFFER         ; Copia a página $02xx inteira (256 bytes)
        sta PPU_OAM_DMA          ; Escrever em $4014 dispara o DMA
        lda #0
        sta IsOamReady
    :

    RefreshRendering:
        lda #PPU_CTRL_GAME       ; NMI ligada, sprites na pattern table 0, fundo na 1
        sta PPU_CTRL
        lda #PPU_MASK_GAME       ; Fundo e sprites ligados, sem recorte à esquerda
        sta PPU_MASK
        lda #0
        sta PPU_SCROLL           ; Reescreve o scroll todo frame: qualquer escrita futura em
        sta PPU_SCROLL           ; $2006 durante o VBlank bagunçaria a posição do scroll

    SetGameClock:
        lda Frame                ; A cada 60 frames (1 segundo em NTSC) incrementa o Clock60
        cmp #60
        bne :+
        inc Clock60
        lda #0
        sta Frame                ; E recomeça a contagem de frames
    :

    SetDrawComplete:
        lda #1
        sta IsDrawComplete       ; Avisa o GameLoop que o frame foi enviado

        PULL_REGS

        rti                      ; Retorna da interrupção

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IRQ: não usamos (fica desligada no INIT_NES)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
IRQ:
    rti

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dados do cenário: paleta (32 cores), nametable (só a moldura) e as fases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PaletteData:
.incbin "palettes_1.dat"
; BackgroundData:
; .incbin "wq_nametable_0.nam"
.include "BackgroundData.asm"
.include "src/levels.asm"

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ;; Músicas e efeitos exportados pelo FamiStudio (desativados por enquanto)
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MusicData:
; .include "music/titan.asm"
; .include "music/maritime.asm"

; SoundFXData:
; .include "sfx/sounds.asm"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tabelas de direção, indexadas por Side (NONE, UP, DOWN, RIGHT, LEFT).
;; $FF = -1 e $FC = -4: somar esses valores em 8 bits equivale a subtrair.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               NONE  UP    DOWN  RIGHT LEFT
SideDeltaX:  .byte  0,    0,    0,    1,   $FF      ; Deslocamento em X ao andar 1 pixel
SideDeltaY:  .byte  0,  $FF,    1,    0,     0      ; Deslocamento em Y ao andar 1 pixel


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Metasprites (lidos pelo DrawMetasprite). 4 bytes por sprite 8x8:
;;   dy, tile, atributo, dx     -> dy/dx relativos ao canto superior esquerdo
;; e METASPRITE_END no final. O byte de atributo está descrito em utils.asm.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Mago e inimigos (gerados por tools/draw_sprites.py)
.include "src/player_anim.asm"
.include "src/enemy_anim.asm"

;; Bloco: a paleta vem de BlockAttr (MetaAttr), por isso os bits 0-1 são 0
MetaBlock:
;;     dy  tile  atributo   dx
.byte  0,  $06,  %00000000,  0
.byte  0,  $07,  %00000000,  8
.byte  8,  $08,  %01000000,  0      ; Canto inferior esquerdo: tile $08 espelhado
.byte  8,  $08,  %00000000,  8
.byte  METASPRITE_END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CHR-ROM (8KB): pattern table 0 = sprites, pattern table 1 = fundo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "CHARS1"
.incbin "wand_quest_spr.chr"      ; 4KB em $0000
.incbin "wand_quest_bg.chr"       ; 4KB em $1000

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vetores de interrupção: ficam sempre em $FFFA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "VECTORS"
.word NMI                    ; $FFFA: endereço da rotina de NMI
.word Reset                  ; $FFFC: endereço da rotina de Reset
.word IRQ                    ; $FFFE: endereço da rotina de IRQ
