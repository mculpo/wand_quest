.include "consts.inc"
.include "header.inc"
.include "actor.inc"
.include "state.inc"
.include "reset.inc"
.include "utils.inc"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables declared in RAM zero-page
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.include "variables.asm"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRG-ROM code located at $8000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "CODE"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FamiStudio audio engine configuration
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

.include "audioengine.asm"
.include "utils.asm"
.include "ppu_utils.asm"
.include "blocks.asm"
.include "player.asm"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reset handler (called when the NES resets or powers on)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Reset:
    INIT_NES                 ; Macro to initialize the NES to a known state

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;   G A M E   P L A Y   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc GamePlay

    lda #State::PLAYING
    sta GameState            ; GameState = PLAYING

    jsr LoadPalette
    ;;jsr LoadBackground
    ;;jsr LoadEnemys
    jsr LoadPlayer
    jsr LoadAllBlock

  InitVariables:
      lda #0
      sta Frame                ; Frame = 0
      sta Clock60              ; Clock60 = 0

      lda #$10
      sta Seed+1
      sta Seed               ; Initialize the Seed with any value different than zero

  EnableRendering:
      lda #%10010000           ; Enable NMI and set background to use the 2nd pattern table (at $1000)
      sta PPU_CTRL
      lda #0
      sta PPU_SCROLL           ; Disable scroll in X
      sta PPU_SCROLL           ; Disable scroll in Y
      lda #%00011110
      sta PPU_MASK             ; Set PPU_MASK bits to render the background

  GameLoop:
    lda Buttons
    sta PrevButtons          ; Stores the previously pressed buttons

    jsr ReadControllers      ; Read joypad and load button state
    jsr InputPlayer 
    
    lda #0
    sta PrevOAMCount

    jsr UpdateBlock

    jsr RenderPlayer
    jsr RenderBlocks

    WaitForVBlank:           ; We lock the execution of the game logic here
        lda IsDrawComplete     ; Here we check and only perform a game loop call once NMI is done drawing
        beq WaitForVBlank      ; Otherwise, we keep looping

        lda #0
        sta IsDrawComplete       ; Once we're done, we set the DrawComplete flag back to 0

        jmp GameLoop
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NMI interrupt handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
NMI:
        PUSH_REGS                                   ; Macro to save register values by pushing them to the stack

        inc Frame                                   ; Frame++
        inc Players+Player::t_anim                  ; Frame++

        ;jsr ApplySpriteFlicker                      ; Aplica o flickering para alternar os sprites


    OAMStartDMACopy:             ; DMA copy of OAM data from RAM to PPU
        lda #$02                 ; Every frame, we copy spite data starting at $02**
        sta PPU_OAM_DMA          ; The OAM-DMA copy starts when we write to $4014

    ;;jsr UpdatePlayerPositionOAM

        ;;;; ABAIXO AQUI COLOCAR A ATUALIZAÇÃO DAS OUTRAS SPRITES JÁ CARREGADAS
        ;;;; OS PRIMEIROS PONTEIROS SÃO PARA O PLAYER 1 E PLAYER 2 ( AQUI TMB PRETENDO COLOCAR ARRAY PARA NÃO PRECISAR, SETAR DIRETAMENTE)
        ;;;; OS DEMAIS SÃO PARA BLOCOS E INIMIGOS, QUE VÃO SER ARRAYS.

    RefreshRendering:
        lda #%10010000           ; Enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
        ora #0                   ; OR with CurrNametable (0 or 1) to set PPU_CTRL bit-0 (starting nametable)
        sta PPU_CTRL
        lda #%00011110           ; Enable sprites, enable background, no clipping on left side
        sta PPU_MASK

    SetGameClock:
        lda Frame                ; Increment Clock60 every time we reach 60 frames (NTSC = 60Hz)
        cmp #60                  ; Is Frame equal to #60?
        bne :+                   ; If not, bypass Clock60 increment
        inc Clock60              ; But if it is 60, then increment Clock60 and zero Frame counter
        lda #0
        sta Frame
    :

    setAnimationClock:
        lda Players+Player::t_anim
        cmp #30
        bne :+                                          ; Se  < 30, pula a atualização
            inc Players+Player::c_anim             ; Incrementa currentAnim
            lda #0
            sta Players+Player::t_anim             ; Reseta 
            lda Players+Player::c_anim
            cmp #2                                      ; Se currentAnim > 1 (ou seja, 2 ou mais)
            bcc :+                                      ; Se for menor que 2, mantém
                lda #0
                sta Players+Player::c_anim         ; Reseta para 0 se passar de 1    
    :

    SetDrawComplete:
        lda #1
        sta IsDrawComplete       ; Set the DrawComplete flag to indicate we are done drawing to the PPU

        PULL_REGS

        rti                      ; Return from interrupt


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; IRQ interrupt handler
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
IRQ:
    rti                      ; Return from interrupt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hardcoded list of color values in ROM to be loaded by the PPU
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PaletteData:
.incbin "palettes_1.dat"
BackgroundData:
.incbin "wq_nametable_0.nam"
;.include "nametable0.asm"

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ;; Here goes the encoded music data that was exported by FamiStudio
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MusicData:
; .include "music/titan.asm"
; .include "music/maritime.asm"

; SoundFXData:
; .include "sfx/sounds.asm"

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ;; Here we add the CHR-ROM data, included from an external .CHR file
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is the OAM sprite attribute data data we will use in our game.
;; We have only one big metasprite that is composed of 4 hardware sprites.
;; The OAM is organized in sets of 4 bytes per tile.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SpriteData:
;--------------------------------
; Mage: Sprite 1
;      Y   tile#  attributes   X
.byte $AE,  $00,  %00000001,  $98
.byte $AE,  $00,  %01000001,  $A0
.byte $B6,  $01,  %00000001,  $98
.byte $B6,  $01,  %01000001,  $A0
;--------------------------------
; Mage: Sprite 2
;      Y   tile#  attributes   X
.byte $AE,  $03,  %00000001,  $98
.byte $AE,  $03,  %01000001,  $A0
.byte $B6,  $02,  %00000001,  $98
.byte $B6,  $02,  %01000001,  $A0


; Sprite Attribute Byte:
;-----------------------
; 76543210
; |||   ||
; |||   ++- Color Palette of sprite. Choose which set of 4 from the 16 colors to use
; |||
; ||+------ Priority (0: in front of background; 1: behind background)
; |+------- Flip sprite horizontally
; +-------- Flip sprite vertically


;; Sprite Block Data Attributes
;; Y , X
SpriteBlockData:
.byte $64, $64  
.byte $64, $18  
.byte $18, $36  
.byte $18, $54  
.byte $18, $90  
.byte $00


.segment "CHARS1"
.incbin "wand_quest_spr.chr"
.incbin "wand_quest_bg.chr"    ; This is the 1nd bank of CHR-ROM tiles


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vectors with the addresses of the handlers that we always add at $FFFA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "VECTORS"
.word NMI                    ; Address (2 bytes) of the NMI handler
.word Reset                  ; Address (2 bytes) of the Reset handler
.word IRQ                    ; Address (2 bytes) of the IRQ handler
