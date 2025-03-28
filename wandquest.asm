.include "consts.inc"
.include "header.inc"
.include "actor.inc"
.include "state.inc"
.include "reset.inc"
.include "utils.inc"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables declared in RAM zero-page
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "ZEROPAGE"

Players:             .res 2 * .sizeof(Player)
blocks:              .res 5 * .sizeof(Block)

Buttons:            .res 1       ; Pressed buttons (A|B|Sel|Start|Up|Dwn|Lft|Rgt)
PrevButtons:        .res 1       ; Stores the previous buttons from the last frame

Frame:              .res 1       ; Counts frames (0 to 255 and repeats)
IsDrawComplete:     .res 1       ; Flag to indicate when VBlank is done drawing
Clock60:            .res 1       ; Counter that increments per second (60 frames)

BgPtr:              .res 2       ; Pointer to background address - 16bits (lo,hi)
SprPtr:             .res 2       ; Pointer to the sprite address - 16bits (lo,hi)
BufPtr:             .res 2       ; Pointer to the buffer address - 16bits (lo,hi)
PalPtr:             .res 2       ; Pointer to the palette address - 16bits (lo,hi)

PrevOAMCount:       .res 1       ; Store the previous number of bytes that were sent to the OAM

Seed:                                       .res 2       ; Initialize 16-bit seed to any value except 0
GameState:                                  .res 1       ; Keep track of game state

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to load all 32 color palette values from ROM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc LoadPalette
    PPU_SETADDR $3F00
    ldy #0                   ; Y = 0
  :   lda PaletteData,y        ; Lookup byte in ROM
      sta PPU_DATA             ; Set value to send to PPU_DATA
      iny                      ; Y++
      cpy #32                  ; Is Y equal to 32?
      bne :-                   ; Not yet, keep looping
      rts                      ; Return from subroutine
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to load tiles and attributes into the first nametable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc LoadBackground
    lda #<BackgroundData     ; Fetch the lo-byte of BackgroundData address
    sta BgPtr
    lda #>BackgroundData     ; Fetch the hi-byte of BackgroundData address
    sta BgPtr+1

    PPU_SETADDR $2000

    ldx #$00                 ; X = 0 --> x is the outer loop index (hi-byte) from $0 to $4
    ldy #$00                 ; Y = 0 --> y is the inner loop index (lo-byte) from $0 to $FF

    OuterLoop:
    InnerLoop:
        lda (BgPtr),y            ; Fetch the value *pointed* by BgPtr + Y
        sta PPU_DATA             ; Store in the PPU data
        iny                      ; Y++
        cpy #0                   ; If Y == 0 (wrapped around 256 times)?
        beq IncreaseHiByte       ;   Then: we need to increase the hi-byte
        jmp InnerLoop            ;   Else: Continue with the inner loop
    IncreaseHiByte:
    inc BgPtr+1              ; We increment the hi-byte pointer to point to the next background section (next 255-chunk)
    inx                      ; X++
    cpx #4                   ; Compare X with #4
    bne OuterLoop            ;   If X is still not 4, then we keep looping back to the outer loop

    rts                      ; Return from subroutine
.endproc


.proc LoadBlock
    lda #$02
    sta SprPtr+1
    lda #$00
    sta SprPtr                         ; Point SprPtr to $0200

    ldx #0
    LoopSprite:
        lda SpriteBlockData, x       ; We fetch bytes from the  lookup table
        ldy PrevOAMCount
        sta (SprPtr), y      ; We store the bytes starting at OAM address $0200
        inc PrevOAMCount
        inx                          ; X++
        cpx #80
        bne LoopSprite               ; Loop 16 times (4 hardware sprites, 4 bytes each)

.endproc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to load all 16 bytes into OAM-RAM starting at $0200
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc LoadSprites
    lda #$02
    sta SprPtr+1
    lda #$00
    sta SprPtr                         ; Point SprPtr to $0200

    lda #0               ; Define o índice inicial como 0
    LoopSprite:
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;
        ;; Atualiza o Y  attributes
        ;; Apenas incremento para pular a inserção do valor Y
        ;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        inx
        inc PrevOAMCount
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;
        ;; Atualiza o Tiletile#  attributes
        ;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        lda SpriteData, x
        ldy PrevOAMCount
        sta (SprPtr), y      ; We store the bytes starting at OAM address $0200
        inx
        inc PrevOAMCount

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;
        ;; Atualiza o attributes
        ;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        lda SpriteData, x
        ldy PrevOAMCount
        sta (SprPtr), y      ; We store the bytes starting at OAM address $0200
        inx
        inc PrevOAMCount

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;
        ;; Atualiza o X  attributes
        ;; Apenas incremento para pular a inserção do valor X
        ;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        inx
        inc PrevOAMCount

        ldy PrevOAMCount
        cpy #16              ; 16 bytes carregados? (4 sprites de 4 bytes)
        bne LoopSprite       ; Se não, continua carregando

        rts
.endproc

.proc UpdateAnimationSprites
    lda #$02
    sta SprPtr+1
    lda #$00
    sta SprPtr                         ; Point SprPtr to $0200

    ;; Apenas para teste.
    lda Players+Player::c_anim
    beq init_sprite      ; Se currentAnim == 0, começa do primeiro MetaSprite

    lda #16              ; Se currentAnim == 1, começa do segundo MetaSprite (pula 16 bytes)
    jmp StartLoop

    init_sprite:
        lda #0               ; Define o índice inicial como 0

    StartLoop:
        tax

    LoopSprite:
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;
        ;; Atualiza o Y  attributes
        ;; Apenas incremento para pular a inserção do valor Y
        ;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        inx
        inc PrevOAMCount
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;
        ;; Atualiza o Tiletile#  attributes
        ;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        lda SpriteData, x
        ldy PrevOAMCount
        sta (SprPtr), y      ; We store the bytes starting at OAM address $0200
        inx
        inc PrevOAMCount

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;
        ;; Atualiza o attributes
        ;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        lda SpriteData, x
        ldy PrevOAMCount
        sta (SprPtr), y      ; We store the bytes starting at OAM address $0200
        inx
        inc PrevOAMCount

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;
        ;; Atualiza o X  attributes
        ;; Apenas incremento para pular a inserção do valor X
        ;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        inx
        inc PrevOAMCount

        ldy PrevOAMCount
        cpy #16              ; 16 bytes carregados? (4 sprites de 4 bytes)
        bne LoopSprite       ; Se não, continua carregando

        rts
.endproc

.proc ApplySpriteFlicker
    lda Frame
    and #$01        ; Alterna a cada frame ímpar
    beq NoFlicker   ; Se for par, não faz nada

    ldx #0
    HideSprites:
        lda $0200,X      ; Pega a posição Y do sprite X na OAM
        ora #$F8         ; Move o sprite para fora da tela
        sta $0200,X      ; Salva de volta na OAM

        inx
        inx
        inx
        inx             ; Pula 4 bytes para o próximo sprite
        cpx #32         ; No máximo 8 sprites escondidos
        bne HideSprites

    NoFlicker:
        rts
.endproc

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

    lda #0
    sta Players+Player::t_anim
    sta Players+Player::c_anim
    sta PrevOAMCount

    ldx #0
    lda SpriteData,x
    sta Players+Player::y_pos
    inx
    inx
    inx
    lda SpriteData,x
    sta Players+Player::x_pos
    
    jsr LoadPalette              ; Call LoadPalette subroutine to load 32 colors into our palette
    jsr LoadBackground         ; Call LoadBackground subroutine to load a full nametable of tiles and attributes
    ;;jsr LoadEnemys
    jsr LoadSprites              ; Call LoadSprites subroutine to load all sprites into OAM-RAM
    jsr LoadBlock                ; Load All Blocks Fase

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

  CheckRightButton:
        lda Buttons
        and #BUTTON_RIGHT                                   ; Perform a bitwise AND with the accumulator
        beq CheckLeftButton                                 ; If the right button is not pressed, we skip to test the left button
            inc Players+Player::x_pos                  ; X++, which is only performed if right button is being pressed
            lda #SIDE_RIGHT
            sta Players+Player::side
  CheckLeftButton:
        lda Buttons
        and #BUTTON_LEFT                                  ; Perform a bitwise AND with the accumulator
        beq CheckDownButton                               ; If the left button is not pressed, we skip to test the down button
            dec Players+Player::x_pos                ; X--, which is only performed if left button is being pressed
            lda #SIDE_LEFT
            sta Players+Player::side
  CheckDownButton:
        lda Buttons
        and #BUTTON_DOWN                                  ; Perform a bitwise AND with the accumulator
        beq CheckUpButton                                 ; If the down button is not pressed, we skip to test the up button
            inc Players+Player::y_pos                ; Y++, which is only performed if down button is being pressed
            lda #SIDE_DOWN
            sta Players+Player::side
  CheckUpButton:
        lda Buttons
        and #BUTTON_UP                                    ; Perform a bitwise AND with the accumulator
        beq :+                                            ; If the up button is not pressed, we skip to the end of our button check
            dec Players+Player::y_pos                ; Y--, which is only performed if up button is being pressed
            lda #SIDE_UP
            sta Players+Player::side
  :

  lda #0
  sta PrevOAMCount

  jsr UpdateAnimationSprites

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

    UpdateSpritePosition:
        ;; Atualizando o player diretamente na memoria.
        lda Players+Player::x_pos
        sta $0203                ; Set the 1st sprite X position to be XPos
        sta $020B                ; Set the 3rd sprite X position to be XPos
        clc
        adc #8
        sta $0207                ; Set the 2nd sprite X position to be XPos + 8
        sta $020F                ; Set the 4th sprite X position to be XPos + 8

        lda Players+Player::y_pos
        sta $0200                ; Set the 1st sprite Y position to be YPos
        sta $0204                ; Set the 2nd sprite Y position to be YPos
        clc
        adc #8
        sta $0208                ; Set the 3rd sprite Y position to be YPos + 8
        sta $020C                ; Set the 4th sprite Y position to be YPos + 8

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
.byte $AE,  $00,  %00000000,  $98
.byte $AE,  $00,  %01000000,  $A0
.byte $B6,  $01,  %00000000,  $98
.byte $B6,  $01,  %01000000,  $A0
;--------------------------------
; Mage: Sprite 2
;      Y   tile#  attributes   X
.byte $AE,  $03,  %00000011,  $98
.byte $AE,  $03,  %01000011,  $A0
.byte $B6,  $02,  %00000011,  $98
.byte $B6,  $02,  %01000011,  $A0


; Sprite Attribute Byte:
;-----------------------
; 76543210
; |||   ||
; |||   ++- Color Palette of sprite. Choose which set of 4 from the 16 colors to use
; |||
; ||+------ Priority (0: in front of background; 1: behind background)
; |+------- Flip sprite horizontally
; +-------- Flip sprite vertically

SpriteBlockData:
.byte $64,  $04,  %00000010,  $64  ; Sprite 1 -> X=100, Y=100
.byte $64,  $04,  %01000010,  $6C  ; Sprite 2 -> X=108, Y=100
.byte $6C,  $04,  %10000010,  $64  ; Sprite 3 -> X=100, Y=108
.byte $6C,  $04,  %11000010,  $6C  ; Sprite 4 -> X=108, Y=108

; Meta Sprite 1 (X base = 24)
.byte $64,  $04,  %00000010,  $18  ; Sprite 1 -> Y=24, X=24
.byte $64,  $04,  %01000010,  $20  ; Sprite 2 -> Y=24, X=32
.byte $6C,  $04,  %10000010,  $18  ; Sprite 3 -> Y=32, X=24
.byte $6C,  $04,  %11000010,  $20  ; Sprite 4 -> Y=32, X=32

; Meta Sprite 2 (X base = 54) (+30px)
.byte $18,  $04,  %00000010,  $36  ; Sprite 1 -> Y=24, X=54
.byte $20,  $04,  %10000010,  $36  ; Sprite 3 -> Y=32, X=54
.byte $18,  $04,  %01000010,  $3E  ; Sprite 2 -> Y=24, X=62
.byte $20,  $04,  %11000010,  $3E  ; Sprite 4 -> Y=32, X=62

; Meta Sprite 3 (X base = 84) (+30px)
.byte $18,  $04,  %00000010,  $54  ; Sprite 1 -> Y=24, X=84
.byte $20,  $04,  %10000010,  $54  ; Sprite 3 -> Y=32, X=84
.byte $18,  $04,  %01000010,  $5C  ; Sprite 2 -> Y=24, X=92
.byte $20,  $04,  %11000010,  $5C  ; Sprite 4 -> Y=32, X=92

; Meta Sprite 5 (X base = 144) (+30px)
.byte $18,  $04,  %00000010,  $90  ; Sprite 1 -> Y=24, X=144
.byte $20,  $04,  %10000010,  $90  ; Sprite 3 -> Y=32, X=144
.byte $18,  $04,  %01000010,  $98  ; Sprite 2 -> Y=24, X=152
.byte $20,  $04,  %11000010,  $98  ; Sprite 4 -> Y=32, X=152



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
