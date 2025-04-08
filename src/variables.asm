.segment "ZEROPAGE"
Buttons:                                .res 1       ; Pressed buttons
PrevButtons:                            .res 1       ; Buttons pressed in the previous frame

Frame:                                  .res 1       ; Frame counter
IsDrawComplete:                         .res 1       ; Flag to indicate end of screen drawing
Clock60:                                .res 1       ; Frame counter (60 per second)

BgPtr:                                  .res 2       ; Pointer to background address (16-bit)
SprPtr:                                 .res 2       ; Pointer to sprite address (16-bit)
BufPtr:                                 .res 2       ; Pointer to buffer address (16-bit)
PalPtr:                                 .res 2       ; Pointer to palette address (16-bit)

Collision:                              .res 1       ; Flag if a collision happened or not

ParamXPos:                              .res 1
ParamX2Pos:                             .res 1
ParamYPos:                              .res 1
ParamY2Pos:                             .res 1
ParamRectX1:                            .res 1
ParamRectX2:                            .res 1
ParamRectY1:                            .res 1
ParamRectY2:                            .res 1

ParamCurrentNumActor:                   .res 1
ParamScreen:                            .res 1

.segment "RAM"
; Variables
blocks:                                 .res MAX_BLOCKS * .sizeof(Block)
Players:                                .res 2 * .sizeof(Player)

PrevOAMCount:                           .res 1
Seed:                                   .res 2       ; 16-bit seed for any non-zero value
GameState:                              .res 1       ; Game state

