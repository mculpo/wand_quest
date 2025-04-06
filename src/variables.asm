.segment "ZEROPAGE"

Buttons:                                .res 1       ; Botões pressionados
PrevButtons:                            .res 1       ; Botões pressionados no quadro anterior

Frame:                                  .res 1       ; Contador de quadros
IsDrawComplete:                         .res 1       ; Flag para indicar o fim do desenho da tela
Clock60:                                .res 1       ; Contador de frames (60 por segundo)

BgPtr:                                  .res 2       ; Ponteiro para o endereço de fundo (16-bits)
SprPtr:                                 .res 2       ; Ponteiro para o endereço de sprites (16-bits)
BufPtr:                                 .res 2       ; Ponteiro para o buffer (16-bits)
PalPtr:                                 .res 2       ; Ponteiro para a paleta (16-bits)

Collision:                              .res 1       ; Flag if a collision happened or not

ParamXPos:                              .res 1
ParamX2Pos:                             .res 1
ParamYPos:                              .res 1
ParamY2Pos:                             .res 1

ParamRectX1:                            .res 1
ParamRectX2:                            .res 1
ParamRectY1:                            .res 1
ParamRectY2:                            .res 1

ParamNumActor:                          .res 1
ParamCurrentNumActor:                   .res 1
ParamData:                              .res 1
ParamScreen:                            .res 1

.segment "RAM"
; Variáveis
blocks:                                 .res MAX_BLOCKS * .sizeof(Block)
Players:                                .res 2 * .sizeof(Player)

PrevOAMCount:                           .res 1
Seed:                                   .res 2       ; Semente 16-bits para qualquer valor não nulo
GameState:                              .res 1       ; Estado do jogo
