;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Zero page: o que é usado o tempo todo (acesso mais rápido)
;; Valores de 16 bits: byte baixo primeiro (Nome = baixo, Nome+1 = alto).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "ZEROPAGE"
Buttons:        .res 1          ; Botões apertados agora
PrevButtons:    .res 1          ; Botões no frame anterior
Pressed:        .res 1          ; Botões que acabaram de ser apertados (borda)
Frame:          .res 1          ; Contador de frames
IsFrameReady:   .res 1          ; 1 = o loop terminou o frame (a NMI pode mandar para a PPU)
NmiDone:        .res 1          ; A NMI coloca 1 quando termina
RenderOn:       .res 1          ; 0 = tela desligada (carregando), a NMI não liga a renderização
GameState:      .res 1          ; GState
StateTimer:     .res 1

Temp:           .res 1          ; Temporários de uso rápido
Temp2:          .res 1
Temp3:          .res 1
Ptr:            .res 2          ; Ponteiro de uso geral (mapa)
TopTbl:         .res 2          ; BuildColumn/BuildRow: tabela de tiles de cima (MetaTL/MetaTR)
BotTbl:         .res 2          ; BuildColumn/BuildRow: tabela de tiles de baixo (MetaBL/MetaBR)
Seed:           .res 2          ; Semente do gerador aleatório

;; Câmera e scroll
CamX:           .res 2          ; Canto superior esquerdo da tela no mundo (pixels)
CamY:           .res 2
CamTileX:       .res 1          ; CamX / 8 (coluna de tiles na borda esquerda)
CamTileY:       .res 1          ; CamY / 8
ScrollX:        .res 1          ; Valores prontos para a NMI escrever na PPU
ScrollY:        .res 1          ; (CamY mod 240: o nametable tem 30 linhas de tiles)
ScrollNt:       .res 1          ; Bit do nametable da esquerda/direita (CamX / 256)
NmiScrollX:     .res 1          ; Scroll que a NMI aplica (copiado quando o frame fica pronto)
NmiScrollY:     .res 1
NmiScrollNt:    .res 1
ColReady:       .res 1          ; 1 = ColBuf pronta para a NMI
ColAddrHi:      .res 1          ; Endereço da coluna no nametable (linha 0)
ColAddrLo:      .res 1
ColAttrHi:      .res 1          ; $23 ou $27 (tabela de atributos do nametable da coluna)
ColAttrLo:      .res 1          ; $C0 + coluna de atributos
ColAttrIndex:   .res 1          ; Posição na AttrShadow da coluna de atributos (linha 0)
RowReady:       .res 1          ; 1 = RowBuf pronta para a NMI
RowSeg1Hi:      .res 1          ; A linha tem 33 tiles: um pedaço em cada nametable
RowSeg1Lo:      .res 1
RowSeg1Len:     .res 1
RowSeg2Hi:      .res 1
RowSeg2Lo:      .res 1
RowSeg2Len:     .res 1
RowAttrOff:     .res 1          ; (linha de atributos) * 8
RowTarget:      .res 1          ; UpdateCamera: linha de tiles a carregar ($FF = nenhuma)
ColTarget:      .res 1          ; UpdateCamera: coluna de tiles a carregar ($FF = nenhuma)
BuildTC:        .res 1          ; BuildColumn/BuildRow: coluna de tiles atual
BuildTR:        .res 1          ; BuildColumn/BuildRow: linha de tiles atual
BuildQ:         .res 1          ; BuildColumn: linha no nametable; BuildRow: posição no RowBuf
BuildCount:     .res 1          ; Quantos tiles ainda faltam
BuildLr:        .res 1          ; Linha de metatiles no nametable (0-14)
AttrIdx:        .res 1          ; SetAttrQuad: posição na AttrShadow
AttrQuad:       .res 1          ; SetAttrQuad: quadrante (0-3) dentro do byte de atributo

;; Consultas ao mapa
PointX:         .res 2          ; Ponto em pixels do mundo
PointY:         .res 2

;; Parâmetros para criar tiros e efeitos
SpawnX:         .res 2          ; Posição no mundo
SpawnY:         .res 2
SpawnVX:        .res 2          ; Velocidade 8.8 (tiros dos inimigos)
SpawnVY:        .res 2

;; Caixas para colisão entre objetos (16 bits, limites inclusivos)
AX1:            .res 2
AX2:            .res 2
AY1:            .res 2
AY2:            .res 2
BX1:            .res 2
BX2:            .res 2
BY1:            .res 2
BY2:            .res 2

;; Agente
PX:             .res 2          ; Canto superior esquerdo da caixa 16x24 (pixels do mundo)
PY:             .res 2
PXSub:          .res 1          ; Fração de pixel
PYSub:          .res 1
PVX:            .res 2          ; Velocidade 8.8 com sinal
PVY:            .res 2
PFacing:        .res 1          ; 0 = direita, 1 = esquerda
POnGround:      .res 1
PState:         .res 1          ; PState
PTimer:         .res 1          ; Tempo do estado atual (rolamento, recuo, morte)
PInvuln:        .res 1          ; Frames de invencibilidade (pisca)
PHealth:            .res 1          ; Vida (0 a PLAYER_MAX_HP)
PShootCD:       .res 1          ; Espera até o próximo tiro
PShootPose:     .res 1          ; Frames mostrando a pose de tiro
PShootUp:       .res 1          ; 1 = o último tiro foi para cima
PBoxTop:        .res 1          ; PBOX_Y1 ou PBOX_Y1_LOW
OldFeet:        .res 2          ; Y dos pés antes de mover (passarelas)
AnimPtr:        .res 2          ; Tabela de quadros da animação atual
AnimFrame:      .res 1
AnimTimer:      .res 1
AnimCount:      .res 1          ; Quadros da animação atual

;; Desenho de sprites
MetaPtr:        .res 2          ; Metasprite a desenhar
MetaWX:         .res 2          ; Posição no mundo
MetaWY:         .res 2
BaseSX:         .res 2          ; Posição na tela (pode ser negativa ou passar da tela)
BaseSY:         .res 2
SprX:           .res 2          ; DrawMetaWorld: X de um sprite na tela
OamIndex:       .res 1          ; Próxima posição livre no OAM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RAM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "RAM"
AttrShadow:     .res 128        ; Cópia das 2 tabelas de atributos (64 bytes cada)
ColBuf:         .res 30         ; Coluna de tiles na ordem das linhas do nametable (0-29)
RowBuf:         .res 33         ; Linha de tiles da esquerda para a direita

;; Tiros do agente (Structure of Arrays, índice em X)
PBActive:       .res MAX_PBULLETS
PBXL:           .res MAX_PBULLETS
PBXH:           .res MAX_PBULLETS
PBYL:           .res MAX_PBULLETS
PBYH:           .res MAX_PBULLETS
PBVX:           .res MAX_PBULLETS   ; Pixels por frame com sinal
PBVY:           .res MAX_PBULLETS

;; Tiros dos inimigos (velocidade 8.8, com fração de posição)
EBActive:       .res MAX_EBULLETS
EBXL:           .res MAX_EBULLETS
EBXH:           .res MAX_EBULLETS
EBYL:           .res MAX_EBULLETS
EBYH:           .res MAX_EBULLETS
EBXS:           .res MAX_EBULLETS
EBYS:           .res MAX_EBULLETS
EBVXL:          .res MAX_EBULLETS
EBVXH:          .res MAX_EBULLETS
EBVYL:          .res MAX_EBULLETS
EBVYH:          .res MAX_EBULLETS

;; Inimigos ativos
EType:          .res MAX_ENEMIES    ; EnemyType (NONE = slot livre)
EXL:            .res MAX_ENEMIES    ; Posição (canto superior esquerdo)
EXH:            .res MAX_ENEMIES
EYL:            .res MAX_ENEMIES
EYH:            .res MAX_ENEMIES
EXS:            .res MAX_ENEMIES    ; Frações (drone e soldado)
EYS:            .res MAX_ENEMIES
EVXL:           .res MAX_ENEMIES    ; Velocidade 8.8 (drone)
EVXH:           .res MAX_ENEMIES
EVYL:           .res MAX_ENEMIES
EVYH:           .res MAX_ENEMIES
EDir:           .res MAX_ENEMIES    ; 0 = direita, 1 = esquerda
EHP:            .res MAX_ENEMIES
ETimer:         .res MAX_ENEMIES    ; Espera até o próximo tiro
EPose:          .res MAX_ENEMIES    ; Frames mostrando a pose de tiro
EFlash:         .res MAX_ENEMIES    ; Frames piscando depois de levar tiro
EFrame:         .res MAX_ENEMIES    ; Quadro da animação
EAnimT:         .res MAX_ENEMIES
EAim:           .res MAX_ENEMIES    ; Torreta: 0 = frente, 1 = diagonal, 2 = cima
EList:          .res MAX_ENEMIES    ; Posição na lista da fase (para marcar como morto)
EListState:     .res MAX_LIST       ; 0 = esperando, 1 = ativo, 2 = morto

;; Efeitos
FxType:         .res MAX_FX
FxXL:           .res MAX_FX
FxXH:           .res MAX_FX
FxYL:           .res MAX_FX
FxYH:           .res MAX_FX
FxTimer:        .res MAX_FX
FxFlip:         .res MAX_FX         ; $40 = espelhado
