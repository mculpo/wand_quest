;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variáveis na zero page ($0000-$00FF): acesso mais rápido e menor.
;; Aqui ficam os controles, flags de frame e os "parâmetros" das rotinas.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "ZEROPAGE"
Buttons:                                .res 1       ; Botões pressionados neste frame
PrevButtons:                            .res 1       ; Botões pressionados no frame anterior

Frame:                                  .res 1       ; Contador de frames (0..59)
IsDrawComplete:                         .res 1       ; A NMI coloca 1 quando termina o frame
IsOamReady:                             .res 1       ; O loop coloca 1 quando o buffer de OAM em $0200 está completo
Clock60:                                .res 1       ; Contador de segundos (incrementa a cada 60 frames)

BgPtr:                                  .res 2       ; Ponteiro (16 bits) para os dados do background
MetaPtr:                                .res 2       ; Ponteiro (16 bits) para a tabela do metasprite a desenhar
AnimPtr:                                .res 2       ; Tabela de quadros da animação atual do mago (src/player_anim.asm)
PlayerMoved:                            .res 1       ; 1 = o player andou neste frame (animação de andar)
NewAnimPtr:                             .res 2       ; UpdatePlayerAnimation: animação escolhida neste frame
NewAnimCount:                           .res 1       ; UpdatePlayerAnimation: quadros da animação escolhida
NewAnimDelay:                           .res 1       ; UpdatePlayerAnimation: frames por quadro

;; Bola de pedra (src/ball.asm)
BallActive:                             .res 1       ; 1 = a bola está voando
BallX:                                  .res 1       ; Posição da bola (canto superior esquerdo, 8x8)
BallY:                                  .res 1
BallSide:                               .res 1       ; Direção da bola (Side)
BallSteps:                              .res 1       ; Pixels que ainda faltam andar neste frame
ImpactTimer:                            .res 1       ; Frames que a poeira do impacto ainda aparece
ImpactX:                                .res 1       ; Onde a poeira aparece
ImpactY:                                .res 1

;; Inimigos (src/enemies.asm): temporários
EnemyCell:                              .res 1       ; Célula do mapa do inimigo sendo pensado
FreeDirs:                               .res 1       ; Bits das direções livres (bit N = Side N)
TryDir:                                 .res 1       ; Direção sendo testada na escolha aleatória
TryCount:                               .res 1       ; Quantas direções ainda faltam testar

;; Parâmetros de colisão. Toda rotina de colisão devolve o resultado no
;; CARRY: C = 1 houve colisão, C = 0 não houve.
BoxX1:                                  .res 1       ; Hitbox do objeto sendo testado (limites inclusivos)
BoxX2:                                  .res 1
BoxY1:                                  .res 1
BoxY2:                                  .res 1
RectX1:                                 .res 1       ; Hitbox do obstáculo (outro bloco, player...)
RectX2:                                 .res 1
RectY1:                                 .res 1
RectY2:                                 .res 1
PointX:                                 .res 1       ; Ponto (em pixels) consultado no mapa de colisão
PointY:                                 .res 1
ParamTile:                              .res 1       ; Tipo de tile procurado no mapa (TILE_SOLID, TILE_SLOT...)
IgnoreBlock:                            .res 1       ; Índice do bloco que não deve colidir consigo mesmo (NO_BLOCK = nenhum)

;; Parâmetros de uso geral
ParamX:                                 .res 1       ; Posição X de entrada (ex.: AddNewBlock)
ParamY:                                 .res 1       ; Posição Y de entrada (ex.: AddNewBlock)
ParamAttrIn:                            .res 1       ; Entrada das rotinas SetSprite* (paleta/flag)
ParamAttrOut:                           .res 1       ; Byte de atributo de sprite lido/alterado pelas SetSprite*
MoveSide:                               .res 1       ; Direção do movimento em teste (TryMovePlayer)
StepCount:                              .res 1       ; Quantos pixels ainda faltam andar neste frame (MoveBlock)
Temp:                                   .res 1       ; Temporário de uso rápido (não sobrevive a um jsr)

;; Movimento do player (TryMovePlayer / assistência de quina)
OffX:                                   .res 1       ; Deslocamento X testado a partir da posição do player
OffY:                                   .res 1       ; Deslocamento Y testado a partir da posição do player
PerpShift:                              .res 1       ; Deslocamento perpendicular (com sinal) em teste
AssistK:                                .res 1       ; Distância atual da busca da assistência de quina
AssistAllowed:                          .res 1       ; 1 = só uma direção apertada (assistência liberada)

;; Fases
LevelPtr:                               .res 2       ; Ponteiro para os dados ASCII da fase sendo carregada
SelectComboUsed:                        .res 1       ; 1 = Select foi usado em um combo (não reinicia ao soltar)

;; Efeitos (src/effects.asm)
IsVramReady:                            .res 1       ; O loop coloca 1 quando o VramBuffer está completo
VramIndex:                              .res 1       ; Próxima posição livre no VramBuffer
DissolveLfsr:                           .res 1       ; Estado do LFSR que sorteia a ordem dos metatiles
DissolveSteps:                          .res 1       ; Quantos passos do LFSR ainda faltam (255 no total)
PendingCount:                           .res 1       ; Quantos metatiles estão mostrando o brilho agora
MtKind:                                 .res 1       ; QueueMetatile: tipo do metatile (MT_*)
MtCell:                                 .res 1       ; QueueMetatile: célula do mapa
MtHi:                                   .res 1       ; QueueMetatile: byte alto do endereço no nametable
MtLo:                                   .res 1       ; QueueMetatile: byte baixo do endereço no nametable
TextPtr:                                .res 2       ; QueueText: ponteiro para o texto
TextLen:                                .res 1       ; QueueText: tamanho do texto
TextRow:                                .res 1       ; QueueText: linha de tiles no nametable (0-29)
TextCol:                                .res 1       ; QueueText: coluna de tiles no nametable (0-31)
PalIndex:                               .res 1       ; QueueTierPalette: paleta sendo enfileirada

;; Contadores de loop
LoadRow:                                .res 1       ; LoadLevel: linhas que faltam ler
LoadCol:                                .res 1       ; LoadLevel: colunas que faltam ler
LoadTextIndex:                          .res 1       ; LoadLevel: posição no texto da fase
OnSlotCount:                            .res 1       ; CheckLevelComplete: blocos em cima de encaixes
RenderCount:                            .res 1       ; RenderBlocks: blocos que faltam desenhar
RenderStart:                            .res 1       ; RenderBlocks: bloco desenhado primeiro (gira a cada frame)

;; Parâmetros do DrawMetasprite
MetaX:                                  .res 1       ; Posição X na tela do canto superior esquerdo
MetaY:                                  .res 1       ; Posição Y na tela do canto superior esquerdo
MetaAttr:                               .res 1       ; Bits de atributo somados (OR) a todos os sprites
OamIndex:                               .res 1       ; Próxima posição livre no buffer de OAM (0, 4, 8...)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variáveis na RAM comum ($0300-$07FF)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "RAM"

;; Blocos em Structure of Arrays: o bloco N é BlockX+N, BlockY+N, etc.
;; Com o índice em X: "lda BlockX,x" / "inx" para ir ao próximo.
BlockType:                              .res MAX_BLOCKS   ; GameObjectType (NULL = slot livre)
BlockX:                                 .res MAX_BLOCKS   ; Posição X em pixels (canto superior esquerdo)
BlockY:                                 .res MAX_BLOCKS   ; Posição Y em pixels (canto superior esquerdo)
BlockSpeed:                             .res MAX_BLOCKS   ; Pixels por frame quando empurrado
BlockAttr:                              .res MAX_BLOCKS   ; Atributo de sprite (paleta) aplicado ao metasprite
BlockSide:                              .res MAX_BLOCKS   ; Direção em que está deslizando (Side::NONE = parado)

;; Inimigos em Structure of Arrays (índice do inimigo em X)
EnemyType:                              .res MAX_ENEMIES  ; EnemyType (NONE = slot livre)
EnemyX:                                 .res MAX_ENEMIES  ; Posição X em pixels
EnemyY:                                 .res MAX_ENEMIES  ; Posição Y em pixels
EnemyDir:                               .res MAX_ENEMIES  ; Direção em que anda (Side; NONE = parado)
EnemyDying:                             .res MAX_ENEMIES  ; 0 = vivo; senão frames que faltam do poof
EnemyPause:                             .res MAX_ENEMIES  ; Frames que ainda fica parado numa célula
EnemySub:                               .res MAX_ENEMIES  ; Fração de pixel acumulada (velocidade em 1/256 px)
EnemyFrame:                             .res MAX_ENEMIES  ; Quadro da animação (0 a ENEMY_FRAMES - 1)
EnemyAnimTimer:                         .res MAX_ENEMIES  ; Frames até o próximo quadro

;; Player
PlayerX:                                .res 1       ; Posição X em pixels (canto superior esquerdo)
PlayerY:                                .res 1       ; Posição Y em pixels (canto superior esquerdo)
PlayerSide:                             .res 1       ; Última direção em que andou (mira da magia)
AnimTimer:                              .res 1       ; Frames que faltam para o próximo quadro da animação
CastActive:                             .res 1       ; 1 = tocando a animação de lançar a bola
FaceTimer:                              .res 1       ; Frames que ele ainda fica virado para PlayerSide antes do idle
AnimFrame:                              .res 1       ; Quadro atual (0 a MAGE_FRAMES - 1) da animação em AnimPtr
PlayerVisible:                          .res 1       ; 0 = escondido (durante a transição)

;; Fase atual
LevelMap:                               .res 256     ; Mapa de colisão 16x16 da fase (TILE_*), montado pelo LoadLevel
CurrentLevel:                           .res 1       ; Fase atual (0 = fase 1)
LevelTier:                              .res 1       ; Dificuldade da fase atual (0 a TIER_COUNT - 1)
NextLevel:                              .res 1       ; Fase que a transição vai montar (LEVEL_COUNT = tela final)
SlotCount:                              .res 1       ; Quantos encaixes a fase tem (vitória = todos ocupados)
LevelStartCount:                        .res 1       ; Quantos blocos a fase tem na posição inicial
LevelStartCell:                         .res MAX_BLOCKS ; Célula do mapa de cada bloco na posição inicial
PlayerStartCell:                        .res 1       ; Célula do mapa onde o player começa
LevelEnemyCount:                        .res 1       ; Quantos inimigos a fase tem na posição inicial
LevelEnemyCell:                         .res MAX_ENEMIES ; Célula do mapa de cada inimigo na posição inicial
LevelEnemyType:                         .res MAX_ENEMIES ; Tipo (EnemyType) de cada inimigo na posição inicial
StateTimer:                             .res 1       ; Contador de frames dos estados com pausa
TransitionPhase:                        .res 1       ; Etapa da transição (enum TransPhase)

;; Efeitos
VramBuffer:                             .res VRAM_BUFFER_SIZE ; Fila de escritas na VRAM: hi, lo, tamanho, bytes... e 0 no fim
PendingCells:                           .res DISSOLVE_PER_FRAME ; Metatiles mostrando o brilho, trocados no próximo frame
TextBuffer:                             .res 16      ; Texto montado para o QueueText

Seed:                                   .res 2       ; Semente de 16 bits do gerador aleatório (não pode ser 0)
GameState:                              .res 1       ; Estado atual do jogo (enum State)
