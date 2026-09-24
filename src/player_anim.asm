;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Animações do mago e da bola de pedra. Gerado por tools/draw_sprites.py
;; (não edite à mão).
;;
;; Cada animação é uma tabela de endereços de metasprite (ver
;; DrawMetasprite em src/utils.asm). As tabelas *BySide dão a animação
;; para cada valor de Side (NONE, UP, DOWN, RIGHT, LEFT).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

MAGE_IDLE_FRAMES = 16
MAGE_WALK_FRAMES = 12
MAGE_CAST_FRAMES = 4
MAGE_DIE_FRAMES  = 8
BALL_FRAMES      = 4
PUFF_FRAMES      = 3

MageWalkBySide:
    .word MageWalkDown          ; Side::NONE (de frente)
    .word MageWalkUp            ; Side::UP
    .word MageWalkDown          ; Side::DOWN
    .word MageWalkRight         ; Side::RIGHT
    .word MageWalkLeft          ; Side::LEFT

MageCastBySide:
    .word MageCastDown          ; Side::NONE (de frente)
    .word MageCastUp            ; Side::UP
    .word MageCastDown          ; Side::DOWN
    .word MageCastRight         ; Side::RIGHT
    .word MageCastLeft          ; Side::LEFT

MageIdle:
    .word MageIdle0
    .word MageIdle1
    .word MageIdle2
    .word MageIdle3
    .word MageIdle4
    .word MageIdle5
    .word MageIdle6
    .word MageIdle7
    .word MageIdle8
    .word MageIdle9
    .word MageIdle10
    .word MageIdle11
    .word MageIdle12
    .word MageIdle13
    .word MageIdle14
    .word MageIdle15

MageDie:
    .word MageDie0
    .word MageDie1
    .word MageDie2
    .word MageDie3
    .word MageDie4
    .word MageDie5
    .word MageDie6
    .word MageDie7

MageWalkUp:
    .word MageWalkUp0
    .word MageWalkUp1
    .word MageWalkUp2
    .word MageWalkUp3
    .word MageWalkUp4
    .word MageWalkUp5
    .word MageWalkUp6
    .word MageWalkUp7
    .word MageWalkUp8
    .word MageWalkUp9
    .word MageWalkUp10
    .word MageWalkUp11

MageWalkDown:
    .word MageWalkDown0
    .word MageWalkDown1
    .word MageWalkDown2
    .word MageWalkDown3
    .word MageWalkDown4
    .word MageWalkDown5
    .word MageWalkDown6
    .word MageWalkDown7
    .word MageWalkDown8
    .word MageWalkDown9
    .word MageWalkDown10
    .word MageWalkDown11

MageWalkLeft:
    .word MageWalkLeft0
    .word MageWalkLeft1
    .word MageWalkLeft2
    .word MageWalkLeft3
    .word MageWalkLeft4
    .word MageWalkLeft5
    .word MageWalkLeft6
    .word MageWalkLeft7
    .word MageWalkLeft8
    .word MageWalkLeft9
    .word MageWalkLeft10
    .word MageWalkLeft11

MageWalkRight:
    .word MageWalkRight0
    .word MageWalkRight1
    .word MageWalkRight2
    .word MageWalkRight3
    .word MageWalkRight4
    .word MageWalkRight5
    .word MageWalkRight6
    .word MageWalkRight7
    .word MageWalkRight8
    .word MageWalkRight9
    .word MageWalkRight10
    .word MageWalkRight11

MageCastUp:
    .word MageCastUp0
    .word MageCastUp1
    .word MageCastUp2
    .word MageCastUp3

MageCastDown:
    .word MageCastDown0
    .word MageCastDown1
    .word MageCastDown2
    .word MageCastDown3

MageCastLeft:
    .word MageCastLeft0
    .word MageCastLeft1
    .word MageCastLeft2
    .word MageCastLeft3

MageCastRight:
    .word MageCastRight0
    .word MageCastRight1
    .word MageCastRight2
    .word MageCastRight3

BallFrames:
    .word Ball0
    .word Ball1
    .word Ball2
    .word Ball3

PuffFrames:
    .word Puff0
    .word Puff1
    .word Puff2

;;     dy  tile  atributo    dx
MageIdle0:
    .byte  0, $10, %00000011,  0
    .byte  0, $11, %00000011,  8
    .byte  8, $12, %00000011,  0
    .byte  8, $13, %00000011,  8
    .byte METASPRITE_END
MageIdle1:
    .byte  0, $14, %00000011,  0
    .byte  0, $15, %00000011,  8
    .byte  8, $16, %00000011,  0
    .byte  8, $17, %00000011,  8
    .byte METASPRITE_END
MageIdle2:
    .byte  0, $14, %00000011,  0
    .byte  0, $15, %00000011,  8
    .byte  8, $16, %00000011,  0
    .byte  8, $17, %00000011,  8
    .byte METASPRITE_END
MageIdle3:
    .byte  0, $14, %00000011,  0
    .byte  0, $15, %00000011,  8
    .byte  8, $16, %00000011,  0
    .byte  8, $17, %00000011,  8
    .byte METASPRITE_END
MageIdle4:
    .byte  0, $14, %00000011,  0
    .byte  0, $18, %00000011,  8
    .byte  8, $16, %00000011,  0
    .byte  8, $19, %00000011,  8
    .byte METASPRITE_END
MageIdle5:
    .byte  0, $14, %00000011,  0
    .byte  0, $1A, %00000011,  8
    .byte  8, $16, %00000011,  0
    .byte  8, $1B, %00000011,  8
    .byte METASPRITE_END
MageIdle6:
    .byte  0, $14, %00000011,  0
    .byte  0, $1C, %00000011,  8
    .byte  8, $16, %00000011,  0
    .byte  8, $1D, %00000011,  8
    .byte METASPRITE_END
MageIdle7:
    .byte  0, $14, %00000011,  0
    .byte  0, $1E, %00000011,  8
    .byte  8, $16, %00000011,  0
    .byte  8, $1B, %00000011,  8
    .byte METASPRITE_END
MageIdle8:
    .byte  0, $1F, %00000011,  0
    .byte  0, $20, %00000011,  8
    .byte  8, $12, %00000011,  0
    .byte  8, $21, %00000011,  8
    .byte METASPRITE_END
MageIdle9:
    .byte  0, $1F, %00000011,  0
    .byte  0, $22, %00000011,  8
    .byte  8, $12, %00000011,  0
    .byte  8, $13, %00000011,  8
    .byte METASPRITE_END
MageIdle10:
    .byte  0, $1F, %00000011,  0
    .byte  0, $22, %00000011,  8
    .byte  8, $12, %00000011,  0
    .byte  8, $13, %00000011,  8
    .byte METASPRITE_END
MageIdle11:
    .byte  0, $1F, %00000011,  0
    .byte  0, $22, %00000011,  8
    .byte  8, $12, %00000011,  0
    .byte  8, $13, %00000011,  8
    .byte METASPRITE_END
MageIdle12:
    .byte  0, $1F, %00000011,  0
    .byte  0, $23, %00000011,  8
    .byte  8, $12, %00000011,  0
    .byte  8, $13, %00000011,  8
    .byte METASPRITE_END
MageIdle13:
    .byte  0, $10, %00000011,  0
    .byte  0, $24, %00000011,  8
    .byte  8, $25, %00000011,  0
    .byte  8, $26, %00000011,  8
    .byte METASPRITE_END
MageIdle14:
    .byte  0, $10, %00000011,  0
    .byte  0, $11, %00000011,  8
    .byte  8, $12, %00000011,  0
    .byte  8, $13, %00000011,  8
    .byte METASPRITE_END
MageIdle15:
    .byte  0, $10, %00000011,  0
    .byte  0, $11, %00000011,  8
    .byte  8, $12, %00000011,  0
    .byte  8, $13, %00000011,  8
    .byte METASPRITE_END

MageDie0:
    .byte  0, $1F, %00000011,  0
    .byte  0, $94, %00000011,  8
    .byte  8, $95, %00000011,  0
    .byte  8, $96, %00000011,  8
    .byte METASPRITE_END
MageDie1:
    .byte  0, $97, %00000011,  0
    .byte  0, $98, %00000011,  8
    .byte  8, $99, %00000011,  0
    .byte  8, $9A, %00000011,  8
    .byte METASPRITE_END
MageDie2:
    .byte  0, $9B, %00000011,  0
    .byte  0, $9C, %00000011,  8
    .byte  8, $9D, %00000011,  0
    .byte  8, $9E, %00000011,  8
    .byte METASPRITE_END
MageDie3:
    .byte  0, $9F, %00000011,  0
    .byte  0, $A0, %00000011,  8
    .byte  8, $A1, %00000011,  0
    .byte  8, $A2, %00000011,  8
    .byte METASPRITE_END
MageDie4:
    .byte  0, $A3, %00000011,  0
    .byte  0, $A4, %00000011,  8
    .byte  8, $A5, %00000011,  0
    .byte  8, $A6, %00000011,  8
    .byte METASPRITE_END
MageDie5:
    .byte  0, $A7, %00000011,  0
    .byte  0, $A8, %00000011,  8
    .byte  8, $A9, %00000011,  0
    .byte  8, $AA, %00000011,  8
    .byte METASPRITE_END
MageDie6:
    .byte  0, $AB, %00000011,  0
    .byte  0, $AC, %00000011,  8
    .byte METASPRITE_END
MageDie7:
    .byte  0, $AD, %00000011,  0
    .byte  0, $AE, %00000011,  8
    .byte METASPRITE_END

MageWalkUp0:
    .byte  0, $47, %00000011,  0
    .byte  0, $44, %00000011,  8
    .byte  8, $48, %00000011,  0
    .byte  8, $49, %00000011,  8
    .byte METASPRITE_END
MageWalkUp1:
    .byte  0, $1F, %00000011,  0
    .byte  0, $4A, %00000011,  8
    .byte  8, $4B, %00000011,  0
    .byte  8, $4C, %00000011,  8
    .byte METASPRITE_END
MageWalkUp2:
    .byte  0, $14, %00000011,  0
    .byte  0, $15, %00000011,  8
    .byte  8, $4D, %00000011,  0
    .byte  8, $4E, %00000011,  8
    .byte METASPRITE_END
MageWalkUp3:
    .byte  0, $14, %00000011,  0
    .byte  0, $4F, %00000011,  8
    .byte  8, $50, %00000011,  0
    .byte  8, $51, %00000011,  8
    .byte METASPRITE_END
MageWalkUp4:
    .byte  0, $14, %00000011,  0
    .byte  0, $52, %00000011,  8
    .byte  8, $50, %00000011,  0
    .byte  8, $51, %00000011,  8
    .byte METASPRITE_END
MageWalkUp5:
    .byte  0, $1F, %00000011,  0
    .byte  0, $53, %00000011,  8
    .byte  8, $54, %00000011,  0
    .byte  8, $55, %00000011,  8
    .byte METASPRITE_END
MageWalkUp6:
    .byte  0, $56, %00000011,  0
    .byte  0, $53, %00000011,  8
    .byte  8, $57, %00000011,  0
    .byte  8, $58, %00000011,  8
    .byte METASPRITE_END
MageWalkUp7:
    .byte  0, $59, %00000011,  0
    .byte  0, $39, %00000011,  8
    .byte  8, $5A, %00000011,  0
    .byte  8, $5B, %00000011,  8
    .byte METASPRITE_END
MageWalkUp8:
    .byte  0, $5C, %00000011,  0
    .byte  0, $3C, %00000011,  8
    .byte  8, $5D, %00000011,  0
    .byte  8, $5E, %00000011,  8
    .byte METASPRITE_END
MageWalkUp9:
    .byte  0, $5F, %00000011,  0
    .byte  0, $40, %00000011,  8
    .byte  8, $60, %00000011,  0
    .byte  8, $61, %00000011,  8
    .byte METASPRITE_END
MageWalkUp10:
    .byte  0, $5F, %00000011,  0
    .byte  0, $43, %00000011,  8
    .byte  8, $60, %00000011,  0
    .byte  8, $61, %00000011,  8
    .byte METASPRITE_END
MageWalkUp11:
    .byte  0, $62, %00000011,  0
    .byte  0, $44, %00000011,  8
    .byte  8, $63, %00000011,  0
    .byte  8, $64, %00000011,  8
    .byte METASPRITE_END

MageWalkDown0:
    .byte  0, $10, %00000011,  0
    .byte  0, $11, %00000011,  8
    .byte  8, $27, %00000011,  0
    .byte  8, $28, %00000011,  8
    .byte METASPRITE_END
MageWalkDown1:
    .byte  0, $1F, %00000011,  0
    .byte  0, $29, %00000011,  8
    .byte  8, $2A, %00000011,  0
    .byte  8, $2B, %00000011,  8
    .byte METASPRITE_END
MageWalkDown2:
    .byte  0, $14, %00000011,  0
    .byte  0, $2C, %00000011,  8
    .byte  8, $2D, %00000011,  0
    .byte  8, $2E, %00000011,  8
    .byte METASPRITE_END
MageWalkDown3:
    .byte  0, $14, %00000011,  0
    .byte  0, $2F, %00000011,  8
    .byte  8, $30, %00000011,  0
    .byte  8, $31, %00000011,  8
    .byte METASPRITE_END
MageWalkDown4:
    .byte  0, $14, %00000011,  0
    .byte  0, $32, %00000011,  8
    .byte  8, $30, %00000011,  0
    .byte  8, $31, %00000011,  8
    .byte METASPRITE_END
MageWalkDown5:
    .byte  0, $1F, %00000011,  0
    .byte  0, $33, %00000011,  8
    .byte  8, $34, %00000011,  0
    .byte  8, $35, %00000011,  8
    .byte METASPRITE_END
MageWalkDown6:
    .byte  0, $1F, %00000011,  0
    .byte  0, $36, %00000011,  8
    .byte  8, $37, %00000011,  0
    .byte  8, $38, %00000011,  8
    .byte METASPRITE_END
MageWalkDown7:
    .byte  0, $1F, %00000011,  0
    .byte  0, $39, %00000011,  8
    .byte  8, $3A, %00000011,  0
    .byte  8, $3B, %00000011,  8
    .byte METASPRITE_END
MageWalkDown8:
    .byte  0, $14, %00000011,  0
    .byte  0, $3C, %00000011,  8
    .byte  8, $3D, %00000011,  0
    .byte  8, $3E, %00000011,  8
    .byte METASPRITE_END
MageWalkDown9:
    .byte  0, $3F, %00000011,  0
    .byte  0, $40, %00000011,  8
    .byte  8, $41, %00000011,  0
    .byte  8, $42, %00000011,  8
    .byte METASPRITE_END
MageWalkDown10:
    .byte  0, $3F, %00000011,  0
    .byte  0, $43, %00000011,  8
    .byte  8, $41, %00000011,  0
    .byte  8, $42, %00000011,  8
    .byte METASPRITE_END
MageWalkDown11:
    .byte  0, $10, %00000011,  0
    .byte  0, $44, %00000011,  8
    .byte  8, $45, %00000011,  0
    .byte  8, $46, %00000011,  8
    .byte METASPRITE_END

MageWalkLeft0:
    .byte  0, $65, %00000011,  0
    .byte  0, $66, %00000011,  8
    .byte  8, $67, %00000011,  0
    .byte  8, $68, %00000011,  8
    .byte METASPRITE_END
MageWalkLeft1:
    .byte  0, $65, %00000011,  0
    .byte  0, $66, %00000011,  8
    .byte  8, $69, %00000011,  0
    .byte  8, $6A, %00000011,  8
    .byte METASPRITE_END
MageWalkLeft2:
    .byte  0, $6B, %00000011,  0
    .byte  0, $6C, %00000011,  8
    .byte  8, $6D, %00000011,  0
    .byte  8, $6E, %00000011,  8
    .byte METASPRITE_END
MageWalkLeft3:
    .byte  0, $6B, %00000011,  0
    .byte  0, $6F, %00000011,  8
    .byte  8, $70, %00000011,  0
    .byte  8, $71, %00000011,  8
    .byte METASPRITE_END
MageWalkLeft4:
    .byte  0, $6B, %00000011,  0
    .byte  0, $6F, %00000011,  8
    .byte  8, $72, %00000011,  0
    .byte  8, $73, %00000011,  8
    .byte METASPRITE_END
MageWalkLeft5:
    .byte  0, $65, %00000011,  0
    .byte  0, $74, %00000011,  8
    .byte  8, $75, %00000011,  0
    .byte  8, $76, %00000011,  8
    .byte METASPRITE_END
MageWalkLeft6:
    .byte  0, $65, %00000011,  0
    .byte  0, $66, %00000011,  8
    .byte  8, $67, %00000011,  0
    .byte  8, $68, %00000011,  8
    .byte METASPRITE_END
MageWalkLeft7:
    .byte  0, $65, %00000011,  0
    .byte  0, $66, %00000011,  8
    .byte  8, $77, %00000011,  0
    .byte  8, $78, %00000011,  8
    .byte METASPRITE_END
MageWalkLeft8:
    .byte  0, $6B, %00000011,  0
    .byte  0, $6C, %00000011,  8
    .byte  8, $79, %00000011,  0
    .byte  8, $7A, %00000011,  8
    .byte METASPRITE_END
MageWalkLeft9:
    .byte  0, $6B, %00000011,  0
    .byte  0, $6F, %00000011,  8
    .byte  8, $7B, %00000011,  0
    .byte  8, $7C, %00000011,  8
    .byte METASPRITE_END
MageWalkLeft10:
    .byte  0, $6B, %00000011,  0
    .byte  0, $6F, %00000011,  8
    .byte  8, $7D, %00000011,  0
    .byte  8, $7E, %00000011,  8
    .byte METASPRITE_END
MageWalkLeft11:
    .byte  0, $65, %00000011,  0
    .byte  0, $74, %00000011,  8
    .byte  8, $7F, %00000011,  0
    .byte  8, $80, %00000011,  8
    .byte METASPRITE_END

MageWalkRight0:
    .byte  0, $65, %01000011,  8
    .byte  0, $66, %01000011,  0
    .byte  8, $67, %01000011,  8
    .byte  8, $68, %01000011,  0
    .byte METASPRITE_END
MageWalkRight1:
    .byte  0, $65, %01000011,  8
    .byte  0, $66, %01000011,  0
    .byte  8, $69, %01000011,  8
    .byte  8, $6A, %01000011,  0
    .byte METASPRITE_END
MageWalkRight2:
    .byte  0, $6B, %01000011,  8
    .byte  0, $6C, %01000011,  0
    .byte  8, $6D, %01000011,  8
    .byte  8, $6E, %01000011,  0
    .byte METASPRITE_END
MageWalkRight3:
    .byte  0, $6B, %01000011,  8
    .byte  0, $6F, %01000011,  0
    .byte  8, $70, %01000011,  8
    .byte  8, $71, %01000011,  0
    .byte METASPRITE_END
MageWalkRight4:
    .byte  0, $6B, %01000011,  8
    .byte  0, $6F, %01000011,  0
    .byte  8, $72, %01000011,  8
    .byte  8, $73, %01000011,  0
    .byte METASPRITE_END
MageWalkRight5:
    .byte  0, $65, %01000011,  8
    .byte  0, $74, %01000011,  0
    .byte  8, $75, %01000011,  8
    .byte  8, $76, %01000011,  0
    .byte METASPRITE_END
MageWalkRight6:
    .byte  0, $65, %01000011,  8
    .byte  0, $66, %01000011,  0
    .byte  8, $67, %01000011,  8
    .byte  8, $68, %01000011,  0
    .byte METASPRITE_END
MageWalkRight7:
    .byte  0, $65, %01000011,  8
    .byte  0, $66, %01000011,  0
    .byte  8, $77, %01000011,  8
    .byte  8, $78, %01000011,  0
    .byte METASPRITE_END
MageWalkRight8:
    .byte  0, $6B, %01000011,  8
    .byte  0, $6C, %01000011,  0
    .byte  8, $79, %01000011,  8
    .byte  8, $7A, %01000011,  0
    .byte METASPRITE_END
MageWalkRight9:
    .byte  0, $6B, %01000011,  8
    .byte  0, $6F, %01000011,  0
    .byte  8, $7B, %01000011,  8
    .byte  8, $7C, %01000011,  0
    .byte METASPRITE_END
MageWalkRight10:
    .byte  0, $6B, %01000011,  8
    .byte  0, $6F, %01000011,  0
    .byte  8, $7D, %01000011,  8
    .byte  8, $7E, %01000011,  0
    .byte METASPRITE_END
MageWalkRight11:
    .byte  0, $65, %01000011,  8
    .byte  0, $74, %01000011,  0
    .byte  8, $7F, %01000011,  8
    .byte  8, $80, %01000011,  0
    .byte METASPRITE_END

MageCastUp0:
    .byte  0, $87, %00000011,  0
    .byte  0, $88, %00000011,  8
    .byte  8, $89, %00000011,  0
    .byte  8, $8A, %00000011,  8
    .byte METASPRITE_END
MageCastUp1:
    .byte  0, $8B, %00000011,  0
    .byte  0, $88, %00000011,  8
    .byte  8, $89, %00000011,  0
    .byte  8, $8A, %00000011,  8
    .byte METASPRITE_END
MageCastUp2:
    .byte  0, $8C, %00000011,  0
    .byte  0, $88, %00000011,  8
    .byte  8, $8D, %00000011,  0
    .byte  8, $8A, %00000011,  8
    .byte METASPRITE_END
MageCastUp3:
    .byte  0, $59, %00000011,  0
    .byte  0, $88, %00000011,  8
    .byte  8, $5A, %00000011,  0
    .byte  8, $8A, %00000011,  8
    .byte METASPRITE_END

MageCastDown0:
    .byte  0, $1F, %00000011,  0
    .byte  0, $81, %00000011,  8
    .byte  8, $12, %00000011,  0
    .byte  8, $82, %00000011,  8
    .byte METASPRITE_END
MageCastDown1:
    .byte  0, $1F, %00000011,  0
    .byte  0, $83, %00000011,  8
    .byte  8, $12, %00000011,  0
    .byte  8, $82, %00000011,  8
    .byte METASPRITE_END
MageCastDown2:
    .byte  0, $1F, %00000011,  0
    .byte  0, $84, %00000011,  8
    .byte  8, $12, %00000011,  0
    .byte  8, $85, %00000011,  8
    .byte METASPRITE_END
MageCastDown3:
    .byte  0, $1F, %00000011,  0
    .byte  0, $86, %00000011,  8
    .byte  8, $12, %00000011,  0
    .byte  8, $2B, %00000011,  8
    .byte METASPRITE_END

MageCastLeft0:
    .byte  0, $6B, %00000011,  0
    .byte  0, $8E, %00000011,  8
    .byte  8, $8F, %00000011,  0
    .byte  8, $90, %00000011,  8
    .byte METASPRITE_END
MageCastLeft1:
    .byte  0, $6B, %00000011,  0
    .byte  0, $8E, %00000011,  8
    .byte  8, $91, %00000011,  0
    .byte  8, $90, %00000011,  8
    .byte METASPRITE_END
MageCastLeft2:
    .byte  0, $6B, %00000011,  0
    .byte  0, $8E, %00000011,  8
    .byte  8, $92, %00000011,  0
    .byte  8, $90, %00000011,  8
    .byte METASPRITE_END
MageCastLeft3:
    .byte  0, $6B, %00000011,  0
    .byte  0, $8E, %00000011,  8
    .byte  8, $93, %00000011,  0
    .byte  8, $90, %00000011,  8
    .byte METASPRITE_END

MageCastRight0:
    .byte  0, $6B, %01000011,  8
    .byte  0, $8E, %01000011,  0
    .byte  8, $8F, %01000011,  8
    .byte  8, $90, %01000011,  0
    .byte METASPRITE_END
MageCastRight1:
    .byte  0, $6B, %01000011,  8
    .byte  0, $8E, %01000011,  0
    .byte  8, $91, %01000011,  8
    .byte  8, $90, %01000011,  0
    .byte METASPRITE_END
MageCastRight2:
    .byte  0, $6B, %01000011,  8
    .byte  0, $8E, %01000011,  0
    .byte  8, $92, %01000011,  8
    .byte  8, $90, %01000011,  0
    .byte METASPRITE_END
MageCastRight3:
    .byte  0, $6B, %01000011,  8
    .byte  0, $8E, %01000011,  0
    .byte  8, $93, %01000011,  8
    .byte  8, $90, %01000011,  0
    .byte METASPRITE_END

Ball0:
    .byte  0, $AF, %00000010,  0
    .byte METASPRITE_END
Ball1:
    .byte  0, $B0, %00000010,  0
    .byte METASPRITE_END
Ball2:
    .byte  0, $B1, %00000010,  0
    .byte METASPRITE_END
Ball3:
    .byte  0, $B2, %00000010,  0
    .byte METASPRITE_END

Puff0:
    .byte  0, $B3, %00000010,  0
    .byte METASPRITE_END
Puff1:
    .byte  0, $B4, %00000010,  0
    .byte METASPRITE_END
Puff2:
    .byte  0, $B5, %00000010,  0
    .byte METASPRITE_END
