;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sprites do protótipo. Gerado por tools/draw_sprites.py (não edite à mão).
;;
;; Cada tabela <Nome> é uma lista de endereços de metasprite (um por quadro),
;; e <NOME>_FRAMES é quantos quadros ela tem. Metasprite: .byte dy, tile,
;; atributo, dx ... e METASPRITE_END. R/L = olhando para a direita/esquerda.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

TILE_HUD_CAP    = $6C
TILE_HUD_FULL   = $6D
TILE_HUD_HALF   = $6E
TILE_HUD_EMPTY  = $6F
TILE_LETTER_A = $70
TILE_LETTER_C = $71
TILE_LETTER_E = $72
TILE_LETTER_I = $73
TILE_LETTER_L = $74
TILE_LETTER_M = $75
TILE_LETTER_O = $76
TILE_LETTER_P = $77
TILE_LETTER_S = $78
TILE_LETTER_T = $79
TILE_LETTER_U = $7A

AGENT_IDLE_R_FRAMES = 6
AGENT_IDLE_L_FRAMES = 6
AGENT_RUN_R_FRAMES = 8
AGENT_RUN_L_FRAMES = 8
AGENT_JUMP_R_FRAMES = 2
AGENT_JUMP_L_FRAMES = 2
AGENT_FALL_R_FRAMES = 1
AGENT_FALL_L_FRAMES = 1
AGENT_CROUCH_R_FRAMES = 2
AGENT_CROUCH_L_FRAMES = 2
AGENT_ROLL_R_FRAMES = 6
AGENT_ROLL_L_FRAMES = 6
AGENT_SHOOT_STAND_R_FRAMES = 1
AGENT_SHOOT_STAND_L_FRAMES = 1
AGENT_SHOOT_UP_R_FRAMES = 1
AGENT_SHOOT_UP_L_FRAMES = 1
AGENT_HURT_R_FRAMES = 1
AGENT_HURT_L_FRAMES = 1
ROBOT_R_FRAMES = 5
ROBOT_L_FRAMES = 5
TURRET_R_FRAMES = 3
TURRET_L_FRAMES = 3
DRONE_FRAMES = 4
SHOT_H_FRAMES = 1
SHOT_V_FRAMES = 1
ORB_FRAMES = 2
MUZZLE_FRAMES = 1
SPARK_FRAMES = 3
EXPLOSION_FRAMES = 4

AgentIdleR:
    .word AgentIdleR0
    .word AgentIdleR1
    .word AgentIdleR2
    .word AgentIdleR3
    .word AgentIdleR4
    .word AgentIdleR5
AgentIdleL:
    .word AgentIdleL0
    .word AgentIdleL1
    .word AgentIdleL2
    .word AgentIdleL3
    .word AgentIdleL4
    .word AgentIdleL5
AgentRunR:
    .word AgentRunR0
    .word AgentRunR1
    .word AgentRunR2
    .word AgentRunR3
    .word AgentRunR4
    .word AgentRunR5
    .word AgentRunR6
    .word AgentRunR7
AgentRunL:
    .word AgentRunL0
    .word AgentRunL1
    .word AgentRunL2
    .word AgentRunL3
    .word AgentRunL4
    .word AgentRunL5
    .word AgentRunL6
    .word AgentRunL7
AgentJumpR:
    .word AgentJumpR0
    .word AgentJumpR1
AgentJumpL:
    .word AgentJumpL0
    .word AgentJumpL1
AgentFallR:
    .word AgentFallR0
AgentFallL:
    .word AgentFallL0
AgentCrouchR:
    .word AgentCrouchR0
    .word AgentCrouchR1
AgentCrouchL:
    .word AgentCrouchL0
    .word AgentCrouchL1
AgentRollR:
    .word AgentRollR0
    .word AgentRollR1
    .word AgentRollR2
    .word AgentRollR3
    .word AgentRollR4
    .word AgentRollR5
AgentRollL:
    .word AgentRollL0
    .word AgentRollL1
    .word AgentRollL2
    .word AgentRollL3
    .word AgentRollL4
    .word AgentRollL5
AgentShootStandR:
    .word AgentShootStandR0
AgentShootStandL:
    .word AgentShootStandL0
AgentShootUpR:
    .word AgentShootUpR0
AgentShootUpL:
    .word AgentShootUpL0
AgentHurtR:
    .word AgentHurtR0
AgentHurtL:
    .word AgentHurtL0
RobotR:
    .word RobotR0
    .word RobotR1
    .word RobotR2
    .word RobotR3
    .word RobotR4
RobotL:
    .word RobotL0
    .word RobotL1
    .word RobotL2
    .word RobotL3
    .word RobotL4
TurretR:
    .word TurretR0
    .word TurretR1
    .word TurretR2
TurretL:
    .word TurretL0
    .word TurretL1
    .word TurretL2
Drone:
    .word Drone0
    .word Drone1
    .word Drone2
    .word Drone3
ShotH:
    .word ShotH0
ShotV:
    .word ShotV0
Orb:
    .word Orb0
    .word Orb1
Muzzle:
    .word Muzzle0
Spark:
    .word Spark0
    .word Spark1
    .word Spark2
Explosion:
    .word Explosion0
    .word Explosion1
    .word Explosion2
    .word Explosion3

;;     dy  tile  atributo    dx
AgentIdleR0:
    .byte  0, $00, %00000000,  0
    .byte  0, $01, %00000000,  8
    .byte  8, $02, %00000000,  0
    .byte  8, $03, %00000000,  8
    .byte 16, $04, %00000000,  0
    .byte 16, $05, %00000000,  8
    .byte METASPRITE_END
AgentIdleR1:
    .byte  0, $06, %00000000,  0
    .byte  0, $01, %00000000,  8
    .byte  8, $02, %00000000,  0
    .byte  8, $03, %00000000,  8
    .byte 16, $04, %00000000,  0
    .byte 16, $05, %00000000,  8
    .byte METASPRITE_END
AgentIdleR2:
    .byte  0, $00, %00000000,  0
    .byte  0, $07, %00000000,  8
    .byte  8, $02, %00000000,  0
    .byte  8, $03, %00000000,  8
    .byte 16, $04, %00000000,  0
    .byte 16, $05, %00000000,  8
    .byte METASPRITE_END
AgentIdleR3:
    .byte  0, $00, %00000000,  0
    .byte  0, $08, %00000000,  8
    .byte  8, $02, %00000000,  0
    .byte  8, $03, %00000000,  8
    .byte 16, $04, %00000000,  0
    .byte 16, $05, %00000000,  8
    .byte METASPRITE_END
AgentIdleR4:
    .byte  0, $00, %00000000,  0
    .byte  0, $09, %00000000,  8
    .byte  8, $02, %00000000,  0
    .byte  8, $03, %00000000,  8
    .byte 16, $04, %00000000,  0
    .byte 16, $05, %00000000,  8
    .byte METASPRITE_END
AgentIdleR5:
    .byte  0, $00, %00000000,  0
    .byte  0, $01, %00000000,  8
    .byte  8, $0A, %00000000,  0
    .byte  8, $03, %00000000,  8
    .byte 16, $04, %00000000,  0
    .byte 16, $05, %00000000,  8
    .byte METASPRITE_END
AgentIdleL0:
    .byte  0, $00, %01000000,  8
    .byte  0, $01, %01000000,  0
    .byte  8, $02, %01000000,  8
    .byte  8, $03, %01000000,  0
    .byte 16, $04, %01000000,  8
    .byte 16, $05, %01000000,  0
    .byte METASPRITE_END
AgentIdleL1:
    .byte  0, $06, %01000000,  8
    .byte  0, $01, %01000000,  0
    .byte  8, $02, %01000000,  8
    .byte  8, $03, %01000000,  0
    .byte 16, $04, %01000000,  8
    .byte 16, $05, %01000000,  0
    .byte METASPRITE_END
AgentIdleL2:
    .byte  0, $00, %01000000,  8
    .byte  0, $07, %01000000,  0
    .byte  8, $02, %01000000,  8
    .byte  8, $03, %01000000,  0
    .byte 16, $04, %01000000,  8
    .byte 16, $05, %01000000,  0
    .byte METASPRITE_END
AgentIdleL3:
    .byte  0, $00, %01000000,  8
    .byte  0, $08, %01000000,  0
    .byte  8, $02, %01000000,  8
    .byte  8, $03, %01000000,  0
    .byte 16, $04, %01000000,  8
    .byte 16, $05, %01000000,  0
    .byte METASPRITE_END
AgentIdleL4:
    .byte  0, $00, %01000000,  8
    .byte  0, $09, %01000000,  0
    .byte  8, $02, %01000000,  8
    .byte  8, $03, %01000000,  0
    .byte 16, $04, %01000000,  8
    .byte 16, $05, %01000000,  0
    .byte METASPRITE_END
AgentIdleL5:
    .byte  0, $00, %01000000,  8
    .byte  0, $01, %01000000,  0
    .byte  8, $0A, %01000000,  8
    .byte  8, $03, %01000000,  0
    .byte 16, $04, %01000000,  8
    .byte 16, $05, %01000000,  0
    .byte METASPRITE_END
AgentRunR0:
    .byte  0, $00, %00000000,  0
    .byte  0, $01, %00000000,  8
    .byte  8, $02, %00000000,  0
    .byte  8, $03, %00000000,  8
    .byte 16, $0B, %00000000,  0
    .byte 16, $0C, %00000000,  8
    .byte METASPRITE_END
AgentRunR1:
    .byte  0, $00, %00000000,  0
    .byte  0, $01, %00000000,  8
    .byte  8, $02, %00000000,  0
    .byte  8, $03, %00000000,  8
    .byte 16, $0D, %00000000,  0
    .byte 16, $0E, %00000000,  8
    .byte METASPRITE_END
AgentRunR2:
    .byte  0, $00, %00000000,  0
    .byte  0, $01, %00000000,  8
    .byte  8, $02, %00000000,  0
    .byte  8, $03, %00000000,  8
    .byte 16, $0F, %00000000,  0
    .byte 16, $10, %00000000,  8
    .byte METASPRITE_END
AgentRunR3:
    .byte  0, $00, %00000000,  0
    .byte  0, $01, %00000000,  8
    .byte  8, $02, %00000000,  0
    .byte  8, $03, %00000000,  8
    .byte 16, $11, %00000000,  0
    .byte 16, $12, %00000000,  8
    .byte METASPRITE_END
AgentRunR4:
    .byte  0, $00, %00000000,  0
    .byte  0, $01, %00000000,  8
    .byte  8, $02, %00000000,  0
    .byte  8, $03, %00000000,  8
    .byte 16, $13, %00000000,  0
    .byte 16, $14, %00000000,  8
    .byte METASPRITE_END
AgentRunR5:
    .byte  0, $00, %00000000,  0
    .byte  0, $01, %00000000,  8
    .byte  8, $02, %00000000,  0
    .byte  8, $03, %00000000,  8
    .byte 16, $15, %00000000,  0
    .byte 16, $16, %00000000,  8
    .byte METASPRITE_END
AgentRunR6:
    .byte  0, $00, %00000000,  0
    .byte  0, $01, %00000000,  8
    .byte  8, $02, %00000000,  0
    .byte  8, $03, %00000000,  8
    .byte 16, $17, %00000000,  0
    .byte 16, $18, %00000000,  8
    .byte METASPRITE_END
AgentRunR7:
    .byte  0, $00, %00000000,  0
    .byte  0, $01, %00000000,  8
    .byte  8, $02, %00000000,  0
    .byte  8, $03, %00000000,  8
    .byte 16, $19, %00000000,  0
    .byte 16, $05, %00000000,  8
    .byte METASPRITE_END
AgentRunL0:
    .byte  0, $00, %01000000,  8
    .byte  0, $01, %01000000,  0
    .byte  8, $02, %01000000,  8
    .byte  8, $03, %01000000,  0
    .byte 16, $0B, %01000000,  8
    .byte 16, $0C, %01000000,  0
    .byte METASPRITE_END
AgentRunL1:
    .byte  0, $00, %01000000,  8
    .byte  0, $01, %01000000,  0
    .byte  8, $02, %01000000,  8
    .byte  8, $03, %01000000,  0
    .byte 16, $0D, %01000000,  8
    .byte 16, $0E, %01000000,  0
    .byte METASPRITE_END
AgentRunL2:
    .byte  0, $00, %01000000,  8
    .byte  0, $01, %01000000,  0
    .byte  8, $02, %01000000,  8
    .byte  8, $03, %01000000,  0
    .byte 16, $0F, %01000000,  8
    .byte 16, $10, %01000000,  0
    .byte METASPRITE_END
AgentRunL3:
    .byte  0, $00, %01000000,  8
    .byte  0, $01, %01000000,  0
    .byte  8, $02, %01000000,  8
    .byte  8, $03, %01000000,  0
    .byte 16, $11, %01000000,  8
    .byte 16, $12, %01000000,  0
    .byte METASPRITE_END
AgentRunL4:
    .byte  0, $00, %01000000,  8
    .byte  0, $01, %01000000,  0
    .byte  8, $02, %01000000,  8
    .byte  8, $03, %01000000,  0
    .byte 16, $13, %01000000,  8
    .byte 16, $14, %01000000,  0
    .byte METASPRITE_END
AgentRunL5:
    .byte  0, $00, %01000000,  8
    .byte  0, $01, %01000000,  0
    .byte  8, $02, %01000000,  8
    .byte  8, $03, %01000000,  0
    .byte 16, $15, %01000000,  8
    .byte 16, $16, %01000000,  0
    .byte METASPRITE_END
AgentRunL6:
    .byte  0, $00, %01000000,  8
    .byte  0, $01, %01000000,  0
    .byte  8, $02, %01000000,  8
    .byte  8, $03, %01000000,  0
    .byte 16, $17, %01000000,  8
    .byte 16, $18, %01000000,  0
    .byte METASPRITE_END
AgentRunL7:
    .byte  0, $00, %01000000,  8
    .byte  0, $01, %01000000,  0
    .byte  8, $02, %01000000,  8
    .byte  8, $03, %01000000,  0
    .byte 16, $19, %01000000,  8
    .byte 16, $05, %01000000,  0
    .byte METASPRITE_END
AgentJumpR0:
    .byte  0, $00, %00000000,  0
    .byte  0, $01, %00000000,  8
    .byte  8, $02, %00000000,  0
    .byte  8, $03, %00000000,  8
    .byte 16, $1A, %00000000,  0
    .byte 16, $1B, %00000000,  8
    .byte METASPRITE_END
AgentJumpR1:
    .byte  0, $00, %00000000,  0
    .byte  0, $01, %00000000,  8
    .byte  8, $02, %00000000,  0
    .byte  8, $03, %00000000,  8
    .byte 16, $1C, %00000000,  0
    .byte 16, $1D, %00000000,  8
    .byte METASPRITE_END
AgentJumpL0:
    .byte  0, $00, %01000000,  8
    .byte  0, $01, %01000000,  0
    .byte  8, $02, %01000000,  8
    .byte  8, $03, %01000000,  0
    .byte 16, $1A, %01000000,  8
    .byte 16, $1B, %01000000,  0
    .byte METASPRITE_END
AgentJumpL1:
    .byte  0, $00, %01000000,  8
    .byte  0, $01, %01000000,  0
    .byte  8, $02, %01000000,  8
    .byte  8, $03, %01000000,  0
    .byte 16, $1C, %01000000,  8
    .byte 16, $1D, %01000000,  0
    .byte METASPRITE_END
AgentFallR0:
    .byte  0, $00, %00000000,  0
    .byte  0, $01, %00000000,  8
    .byte  8, $02, %00000000,  0
    .byte  8, $03, %00000000,  8
    .byte 16, $1E, %00000000,  0
    .byte 16, $1F, %00000000,  8
    .byte METASPRITE_END
AgentFallL0:
    .byte  0, $00, %01000000,  8
    .byte  0, $01, %01000000,  0
    .byte  8, $02, %01000000,  8
    .byte  8, $03, %01000000,  0
    .byte 16, $1E, %01000000,  8
    .byte 16, $1F, %01000000,  0
    .byte METASPRITE_END
AgentCrouchR0:
    .byte  8, $00, %00000000,  0
    .byte  8, $01, %00000000,  8
    .byte 16, $20, %00000000,  0
    .byte 16, $21, %00000000,  8
    .byte METASPRITE_END
AgentCrouchR1:
    .byte  8, $00, %00000000,  0
    .byte  8, $01, %00000000,  8
    .byte 16, $22, %00000000,  0
    .byte 16, $23, %00000000,  8
    .byte METASPRITE_END
AgentCrouchL0:
    .byte  8, $00, %01000000,  8
    .byte  8, $01, %01000000,  0
    .byte 16, $20, %01000000,  8
    .byte 16, $21, %01000000,  0
    .byte METASPRITE_END
AgentCrouchL1:
    .byte  8, $00, %01000000,  8
    .byte  8, $01, %01000000,  0
    .byte 16, $22, %01000000,  8
    .byte 16, $23, %01000000,  0
    .byte METASPRITE_END
AgentRollR0:
    .byte  8, $24, %00000000,  0
    .byte  8, $25, %00000000,  8
    .byte 16, $26, %00000000,  0
    .byte 16, $27, %00000000,  8
    .byte METASPRITE_END
AgentRollR1:
    .byte  8, $28, %00000000,  0
    .byte  8, $29, %00000000,  8
    .byte 16, $2A, %00000000,  0
    .byte 16, $2B, %00000000,  8
    .byte METASPRITE_END
AgentRollR2:
    .byte  8, $29, %01000000,  0
    .byte  8, $28, %01000000,  8
    .byte 16, $2B, %01000000,  0
    .byte 16, $2A, %01000000,  8
    .byte METASPRITE_END
AgentRollR3:
    .byte  8, $25, %01000000,  0
    .byte  8, $24, %01000000,  8
    .byte 16, $27, %01000000,  0
    .byte 16, $26, %01000000,  8
    .byte METASPRITE_END
AgentRollR4:
    .byte  8, $2C, %00000000,  0
    .byte  8, $29, %00000000,  8
    .byte 16, $2A, %00000000,  0
    .byte 16, $2D, %00000000,  8
    .byte METASPRITE_END
AgentRollR5:
    .byte  8, $29, %01000000,  0
    .byte  8, $2C, %01000000,  8
    .byte 16, $2D, %01000000,  0
    .byte 16, $2A, %01000000,  8
    .byte METASPRITE_END
AgentRollL0:
    .byte  8, $24, %01000000,  8
    .byte  8, $25, %01000000,  0
    .byte 16, $26, %01000000,  8
    .byte 16, $27, %01000000,  0
    .byte METASPRITE_END
AgentRollL1:
    .byte  8, $28, %01000000,  8
    .byte  8, $29, %01000000,  0
    .byte 16, $2A, %01000000,  8
    .byte 16, $2B, %01000000,  0
    .byte METASPRITE_END
AgentRollL2:
    .byte  8, $29, %00000000,  8
    .byte  8, $28, %00000000,  0
    .byte 16, $2B, %00000000,  8
    .byte 16, $2A, %00000000,  0
    .byte METASPRITE_END
AgentRollL3:
    .byte  8, $25, %00000000,  8
    .byte  8, $24, %00000000,  0
    .byte 16, $27, %00000000,  8
    .byte 16, $26, %00000000,  0
    .byte METASPRITE_END
AgentRollL4:
    .byte  8, $2C, %01000000,  8
    .byte  8, $29, %01000000,  0
    .byte 16, $2A, %01000000,  8
    .byte 16, $2D, %01000000,  0
    .byte METASPRITE_END
AgentRollL5:
    .byte  8, $29, %00000000,  8
    .byte  8, $2C, %00000000,  0
    .byte 16, $2D, %00000000,  8
    .byte 16, $2A, %00000000,  0
    .byte METASPRITE_END
AgentShootStandR0:
    .byte  0, $2E, %00000000,  0
    .byte  0, $2F, %00000000,  8
    .byte  8, $30, %00000000,  0
    .byte  8, $31, %00000000,  8
    .byte 16, $04, %00000000,  0
    .byte 16, $05, %00000000,  8
    .byte METASPRITE_END
AgentShootStandL0:
    .byte  0, $2E, %01000000,  8
    .byte  0, $2F, %01000000,  0
    .byte  8, $30, %01000000,  8
    .byte  8, $31, %01000000,  0
    .byte 16, $04, %01000000,  8
    .byte 16, $05, %01000000,  0
    .byte METASPRITE_END
AgentShootUpR0:
    .byte  0, $00, %00000000,  0
    .byte  0, $32, %00000000,  8
    .byte  8, $33, %00000000,  0
    .byte  8, $34, %00000000,  8
    .byte 16, $04, %00000000,  0
    .byte 16, $05, %00000000,  8
    .byte METASPRITE_END
AgentShootUpL0:
    .byte  0, $00, %01000000,  8
    .byte  0, $32, %01000000,  0
    .byte  8, $33, %01000000,  8
    .byte  8, $34, %01000000,  0
    .byte 16, $04, %01000000,  8
    .byte 16, $05, %01000000,  0
    .byte METASPRITE_END
AgentHurtR0:
    .byte  0, $35, %00000000,  0
    .byte  0, $36, %00000000,  8
    .byte  8, $37, %00000000,  0
    .byte  8, $38, %00000000,  8
    .byte 16, $39, %00000000,  0
    .byte 16, $3A, %00000000,  8
    .byte METASPRITE_END
AgentHurtL0:
    .byte  0, $35, %01000000,  8
    .byte  0, $36, %01000000,  0
    .byte  8, $37, %01000000,  8
    .byte  8, $38, %01000000,  0
    .byte 16, $39, %01000000,  8
    .byte 16, $3A, %01000000,  0
    .byte METASPRITE_END
RobotR0:
    .byte  0, $3B, %00000010,  0
    .byte  0, $3C, %00000010,  8
    .byte  8, $3D, %00000010,  0
    .byte  8, $3E, %00000010,  8
    .byte 16, $3F, %00000010,  0
    .byte 16, $40, %00000010,  8
    .byte METASPRITE_END
RobotR1:
    .byte  0, $3B, %00000010,  0
    .byte  0, $3C, %00000010,  8
    .byte  8, $3D, %00000010,  0
    .byte  8, $3E, %00000010,  8
    .byte 16, $41, %00000010,  0
    .byte 16, $42, %00000010,  8
    .byte METASPRITE_END
RobotR2:
    .byte  0, $3B, %00000010,  0
    .byte  0, $3C, %00000010,  8
    .byte  8, $3D, %00000010,  0
    .byte  8, $3E, %00000010,  8
    .byte 16, $43, %00000010,  0
    .byte 16, $44, %00000010,  8
    .byte METASPRITE_END
RobotR3:
    .byte  0, $3B, %00000010,  0
    .byte  0, $3C, %00000010,  8
    .byte  8, $3D, %00000010,  0
    .byte  8, $3E, %00000010,  8
    .byte 16, $41, %00000010,  0
    .byte 16, $42, %00000010,  8
    .byte METASPRITE_END
RobotR4:
    .byte  0, $3B, %00000010,  0
    .byte  0, $3C, %00000010,  8
    .byte  8, $45, %00000010,  0
    .byte  8, $46, %00000010,  8
    .byte 16, $3F, %00000010,  0
    .byte 16, $40, %00000010,  8
    .byte METASPRITE_END
RobotL0:
    .byte  0, $3B, %01000010,  8
    .byte  0, $3C, %01000010,  0
    .byte  8, $3D, %01000010,  8
    .byte  8, $3E, %01000010,  0
    .byte 16, $3F, %01000010,  8
    .byte 16, $40, %01000010,  0
    .byte METASPRITE_END
RobotL1:
    .byte  0, $3B, %01000010,  8
    .byte  0, $3C, %01000010,  0
    .byte  8, $3D, %01000010,  8
    .byte  8, $3E, %01000010,  0
    .byte 16, $41, %01000010,  8
    .byte 16, $42, %01000010,  0
    .byte METASPRITE_END
RobotL2:
    .byte  0, $3B, %01000010,  8
    .byte  0, $3C, %01000010,  0
    .byte  8, $3D, %01000010,  8
    .byte  8, $3E, %01000010,  0
    .byte 16, $43, %01000010,  8
    .byte 16, $44, %01000010,  0
    .byte METASPRITE_END
RobotL3:
    .byte  0, $3B, %01000010,  8
    .byte  0, $3C, %01000010,  0
    .byte  8, $3D, %01000010,  8
    .byte  8, $3E, %01000010,  0
    .byte 16, $41, %01000010,  8
    .byte 16, $42, %01000010,  0
    .byte METASPRITE_END
RobotL4:
    .byte  0, $3B, %01000010,  8
    .byte  0, $3C, %01000010,  0
    .byte  8, $45, %01000010,  8
    .byte  8, $46, %01000010,  0
    .byte 16, $3F, %01000010,  8
    .byte 16, $40, %01000010,  0
    .byte METASPRITE_END
TurretR0:
    .byte  0, $47, %00000010,  0
    .byte  0, $48, %00000010,  8
    .byte  8, $49, %00000010,  0
    .byte  8, $4A, %00000010,  8
    .byte METASPRITE_END
TurretR1:
    .byte  0, $47, %00000010,  0
    .byte  0, $4B, %00000010,  8
    .byte  8, $49, %00000010,  0
    .byte  8, $4C, %00000010,  8
    .byte METASPRITE_END
TurretR2:
    .byte  0, $4D, %00000010,  0
    .byte  0, $4E, %00000010,  8
    .byte  8, $49, %00000010,  0
    .byte  8, $4C, %00000010,  8
    .byte METASPRITE_END
TurretL0:
    .byte  0, $47, %01000010,  8
    .byte  0, $48, %01000010,  0
    .byte  8, $49, %01000010,  8
    .byte  8, $4A, %01000010,  0
    .byte METASPRITE_END
TurretL1:
    .byte  0, $47, %01000010,  8
    .byte  0, $4B, %01000010,  0
    .byte  8, $49, %01000010,  8
    .byte  8, $4C, %01000010,  0
    .byte METASPRITE_END
TurretL2:
    .byte  0, $4D, %01000010,  8
    .byte  0, $4E, %01000010,  0
    .byte  8, $49, %01000010,  8
    .byte  8, $4C, %01000010,  0
    .byte METASPRITE_END
Drone0:
    .byte  0, $4F, %00000011,  0
    .byte  0, $4F, %01000011,  8
    .byte  8, $50, %00000011,  0
    .byte  8, $50, %01000011,  8
    .byte METASPRITE_END
Drone1:
    .byte  0, $51, %00000011,  0
    .byte  0, $51, %01000011,  8
    .byte  8, $52, %00000011,  0
    .byte  8, $52, %01000011,  8
    .byte METASPRITE_END
Drone2:
    .byte  0, $53, %00000011,  0
    .byte  0, $53, %01000011,  8
    .byte  8, $50, %00000011,  0
    .byte  8, $50, %01000011,  8
    .byte METASPRITE_END
Drone3:
    .byte  0, $51, %00000011,  0
    .byte  0, $51, %01000011,  8
    .byte  8, $52, %00000011,  0
    .byte  8, $52, %01000011,  8
    .byte METASPRITE_END
ShotH0:
    .byte  0, $54, %00000000,  0
    .byte METASPRITE_END
ShotV0:
    .byte  0, $55, %00000000,  0
    .byte METASPRITE_END
Orb0:
    .byte  0, $56, %00000011,  0
    .byte METASPRITE_END
Orb1:
    .byte  0, $57, %00000011,  0
    .byte METASPRITE_END
Muzzle0:
    .byte  0, $58, %00000001,  0
    .byte METASPRITE_END
Spark0:
    .byte  0, $59, %00000001,  0
    .byte METASPRITE_END
Spark1:
    .byte  0, $5A, %00000001,  0
    .byte METASPRITE_END
Spark2:
    .byte  0, $5B, %00000001,  0
    .byte METASPRITE_END
Explosion0:
    .byte  0, $5C, %00000001,  0
    .byte  0, $5D, %00000001,  8
    .byte  8, $5E, %00000001,  0
    .byte  8, $5F, %00000001,  8
    .byte METASPRITE_END
Explosion1:
    .byte  0, $60, %00000001,  0
    .byte  0, $61, %00000001,  8
    .byte  8, $62, %00000001,  0
    .byte  8, $63, %00000001,  8
    .byte METASPRITE_END
Explosion2:
    .byte  0, $64, %00000001,  0
    .byte  0, $65, %00000001,  8
    .byte  8, $66, %00000001,  0
    .byte  8, $67, %00000001,  8
    .byte METASPRITE_END
Explosion3:
    .byte  0, $68, %00000001,  0
    .byte  0, $69, %00000001,  8
    .byte  8, $6A, %00000001,  0
    .byte  8, $6B, %00000001,  8
    .byte METASPRITE_END

;; Paletas de sprite (4 x 4 cores)
SprPalettes:
    .byte $0F, $06, $16, $2C   ; 0
    .byte $0F, $27, $38, $30   ; 1
    .byte $0F, $00, $10, $16   ; 2
    .byte $0F, $03, $14, $34   ; 3
