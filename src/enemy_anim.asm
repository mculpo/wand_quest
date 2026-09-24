;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Animações dos inimigos. Gerado por tools/draw_sprites.py (não edite à mão).
;;
;; EnemyAnimByType dá a tabela de ENEMY_FRAMES quadros de cada tipo, na
;; ordem do enum EnemyType (a partir de SLIME = 1). PoofFrames é a
;; animação de morte, igual para todos.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ENEMY_FRAMES = 4
POOF_FRAMES  = 4

EnemyAnimByType:
    .word SlimeFrames
    .word BatFrames
    .word SpiderFrames
    .word GhostFrames

SlimeFrames:
    .word Slime0
    .word Slime1
    .word Slime2
    .word Slime3

BatFrames:
    .word Bat0
    .word Bat1
    .word Bat2
    .word Bat3

SpiderFrames:
    .word Spider0
    .word Spider1
    .word Spider2
    .word Spider3

GhostFrames:
    .word Ghost0
    .word Ghost1
    .word Ghost2
    .word Ghost3

PoofFrames:
    .word Poof0
    .word Poof1
    .word Poof2
    .word Poof3

;;     dy  tile  atributo    dx
Slime0:
    .byte  0, $B6, %00000001,  0
    .byte  0, $B6, %01000001,  8
    .byte  8, $B7, %00000001,  0
    .byte  8, $B8, %00000001,  8
    .byte METASPRITE_END
Slime1:
    .byte  8, $B9, %00000001,  0
    .byte  8, $BA, %00000001,  8
    .byte METASPRITE_END
Slime2:
    .byte  0, $B6, %00000001,  0
    .byte  0, $B6, %01000001,  8
    .byte  8, $B7, %00000001,  0
    .byte  8, $B8, %00000001,  8
    .byte METASPRITE_END
Slime3:
    .byte  0, $BB, %00000001,  0
    .byte  0, $BC, %00000001,  8
    .byte  8, $BD, %00000001,  0
    .byte  8, $BD, %01000001,  8
    .byte METASPRITE_END

Bat0:
    .byte  0, $BE, %00000001,  0
    .byte  0, $BF, %00000001,  8
    .byte  8, $C0, %00000001,  0
    .byte  8, $C0, %01000001,  8
    .byte METASPRITE_END
Bat1:
    .byte  0, $C1, %00000001,  0
    .byte  0, $C1, %01000001,  8
    .byte  8, $C2, %00000001,  0
    .byte  8, $C3, %00000001,  8
    .byte METASPRITE_END
Bat2:
    .byte  0, $C4, %00000001,  0
    .byte  0, $C4, %01000001,  8
    .byte  8, $C5, %00000001,  0
    .byte  8, $C6, %00000001,  8
    .byte METASPRITE_END
Bat3:
    .byte  0, $C1, %00000001,  0
    .byte  0, $C1, %01000001,  8
    .byte  8, $C2, %00000001,  0
    .byte  8, $C3, %00000001,  8
    .byte METASPRITE_END

Spider0:
    .byte  0, $C7, %00000001,  0
    .byte  0, $C7, %01000001,  8
    .byte  8, $C8, %00000001,  0
    .byte  8, $C8, %01000001,  8
    .byte METASPRITE_END
Spider1:
    .byte  0, $C9, %00000001,  0
    .byte  0, $C9, %01000001,  8
    .byte  8, $CA, %00000001,  0
    .byte  8, $CA, %01000001,  8
    .byte METASPRITE_END
Spider2:
    .byte  0, $C7, %00000001,  0
    .byte  0, $C7, %01000001,  8
    .byte  8, $C8, %00000001,  0
    .byte  8, $C8, %01000001,  8
    .byte METASPRITE_END
Spider3:
    .byte  0, $C9, %00000001,  0
    .byte  0, $C9, %01000001,  8
    .byte  8, $CA, %00000001,  0
    .byte  8, $CA, %01000001,  8
    .byte METASPRITE_END

Ghost0:
    .byte  0, $CB, %00000001,  0
    .byte  0, $CC, %00000001,  8
    .byte  8, $CD, %00000001,  0
    .byte  8, $CE, %00000001,  8
    .byte METASPRITE_END
Ghost1:
    .byte  0, $CF, %00000001,  0
    .byte  0, $D0, %00000001,  8
    .byte  8, $D1, %00000001,  0
    .byte  8, $D2, %00000001,  8
    .byte METASPRITE_END
Ghost2:
    .byte  0, $D3, %00000001,  0
    .byte  0, $D3, %01000001,  8
    .byte  8, $D4, %00000001,  0
    .byte  8, $D5, %00000001,  8
    .byte METASPRITE_END
Ghost3:
    .byte  0, $CF, %00000001,  0
    .byte  0, $D0, %00000001,  8
    .byte  8, $D1, %00000001,  0
    .byte  8, $D2, %00000001,  8
    .byte METASPRITE_END

Poof0:
    .byte  0, $D6, %00000001,  0
    .byte  0, $D7, %00000001,  8
    .byte  8, $D8, %00000001,  0
    .byte  8, $D9, %00000001,  8
    .byte METASPRITE_END
Poof1:
    .byte  0, $DA, %00000001,  0
    .byte  0, $DB, %00000001,  8
    .byte  8, $DC, %00000001,  0
    .byte  8, $DD, %00000001,  8
    .byte METASPRITE_END
Poof2:
    .byte  0, $DE, %00000001,  0
    .byte  0, $DF, %00000001,  8
    .byte  8, $E0, %00000001,  0
    .byte  8, $E1, %00000001,  8
    .byte METASPRITE_END
Poof3:
    .byte  0, $E2, %00000001,  0
    .byte  0, $E3, %00000001,  8
    .byte  8, $E4, %00000001,  0
    .byte  8, $E5, %00000001,  8
    .byte METASPRITE_END
