# Agente Vermelho — protótipo de metroidvania para NES

Protótipo inspirado no **Shatterhand** (NES, 1991): um agente do futuro de
armadura de soldado vermelha explora uma base futurista com **câmera livre
nos dois eixos**. Ele corre, pula, agacha, rola e atira.

## Como rodar

No `cmd`, dentro da pasta `prototipo`:

```bat
makefile build
makefile run
```

| Comando | O que faz |
|---|---|
| `makefile build` | Monta a ROM `agente.nes` (e `agente.lbl`, usado pelos testes) |
| `makefile run`   | Abre a ROM no FCEUX |
| `makefile art`   | Regenera o cenário, a fase e os sprites (`tools/*.py`) e monta a ROM |
| `makefile clean` | Apaga os arquivos gerados |

## Controles

| Botão | Ação |
|---|---|
| Esquerda / Direita | Anda (com aceleração curta) |
| **A** | Pula — segurar pula mais alto |
| **Baixo** | Agacha (tiro rente ao chão, desvia de tiros altos) |
| **Baixo + A** | Rola: rápido e invencível quase o tempo todo |
| **B** | Atira para frente |
| **Cima + B** | Atira para cima |
| **Start** | Pausa |

## A fase

Quatro áreas ligadas, para a câmera andar para todo lado:

1. **Corredor de entrada** (embaixo, à esquerda): soldados-robôs, um caixote e
   um fosso de espinhos.
2. **Poço vertical**: passarelas em zigue-zague para subir, com drones.
   Tem uma sala lateral com mais um soldado.
3. **Passarela de cima**, voltando para a esquerda: torretas, um soldado e
   dois fossos de espinhos.
4. **Descida** pela esquerda até a **sala final**, com a porta verde da saída.

Inimigos: **soldado-robô** (patrulha e atira quando te vê na mesma altura),
**torreta** (mira para frente, na diagonal ou para cima) e **drone** (voa atrás
de você e machuca no contato). Vida: 12 pontos (barra vertical à esquerda).
Encostar num inimigo ou levar tiro tira 2; espinho tira 3. Sem vida, o agente
explode e a fase recomeça.

## Editando

- **Fase**: `levels/level1.txt` é um mapa em texto de 64×48 (legenda no
  começo do arquivo). Depois rode `python tools/make_level.py` (ou
  `makefile art`); a fase inteira desenhada fica em `tools/level1_preview.png`.
- **Cenário**: `tools/draw_tiles.py` (desenhos dos metatiles 16×16 e as 4
  paletas de fundo).
- **Sprites**: `tools/draw_sprites.py` (agente, inimigos, tiros, efeitos,
  barra de vida e letras). A prévia fica em `tools/sprites_preview.png`.
- **Física, câmera e inimigos**: constantes em `inc/consts.inc`.

## Como funciona o scroll livre

Mirroring vertical: os dois nametables ficam lado a lado (512 px), então a
rolagem horizontal é perfeita. Na vertical o nametable tem a altura da tela
(240 px): a linha de tiles do mundo `tr` fica na linha `tr mod 30`, e a linha
nova aparece na borda de cima/baixo, escondida pelo overscan (como no Super
Mario Bros. 3). A cada 8 px que a câmera anda, o jogo monta **uma coluna** e/ou
**uma linha** de tiles (com os atributos) e a NMI copia para a PPU, com o
código desenrolado para caber no VBlank. Detalhes em `src/scroll.asm`.

## Testes (FCEUX + Lua)

```bat
qfceux --loadlua tests\scroll_test.lua agente.nes
python tests\compare_scroll.py
```
O primeiro passeia pelas 4 áreas e tira prints; o segundo compara o fundo de
cada print com a prévia da fase (acima de 99% = scroll e atributos certos).
`tests\gameplay_test.lua` e `tests\areas_test.lua` jogam as ações, os
inimigos, a morte e a saída e guardam prints e um relatório em `tests\`.
