-- Teste automático: joga as 20 fases no FCEUX seguindo as soluções do solver.
--
-- Rodar:   makefile test
-- (ou)     qfceux --loadlua tests\levels_test.lua wandquest.nes
--
-- Precisa de:
--   wandquest.lbl       gerado pelo "makefile build" (endereços das variáveis)
--   tests/solutions.lua gerado pelo "makefile solve" (tools/solver.py)
-- Resultado em tests/result.txt (termina com ALL PASSED ou FAILURES: n).
--
-- Para cada empurrão da solução o teste teleporta o player para a célula de
-- onde atirar (o solver já provou que dá para andar até lá), aperta a
-- direção (o player vira para o bloco), aperta B para atirar a bola de pedra
-- e espera a bola bater e o bloco parar.
-- Os inimigos são removidos antes de cada tiro: o teste confere o puzzle,
-- e os inimigos não fazem parte dele (ver tools/solver.py).

local here = debug.getinfo(1, "S").source:match("^@?(.*[/\\])") or "./"
local root = here .. "../"
local out = io.open(here .. "result.txt", "w")
local fails = 0

local function log(fmt, ...)
  local s = string.format(fmt, ...)
  out:write(s .. "\n"); out:flush()
  print(s)
end
local function check(name, ok, detail)
  if not ok then fails = fails + 1 end
  log("%-4s %s%s", ok and "OK" or "FAIL", name, detail and ("  (" .. detail .. ")") or "")
  return ok
end

-- Endereços das variáveis a partir do arquivo de labels do ld65
local A = {}
for line in io.lines(root .. "wandquest.lbl") do
  local addr, name = line:match("^al (%x+) %.(%S+)")
  if addr then A[name] = tonumber(addr, 16) end
end
for _, n in ipairs({"GameState", "CurrentLevel", "NextLevel", "TransitionPhase", "PlayerX", "PlayerY",
                    "BlockType", "BlockX", "BlockY", "BlockSide", "BallActive", "EnemyType"}) do
  assert(A[n], "label não encontrado no wandquest.lbl: " .. n)
end

local solutions = dofile(here .. "solutions.lua")
local MAX_BLOCKS, LEVEL_COUNT = 5, #solutions
local PLAYING, TRANSITION = 1, 5      -- enum State
local SHOW_TEXT = 1                   -- enum TransPhase

local rb, wb = memory.readbyte, memory.writebyte
local function hold(pad, n) for _ = 1, n or 1 do joypad.set(1, pad); emu.frameadvance() end end
local function idle(n) hold({}, n) end
local function waitFor(cond, max)
  for _ = 1, max do
    if cond() then return true end
    idle(1)
  end
  return cond()
end
local function blockAt(px, py)
  for i = 0, MAX_BLOCKS - 1 do
    if rb(A.BlockType + i) ~= 0 and rb(A.BlockX + i) == px and rb(A.BlockY + i) == py then return i end
  end
end
local function anyBlockMoving()
  for i = 0, MAX_BLOCKS - 1 do
    if rb(A.BlockType + i) ~= 0 and rb(A.BlockSide + i) ~= 0 then return true end
  end
  return false
end
local DELTA = {up = {0, -1}, down = {0, 1}, left = {-1, 0}, right = {1, 0}}

-- Primeiro bloco na linha de tiro a partir da célula (col, row) na direção d
local function firstBlockInLine(col, row, d)
  for k = 1, 16 do
    local i = blockAt((col + d[1] * k) * 16, (row + d[2] * k) * 16)
    if i then return i end
  end
end

local MAX_ENEMIES = 4
local function clearEnemies()
  for i = 0, MAX_ENEMIES - 1 do wb(A.EnemyType + i, 0) end
end

local function push(step)
  clearEnemies()
  wb(A.PlayerX, step.col * 16)
  wb(A.PlayerY, step.row * 16)
  idle(1)
  local d = DELTA[step.dir]
  local target = firstBlockInLine(step.col, step.row, d)
  hold({[step.dir] = true}, 1)          -- Vira para o bloco (pode andar 1 px na direção dele)
  idle(1)
  hold({B = true}, 1)                   -- Atira a bola de pedra
  idle(1)
  waitFor(function() return rb(A.BallActive) == 0 and not anyBlockMoving() end, 200)
  local ok = target ~= nil and rb(A.BlockX + target) == step.endcol * 16 and rb(A.BlockY + target) == step.endrow * 16
  return ok, target
end

log("Esperando a fase 1...")
check("jogo começa na fase 1", waitFor(function() return rb(A.GameState) == PLAYING end, 400)
      and rb(A.CurrentLevel) == 0)

-- Select reinicia a fase: na primeira fase com mais de um empurrão, faz o
-- primeiro empurrão, aperta e solta o Select e confere que o bloco voltou.
local function testRestart(n, steps)
  local first = steps[1]
  local d = DELTA[first.dir]
  local i = firstBlockInLine(first.col, first.row, d)
  local sx, sy = rb(A.BlockX + i), rb(A.BlockY + i)
  push(first)
  hold({select = true}, 3); idle(1)
  local restarted = waitFor(function() return rb(A.GameState) == TRANSITION end, 10)
                    and waitFor(function() return rb(A.GameState) == PLAYING end, 400)
  check(string.format("Select reinicia a fase %02d", n),
        restarted and rb(A.CurrentLevel) == n - 1 and blockAt(sx, sy) ~= nil)
end
local restartTested = false

for n, steps in ipairs(solutions) do
  if not restartTested and #steps > 1 and rb(A.GameState) == PLAYING then
    testRestart(n, steps)
    restartTested = true
  end
  local ok = rb(A.GameState) == PLAYING and rb(A.CurrentLevel) == n - 1
  for k, step in ipairs(steps) do
    if not ok then break end
    local moved, target = push(step)
    if not moved then
      ok = false
      check(string.format("fase %02d, empurrão %d", n, k), false,
            string.format("bloco %s não parou em col %d, lin %d", tostring(target), step.endcol, step.endrow))
    end
  end
  if ok then
    local done
    if n < LEVEL_COUNT then
      done = waitFor(function() return rb(A.GameState) == PLAYING and rb(A.CurrentLevel) == n end, 600)
    else
      done = waitFor(function()
        return rb(A.GameState) == TRANSITION and rb(A.TransitionPhase) == SHOW_TEXT and rb(A.NextLevel) == LEVEL_COUNT
      end, 600)
    end
    check(string.format("fase %02d resolvida (%d empurrões)", n, #steps), done)
  else
    check(string.format("fase %02d resolvida", n), false)
    break
  end
end

-- Tela final: "PARABENS!" e Start volta para a fase 1
idle(20)
gui.savescreenshotas(here .. "ending.png"); idle(1)
hold({start = true}, 2)
check("Start na tela final volta para a fase 1",
      waitFor(function() return rb(A.GameState) == PLAYING and rb(A.CurrentLevel) == 0 end, 400))

log(fails == 0 and "ALL PASSED" or ("FAILURES: " .. fails))
out:close()
emu.exit()
