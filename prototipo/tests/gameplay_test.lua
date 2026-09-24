-- Teste de jogabilidade: joga com o controle (sem teletransporte) e tira
-- prints de cada ação. O resultado (estado do agente em cada passo) vai
-- para tests/gameplay.txt.
--
-- Rodar: qfceux --loadlua tests\gameplay_test.lua agente.nes

local here = debug.getinfo(1, "S").source:match("^@?(.*[/\\])") or "./"
local A = {}
for line in io.lines(here .. "../agente.lbl") do
  local addr, name = line:match("^al (%x+) %.(%S+)")
  if addr then A[name] = tonumber(addr, 16) end
end
local rb = memory.readbyte
local function r16(a) return rb(a) + 256 * rb(a + 1) end
local log = io.open(here .. "gameplay.txt", "w")
local STATES = {[0] = "NORMAL", "CROUCH", "ROLL", "HURT", "DEAD"}

local function state(tag)
  local enemies = {}
  for i = 0, 5 do
    if rb(A.EType + i) ~= 0 then
      enemies[#enemies + 1] = string.format("t%d@%d,%d hp%d", rb(A.EType + i), r16(A.EXL + i) , rb(A.EYL + i) + 256 * rb(A.EYH + i), rb(A.EHP + i))
    end
  end
  log:write(string.format("%-18s x=%4d y=%4d chao=%d estado=%-6s vida=%2d cam=(%d,%d) tiros=%d%d%d inimigos: %s\n",
    tag, r16(A.PX), r16(A.PY), rb(A.POnGround), STATES[rb(A.PState)] or "?", rb(A.PHealth),
    r16(A.CamX), r16(A.CamY), rb(A.PBActive), rb(A.PBActive + 1), rb(A.PBActive + 2), table.concat(enemies, " ")))
end
local function hold(pad, n) for i = 1, n do joypad.set(1, pad); emu.frameadvance() end end
local function shot(name) gui.savescreenshotas(here .. "gp_" .. name .. ".png"); emu.frameadvance() end

hold({}, 60);                           state("parado");       shot("01_parado")
hold({right = true}, 40);               state("correndo");     shot("02_correndo")
hold({right = true, A = true}, 16);     state("pulando");      shot("03_pulo")
hold({}, 40);                           state("pousou")
hold({down = true}, 10);                state("agachado");     shot("04_agachado")
hold({down = true, A = true}, 1); hold({down = true}, 8); state("rolando"); shot("05_rolando")
hold({}, 30)
hold({B = true}, 1); hold({}, 3);       state("tiro frente");  shot("06_tiro")
hold({}, 20)
hold({up = true, B = true}, 1); hold({up = true}, 3); state("tiro cima"); shot("07_tiro_cima")
hold({}, 20)
hold({down = true}, 6); hold({down = true, B = true}, 1); hold({down = true}, 3); state("tiro agachado"); shot("08_tiro_agachado")
hold({}, 20)
-- Anda até o primeiro soldado-robô e espera ele atirar
for i = 1, 120 do
  hold({right = true}, 1)
  if rb(A.PHealth) < 12 then break end
end
state("tomou dano");                    shot("09_dano")
hold({}, 30)
-- Atira no robô até ele explodir
local hits = 0
for i = 1, 30 do
  hold({B = true}, 1); hold({}, 5)
  local alive = false
  for k = 0, 5 do if rb(A.EType + k) == 1 then alive = true end end
  if not alive then break end
end
hold({}, 4);                            state("robo explodindo"); shot("10_explosao")
hold({}, 60);                           state("fim")
log:close()
emu.exit()
