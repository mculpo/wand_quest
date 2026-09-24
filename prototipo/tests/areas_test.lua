-- Passeio pelas áreas com os inimigos vivos (agente invencível), morte e
-- chegada na saída. Prints em tests/area_*.png e estado em tests/areas.txt.
--
-- Rodar: qfceux --loadlua tests\areas_test.lua agente.nes

local here = debug.getinfo(1, "S").source:match("^@?(.*[/\\])") or "./"
local A = {}
for line in io.lines(here .. "../agente.lbl") do
  local addr, name = line:match("^al (%x+) %.(%S+)")
  if addr then A[name] = tonumber(addr, 16) end
end
local rb, wb = memory.readbyte, memory.writebyte
local function r16(a) return rb(a) + 256 * rb(a + 1) end
local function w16(a, v) wb(a, v % 256); wb(a + 1, math.floor(v / 256) % 256) end
local log = io.open(here .. "areas.txt", "w")
local GS = {[0] = "PLAY", "PAUSE", "DEAD", "CLEAR"}

local function enemies()
  local t = {}
  for i = 0, 5 do
    local ty = rb(A.EType + i)
    if ty ~= 0 then
      t[#t + 1] = string.format("%s(%d,%d)", ({"robo", "torreta", "drone"})[ty], r16(A.EXL + i) % 65536,
        rb(A.EYL + i) + 256 * rb(A.EYH + i))
    end
  end
  return table.concat(t, " ")
end
local function note(tag)
  local eb = 0
  for i = 0, 5 do eb = eb + rb(A.EBActive + i) end
  log:write(string.format("%-16s jogo=%-5s agente=(%d,%d) vida=%d tiros-inimigos=%d  %s\n", tag,
    GS[rb(A.GameState)] or "?", r16(A.PX), r16(A.PY), rb(A.PHealth), eb, enemies()))
end
local function frames(n, invincible)
  for i = 1, n do
    if invincible then wb(A.PInvuln, 40); wb(A.PHealth, 12) end
    emu.frameadvance()
  end
end
-- Vai andando (teleporte de 3 px por frame) até o ponto, invencível
local function go(tx, ty)
  local px, py = r16(A.PX), r16(A.PY)
  for i = 1, 500 do
    if px < tx then px = math.min(tx, px + 3) elseif px > tx then px = math.max(tx, px - 3) end
    if py < ty then py = math.min(ty, py + 3) elseif py > ty then py = math.max(ty, py - 3) end
    w16(A.PX, px); w16(A.PY, py); wb(A.PVY, 0); wb(A.PVY + 1, 0)
    wb(A.PInvuln, 40); wb(A.PHealth, 12)
    emu.frameadvance()
    if px == tx and py == ty then break end
  end
end
local function shot(name) gui.savescreenshotas(here .. "area_" .. name .. ".png"); emu.frameadvance() end

frames(60, true)
go(760, 600); frames(90, true); note("poco (drones)"); shot("1_poco")
go(740, 250); frames(60, true); note("poco alto");     shot("2_poco_alto")
go(560, 150); frames(60, true); note("passarela");     shot("3_passarela")
go(300, 150); frames(90, true); note("torretas");      shot("4_torretas")

-- Morte: sem invencibilidade e com 2 de vida perto das torretas
wb(A.PInvuln, 0); wb(A.PHealth, 2)
for i = 1, 400 do
  emu.frameadvance()
  if rb(A.GameState) == 2 then break end
end
note("morreu"); frames(8, false); shot("5_morte")
for i = 1, 300 do
  emu.frameadvance()
  if rb(A.GameState) == 0 and rb(A.PHealth) == 12 then break end
end
frames(10, false); note("recomecou")

-- Saída: vai até a porta (coluna 29, linha 31)
go(29 * 16, 31 * 16 - 8)
frames(4, false); note("chegou na saida"); shot("6_missao")
for i = 1, 400 do
  emu.frameadvance()
  if rb(A.GameState) == 0 then break end
end
note("recomecou de novo")
log:close()
emu.exit()
