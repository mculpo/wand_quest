-- Teste do scroll em 8 direções.
--
-- Leva o agente (por teleporte) por um percurso que passa pelas 4 áreas da
-- fase; a câmera segue sozinha. Em cada parada tira um print e anota a
-- posição da câmera (CamX, CamY, lidos da RAM). Depois o
-- tests/compare_scroll.py compara o fundo de cada print com o recorte da
-- prévia tools/level1_preview.png na mesma posição.
--
-- Rodar: qfceux --loadlua tests\scroll_test.lua agente.nes

local here = debug.getinfo(1, "S").source:match("^@?(.*[/\\])") or "./"
local root = here .. "../"
local A = {}
for line in io.lines(root .. "agente.lbl") do
  local addr, name = line:match("^al (%x+) %.(%S+)")
  if addr then A[name] = tonumber(addr, 16) end
end
local rb, wb = memory.readbyte, memory.writebyte
local function r16(a) return rb(a) + 256 * rb(a + 1) end
local function w16(a, v) wb(a, v % 256); wb(a + 1, math.floor(v / 256) % 256) end
local log = io.open(here .. "scroll_shots.txt", "w")

for i = 1, 60 do emu.frameadvance() end

-- Percurso em pixels do mundo (canto do agente). A câmera anda no máximo
-- 4 px/frame na horizontal e 6 na vertical, então o agente vai em passos
-- pequenos e o script espera ela alcançar.
local path = {
  {48, 688}, {400, 688}, {700, 688}, {760, 600}, {760, 420}, {740, 250},
  {720, 120}, {500, 150}, {250, 150}, {80, 150}, {80, 300}, {80, 450},
  {250, 470}, {450, 470}, {900, 690}, {960, 690},
}
local shot = 0
local px, py = r16(A.PX), r16(A.PY)
for _, target in ipairs(path) do
  local tx, ty = target[1], target[2]
  -- Anda até o alvo 3 px por frame (congela a física escrevendo a posição)
  for step = 1, 400 do
    if px < tx then px = math.min(tx, px + 3) elseif px > tx then px = math.max(tx, px - 3) end
    if py < ty then py = math.min(ty, py + 3) elseif py > ty then py = math.max(ty, py - 3) end
    w16(A.PX, px); w16(A.PY, py)
    wb(A.PVY, 0); wb(A.PVY + 1, 0); wb(A.PInvuln, 30)   -- Sem cair e sem levar dano
    wb(A.PHealth, 12)
    for i = 0, 5 do wb(A.EType + i, 0) end              -- Sem inimigos (só o cenário)
    emu.frameadvance()
    if px == tx and py == ty then break end
  end
  local still, lastX, lastY = 0, -1, -1                 -- Espera a câmera ficar parada 10 frames
  for i = 1, 200 do
    w16(A.PX, px); w16(A.PY, py); wb(A.PVY, 0); wb(A.PVY + 1, 0)
    wb(A.PHealth, 12); wb(A.PInvuln, 30)
    for k = 0, 5 do wb(A.EType + k, 0) end
    emu.frameadvance()
    local cx, cy = r16(A.CamX), r16(A.CamY)
    if cx == lastX and cy == lastY then still = still + 1 else still = 0 end
    lastX, lastY = cx, cy
    if still >= 10 then break end
  end
  shot = shot + 1
  local name = string.format("scroll_%02d.png", shot)
  gui.savescreenshotas(here .. name)
  emu.frameadvance()
  log:write(string.format("%s %d %d\n", name, r16(A.CamX), r16(A.CamY)))
end
log:close()
emu.exit()
