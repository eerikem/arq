local teleporter = {}
root = teleporter
local title = 'Teleport Control'
local TELE_DELAY = 1
local uis = {}
teleporter.uis = uis

INDEX = 0

local ui

local open = false

local cables = {
  admin = BUNDLE:new("bottom",colors.yellow,"AdminShortcut"),
  doors = BUNDLE:new("bottom",colors.magenta,"ChamberDoors"),
  piston = BUNDLE:new("bottom",colors.orange,"PistonDrop"),
  lights = BUNDLE:new("bottom",colors.white,"BackLights"),
  detector = BUNDLE:new("bottom",colors.lightBlue,"PlayerDetector")
  }


local function teleport()
  if open then
    ui:printCentered("Close teleport bay.",10)
    waitSeconds(1.5)
    ui:printCentered("                   ",10)
  else
    writeStatus("Teleporting...")
  end
end

local function detector()
  while true do
    local event = waitSignal("redstone")
    writeStatus("rs signal: "..rs.getBundledInput("bottom"))
    if event == "terminate" then return end
    if cables.detector:isOn() then
      writeStatus("Enabled lights ")
      cables.lights:enable()
    else
      writeStatus("Disabled lights")
      cables.lights:disable()
    end
  end
end

local function doorMenu()
  if open then return "CLOSE Teleport Bay"
  else return "OPEN  Teleport Bay"
  end
end

local function testCable(cable)
  INDEX = INDEX + 1
  local v =  redstone.getBundledOutput(cable.side)
  writeStatus(INDEX .. " output: " .. v)
  --if v then
  --  writeStatus(INDEX .. " Cable is on!")
  --else
  --  writeStatus(INDEX .. " Cable is off.")
  --end
end

local function adminSignal()
  while true do
    local event = waitSignal("redstone")
    if event == "terminate" then return ok end
    if rs.testBundledInput(signals.admin.side,signals.admin.cable) then
      signal("teleport")
    end
  end
end

local function toggleDoors()
  if open then
    open = false
    cables.doors:disable()
  else
    open = true
    cables.doors:enable()
  end
end

local function exit()
  if ui:yesNo("Shutdown?") then
    ui:terminate()
    os.queueEvent("terminate")
    return "kill"
  end
end

local root = {
  doorMenu, toggleDoors,
  "Teleport", teleport,
  "Shutdown", exit
  }

teleporter.init = function()
  writeStatus("Setting "..cables.piston.side .. " to 0")
  rs.setBundledOutput(cables.piston.side,0)
  ui = UI:aquireMonitor("monitor_28")
  table.insert(uis,ui)
  ui.clear()
end
 
teleporter.main = function()
  local m = ui:readMenu(root)
  local co1 = runProcess(m.cycle,"teleporter_main")
  local co2 = runProcess(detector,"teleDetector")
  return co1, co2
end
