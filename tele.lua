local teleporter = {}
root = teleporter
local title = 'Teleport Control'
local TELE_DELAY = 1
local uis = {}
teleporter.uis = uis

INDEX = 0

local ui
local TELE_SOUND = "/playsound frontierdevelopment:event.event_teleport @a 268 62 275 2"

local open = false

local cables = {
  admin = BUNDLE:new("bottom",colors.yellow,"AdminShortcut"),
  doors = BUNDLE:new("bottom",colors.magenta,"ChamberDoors"),
  piston = BUNDLE:new("bottom",colors.orange,"PistonDrop"),
  light = BUNDLE:new("bottom",colors.white,"BackLight"),
  lights = BUNDLE:new("bottom",colors.lime,"OverheadLights"),
  detector = BUNDLE:new("bottom",colors.lightBlue,"PlayerDetector"),
  teleport = BUNDLE:new("bottom",colors.pink,"TeleportCmd")
  }

local function toggleDoors()
  if open then
    open = false
    cables.doors:disable()
  else
    open = true
    cables.doors:enable()
  end
end

local function teleport(ui)
  if open then
    ui:printCentered("Close teleport bay.",10)
    waitSeconds(1.5)
    ui:printCentered("                   ",10)
  else
    if cables.detector:isOn() then
      writeStatus("Teleporting...")
      ui:clear()
      ui:printCentered("Power Diversion Active",3)
      ui.exec(TELE_SOUND)
      cables.lights:disable()
      ui:showDelay(4,4)
      cables.light:enable()
      ui:printCentered("Initializing Quantum Analyzer",3)
      ui:showDelayTwo(2,4)
      cables.piston:enable()
      waitSeconds(0.2)
      cables.teleport:enable()
      ui:printCentered("Executing Teleport Sequence",3)
      waitSeconds(1)
      ui:printCentered("Sequence Complete!",3)
      cables.light:disable()
      cables.piston:disable()
      cables.teleport:disable()
      cables.lights:enable()
      ui.menu.draw()
    else
      ui:clear()
      ui:printCentered("Teleporter empty!",3)
      writeStatus("No one in the teleporter")
      waitSeconds(1)
      ui.menu.draw()
    end
  end
end

local function detector()
  while true do
    local event = waitSignal("redstone")
    writeStatus("rs signal: "..rs.getBundledInput("bottom"))
    if event == "terminate" then return end
    if cables.detector:isOn() and open then
      writeStatus("Closing Door!")
      toggleDoors()
      signal("menuUpdate")
    else
      writeStatus("Detector OFF")
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
    writeStatus("Admin Teleport!")
    if event == "terminate" then return ok end
    if cables.admin:isOn() then
      writeStatus("Admin Teleport!")
      teleport(ui)
    end
  end
end

local function exit()
  --if ui:yesNo("Shutdown?") then
    cables.lights:disable()
    ui:terminate()
    os.queueEvent("terminate")
    return "kill"
  --end
end

local root = {
  doorMenu, toggleDoors,
  "Teleport", teleport,
  "Shutdown", exit
  }

teleporter.init = function()
  writeStatus("Setting "..cables.piston.side .. " to 0")
  rs.setBundledOutput(cables.piston.side,0)
  cables.lights:enable()
  ui = UI:aquireMonitor("monitor_28")
  table.insert(uis,ui)
  ui.clear()
end
 
teleporter.main = function()
  local m = ui:readMenu(root)
  m.setTitle(title)
  ui.menu = m
  local co1 = runProcess(m.cycle,"teleporter_main")
  local co2 = runProcess(detector,"teleDetector")
  local co3 = runProcess(adminSignal,"adminLstnr")
  return co1, co2, co3
end
