local template = {}
root = template

local uis = {}
template.uis = uis

---------------------
--Private variables--
---------------------
local title = 'BATTERY'
local locked = false
local signal = BUNDLE:new("back",colors.yellow,"battery_charged")
local resetButton = BUNDLE:new("back",colors.white,"reset_button")

---------------------
--Private functions--
---------------------
local function writeScreen(ui)
  ui.setBackgroundColor(colors.gray)
  ui.clear()
  ui.setBackgroundColor(colors.lightGray)
  ui:wipe(7,1,1)
  ui:wipe(7,1,5)
  ui.setBackgroundColor(colors.lightGray)
  ui.setTextColor(colors.gray)
  ui:indentLeft(title,0,1)
  ui.setBackgroundColor(colors.gray)
  ui.setTextColor(colors.orange)
  ui:printCentered("Charge!",3)
end

local function charge()
  local ui = uis[1]
  ui.setTextColor(colors.orange)
  ui:printCentered("Charged",3)
  signal:enable()
  locked = true
end

local function chargeListener()
  local ui = uis[1]
  while not locked do
    local event, monitor, xPos, yPos = waitSignal("monitor_touch")
    if event == "terminate" then return end
    if monitor == ui.name and yPos == 3 then
      charge()
    end
  end
end

local function reset()
  while true do
    local event = waitSignal("redstone")
    if event == "terminate" then return end
    if resetButton:isOn() then
      if locked then
        signal:disable()
        locked = not locked
        writeScreen(uis[1])
        runProcess(chargeListener,"charge_listener")
      end
    end
  end
end

---------------------------
--Initialization function--
---------------------------
--sets everything up 
template.init = function()
  signal:disable()
  local ui = UI:aquireAnyMonitor()
    table.insert(uis,ui)
end

-----------------
--Main function--
-----------------
--the function that starts execution
--usual contains a loop that repeats a behaviour
template.main = function()
  writeScreen(uis[1])
  runProcess(chargeListener,"charge_listener")
  runProcess(reset,"reset_listener")
end