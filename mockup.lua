

local mockup = {}
root = mockup

local title = 'Airlock' 
local locked = false
local sealed = true
local active = false
local uis = {}
mockup.uis = uis

local function writeScreen(ui)
  
  ui.setBackgroundColor(colors.gray)
  ui.clear()
  ui.setBackgroundColor(colors.lightGray)
  ui:wipe(7,1,1)
  ui:wipe(7,1,5)
  
  ui.setBackgroundColor(colors.lightGray)
  ui.setTextColor(colors.gray)
  ui:indentLeft("AIRLOCK",0,1)
  ui.setBackgroundColor(colors.gray)
  ui.setTextColor(colors.orange)
  ui:printCentered(" CYCLE ",3)
end

local function toggler()
  local ui = uis[1]
  while true do
    local event, mon, x,y = waitSignal("monitor_touch")
    if event == "terminate" then return
    else
      if not active then
        if y == 3 then 
          active = true  
          signal("cycle")
        else
          ui:beep()
        end
      else
        ui:beep()
        ui.setTextColor(colors.red)
        ui:printCentered("LOCKED!",3)
        ui.setTextColor(colors.orange)
        waitSeconds(1)
        if active then
          ui.setTextColor(colors.white)
          ui:printCentered("CYCLING",3)
          ui.setTextColor(colors.orange)
        end
      end
    end
  end
end

local function cycler()
  local ui = uis[1]
  
  while true do
    ui.setBackgroundColor(colors.gray)
    ui.setTextColor(colors.orange)
    ui:printCentered(" CYCLE ",3)
    waitSignal("cycle")
    ui.setTextColor(colors.white)
    ui:printCentered("CYCLING",3)
    ui.setTextColor(colors.orange)
    for i=1,10 do
      ui:indentLeft(" >  >  ",0,4)
      waitSeconds(0.2)
      ui:indentLeft("  >  > ",0,4)
      waitSeconds(0.2)
      ui:indentLeft(">  >  >",0,4)
      waitSeconds(0.2)
    end
    active = false
    ui:wipe(7,1,4)
  end
end

mockup.init = function()
  local ui = UI:aquireAnyMonitor()
    table.insert(uis,ui)
end


mockup.main = function()
  writeScreen(uis[1])
  runProcess(toggler,"the_toggler")
  runProcess(cycler,"the_cycler")
end
