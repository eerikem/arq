

local airlock = {}
root = airlock

local title = 'Airlock' 
local locked = false
local sealed = true
local active = false
local uis = {}
airlock.uis = uis

local function lockMenuItem()
  if locked then
    return "Unlock?"
  else
    return "Lock"
  end
end

local doors = {
  BUNDLE:new("back",colors.white,"monitor_1"),
  BUNDLE:new("back",colors.magenta,"monitor_3")
  }

local delay = 2

local open = function (door)
  if not locked then
    door:enable()
  end
end

local close = function (door)
  door:disable()
end


local cycleAirlock = function (ui)
  if not locked and sealed then
    sealed = false
    writeStatus("Cycling Airlock")
    ui.clear()
    ui:printCentered("Airlock",1)
    ui:printCentered(" OPEN ",3)
    local insideDoor, outsideDoor
    if doors[1]:getName() == ui.name then
      insideDoor = doors[1]
      outsideDoor = doors[2]
    else 
      insideDoor = doors[2]
      outsideDoor = doors[1]
    end
    open(insideDoor)
    ui:showDelay(delay,4)
    close(insideDoor)
    ui:printCentered("CYCLING",3)
    ui:showDelayTwo(delay,4)
    open(outsideDoor)
    ui:printCentered("LOCKED ",3)
    ui:undoDelay(delay,4)
    close(outsideDoor)
    --ui:clearStatus()
    ui.menu.draw()
    writeStatus("Cycled Airlock")
    
    sealed = true
    active = false
    signal("locked")
  end
end

local cycleLock2 = function (ui)
    ui.clear()
    ui:printCentered("Airlock",1)
    ui:printCentered("LOCKED ",3)
    ui:showDelay(delay,4)
    ui:printCentered("CYCLING",3)
    ui:showDelayTwo(delay,4)
    ui:printCentered(" OPEN  ",3)
    ui:undoDelay(delay,4)
    ui.menu.draw()
end

local lockPeripherals = function (ui)
  --writeStatus("lockPerf")
  error("Called lockPerf some long message",2)
  locked = not locked
  signal("menuUpdate")
end

local exit = function (ui)
  ui:terminate()
end

local startCycle = function(ui)
  if not active and not locked then
    active = true
    signal("cycle",ui)
    waitSignal("locked")
  else
    if locked then
       ui:printCentered("LOCKED!",3)
       waitSeconds(1.5)
    end
  end
end

local menu = {
  'Cycle', startCycle,
  'Other',{
    lockMenuItem, lockPeripherals,
    'Back', "BACK"
    }
}

local cycleMenus = function()
  while true do
    local event, ui = waitSignal("cycle")
    if ui then
      local ui2
      if uis[1] == ui then ui2 = uis[2]
      else ui2 = uis[1] end
      runProcess(function() cycleAirlock(ui) end,"DoorCycler1" )
      runProcess(function() cycleLock2(ui2) end,"DoorCycler2" )
    end
  end
end

airlock.init = function()
  local runs = {}
  for n=1, #doors do
    local ui = UI:aquireMonitor(doors[n]:getName())
    table.insert(uis,ui)
    ui.setBackgroundColor(colors.orange)
    ui.clear()
    ui.setBackgroundColor(colors.black)
  end
end

airlock.main = function()
  for n=1, #uis do
    local m = uis[n]:readMenu(menu)
    --m.run()
    m.setTitle(title)
    uis[n].menu = m
    runProcess(m.cycle,"airlock_main")
  end
  runProcess(cycleMenus,"cycle_lstnr")
end

