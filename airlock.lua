

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
  {
    side="back",
    cable=colors.white,
    monitor="monitor_1"
  },
  {
    side="back",
    cable=colors.orange,
    monitor="monitor_0"
  }
}


local delay = 2

local open = function (door)
  if not locked then
    redstone.setBundledOutput(door.side,colors.combine(door.cable,redstone.getBundledOutput(door.side)))
  end
end

local close = function (door)
  redstone.setBundledOutput(door.side,colors.subtract(redstone.getBundledOutput(door.side),door.cable))
end


local cycleAirlock = function (ui)
  if not locked and sealed then
    sealed = false
    writeStatus("Cycling Airlock")
    
    
    ui.clear()
    ui:printCentered("Airlock",1)
    ui:printCentered(" OPEN ",3)
    local insideDoor, outsideDoor
    if doors[1].monitor == ui.name then
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
    ui:printCentered("LOCKING",3)
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
    ui:printCentered(" OPEN ",3)
    ui:showDelay(delay,4)
    ui:printCentered("CYCLING",3)
    ui:showDelayTwo(delay,4)
    ui:printCentered("LOCKING",3)
    ui:undoDelay(delay,4)
    ui.menu.draw()
end

local lockPeripherals = function (ui)
  --writeStatus("lockPerf")
  --error("Called lockPerf",2)
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
    local ui2
    if uis[1] == ui then ui2 = uis[2]
    else ui2 = uis[1] end
    runProcess(function() cycleAirlock(ui) end,"DoorCycler1" )
    runProcess(function() cycleLock2(ui2) end,"DoorCycler2" )
  end
end

airlock.init = function()
  local runs = {}
  for n=1, #doors do
    local ui = UI:aquireMonitor(doors[n].monitor)
    table.insert(uis,ui)
    ui.clear()
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

