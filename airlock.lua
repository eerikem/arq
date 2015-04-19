

local airlock = {}
root = airlock

local title = 'Airlock' 
local locked = false
local sealed = true
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

local unlock = function (door)
  --print("unlock?")
  if not locked then
    redstone.setBundledOutput(door.side,colors.combine(door.cable,redstone.getBundledOutput(door.side)))
  end
end

local lock = function (door)
  --print("lock?")
  redstone.setBundledOutput(door.side,colors.subtract(redstone.getBundledOutput(door.side),door.cable))
end


local cycleAirlock = function (ui)

  if not locked and sealed then
  
  sealed = false
  
  print("Cycling Airlock")
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
  unlock(insideDoor)
  ui:showDelay(delay,4)
  lock(insideDoor)
  ui:printCentered("CYCLING",3)
  ui:showDelayTwo(delay,4)
  unlock(outsideDoor)
  ui:printCentered("LOCKING",3)
  ui:undoDelay(delay,4)
  lock(outsideDoor)
  --ui:clearStatus()
  ui.menu.draw()
  --print("end cycle")
  
  sealed = true
  else
    if locked then
       ui:printCentered("LOCKED!",3)
       waitSeconds(1.5)
    end
  end
end

local lockPeripherals = function (ui)
  locked = not locked
end

local exit = function (ui)
  ui:terminate()
end

local menu = {
  'Cycle', cycleAirlock,
  'Other',{
    lockMenuItem, lockPeripherals,
    'Back', "BACK"
    }
}


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
    runProcess(m.cycle)
  end
end

