

local airlock = {}
root = airlock

local title = ' Airlock' 
local locked = true
local uis = {}
airlock.uis = uis
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
  print("unlock?")
  redstone.setBundledOutput(door.side,colors.combine(door.cable,redstone.getBundledOutput(door.side)))
end

local lock = function (door)
  print("lock?")
  redstone.setBundledOutput(door.side,colors.subtract(redstone.getBundledOutput(door.side),door.cable))
end


local cycleAirlock = function (ui)
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
  print("end cycle")
end

local lockPeripherals = function (ui)
  --ui:writeStatus("lock?")
end

local exit = function (ui)
  ui:terminate()
end

local menu = {
  'Cycle', cycleAirlock,
  'Other',{
    'Lock', lockPeripherals,
    'Exit', exit
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
    m.run()
    --runProcess(m.run)
  end
end

