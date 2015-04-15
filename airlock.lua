title = ' Airlock'

funList = {}

locked = true
activeMonitor = ""

doors = {
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


delay = 2

funList.unlock = function (door)
  redstone.setBundledOutput(door.side,colors.combine(door.cable,redstone.getBundledOutput(door.side)))
end

funList.lock = function (door)
  redstone.setBundledOutput(door.side,colors.subtract(redstone.getBundledOutput(door.side),door.cable))
end

funList.cycleAirlock = function (ui)
  print("Cycling Airlock")
  local uis = ui.uis
  ui:clear()
  uis:clear()
  ui:printCentered("Airlock",1)
  uis:printCentered("Airlock",1)
  ui:printCentered(" OPEN ",3)
  uis:printCentered(" OPEN ",3)
  side = activeMonitor
  if doors[1].monitor == side then
    insideDoor = doors[1]
    outsideDoor = doors[2]
  else 
    insideDoor = doors[2]
    outsideDoor = doors[1]
  end
  funList.unlock(insideDoor)
  ui:showDelay(delay,4)
  funList.lock(insideDoor)
  ui:printCentered("CYCLING",3)
  uis:printCentered("CYCLING",3)
  ui:showDelayTwo(delay,4)
  funList.unlock(outsideDoor)
  ui:printCentered("LOCKING",3)
  uis:printCentered("LOCKING",3)
  ui:undoDelay(delay,4)
  funList.lock(outsideDoor)
  ui:clearStatus()
  uis:clearStatus()
  print("end cycle")
end

funList.lockPeripherals = function (ui)
  ui:writeStatus("lock?")
end

funList.exit = function (ui)
  ui:exit()
  --term.clear()
  --term.setCursorPos(1,1)
end

root = {
  'Cycle', 'cycleAirlock',
  'Other',{
    'Lock', 'lockPeripherals',
    'Exit', 'exit'
    }
}
