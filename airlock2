--Product of ArqiTeknologies Corp.
--
--present
--
--Author: ArqiTek
--Copyrite 2250

os.loadAPI("perf")


funList = {}

activeMonitor = ""
lockEnabled = false

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

mainMenu = menu.Menu:new()

uis = {
  perf.Perf:new(),
  perf.Perf:new()
}

funList.enableLock = function (ui)
  if lockEnabled then
    ui:writeStatus("Airlock already enabled.")
  else
    lockEnabled = true
    ui:writeStatus("Airlock enabled")
  end
  loadMenu(ui,root)
end

funList.disableLock = function (ui)
  if lockEnabled then
    lockEnabled = false
    ui:writeStatus("Airlock has been disabled.")
  else
    ui:writeStatus("Airlock already disabled.")
  end
  loadMenu(ui,root)
end

funList.unlock = function (door)
  redstone.setBundledOutput(door.side,colors.combine(door.cable,redstone.getBundledOutput(door.side)))
end

funList.lock = function (door)
  redstone.setBundledOutput(door.side,colors.subtract(redstone.getBundledOutput(door.side),door.cable))
end

funList.cycleAirlock = function (ui)
  mainMenu:writeStatus("Cycling Airlock")
  mainMenu:refresh()
  local delay = 2
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
  ui:showDelay(delay,4,uis)
  funList.lock(insideDoor)
  ui:printCentered("CYCLING",3)
  uis:printCentered("CYCLING",3)
  ui:showDelayTwo(delay,4,uis)
  funList.unlock(outsideDoor)
  ui:printCentered("LOCKING",3)
  uis:printCentered("LOCKING",3)
  ui:undoDelay(delay,4,uis)
  funList.lock(outsideDoor)
  ui:clearStatus()
  uis:refresh()
  runPerfMenu(ui,lockMenu)
end

funList.lockPeripherals = function (ui)
  ui:writeStatus("lock?")
  runPerfMenu(ui,lockMenu)
end

funList.exit = function (ui)
  ui:exit()
end


function clearMonitors()
  for m=1, #uis do
    if uis[m].running then
      uis[m]:exit()
    end
  end
end


funList.loadRoot = function (ui)
  loadMenu(ui,root)
end

funList.exitMain = function (ui)
  for i=1,#uis do
    uis[i]:exit()
  end
  ui:exit()
end

root = {
  'Airlock',
   {
    'Enable', 'enableLock',
    'Disable', 'disableLock'
   },
  'jokes', 'loadJokes',
  'Exit', 'exitMain'
}

jokeMenu = {
  'Tomas', 'silly',
  'Ian', 'silly2',
  'Philipe','silly3'
}

lockMenu = {
  'Cycle', 'cycleAirlock',
  'Other',{
    'Lock', 'lockPeripherals',
    'Exit', 'exit'
    }
}

funList.loadJokes = function (ui)
  ui:writeStatus("Yuk yuk")
  ui:refresh()
  runPerfMenu(uis[1],jokeMenu)
  runPerfMenu(uis[2],jokeMenu)
end

funList.silly = function (ui)
  ui:clear()
  ui:printCentered("Tomas",3)
  ui:printCentered("is cheesy",4)
  sleep(4)
  runPerfMenu(ui,jokeMenu)
end

funList.silly2 = function (ui)
  ui:clear()
  sleep(1)
  ui:printCentered("Ian",2)
  sleep(1)
  ui:printCentered("is",3)
  sleep(1)
  ui:printCentered("cool",4)
  sleep(2)
  ui:printCentered("NOT",5)
  sleep(3)
  runPerfMenu(ui,jokeMenu)
end

funList.silly3 = function (ui)
  ui:clear()
  ui:printCentered("Sucks!",3)
  ui:showDelay(2,4,ui)
  ui:undoDelay(2,4,ui)
  ui:showDelay(2,4,ui)
  ui:undoDelay(2,4,ui)
  ui:showDelay(2,4,ui)
  ui:undoDelay(2,4,ui)
  runPerfMenu(ui,jokeMenu)
end

function runMenu1(ui,title)
  local monitor = doors[1].monitor
  --print("ui init for "..monitor)
  ui:init(monitor, uis[2], title)
  runPerfMenu(ui,lockMenu)
end

function runMenu2(ui,title)
  local monitor = doors[2].monitor
  --print("ui init for "..monitor)
  ui:init(monitor, uis[1], title)
  runPerfMenu(ui,lockMenu)
end

parallel.waitForAny(
	function() 
		local title = 'Airlock Control'
  		local ui = mainMenu
  		ui:init(title) 
		loadMenu(ui, root)
		end,
	function()
		local title = 'Airlock'
		--local ui = perf.Perf:new()
		--local ui2 = perf.Perf:new()
		local ui = uis[1]
		local ui2 = uis[2]
		parallel.waitForAny(
			function()
				runMenu1(ui,title) end,
			function()
				runMenu2(ui2,title) end
		)
	end)


		