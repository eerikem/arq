--
--present
--
--Author: ArqiTek
--Copyright 2250

--os.unloadAPI("perf")
os.loadAPI("perf")

print("hello")

function printUsage()
  print( "Usage: load <menu file>" )
  return
end

local tArgs = { ... }
if #tArgs < 1 then
  printUsage()
  return
end

local file = tArgs[1]


function findfunction(x)
  assert(type(x) == "string")
  local f=funList
  for v in x:gmatch("[^%.]+") do
    if type(f) ~= "table" then
       return nil, "looking for '"..v.."' expected table, not "..type(f)
    end
    f=f[v]
  end
  if type(f) == "function" then
    return f
  else
    return nil, "expected function, not "..type(f)
  end
end


function runMenu(ui,menu)
  local items = {}
  local values = {}
  local selected = 1
  for i = 1, #menu, 2 do
    table.insert(items,menu[i])
    table.insert(values,menu[i+1])
  end
  while ui.running do
    ui:setSelected(selected)
    if ui:drawMain(items) then
     activeMonitor = ui:getMonitor()
     selected = ui.select
      v = values[selected]
      if  type(v) == "string" then
        assert(findfunction(v))(ui)
      else
        runMenu(ui,v)
      end
    end
  end
end

function drawMenu()

end

function loadMenus()
  --while true do
    print("running parralel")
    parallel.waitForAny(runMenu1,runMenu2)
    print("Runnables Terminated")
  --end
  clearMonitors()
end

function clearMonitors()
  for m=1, #uis, 1 do
    if uis[m].running then
      uis[m]:exit()
    end
  end
end

function runMenu1()
  local monitor = doors[1].monitor
  print("ui init for "..monitor)
  local ui = uis[1]
  ui:init(monitor, uis[2], title)
  runMenu(ui,root)
end

function runMenu2()
  local monitor = doors[2].monitor
  print("ui init for "..monitor)
  local ui = uis[2]
  ui:init(monitor, uis[1], title)
  runMenu(ui,root)
end

uis = {
  perf.Perf:new(),
  perf.Perf:new()
}
dofile(file)
loadMenus()
