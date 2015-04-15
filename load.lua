--Product of ArqiTeknologies Corp.
--
--present
--
--Author: ArqiTek
--Copyrite 2250

print(shell.path())
shell.setDir("arq")
os.loadAPI("menu")

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


function loadMenu(ui,root)
  items = {}
  values = {}
  for i = 1, #root, 2 do
    table.insert(items,root[i])
    table.insert(values,root[i+1])
  end
  if ui:drawMain(items) then
    v = values[ui.select]
    if  type(v) == "string" then
      assert(findfunction(v))(ui)
    else
      loadMenu(ui, v)
    end
  end
end


function loadMenus()
  --while true do
    print("running parralel")
    parallel.waitForAny(runMenu1,runMenu2)
    print("Runnables Terminated")
  --end
  clearMonitors()
end


function runPerfMenu(ui,menu)
  local items = {}
  local values = {}
  local selected = 1
  for i = 1, #menu, 2 do
    table.insert(items,menu[i])
    table.insert(values,menu[i+1])
  end
  --while ui.running do
    ui:setSelected(selected)
    if ui:drawMain(items) then
     activeMonitor = ui:getMonitor()
     selected = ui.select
      v = values[selected]
      if  type(v) == "string" then
        assert(findfunction(v))(ui)
      else
        runPerfMenu(ui,v)
      end
    end
  --end
end

dofile(file)
