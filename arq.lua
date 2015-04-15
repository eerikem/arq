--Product of ArqiTeknologies Corp.
--
--present
--
--Author: ArqiTek
--Copyright 2250

dofile("arq/WaitSupport.lua")
--os.loadAPI("arq/perf")
dofile("arq/ui.lua")


local ui = UI:new(term)
local parentTerm = ui.current()

local function getFile()
  io.write("->")

  local funs = {}
  local file = io.read()
  
  printFile(file)
end


local printFile = function(file)
  local f = assert(io.open(shell.resolve(file)))
  print(f:read("*a"))
  f:close()
end

local function addStatus(ui)
  local x, y = ui.getSize()
  return window.create(ui.current(), 1, y - 2,x,2)
end

local status = addStatus(ui)

local function setStatusWindow(msg)
  status.setVisible(true)
  term.redirect(status)
  term.clear()
  term.write(msg)
  status.redraw()
end


function writeStatus(str)
  local x,y = ui.getCursorPos()
  local parent = ui.redirect(status)
  term.clear()
  term.setCursorPos(1, 1)
  term.write(str)
  term.redirect(parent)
  status.redraw()
  ui.setCursorPos(x,y)
end


function queryUser(str)
  local w,h = ui.getSize()
  local x = (w - #str) / 2 - 2
  local y = h/2 - 2
  local w = UI:new(window.create(ui.current(),x,y,#str+4,4))
  local parent = ui.redirect(w)
  w.setBackgroundColor(colors.blue)
  w.clear()
  w:printCentered(str,2)
  w.setBackgroundColor(colors.black)
  --w:indentLeft(">",2,3)
  w.setCursorPos(3,3)
  w:wipe(#str)
  w.setCursorBlink(true)
  local r = io.read()
  w.setCursorBlink(false)
  term.redirect(parent)
  return r
  --parent.restoreCursor()
end

function askQuit()
  if ui:yesNo("Do you wish to QUIT?") then
    ui:terminate()
  end
end

function main()
  ui:clear()
  ui:printCentered("ArqiTeknologies",1,2)

  local w,h = ui.getSize()
  ui:indentLeft("a message here",0,h-2)
  sleep(1)
  local r = queryUser("What would you like?")
  writeStatus("You asked for " .. r)
  sleep(1)
  askQuit()
end



local function aquireMonitors(_table)
  local mon1, mon2 = peripheral.find("monitor", function(name,object) return _table[name] end)
  if mon1 and mon2 then
    writeStatus("Detected monitors")
    return mon1, mon2
  else
    error("Problem detecting monitors")
  end
end

local m1, m2 = aquireMonitors({monitor_0=true,monitor_1=true})

root = {
  'Cycle', 'cycleAirlock',
  'Other',{
    'Lock', 'lockPeripherals',
    'Exit', 'exit'
    }
}

local function initAirlocks(m1,m2)
  local init = function(ui)
    ui.clear()
    local menu = ui:readMenu(root)
    menu.draw()
  end
  init(m1)
  init(m2)
end

local ui1, ui2 = UI:new(m1), UI:new(m2)
initAirlocks(ui1,ui2)

--ui:drawHeader()
--ui:indentLeft("Welcome to ARQ",3,0)
