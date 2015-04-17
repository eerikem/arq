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
local uis = {}

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
  w:wipe(#str,3,3)
  w.setCursorBlink(true)
  local r = io.read()
  w.setCursorBlink(false)
  term.redirect(parent)
  return r
  --parent.restoreCursor()
end


local function shutdown()
  for n = 1, #uis do
    uis[n]:terminate()
  end
  ui:terminate()
end


function askQuit()
  if ui:yesNo("Do you wish to QUIT?") then
    shutdown()
  end
end

local function runFile(file)
  dofile(shell.resolve(file))
  assert(root.init,"Error, file is not a correct ARQ executable")
  root.init(function() return writeStatus end)
  for n = 1, #root.uis do
    table.insert(uis,root.uis[n])
  end
  --runProcess(root.main)
  root.main()
end

local function run()
  ui:clear()
  ui:printCentered("ArqiTeknologies",1,2)

  runFile("arq/airlock.lua")
  
  waitSeconds(1)
  print "WAITED 1"
  
  local w,h = ui.getSize()
  --ui:indentLeft("a message here",0,h-2)
  --sleep(1)
  --local r = queryUser("What would you like?")
  --writeStatus("You asked for " .. r)
  --sleep(1)
  askQuit()
end


local function eventListener()
  --local args = {os.pullEvent()}
  while true do
    signal(os.pullEvent())
  end
end


local function main()
  parallel.waitForAny(run,wakerUpper,eventListener)
end




main()
--ui:drawHeader()
--ui:indentLeft("Welcome to ARQ",3,0)
