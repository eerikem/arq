--Product of ArqiTeknologies Corp.
--
--present
--
--Author: ArqiTek
--Copyright 2250


function printTable(_t)
  if _t == nil then
    print "nil table!" 
  else
    for k,v in pairs(_t) do
      if type(v)=="thread" then v = "thread" end
      if type(v)=="table" then
        print(k.." plus table")
        printTable(v)
        print("end table "..k)
      else
      print(k.." "..v)
      end
    end
  end
end


dofile("arq/WaitSupport.lua")
--os.loadAPI("arq/perf")
dofile("arq/ui.lua")


local ui = UI:new(term)
local parentTerm = ui.current()
local uis = {}

local channels = {modem = "top", keeper= 1111, receive = 2222}

local function addStatus(ui)
  local x, y = ui.getSize()
  return window.create(ui.current(), 1, y - 2,x,2)
end

local status = addStatus(ui)

local function setStatusWindow(msg)
  status.setVisible(true)
  local parent = term.redirect(status)
  term.clear()
  term.write(msg)
  status.redraw()
  term.redirect(parent)
end


local function writeStatus(str)
  local x,y = ui.getCursorPos()
  local parent = ui.redirect(status)
  term.clear()
  term.setCursorPos(1, 1)
  term.write(str)
  term.redirect(parent)
  status.redraw()
  ui.setCursorPos(x,y)
end


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

local function handleKeeper()
  local key
  local keeperDistance
  local modem = peripheral.wrap(channels.modem)
  modem.open(channels.keeper)
  while keeper == nil do
    local _, side, senderChannel, replyChannel, message, senderDistance = waitSignal("modem_message")
    if senderChannel == channels.keeper then
      if message == "I am The Keeper!" then
        modem.close(channels.keeper)
        channels.keeper = replyChannel
        modem.open(channels.keeper)
        keeperDistance = senderDistance
      end
    end
  end
  
  modem.transmit(channels.keeper,channels.keeper,"Computer ".. os.getComputerID().."is listening")
  
  local function updateKeeper()
  
  end
  
  local function listenKeeper()
    local _, side, senderChannel, replyChannel, message, senderDistance = waitSignal("modem_message")
    if senderChannel == channels.keeper then
      if senderDistance == keeperDistance then
        writeStatus("Keeper sent: "..message)
      end
    end
  end
  
  runProcess(updateKeeper)
  runProcess(listenKeeper)
end
  
  


local function queryUser(str)
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
  os.queueEvent("terminate")
end


local function askQuit()
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

local function loadProgram()
  local file = queryUser("Name your program")
  runFile(file)
end

local initKeeper = nil

local arqMenu = {
  "Load Program", loadProgram,
  "Initialize Keeper", initKeeper,
  "Shutdown ARQ", askQuit
}


local function eventListener()
  --local args = {os.pullEvent()}
  while true do
    signal(os.pullEvent())
  end
end


local function run()
  ui:clear()
  ui:printCentered("ArqiTeknologies",1,2)

  --runFile("arq/airlock.lua")
  
  local m = ui:readMenu(arqMenu)
  runProcess(m.cycle())
  --waitSeconds(1)
  
  --eventListener()
  
  local w,h = ui.getSize()
  --ui:indentLeft("a message here",0,h-2)
  --sleep(1)
  --local r = queryUser("What would you like?")
  --writeStatus("You asked for " .. r)
  --sleep(1)
  --askQuit()
end




local function main()
  run()
  parallel.waitForAny(eventListener,wakerUpper)
end




main()
--ui:drawHeader()
--ui:indentLeft("Welcome to ARQ",3,0)
