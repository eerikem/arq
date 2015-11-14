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


--Function redefined later
writeStatus = function(str) print(str) end

dofile("arq/WaitSupport.lua")
--os.loadAPI("arq/perf")
dofile("arq/ui.lua")
dofile("arq/bundle.lua")
local TELEFILE = "arq/tele.lua"

local STATUS_HEIGHT = 5
local LOGGING = false
local log = "arq/log8"
local ui = UI:new(term)
local parentTerm = ui.current()
local uis = {}
local coroutines = {}

local channels = {modem = "top", keeper= 1111, receive = 2222}

local function addStatus(ui)
  local x, y = ui.getSize()
  local h = STATUS_HEIGHT
  return window.create(ui.current(), 1, y - h + 1,x,h)
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


writeStatus = function(str)
  local x,y = ui.getCursorPos()
  local w,h = status.getSize()
  local parent = ui.redirect(status)
  term.scroll(1)
  term.setCursorPos(1,h)
  term.write(str)
  if LOGGING then
    local f = fs.open(log,"a")
    f.write(str .. "\n")
    f.close(f)
  end
  term.redirect(parent)
  status.redraw()
  ui.setCursorPos(x,y)
end


local printFile = function(file)
  local f = assert(io.open(shell.resolve(file)))
  print(f:read("*a"))
  f:close()
end

local function getFile()
  io.write("->")
  local funs = {}
  local file = ui:read()
  printFile(file)
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
  
  runProcess(updateKeeper,"updateKeeper")
  runProcess(listenKeeper, "listenKeeper")
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
  w.setCursorPos(3,3)
  w:wipe(#str,3,3)
  term.redirect(parent)
  local r = ui:read()
  --local r = io.read()
  parent.restoreCursor()
  w.clear()
  w.redraw()
  --UI:exec(selectSound)
  return r
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
    --kill msg to turn off menu cycle
    return "kill"
  end
end

local function register(co,...)
  local _t={}
  for i,c in ipairs(arg) do
    table.insert(_t,c)
  end
  if not coroutines[co] then
    coroutines[co]={_t}
  else
    writeStatus("Additional coroutines added to "..co)
    for i,c in ipairs(_t) do
      table.insert(coroutines[co][i],c)
    end
  end
end

local function runFile(file)
  if fs.exists(file) then
    dofile(file)
    assert(root.init,"Error, file is not a correct ARQ executable")
    root.init(function() return writeStatus end)
    for n = 1, #root.uis do
      table.insert(uis,root.uis[n])
    end
    register(file,runProcess(root.main,file))
  else
    writeStatus("Error: "..file.." not found")
  end
end

local function loadProgram()
  local file = queryUser("Name your program")
  runFile(file)
end

local initKeeper = "badarg"

local airlock = function()
  writeStatus("Running airlock")
  runFile("arq/airlock.lua")
end

local teleMenu = function()
  if coroutines[TELEFILE] then
    return "Halt Teleporter"
  else
    return "Start Teleporter"
  end
end

local attack = function()
  writeStatus("Launching Attack!")
  runFile("arq/attack.lua")
end

local teleporter = function()
  if coroutines[TELEFILE] then
    writeStatus("Shutting down Teleporter")
    stop(coroutines[TELEFILE])
  else
    writeStatus("Running Teleporter")
    runFile(TELEFILE)
  end
end

local function pulse()
  local cable = BUNDLE:new("back",colors.white,"pulse")
  cable:pulse()
end

local arqMenu = {
  "Load Program", loadProgram,
  "Run Airlock", airlock,
  teleMenu, teleporter,
  "Launch Attack", attack,
  "Pulse", pulse,
  "Shutdown ARQ", askQuit
}


local function eventListener()
  while true do
    signal(os.pullEvent())
  end
end


local function run()
  ui:clear()
  ui:aquireMonitors()
  ui:printCentered("ArqiTeknologies",1,2)
  local m = ui:readMenu(arqMenu)
  m.cycle()
end


local function main()
  runProcess(run,"arqRun")
  runProcess(wakerUpper,"wakerUpper")
  eventListener()
end


main()
--ui:drawHeader()
--ui:indentLeft("Welcome to ARQ",3,0)
