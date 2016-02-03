--Product of ArqiTeknologies Corp.
--
--present
--
--Author: ArqiTek
--Copyright 2250
local args = {...}


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
writeCo = function(str) print(str) end

VM = dofile("server/vm.lua")
dofile("arq/WaitSupport.lua")
--os.loadAPI("arq/perf")
dofile("arq/ui.lua")
dofile("arq/bundle.lua")
dofile("arq/group.lua")


local TELEFILE = "arq/tele.lua"

local STATUS_HEIGHT = 12
local LOGGING = false
local log = "arq/log8"
local ui = UI:new(term)
local parentTerm = ui.current()
local uis = {}
local coroutines = {}
local supervisor
local channels = {modem = "top", keeper= 1111, receive = 2222}


local function addStatus(ui)
  local x, y = ui.getSize()
  local h = STATUS_HEIGHT
  return window.create(ui.current(), 1, y - h + 1,x/2,h+1)
end


local status = addStatus(ui)


writeStatus = function(str)
  local x,y = ui.getCursorPos()
  local w,h = status.getSize()
  local parent = ui.redirect(status)
  --term.scroll(1)
  term.setCursorPos(1,h)
  if string.find(string.lower(str),'error') then
    local c = term.getTextColor()
    term.setTextColor(colors.red)
    print(str)
    term.setTextColor(c)
  else
    print(str)
  end
  if LOGGING then
    local f = fs.open(log,"a")
    f.write(str .. "\n")
    f.close(f)
  end
  term.redirect(parent)
  status.redraw()
  ui.setCursorPos(x,y)
end

VM.log = writeStatus


local function addCoList(ui)
  local x,y = ui.getSize()
  local w = (x/2)-1
  return window.create(ui.current(), x-w,3,w,y-2)
end

local coList = addCoList(ui) 

writeCo = function(str)
  local x,y = ui.getCursorPos()
  local w,h = coList.getSize()
  local parent = ui.redirect(coList)
  term.scroll(1)
  term.setCursorPos(1,h)
  term.write(str)
  term.redirect(parent)
  coList.redraw()
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

--TODO write supervisor to spawn child programs
--Maintains a list of said programs which it can shutdown.
--TODO listeners can start and stop
--TODO supervisor releases uis
local function supervise(file)
  VM.process_flag("trap_exit",true)
  local module = dofile(file)
  if not module then if root then module = root
  else writeStatus(file.." must return a table with init & main functions")
  return end end
  assert(module.init,"Error, file is not a correct ARQ executable")
  module.init(function() return writeStatus end)
  for n = 1, #module.uis do
    table.insert(uis,module.uis[n])
  end
  module.main()
  local e, msg = VM.receive()
  while e == "EXIT" do
    writeStatus("Received exit message from "..file..": "..msg)
    module.main()
    e, msg = VM.receive()
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
  if not fs.exists(file) then file = "arq/"..file end
  if fs.exists(file) then
    
    local co = VM.spawn(function() supervise(file) end)
    --writeCo(co.." "..file.." supervisor")
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

local function test()
  local T = group.group(95,78,40,101,78,35)
  group.insert(T,97,80,38)
  writeStatus("Grouped: "..table.maxn(T))
  local n
  runProcess(function ()
    local B = commands.getBlockInfo(T[1].x,T[1].y,T[1].z)
    writeStatus("hello "..type(B)) end,"BlockInfo")
  waitSignal("task_complete")
  writeStatus("Found: "..n)
  --local block = queryUser("What block do you want?")
  --group.execute(T,"/setblock %d %d %d "..block)
end

local arqMenu = {
  --"Test", test,
  "Load Program", loadProgram,
  "Run Airlock", airlock,
  --teleMenu, teleporter,
  "Enable Attack", attack,
  "Shutdown ARQ", askQuit
}


local function eventListener()
  while true do
    signal(os.pullEvent())
  end
end


local function run()
  ui:clear()
  status.redraw()
  coList.redraw()
  ui:aquireMonitors()
  ui:printCentered("ArqiTeknologies",1,2)
  local m = ui:readMenu(arqMenu)
  m.cycle()
end


local function main()
  --supervisor = runProcess(supervisorMain,"arq_supervisor")
  --writeStatus("Running1")
  runProcess(wakerUpper,"wakerUpper",supervisor)
  --writeStatus("Running 2")
  for i,file in ipairs(args) do
    writeStatus("Running "..file.." "..i)
    runFile(file)
  end
  runProcess(run,"arq_run")
  eventListener()
end


main()
