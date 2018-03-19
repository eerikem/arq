local gen_server = require "gen_server"
local ui_server = require "ui_server"
local UI = require "lib.ui"
local Graphic = require "lib.graphic"
local Panel = require "lib.ui_obj"
local Password = require "password"
local Door = require "door"
local elvator_panel = require "elevator_panel"
local elvator_call = require "elevator_call"

local Elevator = {}

----------------
--External API--
----------------

--{{name=floorName,
--level=floor#,
--coord={x,y,z,r},
--door=doorCo,
--call=monitor,
--panel=monitor,
--calback=function,
--}}

function Elevator.subscribe(elevator,co)
  if not elevator then error("badarg",2) end
  local co = co or VM.running()
  gen_server.cast(elevator,{"subscribe",co})
end

function Elevator.newCall(monitor,title,number,elevator,password)
  return elvator_call.start_link(monitor,title,number,elevator,Elevator,password)
end

function Elevator.newPanel(monitor,floor,elevator)
  return elvator_panel.start_link(monitor,floor,elevator,Elevator)
end

function Elevator.new(levels)
  local ok, Co = Elevator.start_link(levels)
  return Co
end

function Elevator.callTo(Co,lvl)
  gen_server.cast(Co,{"call",lvl})
end

function Elevator.start_link(levels)
  return gen_server.start_link(Elevator,{levels},{})
end

function Elevator.init(levels)
  local activeLevel = 1
  -- organise levels into hash of levels
  -- sorted by level number
  local byLevel = {}
  for n,l in ipairs(levels) do
    l.waiting = false
    byLevel[l.level]=l
    activeLevel = l.level
    if l.call ~= nil then
      Elevator.newCall(l.call,l.name,l.level,VM.running(),l.password)
    end
    if l.door ~= nil then
      Door.subscribe(l.door)
    end
  end
  return true, {
    activeLevel = activeLevel,
    destQueue = {},
    direction = nil,
    delay = 3,
    subscribers = {},
    levels = byLevel,
    }
end

local function openDoor(State)
  if State.levels[State.activeLevel] then
    if State.levels[State.activeLevel].door ~= nil then
      Door.open(State.levels[State.activeLevel].door)
    end
  end
end

local function closeDoor(State)
  if State.levels[State.activeLevel] then
    if State.levels[State.activeLevel].door ~= nil then
      Door.close(State.levels[State.activeLevel].door)
    end
  end
end

local function setDir(State,level)
  if State.activeLevel > level then
    State.direction = "down"
  else
    State.direction = "up"
  end
end

local function notify(State,event,...)
  for Co,_ in pairs(State.subscribers) do
    gen_server.cast(Co,{event,unpack(arg)})
  end
end

local function sendLift(State)
  if not State.direction then
    if State.destQueue[1] then
      local level = State.destQueue[1]
      setDir(State,level)
      EVE.tick(State.delay)
    end
  end
end

function Elevator.handle_cast(Request,State)
  local event = Request[1]
  if event == "call" then
    local level = Request[2]
    VM.log("Received call from level "..level.." at lvl ".. State.activeLevel)
    if State.activeLevel == level and not State.direction then
      openDoor(State)
      notify(State,"level",State.activeLevel)
    else
      if State.levels[level] == nil then
        VM.log("Not a valid destination: level "..level)
        return State end
      if not State.levels[level].waiting then
        local lvl = State.levels[level]
        lvl.waiting = true
        closeDoor(State)
        if not State.direction then
          setDir(State,level)
          EVE.tick(State.delay)
        end
        table.insert(State.destQueue,level)
      end
    end
  elseif event == "closed" then
    local door = Request[2]
    local lvl = State.levels[State.activeLevel]
    if lvl.door == door then
      sendLift(State)
    end
  elseif event == "subscribe" then
    local _,Co = unpack(Request)
    State.subscribers[Co]=true
  elseif event == "openDoor" then
    local level = Request[2]
  elseif event == "closeDoor" then
    local level = Request[2]
    if State.levels[level] then
      Door.close(State.levels.door)
    end
  end
  return State
end

function Elevator.handle_info(Request,State)
  local event = Request[1]
  if event == "wake" then
    VM.log("Received wake signal")
    if State.direction == "up" then
      State.activeLevel = State.activeLevel + 1 
    elseif State.direction == "down" then
      State.activeLevel = State.activeLevel - 1
    else
      error("Elevator received unexpected wake signal")
    end
    notify(State,"level",State.activeLevel)
--    VM.log("Elevator at "..State.activeLevel)
--    VM.log("Going to "..State.destQueue[1])
    if State.activeLevel ~= State.destQueue[1] then
      EVE.tick(State.delay)
    else
      table.remove(State.destQueue,1)
      State.direction = nil
      local lvl = State.levels[State.activeLevel]
      if lvl.callback ~= nil then
        lvl.callback()
      end
      lvl.waiting = false
      if lvl.door then
        openDoor(State)
      else
        sendLift(State)
      end
    end
  else
    VM.log("Elevator got: "..unpack(Request))
  end
  return State
end

return Elevator