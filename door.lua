local gen_server = require "gen_server"
local ui_server = require "ui_server"
local UI = require "lib.ui"
local Graphic = require "lib.graphic"
local Panel = require "lib.ui_obj"
local Password = require "password"
local doorUI = require "door_ui"

---
-- @module door
-- A module for doors of all types!
local Door = {}

----------------
--External API--
----------------

---
-- @function [parent=#door] new
-- @param lib.bundle#Bundle cable that controls the door
-- @param #number time optional delay before the door auto closes
-- @param lib.bundle#Bundle detector Optional detector cable
-- @return #thread
function Door.new(cable,time,detector)
  if not cable then error("Door must have a cable",2) end
  local properties = {
    type = "normal",
    door_delay = time or 5,
    doorCable = cable,
    detector = detector
  }
  local ok, Co = Door.start_link(properties)
  return Co
end

---
-- @function [parent=#door] newCargo
-- @param lib.bundle#Bundle open opens bay door
-- @param lib.bundle#Bundle close closes bay door
-- @param #number time how long to open/close door
-- @param lib.bundle#Bundle detector Optional detector cable
-- @return #thread
function Door.newCargo(open,close,time,detector)
  if not (open and close) then error("Badarg: cables missing",2) end
  if not time then error("Badarg: time missing",2) end
  local properties = {
    type = "cargo",
    time = time,
    detector = detector,
    openCable = open,
    closeCable = close
  }
  local ok, Co = Door.start_link(properties)
  return Co
end

---
-- @function [parent=#door] newUI
-- @param #string monitor
-- @param #string title
-- @param #thread door to subscribe to
-- @param #string password optional
-- @return #thread
function Door.newUI(monitor,title,door,password)
  if not monitor then error("Badarg: monitor",2) end
  local title = title or "ACCESS"
  return doorUI.start_link(monitor,title,door,Door,password)
end

---
-- @param #thread door
function Door.open(door)
  return gen_server.call(door,{"open"})
end

---
-- Open the door whether locked or unlocked
-- @param #thread door
function Door.forceOpen(door)
  return gen_server.call(door,{"open",true})
end

---
-- @param #thread door
function Door.forceClose(door)
  return gen_server.call(door,{"close",true})
end

---
-- @param #thread door
function Door.close(door)
  return gen_server.call(door,{"close"})
end

---
-- @param #thread door
function Door.lock(door)
  gen_server.cast(door,{"lock"})
end

---
-- @param #thread door
function Door.unlock(door)
  gen_server.cast(door,{"unlock"})
end

---
-- @param #thread door
function Door.getTitle(door)
  return gen_server.call(door,{"get","title"})
end

---
-- @param #thread door
function Door.getType(door)
  return gen_server.call(door,{"get","type"})
end

---
-- @param #thread door
function Door.getState(door)
  local locked = gen_server.call(door,{"get","locked"})
  local open = gen_server.call(door,{"get","open"})
  return open,locked
end

---
-- Instruct a door to subscribe to another coroutine
-- @param #thread door The door that will subscribe
-- @param #thread co Optional. If omitted the door will subscribe to the running coroutine.
function Door.subscribe(door,co)
  if not door then error("badarg",2) end
  local co = co or VM.running()
  gen_server.cast(door,{"subscribe",co})
end

---
-- @param #thread doorUI
function Door.denyAccess(doorUI)
  gen_server.cast(doorUI,{"denyAccess"})
end

---
-- @param #thread doorUI
function Door.allowAccess(doorUI)
  gen_server.cast(doorUI,{"allowAccess"})
end
---------------
--Server & UI--
---------------

function Door.start_link(properties)
  return gen_server.start_link(Door,{properties},{})
end

local function delay(door,State)
  local Ref = EVE.timer(State.door_delay)
  while true do
    local event,ref = VM.receive()
    if event == "wake" and Ref == ref then
      gen_server.cast(door,{"close"})
      State.timer = nil
      break
    elseif event == "reset" then
      Ref = EVE.timer(State.door_delay)
    elseif event == "cancel" then
      State.timer = nil
      break
    end
  end
end

local function notify(State,event)
  for Co,_ in pairs(State.subscribers) do
    gen_server.cast(Co,{event,State.Co})
  end
end

local function setTimer(State)
  local Co = VM.running()
  if not State.timer then
    State.timer = VM.spawn(function()delay(Co,State)end)
  else
    VM.send(State.timer,"reset")
  end
end


local function bayDoorTimer(callback,siloDelay)
  local time = 0
  local r,sleep,reverse = VM.receive()
  if r == "start" then
    time = os.clock()
    if sleep then
      VM.log("silo sleeping "..sleep % siloDelay)
      EVE.tick(sleep % siloDelay)
    else
      VM.log("silo sleeping "..siloDelay)
      EVE.tick(siloDelay)
    end
  else
    error("door Timer received bad signal")
  end
  local r,to = VM.receive()
  if r == "wake" then
    return callback()
  elseif r == "stop_timer" then
    if to then
      if reverse then
        return VM.send(to,"start",siloDelay - (os.clock() - time),false)
      else
        return VM.send(to,"start",os.clock() - time,true)
      end
    end
  else
    error("doorTimer received bad msg: "..r)
  end
end

local function cargoOpen(State)
  State.closeCable:disable()
  State.openCable:enable()
  notify(State,"opened")
  State.opening = true
  State.closed = false
  local fun = function()
    State.openCable:disable()
    State.opening = false
    State.open = true
    State.timer = nil end
  local newTimer = VM.spawn(function()bayDoorTimer(fun,State.time)end)
  if State.closing then
    VM.send(State.timer,"stop_timer",newTimer)
    State.timer = newTimer
  else
    State.timer = newTimer
    VM.send(newTimer,"start")
  end
end

local function cargoClose(State)
  State.openCable:disable()
  State.closeCable:enable()
  notify(State,"closing")
  State.closing = true
  State.open = false

  local fun = function()
    State.closeCable:disable()
    State.closing = false
    State.closed = true
    State.timer = nil
    notify(State,"closed")
  end
  local newTimer = VM.spawn(function()bayDoorTimer(fun,State.time)end)
  if State.opening then
    VM.send(State.timer,"stop_timer",newTimer)
    State.timer = newTimer
  else
    State.timer = newTimer
    VM.send(newTimer,"start")
  end
end

local function initCables(State)
  if State.doorCable then
    State.doorCable:disable()
  else
    State.openCable:disable()
    State.closeCable:disable()
    cargoClose(State)
  end
  if State.detector then
    State.detector:disable()
  end
end

local function open(State)
  if not State.doorCable then
    return cargoOpen(State) end
  State.open = true
  State.doorCable:enable()
  setTimer(State)
  notify(State,"opened")
end

local function close(State)
  if not State.doorCable then
    return cargoClose(State) end
  State.open = false
  State.doorCable:disable()
  notify(State,"closed")
end

local function lock(State)
  State.locked = true
  notify(State,"locked")
end

local function unlock(State)
  State.locked = false
  notify(State,"unlocked")
end

function Door.init(props)
  local State = {
    alreadyOn=false,
    timer = nil,
    subscribers = {},
    locked = false,
    open = false,
    opening = false,
    closing = false,
    closed=true,
    Co = VM.running()
  }

  for k,v in pairs(props) do
    State[k]=v
  end

  initCables(State)
  if State.detector then
    EVE.subscribe("redstone")
  end
  return true, State
end

function Door.handle_call(Request,From,State)
  local event = Request[1]
  if event == "open" then
    local force = Request[2]
    if not force and State.locked then
      gen_server.reply(From,"denied")
    else
      gen_server.reply(From,"opened")
      open(State)
    end
  elseif event == "close" then
    local force = Request[2]
    if State.locked and not force then
      gen_server.reply(From,"denied")
    else
      gen_server.reply(From,"closed")
      close(State)
      if State.timer and State.type == "normal" then
        VM.send(State.timer,"cancel") end
    end
  elseif event == "get" then
    local _,item = unpack(Request)
    if State[item] ~= nil then
      gen_server.reply(From,State[item])
    else
      gen_server.reply(From,nil)
    end
  end
  return State
end

function Door.handle_cast(Request,State)
  local event = unpack(Request)
  if event == "redstone" then
    if State.detector:isOn() and not State.locked then
      if not State.alreadyOn then
        if not State.open then
          open(State)
        else
          if State.timer and State.type == "normal" then
            VM.send(State.timer,"reset")
          end
        end
        State.alreadyOn = true
      end
    else
      State.alreadyOn = false
    end
  elseif event == "close" then
    --"Close signal from timer."
    if not State.locked then
      close(State)
    end
  elseif event == "lock" then
    lock(State)
  elseif event == "unlock" then
    unlock(State)
    if State.open and not State.timer and State.type == "normal" then
      setTimer(State)
    end
  elseif event == "subscribe" then
    local _,Co = unpack(Request)
    State.subscribers[Co]=true
  else
    VM.log("Received "..Request)
  end
  return State
end

function Door.handle_info(Request,State)
  VM.log("got: "..unpack(Request))
  return State
end

return Door
