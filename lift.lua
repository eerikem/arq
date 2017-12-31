local gen_server = require "gen_server"
local ui_server = require "ui_server"
local UI = require "lib.ui"
local Graphic = require "lib.graphic"
local Panel = require "lib.ui_obj"
local Password = require "password"
local liftUI = require "lift_ui"


---
-- @module lift

local Lift = {}

----------------
--External API--
----------------

---
-- @function [parent=#lift] new
-- @param lib.bundle#lib.bundle cable that controls the door
-- @param lib.bundle#lib.bundle cable that controls the door
-- @param #number time time of lift
-- @return #thread
function Lift.new(cableUp,cableDown,time)
  if not (cableUp and cableDown and time) then error("Lift missing args",2) end
  local properties = {
    delay = time,
    upCable = cableUp,
    downCable = cableDown
  }
  local ok, Co = Lift.start_link(properties)
  return Co
end

---
-- @function [parent=#lift] newUI
-- @param #string monitor
-- @param #thread lift to subscribe to
-- @param #string title optional
-- @param #string password optional
-- @return #thread
function Lift.newUI(monitor,lift,title,password)
  if not monitor then error("Badarg: monitor",2) end
  local title = title or "Lift"
  return liftUI.start_link(monitor,title,lift,Lift,password)
end


function Lift.call(door)
  return gen_server.call(door,{"call"})
end

function Lift.forceCall(door)
  return gen_server.call(door,{"call",true})
end

function Lift.lock(door)
  gen_server.cast(door,{"lock"})
end

function Lift.unlock(door)
  gen_server.cast(door,{"unlock"})
end

function Lift.getTitle(door)
  return gen_server.call(door,{"get","title"})
end

function Lift.registerUI(Lift,Ui)
  if not Lift then error("badarg",2) end
  local co = co or VM.running()
  gen_server.cast(Lift,{"register_ui",co})
end

function Lift.subscribe(door,co)
  if not door then error("badarg",2) end
  local co = co or VM.running()
  gen_server.cast(door,{"subscribe",co})
end

function Lift.denyAccess(doorUI)
  gen_server.cast(doorUI,{"denyAccess"})
end

function Lift.allowAccess(doorUI)
  gen_server.cast(doorUI,{"allowAccess"})
end

---------------
--Server & UI--
---------------

function Lift.start_link(properties)
  return gen_server.start_link(Lift,{properties},{})
end

local function notify(State,event)
  for Co,_ in pairs(State.subscribers) do
    gen_server.cast(Co,{event,VM.running()})
  end
end

local function delay(door,State)
  local Ref = EVE.timer(State.delay)
  while true do
    local event,ref = VM.receive()
    if event == "wake" and Ref == ref then
      gen_server.cast(door,{"arrived"})
      State.timer = nil
      break
    elseif event == "reset" then
      Ref = EVE.timer(State.delay)
    elseif event == "cancel" then
      State.timer = nil
      break
    end
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

--TODO Lift Timer


local function initCables(State)
  State.upCable:enable()
  State.downCable:disable()
end

local function lock(State)
  State.locked = true
  notify(State,"locked")
end

local function unlock(State)
  State.locked = false
  notify(State,"unlocked")
end

function Lift.init(props)
  local State = {
    alreadyOn=false,
    timer = nil,
    subscribers = {},
    locked = false,
    up = true,
    down=false,
    ascending = false,
    descending = false
  }

  for k,v in pairs(props) do
    State[k]=v
  end

  initCables(State)
  return true, State
end

local function call(State)
  VM.log("Calling Lift")
  if State.up then
    State.descending = true
    State.upCable:disable()
    State.downCable:enable()
  else
    State.ascending = true
    State.downCable:disable()
    State.upCable:enable()
  end
  State.up = false
  State.down = false
  setTimer(State)
  notify(State,"called")
end

function Lift.handle_call(Request,From,State)
  local event = Request[1]
  if event == "call" then
    local force = Request[2]
    if not force and State.locked then
      gen_server.reply(From,"denied")
    else
      gen_server.reply(From,"called")
      call(State)
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

local function arrived(State)
  --Switch which platform has the lift
  local Co = State.readyPlatform
  State.readyPlatform = State.emptyPlatform
  State.emptyPlatform = Co

  State.upCable:disable()
  State.downCable:disable()

  if State.descending then
    State.descending = false
    State.down = true
  else
    State.ascending = false
    State.up = true
  end
  
  notify(State,"arrived")
end

function Lift.handle_cast(Request,State)
  local event = unpack(Request)
  if event == "lock" then
    lock(State)
  elseif event == "arrived" then
    arrived(State)
  elseif event == "unlock" then
    unlock(State)
  elseif event == "register_ui" then
    local _,Co = unpack(Request)
    if State.readyPlatform then
      State.emptyPlatform = Co
    else
      State.readyPlatform = Co
      gen_server.cast(Co,{"arrived"})
    end
    State.subscribers[Co]=true
  elseif event == "subscribe" then
    local _,Co = unpack(Request)
    State.subscribers[Co]=true
  else
    VM.log("Received "..Request)
  end
  return State
end

function Lift.handle_info(Request,State)
  VM.log("got: "..unpack(Request))
  return State
end

return Lift

