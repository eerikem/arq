local gen_server = require "gen_server"
local UI = require "lib.ui"
local Graphic = require "lib.graphic"
local Panel = require "lib.ui_obj"
local Door = require 'door'
local static_ui = require "static_ui"


local function teleArriveSound()
  local x,y,z = 21,45,0
  local telesound = "/playsound fdi:event.teleport_general_arrive @a[%d,%d,%d,20]"
  exec(telesound,x,y,z)
end

local function teleLeaveSound()
local x,y,z = 21,45,0
  local telesound = "/playsound fdi:event.teleport_general @a[%d,%d,%d,20]"
  exec(telesound,x,y,z)
end


---
-- @type App_Server
-- @extends gen_server#server
local App_Server = {}


----------------
--External API--
----------------

---
-- The Teleporter API
local Teleporter = {}

---
-- @param #function callback The function to execute when teleport is called
-- @param #thread door optional door of teleport bay
-- @param #Bundle detector optional detector cable
-- @param #Bundle light optional
function Teleporter.start(...)
  local ok, co = gen_server.start_link(App_Server,{...})
  assert(ok,"The teleporter failed to start")
  return co
end

-- TODO why does gen_server cast fail when message is not in a table
---
-- Prime the teleporter for transit
function Teleporter.queueTransit(Co,timeout)
  gen_server.cast(Co,{"queue_transit",timeout})
end

---
-- Cancel transit if transit is queued
function Teleporter.cancel(Co)
  gen_server.cast(Co,{"cancel_transit"})
end

function Teleporter.subscribe(teleporter,co)
  if not teleporter then error("badarg",2) end
  local co = co or VM.running()
  gen_server.cast(teleporter,{"subscribe",co})
end

--------------
--  Server  --
--------------

---
-- The Server component of your App
-- Should implement the four gen_server functions

---
-- @param #function callback
-- @param #thread door
-- @param lib.bundle#Bundle detector
-- @param lib.bundle#Bundle light
function App_Server.init(callback,door,detector,light)
  ---
  --@type State
  local State = {
    callback = callback,
    door = door,
    ready = true,
    teleporting=false,
    primed=false,
    subscribers = {},
    detector = detector,
    light = light,
  }
  
  EVE.subscribe("redstone")
  
  return true, State
end


--TODO standardise this behaviour in a super class
local function notify(State,event,...)
  for Co,_ in pairs(State.subscribers) do
    gen_server.cast(Co,{event,unpack(arg)})
  end
end


---
-- @param Request
-- @param #State State
function App_Server.handle_cast(Request,State)
  local event = Request[1]
  if event == "queue_transit" then
    if State.ready then
      State.ready = false
      State.primed = true
      Door.forceOpen(State.door)
      State.light:enable()
      notify(State,"primed")
    end
  elseif event == "redstone" then
    if State.detector:isOn() and State.primed then
      State.primed = false
      State.teleporting = true
      notify(State,"teleporting")
      Door.forceClose(State.door)
      teleLeaveSound()
      EVE.queue("teleport",8)
      EVE.queue("teleport_complete",10)
    elseif not State.detector:isOn() and State.ready then
      Door.forceClose(State.door)
    end
  elseif event == "teleport" then
    if State.teleporting then
      State.callback()
    end
  elseif event == "teleport_complete" then
    if State.teleporting then
      State.teleporting = false
      State.ready = true
      notify(State,"teleported")
      State.light:disable()
      if State.detector:isOn() then
        Door.forceOpen(State.door)
      end
      EVE.queue("reset_telehub",4)
    end
  elseif event == "reset_telehub" then
    notify(State,"telehub_ready")
  elseif event == "subscribe" then
    local _,Co = unpack(Request)
    State.subscribers[Co]=true
  end
  return State
end

function App_Server.handle_call(Request,From,State)

  return State
end

function App_Server.handle_info(Request,State)

  return State
end

return Teleporter