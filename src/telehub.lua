local gen_server = require "gen_server"
local UI = require "lib.ui"
local Graphic = require "lib.graphic"
local Panel = require "lib.ui_obj"
local Door = require 'door'
local static_ui = require "static_ui"

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
-- @param #function startTele The function to execute at the start of teleport
-- @param #function callback The function to execute the moment of teleportation
-- @param #number timeToTeleport The time to wait between executing startTeleporter and callback
-- @param #number timeToComplete The time to wait before signalling teleport process is complete
-- @param #thread door optional door of teleport bay
-- @param #Bundle detector optional detector cable
-- @param #Bundle light optional
-- @param #string emitter optional
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
-- @param #function startTele
-- @param #function callback
-- @param #number timeToTeleport
-- @param #number timeToComplete
-- @param #thread door
-- @param lib.bundle#Bundle detector
-- @param lib.bundle#Bundle light
-- @param #emitter emitter
function App_Server.init(startTele,callback,timeToTeleport,timeToComplete,door,detector,light,emitter)
  ---
  --@type State
  local State = {
    startTele = startTele,
    callback = callback,
    timeToTeleport = timeToTeleport,
    timeToComplete = timeToComplete,
    door = door,
    ready = true,
    teleporting=false,
    primed=false,
    subscribers = {},
    detector = detector,
    light = light,
    emitter = emitter,
  }
  
  if State.emitter then
    emitter.setParticleType("fireworksSpark")
    emitter.setEmitting(false)
  end
  
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
      if State.emitter then
        State.emitter.setRate(0.1)
        State.emitter.setEmitting(true)
      end
    end
  elseif event == "redstone" then
    if State.detector:isOn() and State.primed then
      State.primed = false
      State.teleporting = true
      notify(State,"teleporting")
      Door.forceClose(State.door)
      State.startTele()
      EVE.queue("teleport",State.timeToTeleport)
      EVE.queue("teleport_complete",State.timeToComplete)
    elseif not State.detector:isOn() and State.ready then
      Door.forceClose(State.door)
    end
  elseif event == "teleport" then
    if State.teleporting then
      State.callback()
      if State.emitter then
        State.emitter.setRate(2)
      end
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
      if State.emitter then
        State.emitter.setRate(1)
        State.emitter.setEmitting(false)
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