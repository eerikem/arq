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
-- @param #teleProps properties The teleporter properties. Refer to @{telehub#Teleporter.newProps}
function Teleporter.start(properties)
  local ok, co = gen_server.start_link(App_Server,{properties})
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

--- A collection of utility functions that will return
-- generic callback function for passing into the Teleporter 
Teleporter.callbacks = {
  --- Returns a callback function that will play a generic
  -- teleport arrival sound at the specified coords.
  -- @return #function callback
  genArrivalSound = function(x,y,z)
    return function()
      local telesound = "/playsound fdi:event.teleport_general_arrive @a[%d,%d,%d,23]"
      exec(telesound,x,y,z)
    end
  end,
  --- Returns a callback function that will play a generic
  -- teleport departure sound at the specified coords.
  -- @return #function callback
  genDepartureSound = function(x,y,z)
    return function() local telesound = "/playsound fdi:event.teleport_general @a[%d,%d,%d,23]"
      exec(telesound,x,y,z)
    end
  end,
  genTeleportCmd = function(x,y,z,dim,dx,dy,dz)
    return function()
      exec("tpx @a[x=%d,y=%d,z=%d,r=1] %d %d %d %d",x,y,z,dim,dx,dy,dz)
    end
  end,
}

---
--@type teleProps
--@extends #props
--@field #thread door optional door of teleport bay
--@field #Bundle detector optional detector cable
--@field #Bundle light optional

---
-- @param #table bayCoords the coordinates of the teleport bay
-- @param #table destCoords the coordinates including the dimension, of the location to teleport to
-- @param #string optional the name of the emitter
-- @usage Teleporter.newProps({x,y,z},{dim,x,y,z},"emitter_x")
-- @return telehub#teleProps
function Teleporter.newProps(bayCoords,destCoords,emitter)
  local x,y,z = unpack(bayCoords)
  local dim,dx,dy,dz = unpack(destCoords)
  ---
  --@type props
  --@field #function arrivalTele The function to execute when someone arrives from a teleport
  --@field #function startTele The function to execute at the start of teleport
  --@field #function callback Execute teleport command
  --@field #number timeToTeleport The time to wait between executing startTeleporter and callback
  --@field #number timeToComplete The time to wait before signalling teleport process is complete
  --@field emitter optional emitter peripheral
  local p = {
    arrivalTele = Teleporter.callbacks.genArrivalSound(x,y,z),
    startTele = Teleporter.callbacks.genDepartureSound(x,y,z),
    callback = Teleporter.callbacks.genTeleportCmd(x,y,z,dim,dx,dy,dz),
    timeToTeleport = 8,
    timeToComplete = 10,
  }
  
  if emitter then
    local emit = peripheral.wrap(emitter)
    emit.setSpace(x-1,y,z-1,x+1,y+2,z+1)
    p.emitter = emit
  end
  return p
end

--------------
--  Server  --
--------------

---
-- The Server component of your App
-- Should implement the four gen_server functions

---
-- @param #teleProps properties
function App_Server.init(props)
  ---
  --@type State
  --@extends #teleProps
  local State = {
    ready = true,
    teleporting=false,
    primed=false,
    subscribers = {},
    lastDetectorState = props.detector:isOn(),
  }
  
  for k,v in pairs(props) do State[k] = v end
  
  if State.door then
    Door.lock(State.door)
  end
  
  if State.emitter then
    State.emitter.setParticleType("fireworksSpark")
    State.emitter.setEmitting(false)
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
--@param #State State
local function detectorChanged(State)
  if State.lastDetectorState ~= State.detector:isOn() then
    State.lastDetectorState = State.detector:isOn()
    return true
  else
    return false
  end
end

---
-- @param Request
-- @param #State State
function App_Server.handle_cast(Request,State)
  local event = Request[1]
--  if State.ready then VM.log("State is ready") end
--  VM.log("telehub received event "..event)
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
    elseif detectorChanged(State) then 
      if State.detector:isOn() then
        if State.ready then
          State.ready = false
          if State.emitter then
            State.emitter.setRate(1)
            State.emitter.setEmitting(true)
          end
          State.arrivalTele()
          notify(State,"teleport_arrival")
          EVE.queue("teleport_arrival",4)
        else
--          VM.log("State wasn't ready")  
        end
      else
        Door.forceClose(State.door)
        State.ready = true
      end
    else
--      VM.log("Couldn't satisfy any redstone related conditions")
    end
  elseif event == "teleport_arrival" then
    if State.emitter then
      State.emitter.setEmitting(false)
    end
    notify(State,"arrival_complete")
    Door.forceOpen(State.door)
    EVE.queue("reset_telehub",4)
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