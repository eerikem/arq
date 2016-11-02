local gen_server = require "gen_server"
local ui_server = require "ui_server"
local UI = require "lib.ui"
local Bundle = require "lib.bundle"
local Graphic = require "lib.graphic"
local Panel = require "lib.ui_obj"
local Password = require "password"
local doorUI = require "door_ui"

local CABLE_SIDE = "back"

local function initDoor(State)
  if State.door then
    State.door:disable()
  end
  if State.use_detector then
    State.detector:disable()
    State.inner = nil
  end
end

local Door = {}

----------------
--External API--
----------------

function Door.start()

  local properties = {
    type = "generic",
    --When set to true the inner monitor will be ignored
    --and the detector will be used instead
    use_detector = false,
    door_delay = 5,
    title = "ACCESS",
    door = Bundle:new(CABLE_SIDE,colors.white,"door"),
    detector = Bundle:new(CABLE_SIDE,colors.black,"detector"),
    outer = "monitor_1",
    inner = "monitor_5"
  }
  
  local ok, Co = Door.start_link(properties)
  VM.log("Started Door "..tostring(Co))
  if properties.use_detector then
    EVE.subscribe("redstone",Co)
  end
  return Co
end

function Door.startDetectorDoor(doorCableColor,detectorCableColor,monitor,ui_title,door_delay,password)
  if not doorCableColor or not detectorCableColor or not monitor then error("Badarg",2)end
  local delay = door_delay or 5
  local title = ui_title or "ACCESS"
  local properties = {
    type = "detector",
    use_detector = true,
    door_delay = delay,
    title = title,
    door = Bundle:new(CABLE_SIDE,doorCableColor,"door"),
    detector = Bundle:new(CABLE_SIDE,detectorCableColor,"detector"),
    outer = monitor,
    password = password
  }
  local ok, Co = Door.start_link(properties)
  VM.log("Started Detector Door "..tostring(Co))
  EVE.subscribe("redstone",Co)
  return Co
end

function Door.startMonitorDoor(doorCableColor,innerMonitor,outerMonitor,ui_title,door_delay,password)
  if not doorCableColor or not innerMonitor or not outerMonitor then error("Badarg",2)end
  local delay = door_delay or 5
  local title = ui_title or "ACCESS"
  local properties = {
    type = "monitor",
    use_detector = false,
    door_delay = delay,
    title = title,
    door = Bundle:new(CABLE_SIDE,doorCableColor,"door"),
    inner = innerMonitor,
    outer = outerMonitor,
    password = password
  }
  local ok, Co = Door.start_link(properties)
  VM.log("Started Monitor Door "..tostring(Co))
  return Co
end

function Door.startFakeDoor(monitor,ui_title)
  if not monitor then error("Badarg",2) end
  local title = ui_title or "ACCESS"
  local properties = {
    type = "fake",
    use_detector = false,
    title = title,
    outer = monitor,
    locked = true
  }
  local ok, Co = Door.start_link(properties)
  VM.log("Started Fake Door "..tostring(Co))
  return Co
end

function Door.open(door)
  return gen_server.call(door,{"open"})
end

function Door.forceOpen(door)
  VM.log("Called force Open")
  return gen_server.call(door,{"open",true})
end

function Door.forceClose(door)
  return gen_server.call(door,{"close",true})
end

function Door.close(door)
  return gen_server.call(door,{"close"})
end

function Door.lock(door)
  gen_server.cast(door,{"lock"})
end

function Door.unlock(door)
  gen_server.cast(door,{"unlock"})
end

function Door.getTitle(door)
  return gen_server.call(door,{"get","title"})
end

function Door.getType(door)
  return gen_server.call(door,{"get","type"})
end

function Door.getState(door)
  local locked = gen_server.call(door,{"get","locked"})
  local open = gen_server.call(door,{"get","open"})
  return open,locked
end

function Door.subscribe(door,co)
  local co = co or VM.running()
  gen_server.cast(door,{"subscribe",co})
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
      VM.log("resetting timer")
      Ref = EVE.timer(State.door_delay)
    elseif event == "cancel" then
      State.timer = nil
      break
    else
      VM.log("Warning: delay received unkown event: "..event)
    end
  end
end

local function notify(State,event)
  for Co,_ in pairs(State.subscribers) do
    gen_server.cast(Co,{event,VM.running()})
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

local function open(State)
  State.open = true
  State.door:enable()
  setTimer(State)
  notify(State,"opened")
end

local function close(State)
  State.open = false
  State.door:disable()
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
  
  local uis = {outer=doorUI.start_link(props.outer,props,VM.running(),Door)}
  if props.inner then uis.inner=doorUI.start_link(props.inner,props,VM.running(),Door) end
  local State = {
    uis = uis,
    title = props.title,
    locked = false,
    open = false,
    alreadyOn=false,
    timer = nil,
    subscribers = {}
    }
  
  for k,v in pairs(props) do
    State[k]=v
  end
  
  initDoor(State)
  return true, State
end

function Door.handle_call(Request,From,State)
  local event = Request[1]
  if event == "open" then
    local force = Request[2]
    if not force and State.locked or not State.door then
      gen_server.reply(From,"denied")
    else
      gen_server.reply(From,"opened")
      open(State)
    end
  elseif event == "close" then
    local force = Request[2]
    if State.locked and not force or not State.door then
      gen_server.reply(From,"denied")
    else
      gen_server.reply(From,"closed")
      close(State)
      if State.timer then
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
          if State.timer then VM.send(State.timer,"reset")end
        end
        State.alreadyOn = true
      end
    else
      VM.log("Setting already On to false")
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
    if State.open and not State.timer then
      setTimer(State)
    end
  elseif event == "subscribe" then
    local _,Co = unpack(Request)
    VM.log("Door subscribing "..tostring(Co))
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