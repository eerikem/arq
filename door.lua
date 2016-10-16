local gen_server = require "gen_server"
local UI = require "ui"
local Bundle = require "bundle"
local Graphic = require "graphic"
local Panel = require "ui_obj"
local Password = require "password"

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

function Door.subscribe(door)
  gen_server.cast(door,{"subscribe",VM.running()})
end
---------------
--Server & UI--
---------------

function Door.start_link(properties)
  return gen_server.start_link(Door,{properties},{})
end

local function doorUI(Co,door)
  local ui_co, ui = UI.start(Co,7,5)
  ui.co = ui_co
  ui.handle = function(...)UI.handleEvent(ui_co,...)end
  local title = Graphic:new(door.title)
  local body = Panel:new()
  local open = Graphic:new("OPEN")
  local close = Graphic:new("CLOSE")
  local status = Graphic:new("       ")
  body.width = "max"
  open.xpos = 2
  open.ypos = 2
  close.xpos = 2
  close.ypos = 2
  ui:add(title)
  body:add(open)
  body:add(status)
  ui:add(body)
  
  close.reactor:stop()
  
  --TODO Remove Duplicate Hacks
  local function enable(button2)
    local button1 = body.index[1]
    if button1 ~= button2 then
      body:replace(button1,button2)
      button1.reactor:stop()
      button2.reactor:start()
      ui:update()
    end
    return button1
  end
  
  local lastDenied = nil
  local flashDenied = function()
        EVE.sleep(1)
        if lastDenied == VM.running() then
          status.text="       "
          ui:update()
          lastDenied = nil
        end
      end
  
  local function handler(door,reactor)
    return function()
      if reactor.parent == open then
        local res = Door.open(door)
        if res == "opened" then
          ui:ping()
          enable(close)
        elseif res == "canceled" then
          VM.log("received Canceled")
          ui:tap()
        else
          ui:beep()
          status.text="Denied!"
          ui:update()
          lastDenied = VM.spawn(flashDenied)
        end
      elseif reactor.parent == close then
        local res = Door.close(door)
        if res == "closed" then
          ui:ping()
          enable(open)
        else
          ui:beep()
          status.text="Denied!"
          ui:update()
          lastDenied = VM.spawn(flashDenied)
        end
      end
    end
  end
  
  local function closeHandler()
    enable(open)
  end
  
  local function openHandler()
    enable(close)
  end
  
  open:setOnSelect(ui,handler(VM.running(),open.reactor))
  close:setOnSelect(ui,handler(VM.running(),close.reactor))
  ui.reactor:register("closed",closeHandler)
  ui.reactor:register("opened",openHandler)
  
  
  local function bright()
    ui:setBackground(colors.lightGray)
    ui:setText(colors.gray)
    body:setTextColor(colors.orange)
    body:setBackgroundColor(colors.gray)
    status:setTextColor(colors.red)
  end
  
  bright()
  ui:update()
  
  return ui
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
    VM.log("Sending "..event.." to "..tostring(Co))
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
  if State.door then State.door:disable() end
  if State.locked then
    notify(State,"locked")
  else
    notify(State,"closed")
  end
end

function Door.init(props)
  
  local uis = {outer=doorUI(props.outer,props)}
  if props.inner then uis.inner=doorUI(props.inner,props) end
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
      if State.password and not force then
        local mon = From[1]
        VM.log("from: "..tostring(mon))
        VM.log("outer ui co: "..tostring(State.uis.outer.co))
        if State.uis.inner then
        VM.log("inner ui co: "..tostring(State.uis.inner.co)) end
        if mon == State.uis.outer.co then
          mon = State.outer
        else
          mon = State.inner
        end
        local Mod = {
          success = function (door,From)
            Door.forceOpen(door)
            gen_server.reply(From,"opened")
            end,
          canceled = function (From)
            gen_server.reply(From,"canceled")
          end
        }
        Password.start(State.password,mon,
          {Mod,"success",{VM.running(),From}},
          {Mod,"canceled",{From}})
        return State --TODO handle waiting for password
      end
      gen_server.reply(From,"opened")
      if State.inner then State.uis.inner.handle("opened") end
      State.uis.outer.handle("opened")
      open(State)
    end
  elseif event == "close" then
    local force = Request[2]
    if State.locked and not force then
      gen_server.reply(From,"denied")
    else
      gen_server.reply(From,"closed")
      if State.inner then State.uis.inner.handle("closed") end
      State.uis.outer.handle("closed")
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
          State.uis.outer.handle("opened")
          if State.inner then State.uis.inner.handle("opened") end
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
      State.uis.outer.handle("closed")
      if State.inner then State.uis.inner.handle("closed") end
      close(State)
    end
  elseif event == "lock" then
    State.locked = true
    notify(State,"locked")
    --not automatically closing when locked
    --close(State)
  elseif event == "unlock" then
    State.locked = false
    notify(State,"unlocked")
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