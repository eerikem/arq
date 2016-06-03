local gen_server = require "gen_server"
local ui_server = require "ui_server"
local Bundle = require "bundle"
local Graphic = require "graphic"
local Panel = require "ui_obj"

--When set to true the inner monitor will be ignored
--and the detector will be used instead
local USE_DETECTOR = false

local CABLE_SIDE = "back"
local DOOR_DELAY = 5
local TITLE = "ACCESS"
local door = Bundle:new(CABLE_SIDE,colors.white,"door")
local detector = Bundle:new(CABLE_SIDE,colors.black,"detector")
local outer = "monitor_1"
local inner = "monitor_5"

local function initDoor()
  door:disable()
  detector:disable()
  if USE_DETECTOR then
    inner = nil
  end
end

local timer = nil
local Door = {}

----------------
--External API--
----------------

function Door.start()
  local Co = Door.start_link()
  VM.log("Started "..tostring(Co))
  EVE.subscribe("redstone",Co)
end

function Door.open(door)
  return gen_server.call(door,{"open"})
end

function Door.close(door)
  return gen_server.call(door,{"close"})
end

function Door.lock(door)
  return gen_server.call(door,{"lock"})
end

---------------
--Server & UI--
---------------

function Door.start_link()
  return gen_server.start_link(Door,{},{})
end

local function doorUI(Co)
  local ui = ui_server.newWindow(Co,7,5)
  local title = Graphic:new(TITLE)
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
    body:replace(button1,button2)
    button1.reactor:stop()
    button2.reactor:start()
    ui:update()
    return button1
  end
  
  local function handler(door,reactor)
    return function()
      if reactor.parent == open then
        local res = Door.open(door)
        if res == "opened" then
          ui:ping()
          enable(close)
        else
          ui:beep()
          status.text="Denied"
        end
      elseif reactor.parent == close then
        local res = Door.close(door)
        if res == "closed" then
          ui:ping()
          enable(open)
        else
          ui:beep()
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

local function delay(door)
  local Ref = EVE.timer(DOOR_DELAY)
  while true do
    local event,ref = VM.receive()
    if event == "wake" and Ref == ref then
      gen_server.cast(door,{"close"})
      timer = nil
      break
    elseif event == "reset" then
      VM.log("resetting timer")
      Ref = EVE.timer(DOOR_DELAY)
    elseif event == "cancel" then
      timer = nil
      break
    else
      VM.log("Warning: delay received unkown event: "..event)
    end
  end
end

local function open()
  door:enable()
  local Co = VM.running()
  if not timer then
    timer = VM.spawn(function()delay(Co)end)
  else
    VM.send(timer,"reset")
  end
end

function Door.init()
  initDoor()
  local uis = {outer=doorUI(outer)}
  if inner then uis.inner=doorUI(inner) end
  local State = {uis = uis,locked = false,open = false,alreadyOn=false}
  return State
end

function Door.handle_call(Request,From,State)
  local event = Request[1]
  if event == "open" then
    if State.locked then
      gen_server.reply(From,"denied")
    else
      gen_server.reply(From,"opened")
      if inner then State.uis.inner.reactor:handleEvent("opened") end
      State.open = true
      open()
    end
  elseif event == "close" then
    gen_server.reply(From,"closed")
    if inner then State.uis.inner.reactor:handleEvent("closed") end
    State.open = false
    door:disable()
    if timer then
      VM.send(timer,"cancel") end
  end
  return State
end

function Door.handle_cast(Request,State)
  local event = unpack(Request)
  if event == "redstone" then
    if detector:isOn() then
      if not State.alreadyOn then
        if not State.open then
          open()
          State.uis.outer.reactor:handleEvent("opened")
        else
          if timer then VM.send(timer,"reset")end
        end
        State.alreadyOn = true
      end
    else
      VM.log("Setting already On to false")
      State.alreadyOn = false
    end
  elseif event == "close" then
    State.uis.outer.reactor:handleEvent("closed")
    if inner then State.uis.inner.reactor:handleEvent("closed") end
    door:disable()
    State.open = false
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