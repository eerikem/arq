local gen_server = require "gen_server"
local Bundle = require "bundle"
local Graphic = require "graphic"
local Panel = require "ui_obj"

local CABLE_SIDE = "back"
local DELAY = 2
local MIDDLE = "monitor_5"
local doors = {
  outer = Bundle:new(CABLE_SIDE,colors.lightBlue,"monitor_1"),
  inner = Bundle:new(CABLE_SIDE,colors.lime,"monitor_3")
--  outer = false,
--  inner = false
}

local function initDoors(doors)
  VM.log("Running initDoors")
  for _,door in pairs(doors) do
    door:disable()
  end
end

local function open(door)
  doors[door]:enable()
--  doors[door] = true
end
local function close(door)
  doors[door]:disable()
--  doors[door] = false
end

local Airlock = {}

----------------
--External API--
----------------

function Airlock.open(Co,door)
  return gen_server.call(Co,{"open",door})
end

function Airlock.close(Co,door)
  return gen_server.call(Co,{"close",door})
end

function Airlock.cycle(Co)
  return gen_server.call(Co,{"cycle"})
end

function Airlock.start()
  local Co = Airlock.start_link()
  VM.register("airlock",Co)
end

---------------
--Server & UI--
---------------

function Airlock.start_link()
  return gen_server.start_link(Airlock,{},{})
end

local function outerUI(Co,door)
  local ui = ui_server.newWindow(Co,7,5)
  local title = Graphic:new("AIRLOCK")
  local body = Panel:new()
  local open = Graphic:new("OPEN")
  local close = Graphic:new("CLOSE")
  local cycling = Graphic:new("CYCLING")
  local cycler = Graphic:new("       ")
  body.width = "max"
  open.xpos = 2
  open.ypos = 2
  close.xpos = 2
  close.ypos = 2
  cycling.ypos = 2
  ui:add(title)
  body:add(open)
  body:add(cycler)
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
  
  local function tock(A,B,C)
    cycler.text = A
    ui:update()
    EVE.tick(0.2)
    local r = VM.receive()
    if r == "wake" then
      return tock(B,C,A)
    elseif r == "stop" then
      cycler.text = "       "
      ui:update()
    else
      error("cycleHandler received bad msg")
    end
  end
  
  local function tick()
    tock(" >  >  ","  >  > ",">  >  >")
  end
  
  local ticker = nil
  
  local function cycle()
    enable(cycling)
    ticker = VM.spawn(tick)
  end

  local function handler(Co,reactor)
    return function()
      if reactor.parent == open then
        local res = Airlock.open(Co,door)
        if res == "opened" then
          ui:ping()
          enable(close)
        elseif res == "open" then
          ui:tap()
          enable(close)
        elseif res == "cycling" then
          ui:ping()
          cycle()
        end
      elseif reactor.parent == close then
        local res = Airlock.close(Co,door)
        if res == "closed" then
          ui:ping()
          enable(open)
        else
          ui:tap()
          enable(open)
        end
      end
    end
  end
  
  local function stopTicker()
    if ticker then
      VM.log("Sending stop to ticker")
      VM.send(ticker,"stop")
    else
      error("No ticker to stop!",2)
    end
    ticker = nil
  end
  
  local function openHandler()
    stopTicker()
    enable(close)
  end
  
  local function closeHandler()
    stopTicker()
    enable(open)
  end
  
  open:setOnSelect(ui,handler(VM.running(),open.reactor))
  close:setOnSelect(ui,handler(VM.running(),close.reactor))
  ui.reactor:register("close",closeHandler)
  ui.reactor:register("open",openHandler)
  ui.reactor:register("cycle",cycle)
  
  local function bright()
    ui:setBackground(colors.lightGray)
    ui:setText(colors.gray)
    body:setTextColor(colors.orange)
    body:setBackgroundColor(colors.gray)
    cycling:setTextColor(colors.white)
    cycler:setTextColor(colors.orange)
  end
    
  bright()
  ui:update()
  
  return ui
end

local function innerUI(Co)
  local ui = ui_server.newWindow(Co,7,5)
  local title = Graphic:new("AIRLOCK")
  local body = Panel:new()
  local button = Graphic:new("CYCLE")
  local cycling = Graphic:new("CYCLING")
  local cycler = Graphic:new("       ")
  local cycling = Graphic:new("CYCLING")
  local cycler = Graphic:new("       ")
  body.width = "max"
  button.xpos = 2
  button.ypos = 2
  cycling.ypos = 2
  ui:add(title)
  body:add(button)
  body:add(cycler)
  ui:add(body)
  
  --TODO Remove Duplicate Hacks
  local function enable(button2)
    local button1 = body.index[1]
    body:replace(button1,button2)
    button1.reactor:stop()
    button2.reactor:start()
    ui:update()
    return button1
  end
  
  local ticker = nil
  
  local function tock(A,B,C)
    cycler.text = A
    ui:update()
    EVE.tick(0.2)
    local r = VM.receive()
    if r == "wake" then
      return tock(B,C,A)
    elseif r == "stop" then
      cycler.text = "       "
      ui:update()
    else
      error("cycleHandler received bad msg")
    end
  end
  
  local function tick()
    tock(" >  >  ","  >  > ",">  >  >")
  end
  
  local function cycle()
    enable(cycling)
    ticker = VM.spawn(tick)
  end
  
  local function handler(Co)
    return function()
      local res = Airlock.cycle(Co)
      if res == "cycling" then
        ui:ping()
      else
        ui:beep()
      end
    end
  end
  
  local function stopTicker()
    if ticker then
      VM.log("Sending stop to ticker "..tostring(ticker))
      VM.send(ticker,"stop")
    else
      error("No ticker to stop!",2)
    end
    ticker = nil
  end
  
  local function cycleHandler()
    stopTicker()
    enable(button)
  end
  
  button:setOnSelect(ui,handler(VM.running()))
  ui.reactor:register("cycle",cycle)
  ui.reactor:register("stop_cycle",cycleHandler)
  
  local function bright()
    ui:setBackground(colors.lightGray)
    ui:setText(colors.gray)
    body:setTextColor(colors.orange)
    body:setBackgroundColor(colors.gray)
    cycling:setTextColor(colors.white)
    cycler:setTextColor(colors.orange)
  end
  
  bright()
  ui:update()
  
  return ui
end

function Airlock.testUI(Co)
  return outerUI(Co), innerUI(Co)
end

function Airlock.init()
  initDoors(doors)
  local uis = {}
  local last = "inner"
  for name,door in pairs(doors) do
    uis[name] = outerUI(door.name,name)
  end
  local inner = innerUI(MIDDLE)
  local State = {doors = doors,cycling = false,uis = uis,inner = inner,last = last}
  return State
end

local function otherDoor(doors,door)
  for d,_ in pairs(doors) do
    if d ~= door then return d end
  end 
end

local function cycle(State,door,other)
  VM.log("Cycling airlock")
  close(other)
--  State.cycling = true
  State.uis[other].reactor:handleEvent("cycle")
  State.inner.reactor:handleEvent("cycle")
  EVE.sleep(1)
  State.inner.reactor:handleEvent("stop_cycle")
  State.uis[other].reactor:handleEvent("close")
  State.uis[door].reactor:handleEvent("open")
  open(door)
  State.last = door
end

--TODO redundant?!
local function cycle_call(Request,From,State)
  gen_server.reply(From,"ok")
  return State
end
--TODO redundant?!
local function cycle_cast(Request,State)
  return State
end

function Airlock.handle_call(Request,From,State)
  if State.cycling then return cycle_call(Request,From,State) end
  local event = Request[1]
  if event == "observer" then
    gen_server.reply(From,State)
  elseif event == "open" then
    local door = Request[2]
    local other = otherDoor(State.doors,door)
    if State.doors[door]:isOut() then
--    if State.doors[door] then
      gen_server.reply(From,"open")
    else
      if State.doors[other]:isOut() then
--      if State.doors[other] then
        gen_server.reply(From,"cycling")
        --cycle the airlock!
        cycle(State,door,other)
      else
        open(door)
        State.last = door
        gen_server.reply(From,"opened")
      end
    end
  elseif event == "close" then
    local door = Request[2]
    if State.doors[door]:isOut() then
--    if State.doors[door] then
      close(door)
      gen_server.reply(From,"closed")
    else
      gen_server.reply(From,"not_open")
    end
  elseif event == "cycle" then
    gen_server.reply(From,"cycling")
    local other = otherDoor(State.doors,State.last)
    State.uis[other].reactor:handleEvent("cycle")
    cycle(State,other,State.last)
  end
  return State
end

function Airlock.handle_cast(Request,State)
  if State.cycling then return cycle_cast(Request,State) end
  VM.log("Received "..Request)
  return State
end

function Airlock.handle_info(Request,State)
  VM.log("got: "..unpack(Request))
  return State
end

return Airlock