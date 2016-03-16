
local Server = {}

function Server.start_link()
  return gen_server.start_link(Server,{},{},"events")
end

--TODO handle task complete event?
local function todo() end

local UI_Events = {
  char = ui_sup.sendOsEvent,
  key = ui_sup.sendOsEvent,
  key_up = ui_sup.sendOsEvent,
  paste = ui_sup.sendOsEvent,
  peripheral=ui_sup.sendOsEvent,
  peripheral_detach = ui_sup.sendOsEvent,
  mouse_scroll = ui_sup.sendOsEvent,
  monitor_touch = ui_sup.sendOsEvent,
  monitor_resize = ui_sup.sendOsEvent,
  term_resize = ui_sup.sendOsEvent,
  task_complete = todo
}

local function clickHandler(server)
  return function (event,...)
    if event == "mouse_click" then
      server.clickCounter = server.clickCounter + 1
    end
    ui_sup.sendOsEvent(event,server.clickCounter,unpack(arg))
  end
end

function Server.init()
  local reactor = Reactor:new()
  for event,handler in pairs(UI_Events) do
    reactor:register(event,handler)
  end
  local o = {reactor = reactor,clickCounter=0,timers={}}
  reactor:register("mouse_click",clickHandler(o))
  reactor:register("mouse_up",clickHandler(o))
  reactor:register("mouse_drag",clickHandler(o))
  return o
end

function Server.handle_call(Request,From,State)
  local event = Request[1]
  return State
end

function Server.handle_cast(Request,State)
  local event = Request[1]
  if event=="subscribe" then
    HashArrayInsert(State,Request[3],Request[2])
  elseif UI_Events[event] or event == "mouse_click"
    or event == "mouse_up" or event == "mouse_drag" then
    State.reactor:handleEvent(unpack(Request))
  elseif event == "sleep" then
    local _,time,From = unpack(Request)
    VM.log("Events got sleep "..time)
    local timer = os.startTimer( time )
    State.timers[timer]=From
  elseif event == "timer" then
    VM.log("Events got timer")
    local timer = Request[2]
    if State.timers[timer] then
      VM.log("sending wake")
      VM.send(State.timers[timer],"wake")
      State.timers[timer]=nil
    end
  elseif event == "peripheral_detach" then
    local side = Request[2]
    if peripheral.getType(side)=="monitor" then
      ui_sup.sendOsEvent(event,side)
    end
  elseif event == "peripheral" then
    local side = Request[2]
    if peripheral.getType(side)=="monitor" then
      ui_sup.sendOsEvent(event,side)
    end
  elseif State[event] then
    for _,Co in ipairs(State[event]) do
      gen_server.cast(Co,Request)
    end
  else
    VM.log("events received unhandled: "..event)
  end
  --return "noreply", State
  return State
end

local function sleep( nTime )
    local timer = os.startTimer( nTime or 0 )
    repeat
        local sEvent, param = os.pullEvent( "timer" )
    until param == timer
end

function Server.sleep( time )
  if not time then return end
  if VM.running() == "ROOT" then
    return sleep(time) end
  gen_server.cast("events",{"sleep",time,VM.running()},"infinite")
  repeat
    local event = VM.receive()
--    VM.log(" received something")
  until event == "wake" or event == "stop"
end

function Server.subscribe(event,Co)
  local Co = Co or VM.running()
  gen_server.cast("events",{"subscribe",Co,event})
end

function Server.subscriber(Co,Module)
  return Module.start_link(Co)
end


return Server