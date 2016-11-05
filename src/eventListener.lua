local gen_server = require 'gen_server'
local ui_sup = require 'ui_supervisor'
local Reactor = require 'lib.reactor'

THE_RUNNER = false
local Server = {}

function Server.run()
  if THE_RUNNER then
    error("Event listener is already running.",2)
  else
    THE_RUNNER = true
    while THE_RUNNER do
      gen_server.cast("events",{os.pullEvent()})
    end
  end
end

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
  local o = {clickCounter=0,timers={},events={}}
  local reactor = Reactor:new(o)
  o.reactor = reactor
  for event,handler in pairs(UI_Events) do
    reactor:register(event,handler)
  end
  reactor:register("mouse_click",clickHandler(o))
  reactor:register("mouse_up",clickHandler(o))
  reactor:register("mouse_drag",clickHandler(o))
  return true, o
end

function Server.handle_call(Request,From,State)
  local event = Request[1]
  if event == "timer" then
    local _,time = unpack(Request)
    local timer = os.startTimer(time)
    gen_server.reply(From,timer)
    State.timers[timer]=From[1]
  elseif event == "queue" then
    local Event = Request[2]
    local time = Request[3]
    local timer = os.startTimer(time)
    State.timers[timer]=From[1]
    State.events[timer]=Event
    gen_server.reply(From,timer)
  end
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
--    VM.log("Events got sleep "..time)
    local timer = os.startTimer( time )
    State.timers[timer]=From
  elseif event == "timer" then
--    VM.log("Events got timer")
    local timer = Request[2]
    if State.timers[timer] then
      if State.events[timer] then
        gen_server.cast(State.timers[timer],{State.events[timer],timer})
        State.events[timer]=nil
      else
        VM.send(State.timers[timer],"wake",timer)
      end
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

function Server.handle_info(Request,State)
  VM.log("Warning handle info on EVE")
  return State
end

function Server.tick(time)
  if not time then return end
  if VM.running() == "ROOT" then
    return sleep(time) end
  gen_server.cast("events",{"sleep",time,VM.running()},"infinite")
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

function Server.timer(time)
  if not time then return end
  return gen_server.call("events",{"timer",time},"infinite")
end

function Server.subscribe(event,Co)
  local Co = Co or VM.running()
  if not VM.coroutines[Co] then error("badarg Co",2) end
  gen_server.cast("events",{"subscribe",Co,event})
end

function Server.subscriber(Co,Module)
  local ok, co = Module.start_link(Co)
  if ok ~= true or not co then error("expected: ok, Co",2) end
  return co
end

--Queue an event to be triggered after time.
--Returns reference
function Server.queue(event,time)
  return gen_server.call("events",{"queue",event,time},"infinite")
end

function Server.terminate(Reason,State)
  THE_RUNNER = false
end

return Server