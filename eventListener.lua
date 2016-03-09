
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
  local o = {reactor = reactor,clickCounter=0}
  reactor:register("mouse_click",clickHandler(o))
  reactor:register("mouse_up",clickHandler(o))
  reactor:register("mouse_drag",clickHandler(o))
  return o
end

function Server.handle_call(Request,From,State)
  return State
end

function Server.handle_cast(Request,State)
  local event = Request[1]
  if event=="subscribe" then
    HashArrayInsert(State,Request[3],Request[2])
  elseif UI_Events[event] or "mouse_click" or "mouse_up" or "mouse_drag" then
    State.reactor:handleEvent(unpack(Request))
  else
    VM.log("events received unhandled: "..event)
  end
  --return "noreply", State
  return State
end

function Server.subscribe(Co,event)
  gen_server.cast(Co,{"subscribe",VM.running(),event})
end

function Server.subscriber(Co,Module)
  return Module.start_link(Co)
end


return Server