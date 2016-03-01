
local Server = {}

function Server.start_link()
  return gen_server.start_link(Server,{},{},"events")
end

local UI_Events = {
  char = ui_sup.sendOsEvent,
  key = ui_sup.sendOsEvent,
  key_up = ui_sup.sendOsEvent,
  paste = ui_sup.sendOsEvent,
  peripheral=ui_sup.sendOsEvent,
  peripheral_detach = ui_sup.sendOsEvent,
  mouse_click = ui_sup.sendOsEvent,
  mouse_up = ui_sup.sendOsEvent,
  mouse_scroll = ui_sup.sendOsEvent,
  mouse_drag = ui_sup.sendOsEvent,
  monitor_touch = ui_sup.sendOsEvent,
  monitor_resize = ui_sup.sendOsEvent,
  term_resize = ui_sup.sendOsEvent
}

function Server.init()
  local reactor = Reactor:new()
  for event,handler in pairs(UI_Events) do
    reactor:register(event,handler)
  end
  return {reactor = reactor}
end

function Server.handle_call(Request,From,State)
  return State
end

function Server.handle_cast(Request,State)
  local event = Request[1]
  if event=="subscribe" then
    HashArrayInsert(State,Request[3],Request[2])
  elseif UI_Events[event] then
    State.reactor:handleEvent(unpack(Request))
  else
    VM.log("events received unhandled: "..event)
  end
--     then
--    VM.log("sending "..event)
--    State.reactor.handleEvent(unpack(Request))
--  else
--    if State[Request[1]] then
--      for _,Co in ipairs(State[Request[1]]) do
--        gen_server.cast(Co,Request)
--      end
--    end
--  end
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