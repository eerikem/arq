
local Server = {}

function Server.start_link(Sup)
  return gen_server.start_link(Server,{Sup},{})
end

local function link_ui(term,name)
  return ui_server.start_link(term,name)
end

local events = {
  char = nil,
  key = nil,
  key_up = nil,
  paste = nil,
  peripheral=nil,
  peripheral_detach=nil,
  mouse_click = function(Uis,number,x,y) gen_server.cast(Uis.terminal,{"click",x,y})end,
  mouse_up = nil,
  mouse_scroll = nil,
  mouse_drag = nil,
  monitor_touch = function(Uis,name,x,y) gen_server.cast(Uis[name],{"monitor_touch",x,y}) end,
  monitor_resize = nil,
  term_resize = nil
}

function Server.handle_call(Request,From,State)
  return State
end

local function parse(State,event,...)
  if events[event] then
    events[event](State.uis,unpack(arg))
  else
  --TODO default event and terminate?
  end
  return State
end

function Server.handle_cast(Request,State)
  --return "noreply", parse(State,Request)
  return parse(State,unpack(Request))
end

local function subscribe(Co,event)
  EVE.subscribe(Co,event)
end

function Server.init(eventCo)
  local Uis = {terminal = link_ui(term,"Terminal")}
  subscribe(eventCo,"mouse_click")
  for n,name in ipairs(peripheral.getNames()) do
    if peripheral.getType(name) == "monitor" then
      Uis[name]=link_ui(peripheral.wrap(name),name)
    end
  end
  return {uis = Uis, events = eventCo}
end


return Server
