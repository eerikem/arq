local Client = require "ui_sup_menu"
local statusBar = require "statusBar"

local Server = {}

function Server.start_link(Sup)
  return gen_server.start_link(Server,{Sup},{},"ui_sup")
end

local function link_ui(term,name)
  return ui_server.start_link(term,name)
end

local function toTerminal(...) gen_server.cast("terminal",{...}) end
local function toUI(event,name,...) gen_server.cast(name,{event,...}) end

local UI_Events = {
  char = toTerminal,
  key = toTerminal,
  key_up = toTerminal,
  paste = toTerminal,
  peripheral=nil,
  peripheral_detach=nil,
  mouse_click = toTerminal,
  mouse_up = toTerminal,
  mouse_scroll = toTerminal,
  mouse_drag = toTerminal,
  monitor_touch = toUI,
  monitor_resize = toUI,
  term_resize = toTerminal
}

local function subscribe(Co,event)
  EVE.subscribe(Co,event)
end

function Server.init(eventCo)
  local reactor = Reactor:new()
  for event,handler in pairs(UI_Events) do
    reactor:register(event,handler)
  end
  local Uis = {terminal = link_ui(term.current(),"Terminal")}
  VM.register("terminal",Uis.terminal)
--  subscribe(eventCo,"mouse_click")
--  subscribe(eventCo,"monitor_touch")
  for n,name in ipairs(peripheral.getNames()) do
    if peripheral.getType(name) == "monitor" then
      Uis[name]=link_ui(peripheral.wrap(name),name)
      VM.register(name,Uis[name])
    end
  end
  return {uis = Uis, events = eventCo,reactor = reactor}
end

local getNames = function(State)
  local r = {}
  for k,_ in pairs(State.uis) do
    table.insert(r,k) end
  return r
end

function Server.handle_call(Request,From,State)
  local From, Ref = unpack(From)
  if Request == "get_ui_names" then
    VM.send(From,getNames(State))
  elseif Request == "get_uis" then
    VM.send(From,State.uis)
  end
  return State
end

local function parse(State,event,...)
  if UI_Events[event] then
    UI_Events[event](State.uis,unpack(arg))
  else
    error("received unhandled event: "..event)
  --TODO default event and terminate?
  end
  return State
end

function Server.handle_cast(Request,State)
  local event = Request[1]
  if UI_Events[event] then
    State.reactor:handleEvent(unpack(Request))
    return State
  else
    return parse(State,unpack(Request))
  end
end

function Server.sendOsEvent(...)
  gen_server.cast("ui_sup",{...})
end

function Server.getUInames()
  return gen_server.call("ui_sup","get_ui_names")
end

function Server.getUis()
  return gen_server.call("ui_sup","get_uis")
end

function Server.newWindow(name,w,h)
  local Uis = Server.getUis()
  if Uis[name] then
    return ui_server.newWindow(Uis[name],w,h)
  else
    error("Error: "..name.." not found.",2)
  end
end

function Server.app(Co)
  return Client:new(Co)
end

function Server.statusWindow(Co)
  local bar = statusBar:new(Co,6)
  return function(str)
    bar:write(str)
  end
end

return Server
