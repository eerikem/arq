local gen_server = require "gen_server"
local statusBar = require "statusBar"
local ui_server = require "ui_server"
local Reactor = require "lib.reactor"
local ui_coordinator = require "ui_coordinator"

local Server = {}

function Server.start_link(Sup)
  return gen_server.start_link(Server,{Sup},{},"ui_sup")
end

local function link_ui(term,name)
  local ok, co = ui_server.start_link(term,name)
  return co
end

local function toTerminal(...) gen_server.cast("terminal",{...}) end
local function toUI(event,name,...) gen_server.cast(name,{event,...}) end

local UI_Events = {
  char = toTerminal,
  key = toTerminal,
  key_up = toTerminal,
  paste = toTerminal,
--  peripheral=nil,
--  peripheral_detach=nil,
  mouse_click = toTerminal,
  mouse_up = toTerminal,
  mouse_scroll = toTerminal,
  mouse_drag = toTerminal,
  monitor_touch = toUI,
  monitor_resize = toUI,
  term_resize = toTerminal
}

local function subscribe(Co,event)
  EVE.subscribe(event,Co)
end

local function linkNewMon(name,uis)
    if peripheral.getType(name) == "monitor" then
      uis[name]=link_ui(peripheral.wrap(name),name)
      VM.register(name,uis[name])
    end
end

function Server.init(eventCo)
  local Uis = {terminal = link_ui(term.current(),"Terminal")}
  VM.register("terminal",Uis.terminal)
--  subscribe(eventCo,"mouse_click")
--  subscribe(eventCo,"monitor_touch")
  for n,name in ipairs(peripheral.getNames()) do
    linkNewMon(name,Uis)
  end
  local o = {uis = Uis, events = eventCo}
  local reactor = Reactor:new(o)
  for event,handler in pairs(UI_Events) do
    reactor:register(event,handler)
  end
  o.reactor = reactor
  return true, o
end

local getNames = function(State)
  local r = {}
  for k,_ in pairs(State.uis) do
    table.insert(r,k) end
  return r
end

function Server.handle_call(Request,From,State)
  if Request == "get_ui_names" then
    gen_server.reply(From,getNames(State))
  elseif Request == "get_uis" then
    gen_server.reply(From,State.uis)
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
  elseif event == "peripheral_detach" then
    local name = Request[2]
    VM.log(name.." detached!")
    if State.uis[name] then
      gen_server.cast(name,event)
    end
  elseif event == "peripheral" then
    local name = Request[2]
    if State.uis[name] then
      gen_server.cast(name,event)
    else
      linkNewMon(name,State.uis)
    end
  elseif event == "run_coordinator" then
    local name = Request[2]
    ui_coordinator.start(name)
  else
    return parse(State,unpack(Request))
  end
  return State
end

function Server.handleInfo(Request,State)
  VM.log("Warning handle Info received on UI sup")
  return State
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

function Server.terminate(Reason,State)
  for name,Co in pairs(State.uis) do
    gen_server.stop(Co,Reason)
  end
end

function Server.statusWindow(Co)
  local bar = statusBar:new(Co,4)
  return function(str)
    bar:write(str)
  end
end

return Server
