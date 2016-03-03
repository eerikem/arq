local Client = require "ui_sup_menu"

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
  local height = 6
  local MSG_CNT = 0
  local ui = ui_sup.newWindow(Co,"max",height)
  ui:align("bottom","left")
  ui:setBackground(colors.black)
  ui:setText(colors.gray)
  ui:update()
  ui.term.setCursorBlink(true)
  local co = VM.spawnlink(function()
    local counter = {}
    local n, pos = nil, 0
    local current = pos
    local first = true
    while true do
      local str = VM.receive()
      local sum = 0
      if str == "scroll_up" then
        if current ~= pos then
          current = current - 1
          pos = current
          while sum <height and pos <= table.maxn(counter) do
            sum = sum + counter[pos]
            pos = pos + 1
          end
          pos = pos - 1
        else
          while sum < height and current > 1 do
            sum = sum + counter[current]--todo handle scroll with empty counter
            current = current - 1
          end
        end
        if current <= 1 and sum <= height then
        else
        ui.term.clear()
        ui.term.setCursorPos(1,1)
        ui.pane:drawSubset(ui,current,pos-current+1)
        end
      elseif str == "scroll_down" then
      
      else
        if current ~= pos then
          ui:update() end
        pos = table.maxn(counter) + 1
        current = pos
        if first then first = false
--        else
--        incCursorPos(ui.term,1)
        end
        MSG_CNT = MSG_CNT + 1
        if string.find(string.lower(str),'error') then
          n = ui:add(Graphic:new({text = MSG_CNT.." "..str,textColor=colors.red}))
        else
          n = ui:add(Graphic:new(MSG_CNT.." "..str))
        end
        table.insert(counter,n+1)
      end
    end
  end
  )
 
  --Register scroll listeners to parent.
  ui_server.listen(Co,"mouse_scroll",co)
  
  local send = function(msg)
    VM.send(co,msg) end
    local scroll = function(dir)
      if dir == "up" then
        VM.send(co,"scroll_up")
      elseif dir == "down" then
        VM.send(co,"scoll_down")
      end
    end
  return send
end

return Server
