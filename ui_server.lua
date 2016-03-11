
--
--VM = require 'vm'
local gen_server = require 'gen_server'
--UI = require 'ui'

local Prod = require 'producer'
local Server = {}

function Server.start_link(term,termName)
  return gen_server.start_link(Server,{term,termName},{})
end

local function getTerm(State,x,y)
  for i=#State.stack,1,-1 do
    local UI = State.stack[i]
--    VM.log("Checking "..UI.pane.id)
    if UI:onMe(x,y) then
      return UI
    end
  end
  return nil
end

local function handleMouse(Req,State)
  local event = unpack(Req)
  if event == "mouse_scroll" then
    local _,button,x,y = unpack(Req)
    local ui = getTerm(State,x,y)
    --TODO deal with stack depth and overlap
    if ui then
      if button == -1 then
        ui.reactor:handleEvent("scroll","scroll_up",x,y)
      elseif button == 1 then
        ui.reactor:handleEvent("scroll","scroll_down",x,y)
      else
        error("Bad mouse_scroll received")
      end
    else
      VM.log("No ui for mouse scroll at "..x.." "..y)
    end
  elseif event == "mouse_click" or "mouse_up" then
    local event,id,button,x,y = unpack(Req)
    local ui = getTerm(State,x,y)
    --TODO change of focus event?
    if ui then ui.reactor:handleEvent(unpack(Req))
    else VM.log("No ui for "..event.." at "..x.." "..y) end
  else
    VM.log("UI_Server Received: "..event.." at "..x.." "..y)
  end
  return State
end

local function handleTouch(Req,State)
  local _,x,y = unpack(Req)
  VM.log(State.ui.name.." touched at "..x.." "..y)
  local ui = getTerm(State,x,y)
  if ui then ui.reactor:handleEvent(unpack(Req))
  else VM.log("No ui for "..event.." at "..x.." "..y) end
  return State 
end

local i = 0
local function redrawStack(State,ui)
  i = i + 1
  for _,UI in ipairs(State.stack) do
    UI.term.setVisible(true)
    UI.term.setVisible(false)
--    VM.log("Here "..i.." "..UI.pane.id)
  end
--  if i == 2 then error("reached iteration "..i) end
end

local function resized(_,State)
  VM.log(State.ui.name.." resized")
  redrawStack(State)
  return State
end

local UI_Events = {
  char = function(Req,State)
    local _,char = unpack(Req)
    VM.log("UI_Server Received: "..char)
    return State end,
  key = function(Req,State)
    local _,keycode,helddown = unpack(Req)
    local msg = keys.getName( keycode )
    if helddown then msg = msg.." down at UI_Server" end
    State.focus.reactor:handleEvent(unpack(Req))
    return State 
    end,
  key_up = function(Req,State)
    local _,keycode = unpack(Req)
    VM.log(keys.getName( keycode ).." up at UI_Server")
    return State 
    end,
  paste = function(Req,State)
    local _,string = unpack(Req)
    VM.log("Pasted to UI_Server: "..string)
    return State
    end,
  mouse_click = handleMouse,
  mouse_up = handleMouse,
  mouse_scroll = handleMouse,
  mouse_drag = handleMouse,
  monitor_touch = handleTouch,
  monitor_resize = resized,
  term_resize = resized
}

local function initNativeUI(term,name)
  local w,h = term.getSize()
  local win = window.create(term,1,1,w,h)
  local ui = UI:new(win)
  ui.name = name
  ui:setBackground(colors.black)
  ui:setText(colors.lightGray)
  local label = Graphic:new(ui.name)
  label.align = "center"
--  label.ypos = h/2
  ui:add(label)
  ui:update()
  win.setVisible(false)
  return ui
end

function Server.init(term,name)

  --TODO reactor sends events to coroutine handlers
  local reactor = Reactor:new()
  for event,handler in pairs(UI_Events) do
    reactor:register(event,handler)
  end

  local ui = initNativeUI(term,name)
  local windows = {}
  windows[ui] = true 
  
  return {native = term, ui = ui,focus = ui,stack = {ui},windows=windows,events={},producer=Prod:new(),reactor = reactor}
end

local function newWindow(State,Co,w,h)
  local maxW, maxH = State.native.getSize()
  if not w or not h then w, h = maxW, maxH end --todo no arg goes to best fit?!?
  if w == "max" then w = maxW end
  if h == "max" then h = maxH end
  local ui = UI:new(window.create(State.native,1,1,w,h))
  ui.term.setVisible(false)
  ui.native = State.native
  State.windows[ui] = true --TODO window management?
  ui.redraw = function(self)
    local Msg = gen_server.cast(Co,{"update",self})
--    if Msg ~= "ok" then error("Problem got: "..Msg) end
  end
  table.insert(State.stack,ui)
  State.focus = ui 
  return ui
end

function Server.handle_call(Request,From,State)
  local event = Request[1]
  if event == "new_window" then
    local _,w,h = unpack(Request)
    gen_server.reply(From,newWindow(State,VM.running(),w,h))
  elseif event == "update" then
    local ui = Request[2]
    redrawStack(State,ui)
    gen_server.reply(From,"ok")
  else
    error("received unkown msg")
  end
  return State
end

function Server.handle_cast(Request,State)
  if type(Request) == "table" then
    local event = Request[1]
    if event == "register" then
      local event = Request[2]
      local co = Request [3]
      HashArrayInsert(State.events,event,co)
      VM.log("Subscribed new co to "..event)
      EVE.subscribe("events",event)
    elseif event == "update" then
      local ui = Request[2]
      redrawStack(State,ui)
    elseif UI_Events[event] then
      return State.reactor:handleReq(Request,State)
    elseif State.events[Request[1]] then --todo generic event subscriber handler
      local co = State.events[Request[1]][1]--todo for each
      local event, dir, x, y = unpack(Request)
      if dir == 1 then
        VM.send(co,"scroll_down")
      else
        VM.send(co,"scroll_up")
      end
    else
      VM.log("Got unhandled "..Request[1].." on "..State.ui.name,1)    
    end
  else
    VM.log("Unkown signal type received on "..State.ui.name)
  end
  --return "noreply", State
  return State
end

function Server.listen(Co,event,co)
  gen_server.cast(Co,{"register",event,co})
end

function Server.newWindow(Co,w,h)
  --todo handle requests to bad monitors
  local ui = gen_server.call(Co,{"new_window",w,h})
  return ui
end


return Server