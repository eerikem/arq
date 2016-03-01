
--
--VM = require 'vm'
--gen_server = require 'gen_server'
--UI = require 'ui'

local Prod = require 'producer'
local Server = {}

function Server.start_link(...)
  return gen_server.start_link(Server,arg,{})
end

local function handleMouse(Req,State)
  local event,button,x,y = unpack(Req)
  VM.log("Received: "..event.." at "..x.." "..y)
  return State
end

local function handleTouch(Req,State)
  local _,x,y = unpack(Req)
  VM.log(State.ui.name.." touched at "..x.." "..y)
  return State 
end

local function resized(_,State)
  VM.log(State.ui.name.." resized")
  return State
end

local UI_Events = {
  char = function(Req,State)
    local _,char = unpack(Req)
    VM.log("Received: "..char)
    return State end,
  key = function(Req,State)
    local _,keycode,helddown = unpack(Req)
    local msg = keys.getName( keycode )
    if helddown then msg = msg.." down" end
    VM.log(msg)
    return State 
    end,
  key_up = function(Req,State)
    local _,keycode = unpack(Req)
    VM.log(keys.getName( keycode ).." up")
    return State 
    end,
  paste = function(Req,State)
    local _,string = unpack(Req)
    VM.log("Pasted: "..string)
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

function Server.init(term,name)
  local reactor = Reactor:new()
  for event,handler in pairs(UI_Events) do
    reactor:register(event,handler)
  end

  local w,h = term.getSize()
  local win = window.create(term,1,1,w,h)
  win.setCursorBlink(true)
  local ui = UI:new(win)
  ui.name = name
  ui:setBackground(colors.black)
  ui:setText(colors.gray)
  local label = Graphic:new(ui.name)
  label.align = "center"
  ui:add(label)
  ui:update()
  return {native = term, ui = ui,focus = win,stack = {win},windows={},events={},producer=Prod:new(),reactor = reactor}
end

local function newWindow(State,Co,w,h)
  local maxW, maxH = State.native.getSize()
  if not w or not h then w, h = maxW, maxH end --todo no arg goes to best fit?!?
  if w == "max" then w = maxW end
  if h == "max" then h = maxH end
  local ui = UI:new(window.create(State.native,1,1,w,h))
  ui.native = State.native
  State.windows[ui] = ui.term.redraw --TODO window management?
  ui.redraw = function(self)
    gen_server.cast(Co,{"update",self})
  end
  return ui
end

function Server.handle_call(Request,From,State)
  if Request[1] == "new_window" then
    local _,w,h = unpack(Request)
    VM.send(From[1],newWindow(State,VM.running(),w,h))
  else
    error("received unkown msg")
  end
  return State
end

function Server.handle_cast(Request,State)
  if type(Request) == "table" then
    local event = Request[1]
    if event == "update" then
      local ui = Request[2]
      ui:update()--TODO Redraw the stack
      --State.windows[]()
    elseif event == "register" then
      local event = Request[2]
      local co = Request [3]
      HashArrayInsert(State.events,event,co)
      VM.log("Subscribed new co to "..event)
      EVE.subscribe("events",event)
    elseif UI_Events[event] then
--      VM.log(State.ui.name.." got "..textutils.serialize(Request))
--      VM.log("Logging: "..table.concat({"something",unpack(Request) }," "))
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