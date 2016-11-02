
local gen_server = require 'gen_server'
local UI = require 'lib.ui_lib'
local Reactor = require 'lib.reactor'
local Prod = require 'lib.producer'
local Graphic = require 'lib.graphic'



--- Server for managing terminal events and ui's
-- @module ui.server
-- @return src.ui_server#ui.server

local Server = {}

local errorSound = "/playsound frontierdevelopment:event.mondecline @p"

local function beep()
  exec(errorSound)
end

--- Start new Terminal Server
-- @function [parent=#ui.server] start_link
-- @param lib.ui_lib#term
function Server.start_link(term,termName)
  return gen_server.start_link(Server,{term,termName},{})
end

local function getTerm(State,x,y)
  local ui = State.focus
  while not ui:onMe(x,y) do
    ui = ui.next
  end
  return ui
end

local function handle(Req,State)
  if State.parents[State.focus] then
    gen_server.cast(State.parents[State.focus],{"handle",Req})
  else
    State.focus.reactor:handleEvent(unpack(Req))
  end
end

local function shiftFocus(State,ui)
  if State.focus ~= ui and ui ~= State.background then
    ui:bump(State.focus)
    State.focus = ui
  end
end

local function handleMouse(Req,State)
  local event = unpack(Req)
  if event == "mouse_scroll" then
    local _,button,x,y = unpack(Req)
    local ui = getTerm(State,x,y)
    if ui then
      shiftFocus(State,ui)
      if button == -1 then
        handle({"scroll","scroll_up",x,y},State)
      elseif button == 1 then
        handle({"scroll","scroll_down",x,y},State)
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
    if ui then
      shiftFocus(State,ui)
      handle(Req,State)
    else VM.log("No ui for "..event.." at "..x.." "..y) end
  else
    VM.log("UI_Server Received: "..event.." at "..x.." "..y)
  end
  return State
end

local function handleTouch(Req,State)
  local event,x,y = unpack(Req)
  VM.log(State.ui.name.." touched at "..x.." "..y)
  local ui = getTerm(State,x,y)
  if ui then
    shiftFocus(State,ui)
    handle(Req,State)
  else VM.log("No ui for "..event.." at "..x.." "..y) end
  return State 
end

local function removeUI(State,ui)
  local UI = ui:leave()
  ui.term.setVisible(false)
  if State.focus == ui then
    State.focus = UI
  end
end

local function redraw(ui)
  ui.term.setVisible(true)
  ui.term.setVisible(false)
end

local function redrawStack(State,ui)
  if not ui or ui == State.background then
    local ui = State.background
    redraw(ui)
    ui = ui.last
    while ui ~= State.focus do
      redraw(ui)
      ui = ui.last
    end
    if State.focus ~= State.background then
      redraw(ui) end
  else
    while ui ~= State.focus do
      redraw(ui)
      ui = ui.last
    end
    if State.focus ~= State.background then
      redraw(ui) end
  end
end

--TODO adjust to new double linked list
local function resized(_,State)
  VM.log(State.ui.name.." resized")
  for _,UI in ipairs(State.stack) do
    if UI.alignment then
      UI:align(unpack(UI.alignment)) end
  end
  redrawStack(State)
  return State
end

local shiftDown = false

local function shiftFocusRight(State)
  local ui = State.background.last
  ui:bump(State.focus)
  State.focus = ui 
  redrawStack(State,State.focus)
end

local function shiftFocusLeft(State)
  local ui = State.focus.next
  ui:bump(State.focus)
  State.focus = ui
  redrawStack(State,State.focus)
end

local UI_Events = {
  char = function(Req,State)
    handle(Req,State)
    return State
    end,
  key = function(Req,State)
    local _,keycode,helddown = unpack(Req)
    local msg = keys.getName( keycode )
    if helddown then msg = msg.." down at UI_Server" end
    if keycode == 15 then
      if shiftDown then
        shiftFocusLeft(State)
      else
        shiftFocusRight(State)
      end
    elseif keycode == 42 then
      shiftDown = true
    elseif keycode == 63 then
      redrawStack(State)
      VM.log("Refreshed full stack")
      return State
    else
      handle(Req,State)
    end
    return State 
    end,
  key_up = function(Req,State)
    local _,keycode = unpack(Req)
    if keycode == 42 then
      shiftDown = false
    elseif not keys.getName( keycode ) then
      VM.log(keycode.." up at UI_Server")
    else
      VM.log(keys.getName( keycode ).." up at UI_Server")
    end
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

--- The extended ui obj
-- @type ui
-- @extends lib.ui_lib#ui

local function leave(self)
    self.last.next = self.next
    self.next.last = self.last
    return self.next
end

--- Double linked list bump for focus change
-- @function [parent=#ui.server] bump
-- @param #ui self
-- @param #ui ui The ui to get bumped
local function bump(self,ui)
  if ui.next == nil then
    ui.next = self
    ui.last = self
    self.next = ui
    self.last = ui
  else
    if self.next then 
    self:leave() end
    self.next = ui
    self.last = ui.last
    ui.last.next = self
    ui.last = self
  end
end

local function initNativeUI(term,name)
  local w,h = term.getSize()
  local win = window.create(term,1,1,w,h)
  local ui = UI:new(win)
  
  if peripheral.getType(name) == "monitor" then
    ui.setTextScale = term.setTextScale end
  
  ui.name = name
  ui:setBackground(colors.black)
  ui:setText(colors.lightGray)
  local label = Graphic:new(ui.name)
  label.align = "center"
--  label.ypos = h/2
  ui:add(label)
  ui:update()
  ui.bump = bump
  ui.leave = leave
  win.setVisible(false)
  return ui
end

function Server.init(term,name)

  local ui = initNativeUI(term,name)
  local windows = {}
  windows[ui] = true 
  
  --TODO reactor sends events to coroutine handlers
  local o = {native = term, ui = ui,focus = ui,background = ui,
             windows=windows,events={},producer=Prod:new(),
             monitors={},parents={}
             }
  o.reactor = Reactor:new(o)
  for event,handler in pairs(UI_Events) do
    o.reactor:register(event,handler)
  end
  
  return true, o
end

local function newWindow(State,Co,w,h)
  local maxW, maxH = State.native.getSize()
  if not w or not h then w, h = maxW, maxH end --todo no arg goes to best fit?!?
  if w == "max" then w = maxW end
  if h == "max" then h = maxH end
  local ui = UI:new(window.create(State.native,1,1,w,h))
  ui.term.setVisible(false)
  ui.native = State.native
  ui.setTextScale = State.native.setTextScale
  State.windows[ui] = true --TODO window management?
  ui.redraw = function(self)
    gen_server.cast(Co,{"update",self})
  end
  
  ui.redraw_sync = function(self)
    local Msg = gen_server.call(Co,{"update",self})
    if Msg ~= "ok" then error("Problem got: "..Msg) end
  end
  
  ui.bump = bump
  ui.leave = leave
  ui:bump(State.focus)
  State.focus = ui 
  return ui
end

function Server.handle_call(Request,From,State)
  local event = Request[1]
  
  if event == "new_window" then
    local _,w,h,Parent = unpack(Request)
    local Co = Parent or unpack(From)
    local ref = VM.monitor("process", Co)
    local window = newWindow(State,VM.running(),w,h)
    State.monitors[ref]=window
    if Parent then 
      State.parents[window]=From[1]end
    gen_server.reply(From,window)
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
    elseif event == "remove" then
      removeUI(State,Request[2])
      --TODO redraw new stack
      --TODO remove monitor
      redrawStack(State)
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

function Server.handle_info(Request,State)
  local event = Request[1]
  if event == "DOWN" then
    local _,ref,type,Co,reason = unpack(Request)
    local ui = State.monitors[ref]
    State.monitors[ref]=nil
    removeUI(State,ui)
    redrawStack(State)
  else
    VM.log("Warning UI server handleInfo")
  end
  return State
end

function Server.listen(Co,event,co)
  gen_server.cast(Co,{"register",event,co})
end

--- Request a new UI object from server
-- @function [parent=#ui.server] newWindow
-- @param #string Co the name of the term
-- @param #number w width
-- @param #number h height
-- @param #thread Parent The parent coroutine to monitor in case of crashes
-- @return lib.ui_lib#ui
function Server.newWindow(Co,w,h,Parent)
  --TODO handle requests to bad monitors
  local ui = gen_server.call(Co,{"new_window",w,h,Parent})
  return ui
end

return Server