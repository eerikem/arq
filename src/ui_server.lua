
local gen_server = require 'gen_server'
local UI = require 'lib.ui_lib'
local Reactor = require 'lib.reactor'
local Prod = require 'lib.producer'
local Graphic = require 'lib.graphic'
local Server = {}


local errorSound = "/playsound frontierdevelopment:event.mondecline @p"

local function beep()
  exec(errorSound)
end


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

local function handle(Req,State)
  if State.parents[State.focus] then
    gen_server.cast(State.parents[State.focus],{"handle",Req})
  else
    State.focus.reactor:handleEvent(unpack(Req))
  end
end

local function handleMouse(Req,State)
  local event = unpack(Req)
  if event == "mouse_scroll" then
    local _,button,x,y = unpack(Req)
    local ui = getTerm(State,x,y)
    --TODO deal with stack depth and overlap
    if ui then
      State.focus = ui
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
      State.focus = ui
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
    State.focus = ui
    handle(Req,State)
  else VM.log("No ui for "..event.." at "..x.." "..y) end
  return State 
end

local function removeUI(State,ui)
  for i,UI in ipairs(State.stack) do
    if ui == UI then
      table.remove(State.stack,i)
      ui.term.setVisible(false)
      if State.focus == ui then
        --TODO unsafe indexing here
        State.focus = State.stack[i-1]
      end
    end
  end
end

--TODO ui moved to top of stack?!?
local function redrawStack(State,ui)
  local redraw = false
  for i,UI in ipairs(State.stack) do
    if UI == ui or not ui then redraw = true end
    if redraw then
      UI.term.setVisible(true)
      UI.term.setVisible(false)
    end
  end
end

local function resized(_,State)
  VM.log(State.ui.name.." resized")
  for _,UI in ipairs(State.stack) do
    if UI.alignment then
      UI:align(unpack(UI.alignment)) end
  end
  redrawStack(State)
  return State
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
    handle(Req,State)
    return State 
    end,
  key_up = function(Req,State)
    local _,keycode = unpack(Req)
    if not keys.getName( keycode ) then
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
  win.setVisible(false)
  return ui
end

function Server.init(term,name)

  local ui = initNativeUI(term,name)
  local windows = {}
  windows[ui] = true 
  
  --TODO reactor sends events to coroutine handlers
  local o = {native = term, ui = ui,focus = ui,stack = {ui},
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
    local Msg = gen_server.cast(Co,{"update",self})
--    if Msg ~= "ok" then error("Problem got: "..Msg) end
  end
  
  ui.redraw_sync = function(self)
    local Msg = gen_server.call(Co,{"update",self})
    if Msg ~= "ok" then error("Problem got: "..Msg) end
  end
  
  table.insert(State.stack,ui)
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

function Server.newWindow(Co,w,h,Parent)
  --TODO handle requests to bad monitors
  local ui = gen_server.call(Co,{"new_window",w,h,Parent})
  return ui
end


return Server