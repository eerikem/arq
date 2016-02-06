
--
--VM = require 'vm'
--gen_server = require 'gen_server'
--UI = require 'ui'


local Server = {}

function Server.start_link(...)
  return gen_server.start_link(Server,arg,{})
end

function Server.init(term,name)
  local w,h = term.getSize()
  local win = window.create(term,1,1,w,h)
  win.setCursorBlink(true)
  local ui = UI:new(win)
  ui.name = name
  ui:setBackground(colors.gray)
  ui:setText(colors.lightGray)
  local label = Graphic:new(ui.name)
  label.align = "center"
  ui:add(label)
  ui:update()
  return {ui = ui,focus = win,stack = {win},windows={}}
end

local function newWindow(State,Co,w,h)
  local maxW, maxH = State.ui.term.getSize()
  if not w or not h then w, h = maxW, maxH end --todo no arg goes to best fit?!?
  if w == "max" then w = maxW end
  if h == "max" then h = maxH end
  local ui = UI:new(window.create(State.focus,1,1,w,h))
  ui.parent = State.ui.term
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
    if Request[1] == "update" then
      local ui = Request[2]
      ui:update()--TODO Redraw the stack
      --State.windows[]()
    elseif Request[1]== "monitor_touch" then
      State.ui:printCentered("touch",2)
      --State.ui:update()
    end
  end
  State.ui:printCentered("Got "..Request[1],1)
  --return "noreply", State
  return State
end

function Server.newWindow(Co,w,h)
  --todo handle requests to bad monitors
  local ui = gen_server.call(Co,{"new_window",w,h})
  return ui
end


return Server