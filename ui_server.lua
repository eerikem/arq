
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
  local ui = UI:new(win)
  ui.name = name
  ui.setBackgroundColor(colors.gray)
  ui.clear()
  ui.setTextColor(colors.lightGray)
  local label = ui.name 
  if w >= string.len(label) then
    ui:printCentered(label,h/2)
  else
    ui:indentLeft(label,0,h/2)
    ui:printCentered(string.sub(ui.name,w+1),h/2+1)
  end
  return {ui = ui,focus = win,stack = {win}}
end

function Server.handle_call(Request,From,State)
  if Request == "new_window" then
    VM.send(From[1],UI:new(window.create(State.focus,1,1,State.focus.getSize())))
  end
  return State
end

function Server.handle_cast(Request,State)
  State.ui:printCentered(Request[1],1)
  --return "noreply", State
  return State
end

function Server.newWindow(Co,w,h)
  local ui = gen_server.call(Co,"new_window") 
  return ui
end


return Server