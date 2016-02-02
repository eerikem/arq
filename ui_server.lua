
--
--VM = require 'vm'
--gen_server = require 'gen_server'
--UI = require 'ui'


local Server = {}

function Server.start_link(...)
  return gen_server.start_link(Server,arg,{})
end

function Server.init(term,name)
  local ui = UI:new(term)
  ui.name = name
  ui.setBackgroundColor(colors.gray)
  ui.clear()
  ui.setTextColor(colors.lightGray)
  local w,h = ui.getSize()
  local label = ui.name 
  if w >= string.len(label) then
    ui:printCentered(label,h/2)
  else
    ui:indentLeft(label,0,h/2)
    ui:printCentered(string.sub(ui.name,w+1),h/2+1)
  end
  return {ui = ui}
end

function Server.handle_call(Request,From,State)
  return State
end

function Server.handle_cast(Request,State)
  State.ui:printCentered(Request[1],1)
  --return "noreply", State
  return State
end


return Server