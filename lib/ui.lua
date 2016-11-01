local gen_server = require "gen_server"
local ui_server = require "ui_server"
local UI = {}

----------------
--External API--
----------------

function UI.handleEvent(Co,...)
  gen_server.cast(Co,{"handle",arg})
end

function UI.handleEventSync(Co,...)
  gen_server.call(Co,{"handle",arg})
end

--Returns co, ui
function UI.start(Co,w,h)
  local ok, co = gen_server.start_link(UI,{Co,w,h,VM.running()},{})
  return UI.getUI(co)
end

function UI.getUI(Co)
  return gen_server.call(Co,{"ui"})
end

function UI.register(Co,event,handler)
  gen_server.cast(Co,{"register",event,handler})
end

-----------
--Server --
-----------

function UI.init(Co,w,h,Parent)
  local ui = ui_server.newWindow(Co,w,h,Parent)
  local co = VM.running()
  ui.handle = function(...)UI.handleEvent(co,...)end
  ui.co = co
  return true, {ui = ui}
end

function UI.handle_call(Request,From,State)
  local event = Request[1]
  if event == "ui" then
    gen_server.reply(From,State.ui)
  elseif event == "handle" then
    gen_server.reply(From,State.ui.reactor:handleEvent(unpack(Request[2])))
  end
	return State
end

function UI.handle_cast(Request,State)
  local event = Request[1]
  --WARNING this event has dependant call in ui_server.lua
  if event == "handle" then
    State.ui.reactor:handleEvent(unpack(Request[2]))
  elseif event == "register" then
    State.ui.reactor:register(Request[2],Request[3])
  else
    error("Problem in UI module handle_cast"..event)
  end
	return State
end

return UI