local gen_server = require "gen_server"
local ui_server = require "ui_server"
local UI = {}

----------------
--External API--
----------------

function UI.handleEvent(Co,...)
  gen_server.cast(Co,arg)
end

function UI.start(Co,w,h)
  local ok, co = gen_server.start_link(UI,{Co,w,h,VM.running()},{})
  return co, UI.getUI(co)
end

function UI.getUI(Co)
  return gen_server.call(Co,{"ui"})
end

-----------
--Server --
-----------

function UI.init(Co,w,h,Parent)
  local ui = ui_server.newWindow(Co,w,h,Parent)
  return true, {ui = ui}
end

function UI.handle_call(Request,From,State)
  local event = Request[1]
  if event == "ui" then
    gen_server.reply(From,State.ui)
  end
	return State
end

function UI.handle_cast(Request,State)
  State.ui.reactor:handleEvent(unpack(Request))
	return State
end

return UI