local gen_server = require "gen_server"
local ui_server = require "src.ui_server"

--- The UI Client Module
-- An event handler for UI Client events
-- @module UI
local UI = {}

---
-- @type ui
-- @extends lib.ui_lib#ui

--- UI Client State
-- @type State
-- @field lib.ui_lib#ui ui

local Server = {}

----------------
--External API--
----------------

function UI.handleEvent(Co,...)
  gen_server.cast(Co,{"handle",arg})
end

function UI.handleEventSync(Co,...)
  gen_server.call(Co,{"handle",arg})
end

--- Start and link a new UI Client
-- @function [parent=#UI] start
-- @param #string Co the terminal name
-- @param #number w width
-- @param #number h height
-- @param #function init optional function to initialize the new ui eg: init(ui)
-- @return lib.ui#ui
-- @return #thread when init provided returns thread
function UI.start(Co,w,h,init)
  local ok, co = gen_server.start_link(Server,{Co,w,h,VM.running(),init},{})
  if not init then
    return UI.getUI(co)
  else
    return co
  end
end

function UI.getUI(Co)
  return gen_server.call(Co,{"ui"})
end

--- Add a new event handler to the UI reactor
-- @function [parent=#UI] register
-- @param #thread Co co of UI Client
-- @param #string event
-- @param #function handler
function UI.register(Co,event,handler)
  gen_server.cast(Co,{"register",event,handler})
end

-----------
--Server --
-----------

---
-- @function [parent=#ui] handle
-- @param event

--- @field [parent=#ui] #thread co

function Server.init(Co,w,h,Parent,init)
  local ui = ui_server.newWindow(Co,w,h,Parent)
  local co = VM.running()
  ui.handle = function(...)UI.handleEvent(co,...)end
  ui.co = co
  if init then
    init(ui) end
  return true, {ui = ui}
end

function Server.handle_call(Request,From,State)
  local event = Request[1]
  if event == "ui" then
    gen_server.reply(From,State.ui)
  elseif event == "handle" then
    gen_server.reply(From,State.ui.reactor:handleEvent(unpack(Request[2])))
  end
	return State
end

--- @type Request
-- @list <#number>

---
-- @function [parent=lib.ui#UI] handle_cast
-- @param Request The event being handled
-- @param #State State
-- @return #State
function Server.handle_cast(Request,State)
  local event = Request[1]
  --WARNING this event has dependant call in ui_server.lua
  if event == "handle" then
    State.ui.reactor:handleEvent(unpack(Request[2]))
  elseif event == "register" then
    State.ui.reactor:register(Request[2],Request[3])
  else
    if State.ui.reactor:handling(event) then
      State.ui.reactor:handleEvent(unpack(Request))
    else
      VM.log("UI module handle_cast received unkown: "..event)
    end
  end
	return State
end

function Server.handle_info(Request,State)
  VM.log("UI got: "..unpack(Request))
  return State
end


return UI