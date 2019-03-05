local gen_server = require "gen_server"
local UI = require "lib.ui"
local Events = require "lib.event"
---
-- The Server component of your App
-- Should implement the four gen_server functions
-- @type App_Server
-- @extends gen_server#server
local App_Server = {}

--- A table with all the event ids for this server
-- Note: type definition here is necessary for autocompletion in external modules.
-- @type events
local e = Events:new()
e.someEvent = "some_event"


---------------- 
--External API--
----------------

---
-- Description of app here
-- This is the API object for all external calls
local App = {e=e}

--------------
--  Server  --
--------------


function App_Server.init()
  ---
  --@type State
  local State = {
    prop1 = nil,
    prop2 = nil,
  }
  
  return true, State
end

---
-- @param Request
-- @param #State State
function App_Server.handle_cast(Request,State)
  local event = Request[1]
  if event == e.someEvent then
  
  end
  return State
end

---
-- @param Request
-- @param #State State
function App_Server.handle_call(Request,From,State)

  return State
end

---
-- @param Request
-- @param #State State
function App_Server.handle_info(Request,State)

  return State
end


return App


