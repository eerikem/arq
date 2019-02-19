local gen_server = require "gen_server"
local UI = require "lib.ui"

---
-- The Server component of your App
-- Should implement the four gen_server functions
-- @type App_Server
-- @extends gen_server#server
local App_Server = {}

----------------
--External API--
----------------

---
-- Description of app here
-- This is the API object for all external calls
local App = {}

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


