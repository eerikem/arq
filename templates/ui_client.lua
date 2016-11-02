----------------------
--UI Client Template--
----------------------
--
local UI = require "lib.ui"


---A template for UI Clients
--@module ui_client


local Client = {}

---
-- @function [parent=#ui_client] start_link
-- @param #string mon the monitor to spawn the ui
-- @return #thread address to new UI Client
function Client.start_link(mon)
  
  --- init function to initialize Client UI.
  -- @function [parent=#ui_client] init
  -- @param lib.ui_lib#ui ui The ui object initialized in UI.lua
  local function init(ui)
    
  end
  
  return UI.start(mon,w,h,init)
end

return Client