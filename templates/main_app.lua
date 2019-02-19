local Bundle = require "lib.bundle"
local Door = require 'door'
local static_ui = require "static_ui"

-- The side which a redstone cable is connected
-- Replace nil with "back", "left", "right", "top" or "bottom"
local CABLE_SIDE = assert(nil, "CABLE_SIDE undefined")


local App = {}

---
-- start() is called when this file is saved in the /apps directory
-- Intended to be used as a start up script for a particular arq instance
function App.start()
  local a_door = Door.new(Bundle:new(CABLE_SIDE,colors.white))
  Door.newUI("monitor_xxx","UTILITY",a_door,455)
end

return App