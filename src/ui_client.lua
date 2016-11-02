----------------------
--UI Client Template--
----------------------
--
local UI = require "lib.ui"


local Client = {}

function Client.start_link()
  
  local function init(ui)
  
  end
  
  local ui = UI.start(mon,w,h,init)
  
  return ui.co
end

return Client