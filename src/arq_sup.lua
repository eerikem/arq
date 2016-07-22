local Li = require 'eventListener'
local ui_sup = require 'ui_supervisor'
--local workers = require 'worker_sup'

local Server = {}

function Server.init()
  local eveSpec = {"eve",{Li,"start_link",{}}}
  local uiSpec = {"uis",{ui_sup,"start_link",{"events"}}}
  local workerSpec = {}
  local serverSpec = {{"one_for_one",1,5},{eveSpec,uiSpec}}
  return true, serverSpec
end

return Server