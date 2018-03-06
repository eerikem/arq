local gen_server = require "gen_server"
local ui_server = require "ui_server"
local UI = require "lib.ui"
local Graphic = require "lib.graphic"
local Panel = require "lib.ui_obj"
local Password = require "password"
local elvator_panel = require "elevator_panel"
local elvator_call = require "elevator_call"

local Elevator = {}

----------------
--External API--
----------------

--{{name=floorName,
--number=floor#,
--coord={x,y,z,r},
--door=doorCo,
--call=monitor,
--panel=monitor,
--}}

function Elevator.newCall(monitor,title,number,elevator)
  
end

function Elevator.newPanel(monitor,floor,elevator)
  return elvator_panel.start_link(monitor,floor,elevator,Elevator)
end

function Elevator.new(levels)
  
  Elevator.start_link(levels)
  return Co
end

function Elevator.subscribe(Co)

end

function Elevator.callTo(Co,lvl)
  gen_server.cast(Co,{"call",lvl})
end


function Elevator.start_link(levels)
  gen_server.start_link(Elevator,{levels},{})
end

function Elevator.init(levels)
  return true, {}
end

return Elevator