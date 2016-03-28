--Product of ArqiTeknologies Corp.
--
--Author: ArqiTek
--Copyright 2250

local args = {...}


print = function() end
--write = function() end
sleep = function() end

VM = require "vm"
--UI = require "ui"
Reactor = require "reactor"
UI = require "ui_lib"
gen_server = require "gen_server"
ui_server = require "ui_server"
ui_sup = require "ui_supervisor"
EVE = require "eventListener"
Graphic = require "graphic"
Panel, List = require "ui_obj"
Menu = require "ui_menu"
Group = require "group"
local Attack = require "attack"
local Elevator = require "elevator"
local Teleport = require "teleport"

VM.init()


local Li = EVE.start_link()
--local Ui = EVE.subscriber(Li,ui_sup)

local Ui = ui_sup.start_link(Li)
local write = ui_sup.statusWindow("terminal")
VM.log = write

ui_sup.app("terminal")

--Attack.start()
Teleport.start()
Elevator.start()

while true do
  --VM.flush()
  gen_server.cast(Li,{os.pullEvent()})
end