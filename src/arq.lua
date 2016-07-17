--Product of ArqiTeknologies Corp.
--
--Author: ArqiTek
--Copyright 2250

local args = {...}

function map(func, tbl)
     local newtbl = {}
     for i,v in pairs(tbl) do
         newtbl[i] = func(v)
     end
     return newtbl
end

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

function exec(cmd,...)
  if commands then
    commands.execAsync(string.format(cmd,unpack(arg)))
  else
    VM.log("Warning: Not a command computer")
  end
end

local Attack = require "attack"
local Elevator = require "elevator"
local Teleport = require "teleport"
local Airlock = require "airlock"
local Observer = require "observer"
local Door = require "door"
local Manager = require "door_manager"

VM.init()


local Li = EVE.start_link()
--local Ui = EVE.subscriber(Li,ui_sup)

local Ui = ui_sup.start_link(Li)
local write = ui_sup.statusWindow("terminal")
VM.log = write

ui_sup.app("terminal")

--Attack.start()
--Teleport.start()
--Elevator.start()
Airlock.start()
--local doors = {Door.startDetectorDoor(colors.white,colors.black,"monitor_0","Room 51"),
--Door.startMonitorDoor(colors.yellow,"monitor_1","monitor_2","ADMIN"),
--Door.startFakeDoor("monitor_3","DENIED")}
--Manager.start(doors)

while true do
  --VM.flush()
  gen_server.cast(Li,{os.pullEvent()})
end