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

sleep = function() error("Use EVE.sleep within ARQ",2) end

VM = require 'vm'

function exec(cmd,...)
  if commands then
    commands.execAsync(string.format(cmd,unpack(arg)))
  else
    VM.log("Warning: Not a command computer")
  end
end

EVE = require 'eventListener'
UI = require 'ui_lib'

-----------
--Run ARQ--
-----------


local supervisor = require 'supervisor'
local arqSup = require 'arq_sup'
local ui_sup = require 'ui_supervisor'
local arqMenu = require 'arqMenu'
local uiMenu = require 'ui_sup_menu'

local Attack = require "attack"
local Elevator = require "elevator"
local Teleport = require "teleport"
local Airlock = require "airlock"
local Observer = require "observer"
local Door = require "door"
local Manager = require "door_manager"

VM.init()

supervisor.start_link(arqSup,{},"arq_sup")
VM.log = ui_sup.statusWindow("terminal")

uiMenu:new("terminal")
arqMenu.start()

--Attack.start()
--Teleport.start()
Elevator.start()
--Airlock.start()
--local doors = {Door.startDetectorDoor(colors.white,colors.black,"monitor_0","Room 51"),
--Door.startMonitorDoor(colors.yellow,"monitor_1","monitor_2","ADMIN"),
--Door.startFakeDoor("monitor_3","DENIED")}
--Manager.start(doors)

EVE:run()