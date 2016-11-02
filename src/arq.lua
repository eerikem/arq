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
--TODO is this necessary?
UI = require 'lib.ui_lib'

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
local Password = require "password"
local ARQ_lab = require "arq_lab"
local status_ui = require "status_ui"
local door_ui = require "door_ui"
VM.init()

supervisor.start_link(arqSup,{},"arq_sup")
VM.log = ui_sup.statusWindow("terminal")

uiMenu:new("terminal")

--Attack.start()
--Teleport.start()
--Elevator.start()
--Password.start(123,"terminal",{{fun=function(str)VM.log(str)end},"fun",{"Password success."}})
--Airlock.start()

local silo = Door.startMonitorDoor(colors.yellow,"monitor_111","monitor_112","Lab 102",nil,"123")
--status_ui.start(silo,"monitor_110")
local ok,str = pcall(door_ui.start_link,"terminal",{title="Test"},silo,Door,"123")
if not ok then VM.log(str) end
local ok,str = pcall(status_ui.start,silo,"monitor_110")
if not ok then VM.log(str) end
--status_ui.start(silo,"monitor_109")
local doors = {
  silo,
  Door.startFakeDoor("monitor_116","Lab 103"),
  Door.startFakeDoor("monitor_118","Lab 101")
--Door.startFakeDoor("monitor_3","DENIED")
}
Manager.start(doors)
ARQ_lab.start()

arqMenu.start()

EVE:run()