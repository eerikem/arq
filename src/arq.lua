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
local Door = require "door"
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

local doors = ARQ_lab.start()
--Manager.start(doors)

arqMenu.start()

EVE.run()