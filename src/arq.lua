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

luaunit = require 'lib.luaunit'
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
local Manager = require "door_manager"
local Password = require "password"
local ARQ_lab = require "arq_lab"
local toggler = require "perphListener"
local apps = require 'apps'
local status_ui = require "status_ui"
local static_ui = require "static_ui"
local Door = require "door"
local door_ui = require "door_ui"
local ui_coordinator = require "ui_coordinator"
local config = require "config"

VM.init()

CONFIG = config.load()

supervisor.start_link(arqSup,{},"arq_sup")
VM.log = ui_sup.statusWindow("terminal",6)

toggler.start()

uiMenu:new("terminal")

arqMenu.start()
apps.start()

EVE.run()