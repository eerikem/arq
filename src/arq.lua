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


--- The virtual machine running the ARQ.
-- Handles threads, event messaging and error propagation.
-- Generally you will only use for logging messages eg: VM.log("my msg")
VM = require 'vm'

---
-- Execute a server command.
-- A leading slash is expected.
-- string is formatted so additional arguments will be programmatically inserted. 
-- @param #string cmd eg: "/kill @a %d %d %d"
function exec(cmd,...)
  if commands then
    commands.execAsync(string.format(cmd,unpack(arg)))
  else
    VM.log("Warning: Not a command computer")
  end
end

---
-- Event Listener for the ARQ.
-- Manages events to and from the ComputerCraft OS.
EVE = require 'eventListener'

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

--- The values stored in the ARQ configuration file.
-- This global is used for reading metadata stored from previous ARQ instances. See config.lua module for interface.
CONFIG = config.load()

supervisor.start_link(arqSup,{},"arq_sup")
VM.log = ui_sup.statusWindow("terminal",6)

toggler.start()

uiMenu:new("terminal")

arqMenu.start()
apps.start()

EVE.run()