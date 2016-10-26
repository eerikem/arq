local gen_server = require "gen_server"
local UI = require "ui"
local Bundle = require "bundle"
local Graphic = require "graphic"
local Panel = require "ui_obj"


local CABLE_SIDE = "back"

local Lab = {}

----------------
--External API--
----------------

function Lab.start()
  
end

function Lab.start_link()
  return gen_server.start_link(Lab,{},{},"arq_lab")
end

---------------
--Server & UI--
---------------

local function initUI()
  local co, ui = UI.start("terminal",12,5)
  ui:align("center")
  ui:setBackground(colors.lightGray)
  ui:update()
end

function Lab.init()
  
end