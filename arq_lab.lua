local gen_server = require "gen_server"
local UI = require "ui"
local Bundle = require "bundle"
local Graphic = require "graphic"
local Panel = require "ui_obj"
local Menu = require 'ui_menu'

local CABLE_SIDE = "back"
local cables = {
  open_silo = Bundle:new(CABLE_SIDE,colors.yellow),
  close_silo = Bundle:new(CABLE_SIDE,colors.lime),
  silo_doors = Bundle:new(CABLE_SIDE,colors.pink),
  storage = Bundle:new(CABLE_SIDE,colors.brown),
  storage_sensor = Bundle:new(CABLE_SIDE,colors.green),
  tube_lights = Bundle:new(CABLE_SIDE,colors.lightGray),
  ignite = Bundle:new(CABLE_SIDE,colors.cyan),
  inject_gas = Bundle:new(CABLE_SIDE,colors.purple),
  prep_piston = Bundle:new(CABLE_SIDE,colors.blue),
  emergency = Bundle:new(CABLE_SIDE,colors.brown),
  lighting = Bundle:new(CABLE_SIDE,colors.white)
}


local Lab = {}

----------------
--External API--
----------------

function Lab.start()
  return gen_server.start(Lab,{},{},"arq_lab")
end

function Lab.start_link()
  return gen_server.start_link(Lab,{},{},"arq_lab")
end

function Lab.callAlarm()
  gen_server.cast("arq_lab",{"enable_alarm"})
end

function Lab.stopAlarm()
  gen_server.cast("arq_lab",{"disable_alarm"})
end
---------------
--Server & UI--
---------------
local changed
local function enable(panel,button,index)
  local i = index or 2
  local old = panel.index[i]
  if old == button then return end
  changed = true
  old.reactor:stop()
  button.reactor:start()
  panel:replace(old,button)
end

--resets color of a disabled button
local function reenable(panel,button,index)
  local old = panel.index[index]
  if old ~= button then
    old.reactor:stop()
    panel:replace(old,button)
  end
  button:setTextColor(nil)
  button.reactor:start()
  changed = true
end

local function disable(panel,index)
  local obj = panel.index[index]
  changed = true
  obj:setTextColor(colors.gray)
  obj.reactor:stop()
end

local function initUI()
  local co, ui = UI.start("terminal",20,5)
  ui:align("center","right")
  ui:setBackground(colors.lightGray)
  ui:setText(colors.gray)
  
  local title = Graphic:new("ARQ LAB Controls")
  
  local open = Graphic:new("Open Silo")
  local close = Graphic:new("Close Silo")
  
  local lights = Graphic:new("Lights Off")
  local alarmOn = Graphic:new("Enable Alarm")
  local alarmOff = Graphic:new("Disable Alarm")
  
  title:align("center")
  title.ypos = 2
  
  local menu = Menu:new()
  menu.xpos = 2
  menu.width = "max"
  
  menu:add(lights)
  menu:add(open)
  menu:add(alarmOn)
  
  menu:link(ui)
  menu:setBackgroundColor(colors.gray)
  menu:setTextColor(colors.lightGray)
  
  ui:add(title)
  ui:add(menu)
  ui:update()
  
  ui.enabledAlarm = function()
    reenable(menu,alarmOff,3)
    ui:update()
  end
  ui.disabledAlarm = function()
    reenable(menu,alarmOn,3)
    ui:update()
  end
  
  alarmOn:setJustOnSelect(ui,Lab.callAlarm)
  alarmOff:setJustOnSelect(ui,Lab.stopAlarm)
  
  return ui
end

function Lab.handle_cast(Request,State)
  local event = Request[1]
  if event == "enable_alarm" then
    cables.emergency:enable()
    State.ui.enabledAlarm()
  elseif event == "disable_alarm" then
    cables.emergency:disable()
    State.ui.disabledAlarm()
  end
  return State
end

function Lab.init()
  local ui = initUI()
  return true, {ui = ui}
end

return Lab