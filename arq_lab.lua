local gen_server = require "gen_server"
local UI = require "lib.ui"
local Bundle = require "lib.bundle"
local Graphic = require "lib.graphic"
local Panel = require "lib.ui_obj"
local Menu = require 'lib.ui_menu'
local Door = require "door"
local door_ui = require "door_ui"
local status_ui = require "status_ui"
local static_ui = require "static_ui"

local SILO_DELAY = 5

local CABLE_SIDE = "back"
local cables = {
  open_silo = Bundle:new(CABLE_SIDE,colors.lime),
  close_silo = Bundle:new(CABLE_SIDE,colors.yellow),
  silo_doors = Bundle:new(CABLE_SIDE,colors.pink),
  storage = Bundle:new(CABLE_SIDE,colors.brown),
  storage_sensor = Bundle:new(CABLE_SIDE,colors.green),
  tube_lights = Bundle:new(CABLE_SIDE,colors.lightGray),
  ignite = Bundle:new(CABLE_SIDE,colors.cyan),
  inject_gas = Bundle:new(CABLE_SIDE,colors.purple),
  prep_piston = Bundle:new(CABLE_SIDE,colors.blue),
  emergency = Bundle:new(CABLE_SIDE,colors.gray),
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

function Lab.openSilo()
  gen_server.cast("arq_lab",{"open_silo"})
end

function Lab.closeSilo()
  gen_server.cast("arq_lab",{"close_silo"})
end

---------------
--Server & UI--
---------------

---Alarm UI module
-- @type arq_lab.AlarmUI
local AlarmUI = {} --#arq_lab.AlarmUI

---
-- @function [parent=#AlarmUI] start_link
-- @param #string mon the monitor to spawn the ui
-- @return #thread address to new AlarmUI
function AlarmUI.start_link(mon)
  local parent = VM.running()
  
  --- @function init
  -- @param lib.ui#ui ui The ui object initialized in UI.lua
  local function init(ui)
    local title = Graphic:new("ALARM")
    local button = Graphic:new("Trigger")
    local body = Panel:new()
    local status = Graphic:new("       ")
    body.width = "max"
    title:align("center")
    body:setLayout("static")
    button.ypos = 2
    button:align("center")
    status.ypos = 3
    body:add(button)
    body:add(status)
    ui:add(title)
    ui:add(body)
    
    button:setTextColor(colors.red)
    ui:setBackground(colors.lightGray)
    ui:setText(colors.gray)
    body:setBackgroundColor(colors.gray)
    body:setTextColor(colors.lightGray)
    
    local function triggered()
      ui:setText(colors.white)
      ui:setBackground(colors.red)
      body:setBackgroundColor(colors.black)
      button.text = "WARNING"
      button.reactor:stop()
      button:setTextColor(colors.red)
      ui:update()    
    end
        
    local function reset()
      ui:setText(colors.gray)
      ui:setBackground(colors.lightGray)
      body:setBackgroundColor(colors.gray)
      button.text = "Trigger"
      button.reactor:start()
      button:setTextColor(colors.lightGray)
      ui:update()
    end
    
    ui.reactor:register("triggered",triggered)
    button:setOnSelect(ui,Lab.callAlarm)
    ui.reactor:register("reset",reset)
    ui:update()
  end
  
  return UI.start(mon,7,5,init)
end

function AlarmUI.trigger(Co)
  gen_server.cast(Co,{"triggered"})
end

function AlarmUI.reset(Co)
  gen_server.cast(Co,{"reset"})
end

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
  local ui = UI.start("terminal",20,8)
  ui:align("center","right")
  ui:setBackground(colors.lightGray)
  ui:setText(colors.gray)
  
  local title = Graphic:new("ARQ LAB Controls")
  
  local open = Graphic:new("Open Silo")
  local close = Graphic:new("Close Silo")
  
  local lights = Graphic:new("Lights Off")
  local alarmOn = Graphic:new("Enable Alarm")
  local alarmOff = Graphic:new("Disable Alarm")
  local resetPanels = Graphic:new("Arm Panels")
  title:align("center")
  title.ypos = 2
  
  local menu = Menu:new()
  menu.xpos = 2
  menu.width = "max"
  
  menu:add(lights)
  menu:add(open)
  menu:add(alarmOn)
  menu:add(resetPanels)
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
  
  ui.openedSilo = function()
    reenable(menu,close,2)
    ui:update()
  end
  ui.closedSilo = function()
    reenable(menu,open,2)
    ui:update()
  end
  
  open:setJustOnSelect(ui,Lab.openSilo)
  close:setJustOnSelect(ui,Lab.closeSilo)
  
  alarmOn:setJustOnSelect(ui,Lab.callAlarm)
  alarmOff:setJustOnSelect(ui,Lab.stopAlarm)
  
  return ui
end

local function triggerAlarms(State)
  for _,alarm in ipairs(State.alarms) do
    AlarmUI.trigger(alarm)
  end
end

local function resetAlarms(State)
  for _,alarm in ipairs(State.alarms) do
    AlarmUI.reset(alarm)
  end
end

function Lab.handle_cast(Request,State)
  local event = Request[1]
  if event == "enable_alarm" then
    cables.emergency:enable()
    State.ui.enabledAlarm()
    triggerAlarms(State)
  elseif event == "disable_alarm" then
    cables.emergency:disable()
    State.ui.disabledAlarm()
    resetAlarms(State)
  elseif event == "open_silo" then
    Door.open(State.silo)
    State.ui.openedSilo()
  elseif event == "close_silo" then
    Door.close(State.silo)
    State.ui.closedSilo()
  elseif event == "opened" then
    State.ui.openedSilo()
    Door.allowAccess(State.siloAccess)
  elseif event == "closed" then
    State.ui.closedSilo()
    Door.denyAccess(State.siloAccess)
  end
  return State
end

function Lab.init()
  
  
  local silo = Door.newCargo(cables.open_silo,cables.close_silo,SILO_DELAY)
  local hallway = Door.new(cables.silo_doors,8)
  local hallAccess = Door.newUI("monitor_111","Lab 102",hallway)
  local siloAccess = Door.newUI("monitor_112","Lab 102",hallway,"123")
  Door.denyAccess(siloAccess)
  status_ui.start(silo,"monitor_110")
--status_ui.start(silo,"monitor_109")
  static_ui.start("monitor_117","Lab 103 - Processing")

  Door.subscribe(silo)

  local storage = Door.new(cables.storage,5,cables.storage_sensor)
  Door.newUI("monitor_120","Storage",storage,"123")
  local doors = {
    silo,
    Door.newUI("monitor_116","Lab 103"),
    Door.newUI("monitor_118","Lab 101"),
    Door.new(cables.storage,cables.storage_sensor),
    storage
  }

  local ui = initUI()
  local alarms = {
    AlarmUI.start_link("monitor_114"),
    AlarmUI.start_link("monitor_115"),
    AlarmUI.start_link("terminal")
  }
  return true, {ui = ui,silo = silo,siloAccess = siloAccess,hallAccess=hallAccess,alarms = alarms}
end

return Lab