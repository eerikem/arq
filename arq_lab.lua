local gen_server = require "gen_server"
local UI = require "lib.ui"
local Bundle = require "lib.bundle"
local Graphic = require "lib.graphic"
local Panel = require "lib.ui_obj"
local Menu = require 'lib.ui_menu'

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

function Lab.openSilo()
  gen_server.cast("arq_lab",{"open_silo"})
end

function Lab.closeSilo()
  gen_server.cast("arq_lab",{"close_silo"})
end

---------------
--Server & UI--
---------------

local function doorTimer(callback)
  local time = 0
  local r,sleep,reverse = VM.receive()
  if r == "start" then
    time = os.clock()
    if sleep then
      EVE.tick(sleep % SILO_DELAY)
    else
      EVE.tick(SILO_DELAY)
    end
  else
    error("door Timer received bad signal")
  end
  local r,to = VM.receive()
  if r == "wake" then
    return callback()
  elseif r == "stop_timer" then
    if to then
      if reverse then
        return VM.send(to,"start",SILO_DELAY - (os.clock() - time),false)
      else
        return VM.send(to,"start",os.clock() - time,true)
      end
    end
  else
    error("doorTimer received bad msg")
  end
end

local function open(door)
  cables.close_silo:disable()
  cables.open_silo:enable()
  door.opening = true
  door.closed = false
  local fun = function()
    cables.open_silo:disable()
    door.opening = false
    door.open = true
    door.timer = nil end
  local newTimer = VM.spawn(function()doorTimer(fun)end)
  if door.closing then
    VM.send(door.timer,"stop_timer",newTimer)
    door.timer = newTimer
  else
    door.timer = newTimer
    VM.send(newTimer,"start")
  end
end

local function close(door)
  cables.open_silo:disable()
  cables.close_silo:enable()
  door.closing = true
  door.open = false
  
  local fun = function()
    cables.close_silo:disable()
    door.closing = false
    door.closed = true
    door.timer = nil end
  local newTimer = VM.spawn(function()doorTimer(fun)end)
  if door.opening then
    VM.send(door.timer,"stop_timer",newTimer)
    door.timer = newTimer
  else
    door.timer = newTimer
    VM.send(newTimer,"start")
  end
end

local function alarmPanel(Co)
  local ui = UI.start(Co,7,5)
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
  
  ui:update()
  return ui
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
  menu:add(close)
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

function Lab.handle_cast(Request,State)
  local event = Request[1]
  if event == "enable_alarm" then
    cables.emergency:enable()
    State.ui.enabledAlarm()
  elseif event == "disable_alarm" then
    cables.emergency:disable()
    State.ui.disabledAlarm()
  elseif event == "open_silo" then
    if State.silo.closed or State.silo.closing then
      open(State.silo)
      State.ui.openedSilo()
    end
  elseif event == "close_silo" then
    if State.silo.open or State.silo.opening then
      close(State.silo)
      State.ui.closedSilo()
    end
  end
  return State
end

function Lab.init()
  local ui = initUI()
  local panel1 = alarmPanel("monitor_114")
  local panel2 = alarmPanel("monitor_115")
  local door = {opening = false, closing = false,open = true, closed=false}
  return true, {ui = ui,silo = door}
end

return Lab