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
local Producer = require 'lib.producer'

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
  return gen_server.call("arq_lab",{"open_silo"})
end

function Lab.closeSilo()
  gen_server.cast("arq_lab",{"close_silo"})
end

function Lab.subscribe(Co)
  local co = Co or VM.running()
  gen_server.cast("arq_lab",{"subscribe",co})
end

function Lab.startGas()
  gen_server.cast("arq_lab",{"start_gas"})
end

function Lab.stopReaction()
  gen_server.cast("arq_lab",{"stop_reaction"})
end

function Lab.ignite()
  gen_server.cast("arq_lab",{"ignite"})
end

---------------
--Server & UI--
---------------


local function enable(panel,button,index)
  local i = index or 2
  local old = panel.index[i]
  if old == button then return end
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
end

local function disable(panel,index)
  local obj = panel.index[index]
  obj:setTextColor(colors.gray)
  obj.reactor:stop()
end



---Gas injection UI.
--@type gasUI
local gasUI = {}

---
-- @function [parent=#gasUI] start_link
-- @param #string mon the monitor to spawn the ui
-- @return #thread address to new UI Client
function gasUI.start_link(mon)
  
  --- init function to initialize Client UI.
  -- @function init
  -- @param lib.ui_lib#ui ui The ui object initialized in UI.lua
  local function init(ui)
    local title = Graphic:new("Plasma Chamber Controls")
    local body = Panel:new()
    local status = Graphic:new("     ")
    
    title.ypos = 2
    title:align("center")
    body:add(title)
    body.width = "max"
    body:setHeight(2)
    ui:add(body)
    ui:add(status)
    
    local inject = Graphic:new("Initiate Plasma Injection")
    
    local injecting = Graphic:new("Injecting Plasma")
    local warning = Graphic:new("Plasma Combustion Volatile")
    local final = Graphic:new("Aborting Reaction Sequence")
    
    inject:setTextColor(colors.green)
    warning:setTextColor(colors.red)
    final:setTextColor(colors.orange)
    inject:align("center")
    injecting:align("center")
    warning:align("center")
    final:align("center")
    
    ui:add(inject)
    
    local function bright()
      body:setBackgroundColor(colors.lightGray)
      body:setTextColor(colors.gray)
      ui:setBackground(colors.gray)
      ui:setText(colors.lightGray)
      ui:update()
    end
    
    inject:setOnSelect(ui,function() ui:ping() Lab.startGas() end)
    
    bright()
  end
  
  return UI.start(mon,29,5,init)
end

---Security Control Panel.
--@type SecurityUI
local SecurityUI = {}

---
-- @function [parent=#SecurityUI] start_link
-- @param #string mon the monitor to spawn the ui
-- @return #thread address to new UI Client
function SecurityUI.start_link(mon,silo)
  
  --- init function to initialize Gas UI.
  -- @function init
  -- @param lib.ui_lib#ui ui The ui object initialized in UI.lua
  local function init(ui)
    local title = Graphic:new(" Security Control ")
    local body = Panel:new()
    
    local open = Graphic:new("Open Silo")
    local close = Graphic:new("Close Silo")
    local alarmOn = Graphic:new("Activate Alarm")
    local alarmOff = Graphic:new("Disable Alarm")
    
    local status = Graphic:new("     ")
    
    title.ypos = 2
    body:add(title)
    body.width = "max"
    ui:add(body)
    ui:add(status)
    ui:add(alarmOn)
--    ui:add(open)
    ui:add(close)
      
    local enabledAlarm = function()
      reenable(ui.pane,alarmOff,3)
    end
    local disabledAlarm = function()
      reenable(ui.pane,alarmOn,3)
    end
    
    local openedSilo = function()
      reenable(ui.pane,close,4)
      ui:update()
    end
    local closedSilo = function()
      reenable(ui.pane,open,4)
      ui:update()
    end

    local openSilo = function()
      if Lab.openSilo() then
        ui:ping()
      else
        ui:beep()
      end
    end
    
    open:setOnSelect(ui,openSilo)
    close:setOnSelect(ui,function() ui:ping() Lab.closeSilo() end)
    
    alarmOn:setOnSelect(ui,Lab.callAlarm)
    alarmOff:setOnSelect(ui,Lab.stopAlarm)
      
    local function bright()
      disabledAlarm()
      body:setBackgroundColor(colors.lightGray)
      body:setTextColor(colors.gray)
      ui:setBackground(colors.gray)
      ui:setText(colors.lightGray)
      ui:update()
    end
    
    local function alarm()
      enabledAlarm()
      ui:setBackground(colors.black)
      body:setBackgroundColor(colors.red)
      body:setTextColor(colors.white)
      ui:update()
    end
    
    bright()
    Door.subscribe(silo)
    Lab.subscribe()
    
    ui.reactor:register("opened",openedSilo)
    ui.reactor:register("closed",closedSilo)
    ui.reactor:register("triggered",alarm)
    ui.reactor:register("reset",bright)
  end
  
  return UI.start(mon,18,5,init)
end

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
    
    Lab.subscribe()
  end
  
  return UI.start(mon,7,5,init)
end

function AlarmUI.trigger(Co)
  gen_server.cast(Co,{"triggered"})
end

function AlarmUI.reset(Co)
  gen_server.cast(Co,{"reset"})
end

local function initUI()
  local ui = UI.start("terminal",20,9)
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
  local ignite = Graphic:new("Ignite Gas")
  local stopReaction = Graphic:new("Stop Reaction")
  title:align("center")
  title.ypos = 2
  
  local menu = Menu:new()
  menu.xpos = 2
  menu.width = "max"
  
  menu:add(lights)
--  menu:add(open)
  menu:add(close)
  menu:add(alarmOn)
  menu:add(resetPanels)
  menu:add(ignite)
  menu:add(stopReaction)
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

  local openSilo = function()
    if Lab.openSilo() then
      ui:ping()
    else
      ui:beep()
    end
  end
    
  ui.denied = function()
    ui:beep()
  end
  
  open:setJustOnSelect(ui,openSilo)
  close:setJustOnSelect(ui,function()ui:ping() Lab.closeSilo() end)
  
  alarmOn:setJustOnSelect(ui,Lab.callAlarm)
  alarmOff:setJustOnSelect(ui,Lab.stopAlarm)
  
  ignite:setJustOnSelect(ui,function()ui:ping() Lab.ignite() end)
  stopReaction:setJustOnSelect(ui,function()ui:ping() Lab.stopReaction() end)
  
  return ui
end

local function triggerAlarms(State)
  State.producer:send("triggered")
end

local function resetAlarms(State)
  State.producer:send("reset")
end

function Lab.handle_call(Request,From,State)
  local event = Request[1]
  if event == "open_silo" then
    if State.alarm then
      gen_server.reply(From,false)
    else
      gen_server.reply(From,true)
      Door.open(State.silo)
      State.ui.openedSilo()
    end
  end
  return State
end

local function pulsePiston()
  local co
  local fun = function()
    if co then
      return
    else
      co = VM.running()
      cables.prep_piston:enable()
      EVE.sleep(1)
      cables.prep_piston:disable()
      co = nil
    end
  end
  VM.spawn(fun)
end

function Lab.handle_cast(Request,State)
  local event = Request[1]
  if event == "enable_alarm" then
    State.alarm = true
    Door.denyAccess(State.hallAccess)
    cables.emergency:enable()
    State.ui.enabledAlarm()
    Door.close(State.silo)
    State.ui.closedSilo()
    triggerAlarms(State)
  elseif event == "disable_alarm" then
    State.alarm = false
    cables.emergency:disable()
    State.ui.disabledAlarm()
    Door.allowAccess(State.hallAccess)
    resetAlarms(State)
  elseif event == "close_silo" then
    Door.close(State.silo)
    State.ui.closedSilo()
  elseif event == "opened" then
    State.ui.openedSilo()
    Door.allowAccess(State.siloAccess)
  elseif event == "closed" then
    State.ui.closedSilo()
    Door.denyAccess(State.siloAccess)
  elseif event == "subscribe" then
    State.producer:subscribe(Request[2])
  elseif event == "start_gas" then
    cables.inject_gas:enable()
    cables.tube_lights:enable()
  elseif event == "stop_reaction" then
    cables.inject_gas:disable()
    cables.ignite:disable()
    cables.tube_lights:disable()
  elseif event == "ignite" then
    pulsePiston()
    cables.ignite:enable()
  end
  return State
end

function Lab.handle_info(Request,State)
  VM.log("warning handle info at arq_lab")
  return State
end

function Lab.init()

  local silo = Door.newCargo(cables.open_silo,cables.close_silo,SILO_DELAY)
  Door.open(silo)
  local hallway = Door.new(cables.silo_doors,8)
  local hallAccess = Door.newUI("monitor_111","Lab 102",hallway)
  local siloAccess = Door.newUI("monitor_112","Lab 102",hallway,"123")
  Door.denyAccess(siloAccess)
  Door.subscribe(silo)
  local ui = initUI()
  
--  local gas = gasUI.start_link("terminal")
  local gas = gasUI.start_link("monitor_73")
  status_ui.start(silo,"monitor_110")
  static_ui.start("monitor_117","Lab 103 - Processing")
  static_ui.start("monitor_118","Lab 101","Re-search")

  local storage = Door.new(cables.storage,5,cables.storage_sensor)
  Door.newUI("monitor_120","Storage",storage,"123")
  local doors = {
    silo,
    Door.newUI("monitor_116","Lab 103"),
    Door.newUI("monitor_119","Lab 101"),
    Door.new(cables.storage,cables.storage_sensor),
    storage
  }
  
  Lab.subscribe(status_ui.start(silo,"monitor_109"))
  
  SecurityUI.start_link("monitor_113",silo)
  AlarmUI.start_link("monitor_114")
  AlarmUI.start_link("monitor_115")
  
  return true, {
    ui = ui,
    silo = silo,
    siloAccess = siloAccess,
    hallAccess=hallAccess,
    producer = Producer:new(),
    alarm = false,
    gas = gas
  }
end

function Lab.terminate(Reason,State)
  cables.close_silo:disable()
  cables.open_silo:enable()
end

return Lab