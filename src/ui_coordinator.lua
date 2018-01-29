----------------------
--  UI Coordinator  --
----------------------
local UI = require "lib.ui"
local gen_server = require "gen_server"
local ui_server = require "ui_server"
local Bundle = require "lib.bundle"
local Door = require 'door'
local Graphic = require "lib.graphic"
local Panel = require "lib.ui_obj"
local config = require "config"

local DEFAULT_RADIUS = 6
local LOCATE = "locate %s"
---An app for caching xyz coords for monitor sounds
--@module ui_coordinator
local Client = {}

local function securityUI(monitor,Parent)
  local ui = ui_server.newWindow(monitor,8,7)
  ui.alwaysOnTop = true
  local title = Graphic:new(monitor)
  title:align("center")
  local body = Panel:new()
  body:setLayout("static")
  body.width = "max"
  
  local ian = Graphic:new("LordKmp")
  local eric = Graphic:new("ArqiTek")
  eric.ypos = 2
  
  local ok = Graphic:new("ok")
  ok:setTextColor(colors.green)
  ok.xpos = 6
  ok.ypos = 3
  
  local left = Graphic:new("<")
  left:setTextColor(colors.orange)
  left.xpos = 1
  left.ypos = 3
  local right = Graphic:new(">")
  right:setTextColor(colors.orange)
  right.xpos = 4
  right.ypos = 3
  local radius = Graphic:new(string.format("%d",DEFAULT_RADIUS))
  radius.xpos = 2
  radius.ypos = 3
  
  body:add(ian)
  body:add(eric)
  body:add(ok)
  body:add(left)
  body:add(radius)
  body:add(right)
  
  ui:add(title)
  ui:add(body)
    
  ui:setBackground(colors.lightGray)
  ui:setText(colors.gray)
  body:setBackgroundColor(colors.gray)
  body:setTextColor(colors.orange)
  
  ian:setOnSelect(ui,function() Client.getUserCoord(Parent,"LordKemp")end)
  eric:setOnSelect(ui,function() Client.getUserCoord(Parent,"ArqiTek")end)
  ok:setOnSelect(ui,function() gen_server.cast(Parent,{"ok"}) end)
  left:setOnSelect(ui,function() gen_server.cast(Parent,{"left"}) end)
  right:setOnSelect(ui,function() gen_server.cast(Parent,{"right"}) end)
  radius:setOnSelect(ui,function() gen_server.cast(Parent,{"radius"}) end)
  
  ui:update()
  return ui, body, radius
end

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
  obj:setTextColor(colors.lightGray)
  obj.reactor:stop()
end

local function updatePanel(State)
  
  State.rButton.text = string.format("%d",State.radius)
  State.ui:update()
end

function Client.getUserCoord(Co,user)
  gen_server.cast(Co,{"get_user_xyz",user})
end

function Client.setRadius(Co,radius)
  gen_server.cast(Co,{"set_radius",radius})
end

function Client.init(monitor)
  local ui, panel, rButton = securityUI(monitor,VM.running())
  return true, {
    ui = ui,
    monitor = monitor,
    panel = panel,
    rButton = rButton,
    radius = DEFAULT_RADIUS
    }
end

function Client.stop(Co)
  gen_server.cast(Co,{"stop"})
end

local function setCoords(State,myString)
--ArqiTek is at -1, 12, -421 in dim 0 with gamemode creative
  local x,y,z = string.match(myString,"is at ([\-0-9]+), ([\-0-9]+), ([\-0-9]+) in dim")
  State.x = tonumber(x)
  State.y = tonumber(y)
  State.z = tonumber(z)
  CONFIG[State.monitor]={State.x,State.y,State.z,State.radius}
  config.save(CONFIG)
end

function Client.handle_call(Request,From,State)
  local event = Request[1]
  VM.log("Got "..event)
  
  return State
end

function Client.handle_cast(Request,State)
  local event = Request[1]
  VM.log("Got "..event)
  if event == "get_user_xyz" then
    local user = Request[2]
    VM.log("Attempting: "..string.format(LOCATE,user))
    EVE.subscribe("task_complete")
    State.cmdId = commands.execAsync(string.format(LOCATE,user))
    VM.log("got id: "..State.cmdId)
    updatePanel(State)
  elseif event == "set_radius" then
    local radius = Request[2]
    if radius > 0 then
      State.radius = radius
      updatePanel(State)
    else
      VM.log("Received bad radius")
    end
  elseif event == "task_complete" then
    local event, id, success, error, payload= unpack(Request)
    if id == State.cmdId and success then
      if string.match(payload[1],"does not exist") then
        VM.log(payload[1])
        State.ui:beep()
      else
          setCoords(State,payload[1])
          EVE.unsubscribe("task_complete")
      end
    end
  elseif event == "left" then
    State.radius = State.radius - 1
    updatePanel(State)
  elseif event == "right" then
    State.radius = State.radius + 1
    updatePanel(State)
  elseif event == "radius" then
    State.radius = State.radius + 5
    updatePanel(State)
  elseif event == "ok" then
    if State.x == nil then
      State.ui:beep()
    else
      State.ui:ping()
      VM.exit("normal")
    end
  elseif event == "stop" then
    VM.exit("normal")
  end
  return State
end

function Client.handle_info(Request,State)
  VM.log("warning handle info at ui_coordinator")
  return State
end

function Client.start(monitor)
  gen_server.start(Client,{monitor},{})    
end

return Client