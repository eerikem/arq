local gen_server = require "gen_server"
local Bundle = require "bundle"

local CABLE_SIDE = "back"
local cables = {
  override = Bundle:new(CABLE_SIDE,colors.black,"Override"),
  white = Bundle:new(CABLE_SIDE,colors.white,"White Signal"),
  yellow = Bundle:new(CABLE_SIDE,colors.yellow,"Yellow Signal")
}

local function elevator()
  local ui = ui_server.newWindow("monitor_1",7,6)
  
  local floor = Graphic:new("1 2 3")
  floor.align= "center"
  
  local floorPanel = Panel:new()
  local lvl = Graphic:new("Level")
  lvl.align = "center"
  floorPanel:add(lvl)
  floorPanel:setTextColor(colors.green)
  floorPanel:add(floor)
  floorPanel.width = "max"
  local button1 = Graphic:new("1")
  local button2 = Graphic:new("2")
  local button3 = Graphic:new("3")
  
  button1.align = "center"
  button2.align = "center"
  button3.align = "center"
  
  local buttonPanel = Panel:new()
  buttonPanel.width = "max"
  
  local buttonRow = Graphic:new("1 2 3")
  buttonRow.xpos = 2
  buttonRow.ypos = 2
  buttonPanel:add(buttonRow)
  
  ui:add(floorPanel)
  ui:add(buttonPanel)
  
  local isDark = true
  local function dark()
    ui:setBackground(colors.gray)
    ui:setText(colors.lightGray)
    floor:setTextColor(colors.gray)
    floorPanel:setBackgroundColor(colors.black)
    isDark = true
  end
  local function bright()
    ui:setBackground(colors.lightGray)
    ui:setText(colors.gray)
    floor:setTextColor(colors.black)
    floorPanel:setBackgroundColor(colors.gray)
    isDark = false
  end
  
  dark()
  
  ui:align("center","left")
  ui:update()
    
  local function colorHandler()
    if isDark then
      bright()
    else
      dark()
    end
    ui:update()
  end
  
  floorPanel:setOnSelect(ui,colorHandler)
    
  return ui,floor
end

local Server = {}

function Server.start()
  local Co = gen_server.start_link(Server,{},{},"elevator")
--  EVE.subscribe("redstone",Co)
end

function Server.init()
  local ui, floor = elevator()
  return {ui = ui,floor = floor,enabled = true}
end

function Server.handle_call(Request,From,State)
  gen_server.reply(From,"ok")
  return State
end

function Server.handle_cast(Request,State)
  local event = Request[1]
  if event == "redstone" then
    if State.enabled then
      State.ui = elevator2()
      State.ui:update()
      State.enabled = false
    else
      State.ui = elevator()
      State.ui:update()
      State.enabled = true
    end
    State.ui:update()
  end
  return State
end

return Server