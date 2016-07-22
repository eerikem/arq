local gen_server = require "gen_server"
local ui_server = require "ui_server"
local Graphic = require "graphic"
local Bundle = require "bundle"
local Radio = require "ui_radio_panel"
local Panel = require "ui_obj"

local CABLE_SIDE = "back"
local cables = {
  white = Bundle:new(CABLE_SIDE,colors.white,"White Signal"),
  yellow = Bundle:new(CABLE_SIDE,colors.yellow,"Yellow Signal")
}

local Elevator = {}

local function elevatorUI(Co,floor)
  local ui = ui_server.newWindow(Co,7,5)
  
  local floors = Radio:new()
  local lvl1 = Graphic:new("1")
  local lvl2 = Graphic:new("2")
  local lvl3 = Graphic:new("3")
  
  lvl2.xpos = 3
  lvl3.xpos = 5
  
  floors:setLayout("static")
  floors:add(lvl1)
  floors:add(lvl2)
  floors:add(lvl3)
  floors:setSelected(floor)
  floors.align= "center"--TODO implement this
  floors.xpos = 2
  floors.proto.textFocus = colors.green
  
  local floorPanel = Panel:new()
  local lvl = Graphic:new("Level")
  lvl.align = "center"
  floorPanel:add(lvl)
  floorPanel:setTextColor(colors.green)
  floorPanel:add(floors)
  floorPanel.width = "max"
  local button1 = Graphic:new("1")
  local button2 = Graphic:new("2")
  local button3 = Graphic:new("3")
  
  button1.xpos = 2
  button1.ypos = 2
  button2.xpos = 4
  button2.ypos = 2
  button3.xpos = 6
  button3.ypos = 2
  
  local buttonPanel = Radio:new()
  
  local handler = function(button)
    return function ()
      local lvl = buttonPanel.content[button]
      VM.log("Button for "..lvl.." pressed!")
      buttonPanel:setSelected(lvl)
      ui:update()
      Elevator.callTo(lvl)
    end
  end
  
  local denied = Graphic:new("DENIED")
  denied.xpos = 2
  denied.ypos = 2
  denied:setTextColor(colors.red)
  local errorHandler = function(button)
    return function()
      local lvl = buttonPanel.content[button]
      VM.log("Button for "..lvl.." pressed!")
      ui:beep()
      buttonPanel:add(denied)
      ui:update()
      
      local fun = function()
        EVE.sleep(1)
        buttonPanel:remove(denied)
        ui:update()
      end
      VM.spawn(fun)
    end
  end
  
  button1:setOnSelect(ui,handler(button1))
  button2:setOnSelect(ui,errorHandler(button2))
  button3:setOnSelect(ui,handler(button3))
  
  buttonPanel.width = "max"
  buttonPanel:setLayout("static")
  
  buttonPanel:add(button1)
  buttonPanel:add(button2)
  buttonPanel:add(button3)
  buttonPanel:noneSelected()
--  local buttonRow = Graphic:new("1 2 3")
--  buttonRow.xpos = 2
--  buttonRow.ypos = 2
--  buttonPanel:add(buttonRow)
  
  
  ui:add(floorPanel)
  ui:add(buttonPanel)
  
  local isDark = true
  local function dark()
    ui:setBackground(colors.gray)
    ui:setText(colors.lightGray)
    floors:setTextColor(colors.gray)
    floors.proto.backgroundFocus = colors.black
    floorPanel:setBackgroundColor(colors.black)
    buttonPanel.proto.backgroundFocus = colors.gray
    buttonPanel.proto.textFocus = colors.white
    isDark = true
  end
  local function bright()
    ui:setBackground(colors.lightGray)
    ui:setText(colors.gray)
    floors:setTextColor(colors.black)
    floors.proto.backgroundFocus = colors.gray
    floorPanel:setBackgroundColor(colors.gray)
    buttonPanel.proto.backgroundFocus = colors.lightGray
    buttonPanel.proto.textFocus = colors.white
    isDark = false
  end
  
  dark()
  
  ui:align("top")
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
    
  return ui,floors,buttonPanel
end

local function callPanelUI(Co,floor)
  local ui = ui_server.newWindow(Co,7,5)
  
  ui:setBackground(colors.lightGray)
  ui:setText(colors.gray)
  
  local title = Graphic:new("Level "..floor)
  title.align = "center"
  
  local body = Panel:new()
  body:setLayout("static")
  body.width = "max"
    
  local callButton = Graphic:new("Call")
  callButton.xpos = 3
  callButton.ypos = 2 
  
  body:setOnSelect(ui,function() Elevator.callTo(floor,callButton) end)
  
  body:add(callButton)
  body:setHeight(3)
  body:setBackgroundColor(colors.gray)
  body:setTextColor(colors.green)
  ui:add(title)
  ui:add(body)
  ui:update()
  return ui, callButton
end

function Elevator.start()
  local Co = gen_server.start_link(Elevator,{},{},"elevator")
--  EVE.subscribe("redstone",Co)
end

function Elevator.init()
  local floor = 3
  local ui, floors, buttons = elevatorUI("monitor_1",floor)
  local ui2, call = callPanelUI("monitor_5",3)
  local ui3, call2 = callPanelUI("monitor_3",1)
  cables.white:disable()
  return true, {cables = cables,
          elevators = {{callPanels = {{ui = ui2,button = call},
                                     {ui = ui3,button = call2}},
                       floorPanels = {{ui = ui,floors = floors,buttons = buttons}},
                       floor = floor}}}
end

function Elevator.handle_call(Request,From,State)
  gen_server.reply(From,"ok")
  return State
end

function Elevator.callTo(floor,button)
  if not floor then error("floor expected",2) end
  gen_server.cast("elevator",{"call",floor,button})
end

local function setFloor(lift,level)
  VM.log("Setting floor to "..level)
  for _,liftPanel in ipairs(lift.floorPanels) do
    liftPanel.floors:setSelected(level)
    liftPanel.ui:update()
  end
end

function Elevator.handle_cast(Request,State)
  local event = Request[1]
  if event == "call" then
    local dest = Request[2]
    local button = Request[3]
    VM.log("Floor "..dest.." called lift")
    local lift = State.elevators[1]
    if button then button:setTextColor(colors.white)
      for _,panel in ipairs(lift.callPanels) do panel.ui:update() end
    end
    if dest == lift.floor then
      lift.callPanels[1].ui.tap()
      EVE.sleep(1)
      local buttonPanel = lift.floorPanels[1]
      buttonPanel.buttons:noneSelected()
      buttonPanel.ui:update()
    else
      local buttonPanel = lift.floorPanels[1]
      lift.callPanels[1].ui.ping()--todo replace with intelligent ping
      EVE.sleep(1)
      buttonPanel.buttons:noneSelected()
      buttonPanel.ui:update()
      VM.log(dest.." "..lift.floor)
      if dest < lift.floor then
        for i=lift.floor,dest,-1 do
          setFloor(lift,i)
          EVE.sleep(2)
        end
      else
        for i=lift.floor,dest,1 do
          setFloor(lift,i)
          EVE.sleep(2)
        end
      end
      lift.floor = dest
    end
    State.cables.white:enable()
    if button then button:setTextColor(colors.green)
      for _,panel in ipairs(lift.callPanels) do panel.ui:update() end
    end
    EVE.sleep(5)
    State.cables.white:disable()
  end
  return State
end

return Elevator