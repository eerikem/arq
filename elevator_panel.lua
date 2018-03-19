local gen_server = require "gen_server"
local UI = require "lib.ui"
local Graphic = require "lib.graphic"
local Radio = require "lib.ui_radio_panel"
local ButtonPanel = require "lib.ui_button_panel"
local Panel = require "lib.ui_obj"
local Password = require "password"
local ui_server = require "ui_server"

local DoorUI = {}

function DoorUI.start_link(monitor,floor,elevator,Elevator,password)
  local ui = ui_server.newWindow(monitor,7,5)
  
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
  
  local buttonPanel = ButtonPanel:new()
  
  local handler = function(button)
    return function ()
      local lvl = buttonPanel.content[button]
      VM.log("Button for "..lvl.." pressed!")
      buttonPanel:setSelected(lvl)
      ui:update()
      Elevator.callTo(elevator,lvl)
    end
  end
  
  local denied = Graphic:new("DENIED")
  denied.xpos = 2
  denied.ypos = 2
  denied:setTextColor(colors.red)
  local erroring = false
  local errorHandler = function(button)
    return function()
      if erroring then
        return end
      erroring = true
      local lvl = buttonPanel.content[button]
      VM.log("Button for "..lvl.." pressed!")
      ui:beep()
      buttonPanel:add(denied)
      ui:update()
      
      local fun = function()
        EVE.sleep(1)
        buttonPanel:remove(denied)
        ui:update()
        erroring = false
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
  
  ui:add(floorPanel)
  ui:add(buttonPanel)
  ui:align("top")
  
  local function dark()
    ui:setBackground(colors.gray)
    ui:setText(colors.lightGray)
    floors:setTextColor(colors.gray)
    floors.proto.backgroundFocus = colors.black
    floorPanel:setBackgroundColor(colors.black)
    buttonPanel.proto.backgroundFocus = colors.gray
    buttonPanel.proto.textFocus = colors.white
    ui:update()
  end
  
  local function bright()
    ui:setBackground(colors.lightGray)
    ui:setText(colors.gray)
    floors:setTextColor(colors.black)
    floors.proto.backgroundFocus = colors.gray
    floorPanel:setBackgroundColor(colors.gray)
    buttonPanel.proto.backgroundFocus = colors.lightGray
    buttonPanel.proto.textFocus = colors.white
    ui:update()
  end
  
  dark()
  
  
    
  return ui,floors,buttonPanel
end

return DoorUI