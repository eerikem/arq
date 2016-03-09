--Product of ArqiTeknologies Corp.
--
--Author: ArqiTek
--Copyright 2250

local args = {...}


print = function() end
--write = function() end
sleep = function() end

VM = require "vm"
--UI = require "ui"
Reactor = require "reactor"
UI = require "ui_lib"
gen_server = require "gen_server"
ui_server = require "ui_server"
ui_sup = require "ui_supervisor"
EVE = require "eventListener"
Graphic = require "graphic"
Panel, List = require "ui_obj"
Menu = require "ui_menu"
--Attack = require "attack"

VM.init()


local Li = EVE.start_link()
--local Ui = EVE.subscriber(Li,ui_sup)
local Ui = ui_sup.start_link(Li)
local write = ui_sup.statusWindow("terminal")
VM.log = write
ui_sup.app("terminal")

function attackUI()
  local ui = ui_server.newWindow("monitor_4",18,5)
  ui:setBackground(colors.lightGray)
  ui:setText(colors.gray)
  local title = Graphic:new("Attack Control")
  title.align= "center"
  
  local toggleButton = Graphic:new("Disable Lock")
  local attackButton = Graphic:new("Launch Attack")
  local hiddenStatus = Graphic:new("         ")
  hiddenStatus.xpos = 2
  
  local menu = Menu:new()
  menu.width = "max"
  menu.xpos = 2
  menu.ypos = 2
  menu:add(toggleButton)
  menu:add(attackButton)
  menu.proto.backgroundFocus = colors.gray
  menu.proto.textFocus = colors.white
  
  local body = Panel:new()
  body.width = "max"
  body:setBackgroundColor(colors.gray)
  body:setTextColor(colors.lightGray)
  
  body:add(menu)
  body:add(hiddenStatus)
  
  ui:add(title)
  ui:add(body)
  
  ui:align("center")
  ui:update()
  
  menu:link(ui)
end
attackUI()

while true do
  --VM.flush()
  gen_server.cast(Li,{os.pullEvent()})
end