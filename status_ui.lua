local gen_server = require "gen_server"
local UI = require "lib.ui"
local Graphic = require "lib.graphic"
local Panel = require "lib.ui_obj"
local Door = require "door"


local Status = {} 

function Status.start(doorCo,monitor)
  local ui = UI.start(monitor,20,2)
  local title = Graphic:new("Lab 102 - ARQ Dev")
  local body = Panel:new()
  local lockdown = Graphic:new("Lockdown in effect")
  
  ui.setTextScale(2)
  
  title:align("center")
  lockdown:align("center")
--  body.width = "max"
  body:add(lockdown)
  
  ui:add(title)
  ui:add(body)
  
  local function dark()
--    lockdown:setTextColor(colors.lightGray)
    lockdown:setTextColor(colors.red)
    ui:setText(colors.white)
    ui:setBackground(colors.gray)
  end
  
  local function bright()
    lockdown:setTextColor(colors.red)
    ui:setBackground(colors.lightGray)
    ui:setText(colors.black)
  end
  
  dark()
--  bright()
  ui:update()
  
  
  
end

return Status