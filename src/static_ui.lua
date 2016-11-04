local gen_server = require "gen_server"
local UI = require "lib.ui"
local Graphic = require "lib.graphic"
local Panel = require "lib.ui_obj"

local Status = {} 

---
-- @function [parent=#src.static_ui] start
-- @param #string monitor
-- @param #string title
-- @param #string status optional secondary message
function Status.start(monitor,title,status)
  local ui = UI.start(monitor)
  local title = Graphic:new(title)
  local body = Panel:new()
  
  ui.setTextScale(2)
  title:align("center")
  ui:add(title)
  
  if status then
    local status = Graphic:new(status)
    status:align("center")
    body:add(status)
    ui:add(body)
    status:setTextColor(colors.red)
  end  

  local function dark()
    ui:setText(colors.white)
    ui:setBackground(colors.gray)
  end
  
  local function bright()
    ui:setBackground(colors.lightGray)
    ui:setText(colors.black)
  end
  
  dark()
  ui:update()  
end

return Status