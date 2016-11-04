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
-- @param #number scale optional monitor scale
function Status.start(monitor,titleStr,status,scale)
  local ui = UI.start(monitor)
  local title = Graphic:new(titleStr)
  local body = Panel:new()
  
  ui.setTextScale(2)
  local w,h = ui.term.getSize()
  if string.len(titleStr) > w then
      ui.setTextScale(1)
  end
  
  if status and string.len(status) > w then
    ui.setTextScale(1)
  end
  
  title:align("center")
  ui:add(title)
  
  if status then
    local status = Graphic:new(status)
    status:align("center")
    body:add(status)
    ui:add(body)
    status:setTextColor(colors.lightGray)
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
  VM.log(string.format("%s size: %d %d",monitor,ui.term.getSize()))
  ui:update()  
end

return Status