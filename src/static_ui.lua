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
  if string.len(titleStr) > w / 2 then
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
--  VM.log(string.format("%s size: %d %d",monitor,ui.term.getSize()))
  ui:update()  
end

---
-- @function [parent=#src.static_ui] startFancy
-- @param #string monitor
-- @param #string title
-- @param #string status optional secondary message
function Status.startFancy(monitor,titleStr,status)
  local ui = UI.start(monitor)
  local title = Graphic:new(titleStr)
  local body = Panel:new()
  body.width = "max"
  
  ui.setTextScale(2)
  local w,h = ui.term.getSize()
  if string.len(titleStr) > w / 2 then
      ui.setTextScale(1)
  end
  
  if status and string.len(status) > w / 2 then
    ui.setTextScale(1)
  end
  
  title.width="max"
  title:align("center")
  ui:add(title)
  
  if status then
    local status = Graphic:new(status)
    status:align("center")
    body:add(status)
    body:add(Graphic:new("                  "))
    ui:add(body)
    status.width = "max"
    status.ypos = 2
  end  

  local function dark()
    ui:setText(colors.white)
    ui:setBackground(colors.gray)
    body:setTextColor(colors.gray)
    body:setBackgroundColor(colors.lightGray)
  end
  
  local function bright()
    ui:setBackground(colors.lightGray)
    ui:setText(colors.gray)
    body:setBackgroundColor(colors.gray)
    body:setTextColor(colors.lightGray)
  end
  
  local function someColor()
    ui:setBackground(colors.blue)
    ui:setText(colors.white)
    body:setBackgroundColor(colors.lightGray)
    body:setTextColor(colors.gray)
  end
--  dark()
  bright()
--  VM.log(string.format("%s size: %d %d",monitor,ui.term.getSize()))
  ui:update()  
end

return Status