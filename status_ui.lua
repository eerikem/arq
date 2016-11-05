local gen_server = require "gen_server"
local UI = require "lib.ui"
local Graphic = require "lib.graphic"
local Panel = require "lib.ui_obj"
local Door = require "door"


local Status = {} 

function Status.start(doorCo,monitor)
  local ui = UI.start(monitor,20,2)
  local title = Graphic:new("Lab 102 - ARQ Dev ")
  local body = Panel:new()
  local lockdown = Graphic:new("Lockdown in effect")
  
  ui.setTextScale(2)
  if ui.native.getSize() < string.len(lockdown.text) then
    ui.setTextScale(1)
    title.ypos = 2
    lockdown.ypos = 2
    
    body:add(title)
    body.width = "max"
    ui:add(body)
--    ui:add(lockdown)
    
    lockdown:setTextColor(colors.red)
    
    local function bright()
      ui:setBackground(colors.gray)
      body:setBackgroundColor(colors.lightGray)
      body:setTextColor(colors.gray)
      ui:update()
    end
    
    local function alarm()
      ui:setBackground(colors.black)
      body:setBackgroundColor(colors.red)
      body:setTextColor(colors.white)
      ui:update()
    end
    
    bright()
    
    local function openedHandler()
      if ui.pane:contains(lockdown) then
        ui.pane:remove(lockdown)
        ui:update()
      end
    end
    
    local function closedHandler()
      if not ui.pane:contains(lockdown) then
        ui:add(lockdown)
        ui:update()
      end
    end
    
    
    
    ui.reactor:register("opened",openedHandler)
    ui.reactor:register("closed",closedHandler)
    ui.reactor:register("triggered",alarm)
    ui.reactor:register("reset",bright)
    
  else
    
    title:align("center")
    title.width = "max"
    lockdown:align("center")
    body.width = "max"
--    body:add(lockdown)
    
    ui:add(title)
    ui:add(body)
    
    lockdown:setTextColor(colors.red)
--    lockdown:setTextColor(colors.lightGray)
    
    local function dark()
      ui:setText(colors.white)
      ui:setBackground(colors.gray)
      ui:update()
    end
      
    dark()
    
    local function openedHandler()
      if body:contains(lockdown) then
        body:remove(lockdown)
        ui:update()
      end
    end
    
    local function closedHandler()
      if not body:contains(lockdown) then
        body:add(lockdown)
        ui:update()
      end
    end
    
    ui.reactor:register("opened",openedHandler)
    ui.reactor:register("closed",closedHandler)
    
  end
  
  Door.subscribe(doorCo,ui.co)
  
  return ui.co
end

return Status