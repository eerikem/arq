local gen_server = require "gen_server"
local UI = require "lib.ui"
local Graphic = require "lib.graphic"
local Panel = require "lib.ui_obj"
local Door = require "door"


local Status = {} 

function Status.start(doorCo,monitor,scheme)
  local ui = UI.start(monitor,20,2)
  local title = Graphic:new("Lab 102 - ARQ Dev ")
  local body = Panel:new()
  local lockdown = Graphic:new("Lockdown in effect")
  
  local function dark()
    ui:setText(colors.white)
    ui:setBackground(colors.gray)
    ui:update()
  end
  
  local function blue()
    ui:setText(colors.blue)
    ui:setBackground(colors.lightGray)
    body:setTextColor(colors.lightBlue)
    body:setBackgroundColor(colors.blue)
    ui:update()
  end
  
  local function bright()
    ui:setBackground(colors.gray)
    body:setBackgroundColor(colors.lightGray)
    body:setTextColor(colors.gray)
    ui:update()
  end
    
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
    ui.reactor:register("bright",bright)
    ui.reactor:register("dark",dark)
    ui.reactor:register("blue",blue)
    
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
    ui.reactor:register("bright",bright)
    ui.reactor:register("dark",dark)
    ui.reactor:register("blue",blue)
    
  end
  
  local function setScheme(scheme)
    if scheme ~= nil then
      if scheme == "bright" then bright()
      elseif scheme == "dark" then dark()
      elseif scheme == "blue" then blue()
      else bright() end  
    end
  end
  
  setScheme(scheme)
  ui.reactor:register("scheme",scheme)
  
  Door.subscribe(doorCo,ui.co)
  
  return ui.co
end

return Status