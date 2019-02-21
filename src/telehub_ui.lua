local Teleporter = require "telehub"
local UI = require "lib.ui"
local Graphic = require "lib.graphic"
local Panel = require "lib.ui_obj"


--------------
--    UI    --
--------------


local function enable(panel,button,index)
  local i = index or 2
  local old = panel.index[i]
  if old == button then return end
  old.reactor:stop()
  button.reactor:start()
  panel:replace(old,button)
end

--resets color of a disabled button
local function reenable(panel,button,index)
  local old = panel.index[index]
  if old ~= button then
    old.reactor:stop()
    panel:replace(old,button)
  end
  button:setTextColor(nil)
  button.reactor:start()
end

local function disable(panel,index)
  local obj = panel.index[index]
  obj:setTextColor(colors.lightGray)
--  obj.reactor:stop()
end

local TeleUI = {}

---
-- @param #string monitor
-- @param #string destination
-- @param #thread teleporter optional coroutine
function TeleUI.start(monitor,destination,teleporter)
---
-- @param lib.ui_lib#ui ui
local function initTeleUI(ui)
    local title = Graphic:new(destination)
    local body = Panel:new()
    local ready = Graphic:new(" READY TRANSIT")
    local prime = Graphic:new("PRIMED!")
    local teleport = Graphic:new("transit")
    local success = Graphic:new("success")
    local denied = Graphic:new("ACCESS ")
    local cycler = Graphic:new("       ")
    body.width = "max"
    ready.ypos = 2
    prime.ypos = 2
    teleport.ypos = 2
    success.ypos = 2
    denied.ypos = 2
--    cycler.ypos = 2
--    cycler:setBackgroundColor(colors.lightGray)
--    cycler:setTextColor(colors.gray)
    ui:add(title)
    body:add(ready)
    body:add(cycler)
    ui:add(body)
--    ui:add(cycler)
    prime.reactor:stop()
    teleport.reactor:stop()
    success.reactor:stop()
    cycler.reactor:stop()
    --TODO Remove Duplicate Hacks
    local function enable(button2)
      local button1 = body.index[1]
      if button1 ~= button2 then
        body:replace(button1,button2)
        button1.reactor:stop()
        button2.reactor:start()
        ui:update()
      end
      return button1
    end
    
    local lastDeny
    local prevButton
    local function deny()
      ui:beep()
      if not lastDeny then
        cycler.text=" DENIED "
        cycler:setTextColor(colors.red)
        prevButton = enable(denied)
      end
      lastDeny = EVE.queue("telehub_deny",1)
    end
    
    ui.reactor:register("telehub_deny",function(_,ref)
      if lastDeny == ref then
        cycler.text="       "
        enable(prevButton)
        prevButton = nil
        lastDeny = nil
      else
        VM.log("lastDeny and ref doesn't match")
      end
    end)
    
    
    if teleporter then
      ready:setOnSelect(ui,function()
        ui:ping()
        Teleporter.queueTransit(teleporter)
      end)
    
      body:setTextColor(colors.orange)
      body:setOnSelect(ui,function()ui:beep() end)
      body.reactor:stop()
    else
      ready.reactor:stop()
      body:setTextColor(colors.lightGray)
      body:setHeight(4)
      body:setOnSelect(ui,function()
        deny()
      end)
    end
        
    
    ui.reactor:register("deny",function()
    
    end)
    
    ui.reactor:register("canceled",function() enable(ready) end)
    
    ui.reactor:register("primed", function()
      enable(prime)
      body.reactor:start()
      end)
    
    local anim = {
      " -     ",
      "  -    ",
      "   -   ",
      "    -  ",
      "     - ",
    }
    local index = 1
    
    ui.reactor:register("teleporting",function()
      body:setTextColor(colors.lightGray)
      enable(teleport)
      cycler.text = "*      "
      cycler:setTextColor(nil)
      EVE.queue("animate_ready",3)
      ui:update()
    end)
    
    ui.reactor:register("animate_ready",function()
      cycler:setTextColor(colors.red)
      ui:update()
      EVE.queue("animate",2)
    end)
    
    ui.reactor:register("animate",function()
      cycler:setTextColor(colors.white)
      cycler.text = anim[index]
      index = index + 1
      if index <= 6 then
        EVE.queue("animate",4/6)
      else
        cycler.text = "      *"
        index = 1
      end
      ui:update()
    end)
    
    ui.reactor:register("teleported",function()
      body:setTextColor(colors.green)
      enable(success)
      cycler:setTextColor(colors.green)
      ui:update()
      end)
      
    ui.reactor:register("telehub_ready",function()
      body:setTextColor(colors.orange)
      enable(ready)
      cycler.text = "       "
      body.reactor:stop()
      ui:update()
    end)
    
    local function bright()
      ui:setBackground(colors.lightGray)
      ui:setText(colors.gray)
      body:setBackgroundColor(colors.gray)
      denied:setTextColor(colors.red)
      prime:setTextColor(colors.red)
    end

    bright()
    ui:update()
    if teleporter then
      Teleporter.subscribe(teleporter)
    end
end

  return UI.start(monitor,7,5,initTeleUI)
end

return TeleUI