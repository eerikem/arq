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
-- @param #table hub_table A list of Destinations and their status
function TeleUI.start(monitor,hub_table)

  ---
  -- @param lib.ui_lib#ui ui
  local function initTeleUI(ui)
    local title = Graphic:new("DEST.   STATUS")
    local body = Panel:new()
    body.width = "max"
    body:setHeight(5)
    body:setLayout("static")
    ui:add(title)
    ui:add(body)
    ui.setTextScale(2)    
    for n,hub in ipairs(hub_table or {}) do
      local dest = Graphic:new(hub[1])
      dest.ypos = n + 1
      local stat = hub[2]
      local status = Graphic:new(stat)
      if stat == "Online" then
        status:setTextColor(colors.green)
        status.xpos = 9
      else
        status.xpos = 8
        status:setTextColor(colors.red)
      end
      status.ypos = n + 1
      body:add(dest)
      body:add(status)
    end
    
    local function bright()
      ui:setBackground(colors.lightGray)
      ui:setText(colors.gray)
      body:setBackgroundColor(colors.gray)
      body:setTextColor(colors.lightGray)
    end
    
    bright()
    
    ui:update()
  end
  
  return UI.start(monitor,14,6,initTeleUI)
end

return TeleUI
