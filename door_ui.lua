local gen_server = require "gen_server"
local UI = require "lib.ui"
local Graphic = require "lib.graphic"
local Panel = require "lib.ui_obj"
local Password = require "password"
local Door = require "door"
local DoorUI = {}

--UI.start pulls UI gen_server from ui_server
--Add eventHandlers with UI.register
--Send events with UI.handleEvent|handleEventSync
function DoorUI.new(monCo,door)
  local ui = UI.start(monCo,7,5)
  local title = Graphic:new(door.title)
  local body = Panel:new()
  local open = Graphic:new("OPEN")
  local close = Graphic:new("CLOSE")
  local status = Graphic:new("       ")
  body.width = "max"
  open.xpos = 2
  open.ypos = 2
  close.xpos = 2
  close.ypos = 2
  ui:add(title)
  body:add(open)
  body:add(status)
  ui:add(body)
  close.reactor:stop()
  
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
  
  local lastDenied = nil
  local denyTime
  local flashDenied = function()
        EVE.sleep(denyTime or 1)
        denyTime = nil
        if lastDenied == VM.running() then
          status.text="       "
          ui:update()
          lastDenied = nil
        end
      end
  
  local function handler(door,reactor)
    return function()
      if reactor.parent == open then
        local res, time = Door.open(door)
        denyTime = time
        if res == "opened" then
          ui:ping()
          enable(close)
        elseif res == "canceled" then
          VM.log("received Canceled")
          ui:tap()
        else
          ui:beep()
          status.text="Denied!"
          ui:update()
          lastDenied = VM.spawn(flashDenied)
        end
      elseif reactor.parent == close then
        local res, time = Door.close(door)
        denyTime = time
        if res == "closed" then
          ui:ping()
          enable(open)
        else
          ui:beep()
          status.text="Denied!"
          ui:update()
          lastDenied = VM.spawn(flashDenied)
        end
      end
    end
  end
  
  local function closeHandler()
    enable(open)
  end
  
  local function openHandler()
    enable(close)
  end
  
  open:setOnSelect(ui,handler(VM.running(),open.reactor))
  close:setOnSelect(ui,handler(VM.running(),close.reactor))
  ui.reactor:register("closed",closeHandler)
  ui.reactor:register("opened",openHandler)
  
  local function bright()
    ui:setBackground(colors.lightGray)
    ui:setText(colors.gray)
    body:setTextColor(colors.orange)
    body:setBackgroundColor(colors.gray)
    status:setTextColor(colors.red)
  end
  
  bright()
  ui:update()
  
  return ui
end

return DoorUI