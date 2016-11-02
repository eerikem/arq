local gen_server = require "gen_server"
local UI = require "lib.ui"
local Graphic = require "lib.graphic"
local Panel = require "lib.ui_obj"
local Password = require "password"

---The UI Client for Door Servers
--@module door_ui

local DoorUI = {}

---
-- @function [parent=#door_ui] start_link
-- @param #string monCo the monitor to spawn the ui
-- @param door The door parameters
-- @param #thread doorCo Address of the door server
function DoorUI.start_link(monCo,door,doorCo,Door,password)
  local function init(ui)
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

    local Mod = {
      success = function (door,From)
        Door.forceOpen(door)
      end,
      canceled = function (UI)
        gen_server.cast(UI,{"canceled"})
      end
    }
    
    local function cancel()
      VM.log("received Canceled")
      ui:tap()
    end
    
    local function handler(door,reactor)
      return function()
        if reactor.parent == open then
          if password then
            return Password.start(password,monCo,
              {Mod,"success",{doorCo,ui.co}},
              {Mod,"canceled",{ui.co}})
          end
          local res, time = Door.open(door)
          denyTime = time
          if res == "opened" then
            ui:ping()
            enable(close)
          elseif res == "canceled" then
            cancel()
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
        else
          error("bad")
        end
      end
    end

    local function closeHandler()
      enable(open)
    end

    local function openHandler()
      enable(close)
    end

    local function denyHandler()
      flashDenied()
    end
    
    local function cancelHandler()
      cancel()
    end

    open:setOnSelect(ui,handler(doorCo,open.reactor))
    close:setOnSelect(ui,handler(doorCo,close.reactor))
    ui.reactor:register("closed",closeHandler)
    ui.reactor:register("opened",openHandler)
    ui.reactor:register("deny",denyHandler)
    ui.reactor:register("canceled",cancelHandler)
    
    local function bright()
      ui:setBackground(colors.lightGray)
      ui:setText(colors.gray)
      body:setTextColor(colors.orange)
      body:setBackgroundColor(colors.gray)
      status:setTextColor(colors.red)
    end

    bright()
    ui:update()
    
    Door.subscribe(doorCo)
    
  end

  return UI.start(monCo,7,5,init)
end

return DoorUI
