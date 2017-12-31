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
-- @param #string monitor
-- @param #string title
-- @param #thread door
-- @param door#door Door The Door module
-- @param #string password optional
-- @return #thread address to new UI Client
function DoorUI.start_link(monitor,title,door,Door,password)
  --- init function to initialize Client UI.
  -- @function [parent=#door_ui] init
  -- @param lib.ui_lib#ui ui The ui object initialized in UI.lua
  local function init(ui)
    local title = Graphic:new(title)
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
        local res, time = Door.open(door)
        if res == "denied" then
          gen_server.cast(From,{"deny",2})
        end
      end,
      canceled = function (UI)
        gen_server.cast(UI,{"canceled"})
      end
    }
        
    local function cancel()
      ui:tap()
    end
    
    local denyAccess = false
    local function deny()
      ui:beep()
      status.text="Denied!"
      ui:update()
      lastDenied = VM.spawn(flashDenied)
    end
    
    local function handler(door,reactor)
      return function()
        if denyAccess then
          return deny()
        end
        if reactor.parent == open then
          if password then
            return Password.start(password,monitor,
              {Mod,"success",{door,ui.co}},
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
            deny()
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

    local function denyHandler(_,time)
      denyTime = time
      deny()
    end
    
    local function denyAll()
      deny()
    end
    
    local function cancelHandler()
      cancel()
    end
    
    local function accessHandler(event)
      if event == "denyAccess" then
        denyAccess = true
      elseif event == "allowAccess" then
        denyAccess = false
      end
    end
    
    if door then
      open:setOnSelect(ui,handler(door,open.reactor))
      close:setOnSelect(ui,handler(door,close.reactor))
      ui.reactor:register("closed",closeHandler)
      ui.reactor:register("opened",openHandler)
      ui.reactor:register("deny",denyHandler)
      ui.reactor:register("canceled",cancelHandler)
      ui.reactor:register("denyAccess",accessHandler)
      ui.reactor:register("allowAccess",accessHandler)
    else
      open:setOnSelect(ui,denyAll)
    end
    
    local function bright()
      ui:setBackground(colors.lightGray)
      ui:setText(colors.gray)
      body:setTextColor(colors.orange)
      body:setBackgroundColor(colors.gray)
      status:setTextColor(colors.red)
    end

    bright()
    ui:update()
    if door then
      Door.subscribe(door)
    end
    
  end

  return UI.start(monitor,7,5,init)
end

return DoorUI
