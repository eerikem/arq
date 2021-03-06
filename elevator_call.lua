local gen_server = require "gen_server"
local UI = require "lib.ui"
local Graphic = require "lib.graphic"
local Panel = require "lib.ui_obj"
local Password = require "password"
local ButtonPanel = require "lib.ui_button_panel"

local DoorUI = {}

function DoorUI.start_link(monitor,title,myLevel,elevator,Elevator,password)
  local function init(ui)
    local title = Graphic:new(title)
    local body = ButtonPanel:new()
    local open = Graphic:new("CALL")
    local status = Graphic:new("       ")
    body.width = "max"
    open.xpos = 2
    open.ypos = 2
    ui:add(title)
--    local buttonPanel = ButtonPanel:new()
    body:add(open)
    body:add(status)
    ui:add(body)
  
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
      success = function ()
        Elevator.callTo(elevator,myLevel)
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
    
    local selected = false
    
    local function handler(elevator)
      return function()
        if denyAccess then
          return deny()
        end
        if password then
          return Password.start(password,monitor,
            {Mod,"success",{elevator,ui.co}},
            {Mod,"canceled",{ui.co}})
        else
          Elevator.callTo(elevator,myLevel)
          if not selected then
            body:setSelected(1)
            ui:update()
            selected = true
          else
            ui:tap()
          end
        end
      end
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
    
    local function levelHandler(event,arrivedAt)
      VM.log("Calling level handler in call panel")
      if arrivedAt == myLevel then
        body:noneSelected()
        selected = false
        ui:update()
      end
    end
    
    if elevator then
      open:setOnSelect(ui,handler(elevator))
      ui.reactor:register("deny",denyHandler)
      ui.reactor:register("canceled",cancelHandler)
      ui.reactor:register("denyAccess",accessHandler)
      ui.reactor:register("allowAccess",accessHandler)
      ui.reactor:register("level",levelHandler)
    else
      open:setOnSelect(ui,denyAll)
    end
    
    local function bright()
      ui:setBackground(colors.lightGray)
      ui:setText(colors.gray)
      body:setTextColor(colors.orange)
      body:setBackgroundColor(colors.gray)
      body.proto.backgroundFocus = colors.gray
      body.proto.textFocus = colors.white
      status:setTextColor(colors.red)
    end

    bright()
    ui:update()
    if elevator then
      Elevator.subscribe(elevator)
    end
    
  end

  return UI.start(monitor,7,5,init)
end

return DoorUI
