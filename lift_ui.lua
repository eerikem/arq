local gen_server = require "gen_server"
local Graphic = require "lib.graphic"
local Panel = require "lib.ui_obj"
local Password = require "password"
local UI = require "lib.ui"


---
--@module lift_ui
local Client = {}

---
-- @function [parent=#lift_ui] start_link
-- @param #string mon the monitor to spawn the ui
-- @param #string title
-- @param #thread lift
-- @param lift#lift Lift The Lift module
-- @param #string password optional
-- @return #thread address to new UI
function Client.start_link(monitor,title,lift,Lift,password)

  
  --- init function to initialize Client UI.
  -- @function [parent=#ui_client] init
  -- @param lib.ui_lib#ui ui The ui object initialized in UI.lua
  local function init(ui)
    local title = Graphic:new(title)
    local body = Panel:new()
    local call = Graphic:new("CALL")
    local calling = Graphic:new("CALLING")
    local run = Graphic:new("RUNNING")
    local go = Graphic:new(" GO ")
    local status = Graphic:new("       ")
    body.width = "max"
    call.xpos = 2
    call.ypos = 2
    go.xpos = 2
    go.ypos = 2
    calling.xpos = 1
    calling.ypos = 2
    run.xpos = 1
    run.ypos = 2
    ui:add(title)
    body:add(call)
    body:add(status)
    ui:add(body)
    calling.reactor:stop()
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
    
    --Define behaviour around Password
    local Mod = {
      success = function (door,From)
        local res, time = Lift.call(door)
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
    
    --Takes a door and menu item
    --Assigns reactor behaviour
    local function handler(door,reactor)
      return function()
        if denyAccess then
          return deny()
        end
        if reactor.parent == call then
          if password then
            return Password.start(password,monitor,
              {Mod,"success",{door,ui.co}},
              {Mod,"canceled",{ui.co}})
          end
          local res, time = Lift.call(door)
          denyTime = time
          if res == "called" then
            ui:tap()
            enable(calling)
          elseif res == "canceled" then
            cancel()
          else
            deny()
          end
        elseif reactor.parent == go then
          local res = Lift.call(door)
          if res == "called" then
            ui:ping()
            enable(run)
          end
        else
          error("bad")
        end
      end
    end
    
    local function denyAll()
      deny()
    end
        
    local function arrivedHandler()
      local button = body.index[1]
      if button == calling or button == call then 
        enable(go)
      else
        enable(call)
      end
    end

    local function callHandler()
      local button = body.index[1]
      if button ~= calling and button ~= run then
        if button == go then
          enable(run)
        else
          enable(calling)
        end
      end
    end
    
    local function denyHandler(_,time)
      denyTime = time
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
    
    if lift then
      call:setOnSelect(ui,handler(lift,call.reactor))
      go:setOnSelect(ui,handler(lift,go.reactor))
      ui.reactor:register("arrived",arrivedHandler)
      ui.reactor:register("called",callHandler)
      ui.reactor:register("deny",denyHandler)
      ui.reactor:register("canceled",cancelHandler)
      ui.reactor:register("denyAccess",accessHandler)
      ui.reactor:register("allowAccess",accessHandler)
    else
      call:setOnSelect(ui,denyAll)
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
    if lift then
      Lift.registerUI(lift)
    end
    
  end
  
  
  
  return UI.start(monitor,7,5,init)
end

return Client